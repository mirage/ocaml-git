open Rresult
module Info = struct type 'a t = string end
module Hmap0 = Hmap.Make (Info)

module rec Fun : sig
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg =
    | Map : ('f, 'a) args * 'f -> 'a arg
    | Req : 'a Hmap0.key -> 'a arg
    | Opt : 'a Hmap0.key -> 'a option arg
    | Dft : 'a * 'a Hmap0.key -> 'a arg

  val req : 'a Hmap0.key -> 'a arg
  val opt : 'a Hmap0.key -> 'a option arg
  val dft : 'a Hmap0.key -> 'a -> 'a arg
  val map : ('k, 'a) args -> 'k -> 'a arg
end = struct
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg =
    | Map : ('f, 'a) args * 'f -> 'a arg
    | Req : 'a Hmap0.key -> 'a arg
    | Opt : 'a Hmap0.key -> 'a option arg
    | Dft : 'a * 'a Hmap0.key -> 'a arg

  let req value = Req value
  let opt value = Opt value
  let dft value v = Dft (v, value)
  let map args k = Map (args, k)
end

and Value : sig
  type 'a t =
    | Val : 'a -> 'a t
    | Fun : ('k, 'a option Lwt.t) Fun.args * 'k -> 'a t
end = struct
  type 'a t =
    | Val : 'a -> 'a t
    | Fun : ('k, 'a option Lwt.t) Fun.args * 'k -> 'a t
end

module Hmap = Hmap0.Make (Value)

type ctx = Hmap.t
type 'edn value = 'edn Hmap0.key

(***** Mirage_flow.S part *****)

module Implicit0 = Implicit.Make (struct
  type 'flow t = (module Mirage_flow.S with type flow = 'flow)
end)

type flow = Implicit0.t = private ..

let ( <.> ) f g x = f (g x)

type error = [ `Msg of string | `Not_found ]
type write_error = [ `Msg of string | `Closed ]

let pp_error ppf = function
  | `Msg err -> Fmt.string ppf err
  | `Not_found -> Fmt.string ppf "No connection found"

let pp_write_error ppf = function
  | `Msg err -> Fmt.string ppf err
  | `Closed -> Fmt.string ppf "Connection closed by peer"

let read flow =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.read flow >|= R.reword_error (R.msg <.> Fmt.to_to_string Flow.pp_error)

let write flow cs =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.write flow cs
  >|= R.reword_error (R.msg <.> Fmt.to_to_string Flow.pp_write_error)

let writev flow css =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.writev flow css
  >|= R.reword_error (R.msg <.> Fmt.to_to_string Flow.pp_write_error)

let close flow =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  Flow.close flow

(***** Protocol (Mirage_flow.S + connect) part *****)

type ('edn, 'flow) snd = Snd : 'flow -> ('edn, 'flow) snd [@@warning "-37"]

type _ pack =
  | Protocol :
      'edn Hmap0.key
      * 'flow Implicit0.witness
      * (module Mirage_protocol.S
           with type flow = 'flow
            and type endpoint = 'edn)
      -> ('edn, 'flow) snd pack

module Implicit1 = Implicit.Make (struct type 'v t = 'v pack end)

type ('edn, 'flow) protocol = {
  flow : 'flow Implicit0.witness;
  protocol : ('edn, 'flow) snd Implicit1.witness;
}

let register :
    type edn flow.
    name:string ->
    (module Mirage_protocol.S with type flow = flow and type endpoint = edn) ->
    edn value * (edn, flow) protocol =
 fun ~name (module Protocol) ->
  let value = Hmap0.Key.create name in
  let flow = Implicit0.inj (module Protocol) in
  let protocol = Implicit1.inj (Protocol (value, flow, (module Protocol))) in
  value, { flow; protocol }

let rec apply :
    type k res. ctx -> (k, res option Lwt.t) Fun.args -> k -> res option Lwt.t =
 fun ctx args f ->
  let open Lwt.Infix in
  let rec go : type k res. ctx -> (k, res) Fun.args -> k -> res Lwt.t =
   fun ctx -> function
    | [] -> fun x -> Lwt.return x
    | Map (args', f') :: tl ->
        fun f -> go ctx args' f' >>= fun v -> go ctx tl (f v)
    | Opt value :: tl -> fun f -> find value ctx >>= fun v -> go ctx tl (f v)
    | Dft (v, value) :: tl -> (
        fun f ->
          find value ctx >>= function
          | Some v' -> go ctx tl (f v')
          | None -> go ctx tl (f v))
    | Req value :: tl -> (
        fun f ->
          find value ctx >>= function
          | Some v -> go ctx tl (f v)
          | None -> Lwt.fail Not_found)
  in
  Lwt.catch (fun () -> go ctx args f >>= fun fiber -> fiber) @@ function
  | Not_found -> Lwt.return_none
  | exn -> Lwt.fail exn

and find : type a. a value -> ctx -> a option Lwt.t =
 fun value ctx ->
  match Hmap.find value ctx with
  | None -> Lwt.return_none
  | Some (Val v) -> Lwt.return_some v
  | Some (Fun (args, f)) -> apply ctx args f

type edn = Edn : 'edn value * 'edn -> edn

let resolve : ctx -> edn list Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  let rec go acc : Hmap.binding list -> _ = function
    | [] -> Lwt.return (List.rev acc)
    | Hmap.B (k, Val v) :: r -> go (Edn (k, v) :: acc) r
    | Hmap.B (k, Fun (args, f)) :: r -> (
        apply ctx args f >>= function
        | Some v -> go (Edn (k, v) :: acc) r
        | None -> go acc r)
  in
  go [] (Hmap.bindings ctx)

let flow_of_value :
    type edn. edn value -> edn -> (flow, [> error ]) result Lwt.t =
 fun k v ->
  let open Lwt.Infix in
  let rec go : Implicit1.pack list -> _ = function
    | [] -> Lwt.return_error `Not_found
    | Implicit1.Key (Protocol (k', (module Witness), (module Protocol))) :: r
      -> (
        match Hmap0.Key.proof k k' with
        | None -> go r
        | Some Teq -> (
            Protocol.connect v >>= function
            | Ok flow -> Lwt.return_ok (Witness.T flow)
            | Error _err -> go r))
  in
  go (Implicit1.bindings ())

let resolve : ctx -> (flow, [> error ]) result Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  resolve ctx >>= fun lst ->
  let rec go : edn list -> _ = function
    | [] -> Lwt.return_error `Not_found
    | Edn (k, v) :: r -> (
        flow_of_value k v >>= function
        | Ok _ as v -> Lwt.return v
        | Error _err -> go r)
  in
  go lst

let make ~name = Hmap0.Key.create name
let add value v ctx = Hmap.add value (Val v) ctx
let fold value args ~k ctx = Hmap.add value (Fun (args, k)) ctx
let empty = Hmap.empty
