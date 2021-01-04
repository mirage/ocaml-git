type 'a info = { name : string; root : bool }

let pp_info ppf { name; root } =
  if root then Fmt.pf ppf "<%s>" name else Fmt.string ppf name

open Rresult
module Mirage_protocol = Mirage_protocol
module Info = struct type 'a t = 'a info end
module Hmap0 = Hmap.Make (Info)

let src = Logs.Src.create "mimic" ~doc:"logs mimic's event"

module Log = (val Logs.src_log src : Logs.LOG)

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
  type priority = int option

  type 'a elt =
    | Val : priority * 'a -> 'a elt
    | Fun : priority * ('k, 'a option Lwt.t) Fun.args * 'k -> 'a elt

  type 'a t = 'a elt list
end = struct
  type priority = int option

  type 'a elt =
    | Val : priority * 'a -> 'a elt
    | Fun : priority * ('k, 'a option Lwt.t) Fun.args * 'k -> 'a elt

  type 'a t = 'a elt list
end

module Hmap = Hmap0.Make (Value)

type ctx = Hmap.t
type 'edn value = 'edn Hmap0.key

let merge ctx0 ctx1 =
  let f :
      type a.
      a value -> a Value.t option -> a Value.t option -> a Value.t option =
   fun _k lst0 lst1 ->
    match lst0, lst1 with
    | Some lst0, Some lst1 -> Some (lst0 @ lst1)
    | Some x, None | None, Some x -> Some x
    | None, None -> None
  in
  Hmap.merge { f } ctx0 ctx1

module Merge (A : sig
  val ctx : ctx
end) (B : sig
  val ctx : ctx
end) =
struct
  let ctx = merge A.ctx B.ctx
end

let add ?priority value v ctx =
  match Hmap.find value ctx with
  | Some lst -> Hmap.add value (lst @ [ Val (priority, v) ]) ctx
  | None -> Hmap.add value [ Val (priority, v) ] ctx

let fold ?priority value args ~k ctx =
  match Hmap.find value ctx with
  | Some lst -> Hmap.add value (lst @ [ Fun (priority, args, k) ]) ctx
  | None -> Hmap.add value [ Fun (priority, args, k) ] ctx

(***** Mirage_flow.S part *****)

module Implicit0 = Implicit.Make (struct
  type 'flow t = (module Mirage_flow.S with type flow = 'flow)
end)

type flow = Implicit0.t = private ..

let ( <.> ) f g x = f (g x)

type error = [ `Msg of string | `Not_found | `Cycle ]
type write_error = [ `Msg of string | `Closed ]

let pp_error ppf = function
  | `Msg err -> Fmt.string ppf err
  | `Not_found -> Fmt.string ppf "No connection found"
  | `Cycle -> Fmt.string ppf "Context contains a cycle"

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
  Flow.write flow cs >|= function
  | Error `Closed -> R.error `Closed
  | Error _ as err ->
      R.reword_error (R.msg <.> Fmt.to_to_string Flow.pp_write_error) err
  | Ok _ as v -> v

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
  let value = Hmap0.Key.create { name; root = true } in
  let flow = Implicit0.inj (module Protocol) in
  let protocol = Implicit1.inj (Protocol (value, flow, (module Protocol))) in
  value, { flow; protocol }

module type REPR = sig
  type t
  type flow += T of t
end

let repr :
    type edn flow. (edn, flow) protocol -> (module REPR with type t = flow) =
 fun { flow; _ } ->
  let (module Witness) = flow in
  let module M = struct
    include Witness

    type t = a
  end in
  (module M)

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
  | None | Some [] -> Lwt.return_none
  | Some lst ->
      (* XXX(dinosaure): priority on values, then we apply the first [Fun] *)
      let rec go fold lst =
        match fold, lst with
        | None, [] -> Lwt.return_none
        | Some (Value.Fun (_p, args, f)), [] -> apply ctx args f
        | Some (Value.Val _), [] -> assert false
        | None, (Value.Fun _ as x) :: r -> go (Some x) r
        | _, Val (_p, v) :: _ -> Lwt.return_some v
        | Some _, Fun _ :: r -> go fold r
      in
      go None lst

type edn = Edn : Value.priority * 'edn value * 'edn -> edn

type fnu =
  | Fun :
      Value.priority * 'edn value * ('k, 'edn option Lwt.t) Fun.args * 'k
      -> fnu

type dep = Dep : 'edn value -> dep

let pp_fnu ppf (Fun (_, dep, _, _)) =
  Fmt.pf ppf "%a" pp_info (Hmap0.Key.info dep)

module Sort = struct
  type t =
    | Val : Value.priority * 'edn value * 'edn -> t
    | Fun :
        Value.priority * 'edn value * ('k, 'edn option Lwt.t) Fun.args * 'k
        -> t

  let pp ppf = function
    | Val (_, k, _) -> pp_info ppf (Hmap0.Key.info k)
    | Fun (_, k, _, _) -> pp_info ppf (Hmap0.Key.info k)
end

let partition bindings =
  let rec go leafs nodes = function
    | [] -> List.rev leafs, List.rev nodes
    | Hmap.B (_, []) :: r -> go leafs nodes r
    | Hmap.B (k, Val (p, v) :: tl) :: r ->
        go (Sort.Val (p, k, v) :: leafs) nodes (Hmap.B (k, tl) :: r)
    | Hmap.B (k, Fun (p, args, f) :: tl) :: r ->
        go leafs (Fun (p, k, args, f) :: nodes) (Hmap.B (k, tl) :: r)
  in
  go [] [] bindings

let exists k bindings =
  let rec go k = function
    | [] -> false
    | Hmap.B (k', _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go k r)
  in
  go k bindings

let dependencies (Fun (_, _, args, _)) bindings =
  let rec go : type k r. _ -> (k, r) Fun.args -> _ =
   fun acc -> function
    | Fun.Req dep :: r -> go (Dep dep :: acc) r
    | Fun.Opt dep :: r when exists dep bindings -> go (Dep dep :: acc) r
    | Fun.Dft (_, dep) :: r when exists dep bindings -> go (Dep dep :: acc) r
    | _ :: r -> go acc r
    | [] -> List.rev acc
  in
  go [] args

let exists leafs (Dep k) =
  let rec go = function
    | [] -> false
    | Sort.Val (_, k', _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go r)
    | Sort.Fun (_, k', _, _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go r)
  in
  go leafs

let sort bindings =
  let rec go acc later todo progress =
    match todo, later with
    | [], [] -> List.rev acc
    | [], _ when progress -> go acc [] later false
    | [], later ->
        (* TODO(dinosaure): check, at least, one root in [acc]. *)
        Log.warn (fun m ->
            m "Found a solution only for: @[<hov>%a@]."
              Fmt.(Dump.list Sort.pp)
              acc);
        Log.warn (fun m ->
            m "Unsolvable values: @[<hov>%a@]." Fmt.(Dump.list pp_fnu) later);
        List.rev acc
    | (Fun (p, k, args, f) as x) :: xs, _ ->
        let deps = dependencies x bindings in
        let available = List.for_all (exists acc) deps in
        if available then go (Sort.Fun (p, k, args, f) :: acc) later xs true
        else go acc (x :: later) xs progress
  in
  let leafs, nodes = partition bindings in
  Log.debug (fun m -> m "Partition done.");
  Log.debug (fun m -> m "Nodes: @[<hov>%a@]." Fmt.(Dump.list pp_fnu) nodes);
  go leafs [] nodes false

let resolve : ctx -> (edn list, [> `Cycle ]) result Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  let rec go ctx acc : Sort.t list -> _ = function
    | [] -> Lwt.return_ok (List.rev acc)
    | Sort.Val (p, k, v) :: r -> go ctx (Edn (p, k, v) :: acc) r
    | Sort.Fun (p, k, args, f) :: r -> (
        apply ctx args f >>= function
        | Some v -> go (add k v ctx) (Edn (p, k, v) :: acc) r
        | None -> go ctx acc r)
  in
  let ordered_bindings = sort (Hmap.bindings ctx) in
  go ctx [] ordered_bindings

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

let inf = -1
and sup = 1

let priority_compare (Edn (p0, _, _)) (Edn (p1, _, _)) =
  match p0, p1 with
  | None, None -> 0
  | Some p0, Some p1 -> p0 - p1
  | None, Some _ -> inf
  | Some _, None -> sup

let resolve : ctx -> (flow, [> error ]) result Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  let rec go : edn list -> _ = function
    | [] -> Lwt.return_error `Not_found
    | Edn (_, k, v) :: r -> (
        flow_of_value k v >>= function
        | Ok _ as v -> Lwt.return v
        | Error _err -> go r)
  in
  resolve ctx >>= function
  | Ok lst ->
      let lst = List.stable_sort priority_compare lst in
      go lst
  | Error _ as err -> Lwt.return err

let make ~name = Hmap0.Key.create { name; root = false }
let empty = Hmap.empty

let get value ctx =
  match Hmap.find value ctx with
  | Some lst ->
      let rec first = function
        | [] -> None
        | Value.Val (_, v) :: _ -> Some v
        | _ :: r -> first r
      in
      first lst
  | None -> None
