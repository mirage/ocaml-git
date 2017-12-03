(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
let ( >!= ) = Lwt_result.bind_lwt_err
let ( >?= ) = Lwt_result.bind

let src = Logs.Src.create "git.sync" ~doc:"logs git's sync event"
module Log = (val Logs.src_log src: Logs.LOG)

module Option = struct
  let mem v x ~equal = match v with Some x' -> equal x x' | None -> false
  let value_exn v ~error =
    match v with Some v -> v | None -> raise (Invalid_argument error)
end

module Default = struct
  let capabilities =
    [ `Multi_ack_detailed
    ; `Thin_pack
    ; `Side_band_64k
    ; `Ofs_delta
    ; `Agent "git/2.0.0"
    ; `Report_status
    ; `No_done ]
end


module type NET = sig
  type socket
  val read  : socket -> Bytes.t -> int -> int -> int Lwt.t
  val write : socket -> Bytes.t -> int -> int -> int Lwt.t
  val socket: string -> int -> socket Lwt.t
  val close : socket -> unit Lwt.t
end

module type S = sig
  module Store: Minimal.S
  module Net: NET
  module Client: Smart.CLIENT with module Hash = Store.Hash

  type error =
    [ `SmartPack of string
    | `Pack      of Store.Pack.error
    | `Clone     of string
    | `Fetch     of string
    | `Ls        of string
    | `Push      of string
    | `Ref       of Store.Ref.error
    | `Not_found ]

  val pp_error: error Fmt.t

  type command =
    [ `Create of (Store.Hash.t * string)
    | `Delete of (Store.Hash.t * string)
    | `Update of (Store.Hash.t * Store.Hash.t * string) ]

  val push :
    Store.t
    -> push:(Store.t -> (Store.Hash.t * string * bool) list -> (Store.Hash.t list * command list) Lwt.t)
    -> ?port:int
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> ((string, string * string) result list, error) result Lwt.t

  val ls :
    Store.t
    -> ?port:int
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> ((Store.Hash.t * string * bool) list, error) result Lwt.t

  val fetch_ext:
    Store.t
    -> ?shallow:Store.Hash.t list
    -> ?capabilities:Capability.t list
    -> notify:(Client.Decoder.shallow_update -> unit Lwt.t)
    -> negociate:((Client.Decoder.acks -> 'state -> ([ `Ready | `Done | `Again of Store.Hash.t list ] * 'state) Lwt.t) * 'state)
    -> has:Store.Hash.t list
    -> want:((Store.Hash.t * string * bool) list -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[ `Depth of int | `Timestamp of int64 | `Ref of string ]
    -> ?port:int
    -> string
    -> string
    -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

  val clone_ext:
    Store.t
    -> ?port:int
    -> ?reference:Store.Reference.t
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> (Store.Hash.t, error) result Lwt.t


  val fetch_all: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    Uri.t ->
    (unit, error) result Lwt.t

  val fetch_one: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    reference:Reference.t ->
    Uri.t -> (unit, error) result Lwt.t

  val clone: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    reference:Reference.t -> Uri.t ->
    (unit, error) result Lwt.t

  val update: Store.t -> ?capabilities:Capability.t list ->
    reference:Reference.t -> Uri.t ->
    ((Reference.t, Reference.t * string) result list, error) result Lwt.t

end

module Make (N: NET) (S: Minimal.S) = struct
  module Store        = S
  module Net          = N

  module Client = Smart.Client(Store.Hash)
  module Hash = Store.Hash
  module Inflate = Store.Inflate
  module Deflate = Store.Deflate
  module Revision = Revision.Make(Store)
  module PACKEncoder = Pack.MakePACKEncoder(Hash)(Deflate)

  type error =
    [ `SmartPack of string
    | `Pack      of Store.Pack.error
    | `Clone     of string
    | `Fetch     of string
    | `Ls        of string
    | `Push      of string
    | `Ref       of S.Ref.error
    | `Not_found ]

  let pp_error ppf = function
    | `SmartPack err -> Helper.ppe ~name:"`SmartPack" Fmt.string ppf err
    | `Pack err       -> Helper.ppe ~name:"`Pack" Store.Pack.pp_error ppf err
    | `Clone err      -> Helper.ppe ~name:"`Clone" Fmt.string ppf err
    | `Fetch err      -> Helper.ppe ~name:"`Fetch" Fmt.string ppf err
    | `Push err       -> Helper.ppe ~name:"`Push" Fmt.string ppf err
    | `Ls err         -> Helper.ppe ~name:"`Ls" Fmt.string ppf err
    | `Ref err        -> Helper.ppe ~name:"`Ref" S.Ref.pp_error ppf err
    | `Not_found      -> Fmt.string ppf "`Not_found"

  type command =
    [ `Create of (Store.Hash.t * string)
    | `Delete of (Store.Hash.t * string)
    | `Update of (Store.Hash.t * Store.Hash.t * string) ]

  type t =
    { socket: Net.socket
    ; input : Bytes.t
    ; output: Bytes.t
    ; ctx   : Client.context
    ; capabilities: Capability.t list }

  let err_unexpected_result result =
    let buf = Buffer.create 64 in
    let ppf = Fmt.with_buffer buf in

    Fmt.pf ppf "Unexpected result: %a%!" (Fmt.hvbox Client.pp_result) result;
    Buffer.contents buf

  let rec process t result =
    match result with
    | `Read (buffer, off, len, continue) ->
      Net.read t.socket t.input 0 len >>= fun len ->
      Cstruct.blit_from_bytes t.input 0 buffer off len;
      process t (continue len)
    | `Write (buffer, off, len, continue) ->
      Cstruct.blit_to_bytes buffer off t.output 0 len;
      Net.write t.socket t.output 0 len >>= fun n ->
      process t (continue n)
    | `Error (err, buf, committed) ->
      let raw = Cstruct.sub buf committed (Cstruct.len buf - committed) in
      Log.err (fun l -> l ~header:"process" "Retrieve an error (%a) on: %a."
                  Client.Decoder.pp_error err
                  (Fmt.hvbox (Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len)) raw);
      assert false (* TODO *)
    | #Client.result as result ->
      Lwt.return result

  let packer ?(window = `Object 10) ?(depth = 50) git ~ofs_delta:_ remote commands =
    let open Client in
    let commands' =
      (List.map (fun (hash, refname, _) -> Encoder.Delete (hash, refname)) remote)
      @ commands
    in

    (* XXX(dinosaure): we don't want to delete remote references but
       we want to exclude any commit already stored remotely. Se, we «
       delete » remote references from the result set. *)

    Lwt_list.fold_left_s
      (fun acc -> function
         | Encoder.Create _ -> Lwt.return acc
         | Encoder.Update (hash, _, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Encoder.Delete (hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc)
      Store.Hash.Set.empty commands'
    >>= fun negative ->
    Lwt_list.fold_left_s
      (fun acc -> function
         | Encoder.Create (hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Encoder.Update (_, hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Encoder.Delete _ -> Lwt.return acc)
      Store.Hash.Set.empty commands
    >|= (fun positive -> Revision.Range.E.diff positive negative)
    >>= fun elements ->
    Lwt_list.fold_left_s
      (fun acc commit ->
         Store.fold git
           (fun acc ?name:_ ~length:_ _ value -> Lwt.return (value :: acc))
           ~path:(Fpath.v "/") acc commit)
      [] (Store.Hash.Set.elements elements)
    >>= fun entries -> Store.Pack.make git ~window ~depth entries

  module Pack = struct
    let default_stdout raw =
      Log.info (fun l -> l ~header:"populate:stdout" "%S" (Cstruct.to_string raw));
      Lwt.return ()

    let default_stderr raw =
      Log.err (fun l -> l ~header:"populate:stderr" "%S" (Cstruct.to_string raw));
      Lwt.return ()

    let populate git ?(stdout = default_stdout) ?(stderr = default_stderr) ctx first =
      let stream, push = Lwt_stream.create () in

      let cstruct_copy cs =
        let ln = Cstruct.len cs in
        let rs = Cstruct.create ln in
        Cstruct.blit cs 0 rs 0 ln;
        rs
      in

      let rec dispatch ctx = function
        | `PACK (`Out raw) ->
          stdout raw >>= fun () ->
          Client.run ctx.ctx `ReceivePACK |> process ctx >>= dispatch ctx
        | `PACK (`Err raw) ->
          stderr raw >>= fun () ->
          Client.run ctx.ctx `ReceivePACK |> process ctx >>= dispatch ctx
        | `PACK (`Raw raw) ->
          push (Some (cstruct_copy raw));
          Client.run ctx.ctx `ReceivePACK |> process ctx >>= dispatch ctx
        | `PACK `End ->
          push None;
          Lwt.return (Ok ())
        | result -> Lwt.return (Error (`SmartPack (err_unexpected_result result)))
      in

      dispatch ctx first >?= fun () ->
      (Store.Pack.from git (fun () -> Lwt_stream.get stream)
       >!= fun err -> Lwt.return (`Pack err))
  end

  let rec clone_handler git reference t r =
    match r with
    | `Negociation _ ->
      Client.run t.ctx `Done
      |> process t
      >>= clone_handler git reference t
    | `NegociationResult _ ->
      Client.run t.ctx `ReceivePACK
      |> process t
      >>= Pack.populate git t
      >>= (function
          | Ok (hash, _) -> Lwt.return (Ok hash)
          | Error _ as err -> Lwt.return err)
    | `ShallowUpdate _ ->
      Client.run t.ctx (`Has []) |> process t
      >>= clone_handler git reference t
    | `Refs refs ->
      (try
         let (hash_head, _, _) =
           List.find
             (fun (_, refname, peeled) -> Store.Reference.(equal reference (of_string refname)) && not peeled)
             refs.Client.Decoder.refs
         in
         Client.run t.ctx (`UploadRequest { Client.Encoder.want = hash_head, [ hash_head ]
                                          ; capabilities = t.capabilities
                                          ; shallow = []
                                          ; deep = None })
         |> process t
         >>= clone_handler git reference t
       with Not_found ->
         Client.run t.ctx `Flush
         |> process t
         >>= function `Flush -> Lwt.return (Error `Not_found)
                    | result -> Lwt.return (Error (`Clone (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Clone (err_unexpected_result result)))

  let ls_handler _ t r =
    match r with
    | `Refs refs ->
      Client.run t.ctx `Flush
      |> process t
      >>= (function `Flush -> Lwt.return (Ok refs.Client.Decoder.refs)
                  | result -> Lwt.return (Error (`Ls (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))

  let fetch_handler git ?(shallow = []) ~notify ~negociate:(fn, state) ~has ~want ?deepen t r =
    let pack asked t =
      Client.run t.ctx `ReceivePACK
      |> process t
      >>= Pack.populate git t
      >>= function
      | Ok (_, n) -> Lwt.return (Ok (asked, n))
      | Error err -> Lwt.return (Error err)
    in

    let rec aux t asked state = function
      | `ShallowUpdate shallow_update ->
        notify shallow_update >>= fun () ->
        Client.run t.ctx (`Has has) |> process t >>= aux t asked state
      | `Negociation acks ->
        Log.debug (fun l -> l ~header:"fetch_handler" "Retrieve the negotiation: %a."
                      (Fmt.hvbox Client.Decoder.pp_acks) acks);

        fn acks state >>=
        (function
          | `Ready, _ ->
            Log.debug (fun l -> l ~header:"fetch_handler" "Retrieve `Ready ACK from negotiation engine.");
            Client.run t.ctx `Done |> process t >>= aux t asked state
          | `Done, state ->
            Log.debug (fun l -> l ~header:"fetch_handler" "Retrieve `Done ACK from negotiation engine.");
            Client.run t.ctx `Done |> process t >>= aux t asked state
          | `Again has, state ->
            Log.debug (fun l -> l ~header:"fetch_handler" "Retrieve `Again ACK from negotiation engine.");
            Client.run t.ctx (`Has has) |> process t >>= aux t asked state)
      | `NegociationResult _ ->
        Log.debug (fun l -> l ~header:"fetch_handler" "Retrieve a negotiation result.");
        pack asked t
      | `Refs refs ->
        want refs.Client.Decoder.refs >>=
        (function
          | first :: rest ->
            Client.run t.ctx
              (`UploadRequest { Client.Encoder.want = snd first, List.map snd rest
                              ; capabilities = t.capabilities
                              ; shallow
                              ; deep = deepen })
            |> process t
            >>= aux t (first :: rest) state
          | [] -> Client.run t.ctx `Flush
                  |> process t
            >>= (function `Flush -> Lwt.return (Ok ([], 0))
                        (* XXX(dinosaure): better return? *)
                        | result -> Lwt.return (Error (`Fetch (err_unexpected_result result)))))
      | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))
    in

    aux t [] state r

  let push_handler git ~push t r =
    let send_pack stream t r =
      let rec go ?keep t r =
        let consume ?keep dst =
          match keep with
          | Some keep ->
            let n = min (Cstruct.len keep) (Cstruct.len dst) in
            Cstruct.blit keep 0 dst 0 n;
            let keep = Cstruct.shift keep n in
            if Cstruct.len keep > 0
            then Lwt.return (`Continue (Some keep, n))
            else Lwt.return (`Continue (None, n))
          | None ->
            stream () >>= function
            | Some keep ->
              let n = min (Cstruct.len keep) (Cstruct.len dst) in
              Cstruct.blit keep 0 dst 0 n;
              let keep = Cstruct.shift keep n in
              if Cstruct.len keep > 0
              then Lwt.return (`Continue (Some keep, n))
              else Lwt.return (`Continue (None, n))
            | None -> Lwt.return `Finish
        in

        match r with
        | `ReadyPACK dst ->
          (consume ?keep dst >>= function
            | `Continue (keep, n) ->
              Client.run t.ctx (`SendPACK n)
              |> process t
              >>= go ?keep t
            | `Finish ->
              Client.run t.ctx `FinishPACK
              |> process t
              >>= go t)
        | `Nothing -> Lwt.return (Ok [])
        | `ReportStatus { Client.Decoder.unpack = Ok (); commands; } ->
          Lwt.return (Ok commands)
        | `ReportStatus { Client.Decoder.unpack = Error err; _ } ->
          Lwt.return (Error (`Push err))
        | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
      in

      go t r
    in

    let rec aux t refs commands = function
      | `Refs refs ->
        Log.debug (fun l -> l ~header:"push_handler" "Receiving reference: %a."
                      (Fmt.hvbox Client.Decoder.pp_advertised_refs) refs);

        let capabilities =
          List.filter (function
              | `Report_status | `Delete_refs | `Ofs_delta | `Push_options | `Agent _ | `Side_band | `Side_band_64k -> true
              | _ -> false)
            t.capabilities
        in

        (push git refs.Client.Decoder.refs >>= function
          | (_,  []) ->
            Client.run t.ctx `Flush
            |> process t
            >>= (function `Flush -> Lwt.return (Ok [])
                        | result -> Lwt.return (Error (`Push (err_unexpected_result result))))
          | (shallow, commands) ->
            Log.debug (fun l ->
                let pp_command ppf = function
                  | `Create (hash, refname) -> Fmt.pf ppf "(`Create (%a, %s))" S.Hash.pp hash refname
                  | `Delete (hash, refname) -> Fmt.pf ppf "(`Delete (%a, %s))" S.Hash.pp hash refname
                  | `Update (_of, _to, refname) -> Fmt.pf ppf "(`Update (of:%a, to:%a, %s))" S.Hash.pp _of S.Hash.pp _to refname
                in

                l ~header:"push_handler" "Sending command(s): %a."
                  (Fmt.hvbox (Fmt.Dump.list pp_command)) commands);

            let x, r =
              List.map (function
                  | `Create (hash, refname) -> Client.Encoder.Create (hash, refname)
                  | `Delete (hash, refname) -> Client.Encoder.Delete (hash, refname)
                  | `Update (_of, _to, refname) -> Client.Encoder.Update (_of, _to, refname))
                commands
              |> fun commands -> List.hd commands, List.tl commands
            in

            Client.run t.ctx (`UpdateRequest { Client.Encoder.shallow
                                             ; requests = Client.Encoder.L (x, r)
                                             ; capabilities })
            |> process t
            >>= aux t (Some refs.Client.Decoder.refs) (Some (x :: r)))
      | `ReadyPACK _ as result ->
        Log.debug (fun l -> l ~header:"push_handler" "The server is ready to receive the PACK file.");

        let ofs_delta = List.exists ((=) `Ofs_delta) (Client.capabilities t.ctx) in
        let commands = match commands with Some commands -> commands | None -> assert false in
        let refs     = match refs with Some refs -> refs | None -> assert false in

        (* XXX(dinosaure): in this case, we can use GADT to describe the
           protocol by the session-type (like, [`UpdateRequest] makes a
           [`] response). So, we can constraint some assertions about
           the context when we catch [`ReadyPACK].

           One of this assertion is about the [commands] variable, which one is
           previously specified. So, the [None] value can not be catch and it's
           why we have an [assert false]. *)

        packer git ~ofs_delta refs commands >>= (function
            | Ok (stream, _) ->
              send_pack stream t result
            | Error err -> Lwt.return (Error (`Pack err)))
      | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
    in

    aux t None None r

  let push git ~push ?(port = 9418) ?(capabilities=Default.capabilities) host path =
    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `ReceivePack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx
            ; capabilities }
    in
    Log.debug (fun l -> l ~header:"push" "Start to process the flow");

    process t state
    >>= push_handler git ~push t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  let ls git ?(port = 9418) ?(capabilities=Default.capabilities) host path =
    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx
            ; capabilities }
    in
    Log.debug (fun l -> l ~header:"ls" "Start to process the flow.");

    process t state
    >>= ls_handler git t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  let fetch_ext git ?(shallow = []) ?(capabilities=Default.capabilities)
      ~notify ~negociate ~has ~want ?deepen ?(port = 9418) host path =
    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx
            ; capabilities }
    in
    Log.debug (fun l -> l ~header:"fetch" "Start to process the flow.");

    process t state
    >>= fetch_handler git ~shallow ~notify ~negociate ~has ~want ?deepen t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  let clone_ext git ?(port = 9418)
      ?(reference = Store.Reference.master)
      ?(capabilities=Default.capabilities) host path =
    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx
            ; capabilities }
    in
    Log.debug (fun l -> l ~header:"clone" "Start to process the flow.");

    process t state
    >>= clone_handler git reference t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  module N = Negociator.Make(S)

  exception Jump of S.Ref.error

  let fetch_all t ?locks ?capabilities repository =

    N.find_common t >>= fun (has, state, continue) ->
    let continue { Client.Decoder.acks; shallow; unshallow } state =
      continue { Negociator.acks; shallow; unshallow } state
    in

    let want refs =
      Lwt_list.filter_map_p
        (function (hash, refname, false) ->
           let reference = S.Reference.of_string refname in
           Lwt.return (Some (reference, hash))
                | _ -> Lwt.return None)
        refs
    in

    let notify _ = Lwt.return () in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in
    fetch_ext t ?port:(Uri.port repository) ?capabilities
      ~notify ~negociate:(continue, state) ~has ~want host
      (Uri.path_and_query repository)
    >?= fun (lst, _) ->
    Lwt.catch
      (fun () ->
         Lwt_list.iter_s
           (fun (reference, hash) ->
              Log.debug (fun l -> l ~header:"fetch_all" "Update reference %a to %a."
                            S.Reference.pp reference S.Hash.pp hash);

              S.Ref.write t ?locks reference (S.Reference.Hash hash) >>= function
              | Ok _ -> Lwt.return ()
              | Error err -> Lwt.fail (Jump err))
           lst >>= fun () -> Lwt.return (Ok ()))
      (function Jump err -> Lwt.return (Error (`Ref err))
              | exn -> Lwt.fail exn) (* XXX(dinosaure): should never happen. *)

  let fetch_one t ?locks ?capabilities ~reference repository =
    N.find_common t >>= fun (has, state, continue) ->
    let continue { Client.Decoder.acks; shallow; unshallow } state =
      continue { Negociator.acks; shallow; unshallow } state
    in

    let want refs =
      Lwt_list.filter_map_p
        (function (hash, refname, false) ->
           let reference' = S.Reference.of_string refname in
           if S.Reference.equal reference reference'
           then Lwt.return (Some (reference, hash))
           else Lwt.return None
                | _ -> Lwt.return None)
        refs
    in

    let notify _ = Lwt.return () in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in

    fetch_ext t ?port:(Uri.port repository) ?capabilities
      ~notify ~negociate:(continue, state) ~has ~want host
      (Uri.path_and_query repository)
    >?= function
    | [ (reference', hash') ], _ ->
      Log.debug (fun l -> l ~header:"fetch_one" "Update reference %a to %a."
                    S.Reference.pp reference' S.Hash.pp hash');

      S.Ref.write t ?locks reference' (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`Ref err))
    | _ -> Lwt.return (Ok ()) (* TODO *)

  let clone t ?locks ?capabilities ~reference repository =
    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in
    clone_ext t ?port:(Uri.port repository) ?capabilities
      ~reference host (Uri.path_and_query repository)
    >?= function
    | hash' ->
      Log.debug (fun l ->
          l ~header:"easy_clone" "Update reference %a to %a."
            S.Reference.pp reference S.Hash.pp hash');

      S.Ref.write t ?locks reference (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`Ref err))
      >?= fun () -> S.Ref.write t ?locks S.Reference.head (S.Reference.Ref reference)
      >!= (fun err -> Lwt.return (`Ref err))

  let update t ?capabilities ~reference repository =
    let push_handler git remote_refs =
      S.Ref.list git >>=
      Lwt_list.find_s (fun (reference', _) -> Lwt.return S.Reference.(equal reference reference')) >>= fun (_, local_hash) ->
      Lwt_list.find_s (function (_, refname, false) -> Lwt.return S.Reference.(equal reference (of_string refname))
                              | (_, _, true) -> Lwt.return false) remote_refs >>= fun (remote_hash, remote_refname, _) ->

      if S.Hash.equal local_hash remote_hash
      then Lwt.return ([], [])
      else Lwt.return ([], [ `Update (remote_hash, local_hash, remote_refname) ])
    in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in
    push t ~push:push_handler ?port:(Uri.port repository) ?capabilities
      host (Uri.path_and_query repository)
    >?= fun lst ->
    Lwt_result.ok (Lwt_list.map_p (function
          | Ok refname -> Lwt.return (Ok (S.Reference.of_string refname))
          | Error (refname, err) ->
            Lwt.return (Error (S.Reference.of_string refname, err))
        ) lst)

end
