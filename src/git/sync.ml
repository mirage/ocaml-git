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

module type CAPABILITIES =
sig
  val default : Capability.t list
end

module type NET =
sig
  type socket

  val read   : socket -> Bytes.t -> int -> int -> int Lwt.t
  val write  : socket -> Bytes.t -> int -> int -> int Lwt.t
  val socket : string -> int -> socket Lwt.t
  val close  : socket -> unit Lwt.t
end

module Make
    (Net : NET)
    (Store : Minimal.S with type Hash.Digest.buffer = Cstruct.t
                        and type Hash.hex = string)
    (Capabilities : CAPABILITIES)
= struct
  module Client = Smart.Client(Store.Hash)
  module Hash = Store.Hash
  module Inflate = Store.Inflate
  module Deflate = Store.Deflate
  module Path = Store.Path
  module Revision = Revision.Make(Store)

  module PACKEncoder = Pack.MakePACKEncoder
      (Hash)
      (Deflate)

  module Log =
  struct
    let src = Logs.Src.create "git.sync" ~doc:"logs git's sync event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type error =
    [ `SmartPack of string
    | `Pack      of PACKEncoder.error
    | `Clone     of string
    | `Ls        of string
    | `Push      of string
    | `Not_found ]

  let pp_error ppf = function
    | `SmartPack err           -> Helper.ppe ~name:"`SmartPack" Fmt.string ppf err
    | `Pack err                -> Helper.ppe ~name:"`Pack" (Fmt.hvbox PACKEncoder.pp_error) ppf err
    | `Clone err               -> Helper.ppe ~name:"`Clone" Fmt.string ppf err
    | `Push err                -> Helper.ppe ~name:"`Push" Fmt.string ppf err
    | `Ls err                  -> Helper.ppe ~name:"`Ls" Fmt.string ppf err
    | `Not_found               -> Fmt.string ppf "`Not_found"

  type t =
    { socket : Net.socket
    ; input  : Bytes.t
    ; output : Bytes.t
    ; ctx    : Client.context }

  let err_unexpected_result result =
    let buf = Buffer.create 64 in
    let ppf = Fmt.with_buffer buf in

    Fmt.pf ppf "Unexpected result: %a%!" (Fmt.hvbox Client.pp_result) result;
    Buffer.contents buf

  let rec process t result =
    let open Lwt.Infix in

    match result with
    | `Read (buffer, off, len, continue) ->
      Net.read t.socket t.input 0 len >>= fun n ->
      Cstruct.blit_from_bytes t.input 0 buffer off len;
      process t (continue n)
    | `Write (buffer, off, len, continue) ->
      Cstruct.blit_to_bytes buffer off t.output 0 len;
      Net.write t.socket t.output 0 len >>= fun n ->
      process t (continue n)
    | `Error _ ->
      assert false (* TODO *)
    | #Client.result as result -> Lwt.return result

  let option_map f = function
    | Some v -> Some (f v)
    | None -> None

  let packer ~window ~depth git ~ofs_delta:_ remote commands =
    let open Lwt.Infix in

    let commands' =
      (List.map (fun (hash, refname, _) -> Client.Encoder.Delete (hash, refname)) remote
       @ commands)
    in

    (* XXX(dinosaure): we don't want to delete remote references but we want to
       exclude any commit already stored remotely. So, we « delete » remote
       references from the result set. *)

    Lwt_list.fold_left_s
      (fun acc -> function
         | Client.Encoder.Create _ -> Lwt.return acc
         | Client.Encoder.Update (hash, _, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Client.Encoder.Delete (hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc)
      Store.Hash.Set.empty commands'
    >>= fun negative ->
    Lwt_list.fold_left_s
      (fun acc -> function
         | Client.Encoder.Create (hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Client.Encoder.Update (_, hash, _) ->
           Revision.(Range.normalize git (Range.Include (from_hash hash)))
           >|= Store.Hash.Set.union acc
         | Client.Encoder.Delete _ -> Lwt.return acc)
      Store.Hash.Set.empty commands'
    >|= (fun positive -> Revision.Range.E.diff positive negative)
    >>= fun elements ->
    Lwt_list.fold_left_s
      (fun acc commit ->
         Format.printf "send commit: %a\n%!" Store.Hash.pp commit;

         Store.fold git
           (fun (acc, max_length) ?name ~length hash -> function
              | Store.Value.Commit _ ->
                PACKEncoder.Entry.make hash
                  Pack.Kind.Commit
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Tree _ ->
                PACKEncoder.Entry.make hash
                  ?name:(option_map Path.to_string name)
                  Pack.Kind.Tree
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Blob _ ->
                PACKEncoder.Entry.make hash
                  ?name:(option_map Path.to_string name)
                  Pack.Kind.Blob
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Tag _ ->
                PACKEncoder.Entry.make hash
                  Pack.Kind.Tag
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length))
           ~path:(Path.v "/") acc commit)
      (Store.Hash.Map.empty, 0L) (Store.Hash.Set.elements elements)
    >>= fun (entries, _) ->
    PACKEncoder.Delta.deltas
      ~memory:false
      (Store.Hash.Map.bindings entries |> List.map snd)
      (fun hash -> Store.read_inflated git hash >|= function Some (_, raw) -> Some raw | None -> None)
      (fun _ -> false) (* TODO *)
      window depth
    >|= function
    | Ok lst ->
      let htmp = Cstruct.create 0x8000 in
      Ok (PACKEncoder.default htmp lst)
    | Error (PACKEncoder.Delta.Invalid_hash hash) -> Error (`Pack (PACKEncoder.Invalid_hash hash))

  module Pack =
  struct
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

      let open Lwt.Infix in

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

      let open Lwt_result in

      let ( >!= ) = Lwt_result.bind_lwt_err in

      dispatch ctx first >>= fun () ->
      (Store.Pack.from git (fun () -> Lwt_stream.get stream)
       >!= fun err -> Lwt.return (`StorePack err))
  end

  let rec clone_handler git t r =
    let open Lwt.Infix in

    match r with
    | `Negociation _ ->
      Client.run t.ctx `Done
      |> process t
      >>= clone_handler git t
    | `NegociationResult _ ->
      Client.run t.ctx `ReceivePACK
      |> process t
      >>= Pack.populate git t
    | `ShallowUpdate _ ->
      Client.run t.ctx (`Has []) |> process t >>= clone_handler git t
    | `Refs refs ->
      (try
         let (hash_head, _, _) =
           List.find
             (fun (_, refname, peeled) -> refname = "HEAD" && not peeled)
             refs.Client.Decoder.refs
         in
         Client.run t.ctx (`UploadRequest { Client.Encoder.want = hash_head, [ hash_head ]
                                          ; capabilities = Capabilities.default
                                          ; shallow = []
                                          ; deep = None })
         |> process t
         >>= clone_handler git t
       with Not_found ->
         Client.run t.ctx `Flush
         |> process t
         >>= function `Flush -> Lwt.return (Error `Not_found)
                    | result -> Lwt.return (Error (`Clone (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Clone (err_unexpected_result result)))

  let ls_handler _ t r =
    let open Lwt.Infix in

    match r with
    | `Refs refs ->
      Client.run t.ctx `Flush
      |> process t
      >>= (function `Flush -> Lwt.return (Ok refs.Client.Decoder.refs)
                  | result -> Lwt.return (Error (`Ls (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))

  let fetch_handler git ?(shallow = []) ~notify ~negociate:(fn, state) ~has ~want ?deepen ~thin:_ t r =
    let open Lwt.Infix in

    let pack t =
      Client.run t.ctx `ReceivePACK
      |> process t
      >>= Pack.populate git t
    in

    let rec aux t state = function
      | `ShallowUpdate shallow_update ->
        notify shallow_update >>= fun () ->
        Client.run t.ctx (`Has has) |> process t >>= aux t state
      | `Negociation acks ->
        fn acks state >>=
        (function
          | `Ready, _ -> pack t
          | `Done, state ->
            Client.run t.ctx `Done |> process t >>= aux t state
          | `Again has, state ->
            Client.run t.ctx (`Has has) |> process t >>= aux t state)
      | `NegociationResult _ -> pack t
      | `Refs refs ->
        want refs.Client.Decoder.refs >>=
        (function
          | first :: rest ->
            Client.run t.ctx (`UploadRequest { Client.Encoder.want = first, rest
                                             ; capabilities = Capabilities.default
                                             ; shallow
                                             ; deep = deepen })
            |> process t
            >>= aux t state
          | [] -> Client.run t.ctx `Flush
                  |> process t
            >>= (function `Flush -> Lwt.return (Ok (Hash.of_string (String.make (Hash.Digest.length * 2) '0'), 0))
                        (* XXX(dinosaure): better return? *)
                        | result -> Lwt.return (Error (`Fetch (err_unexpected_result result)))))
      | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))
    in

    aux t state r

  let push_handler git ~push ~packer t r =
    let open Lwt.Infix in

    let empty = Cstruct.create 0 in
    let option_value ~default = function Some v -> v | None -> default in

    let rec pack src state t r =
      let rec go src dst state t =
        match PACKEncoder.eval (option_value ~default:empty src) dst state with
        | `Flush state -> Lwt.return (Ok (`Continue (state, src)))
        | `End (state, _) ->
          (if PACKEncoder.used_out state = 0
           then Lwt.return (Ok `Finish)
           else Lwt.return (Ok (`Continue (state, src))))
        | `Error (_, err) -> Lwt.return (Error (`Pack err))
        | `Await state ->
          (match src with
           | Some _ ->
             Lwt.return (Ok (None, PACKEncoder.finish state))
           | None ->
             let hash = PACKEncoder.expect state in

             Store.read_inflated git hash >>= function
             | Some (_, raw) -> Lwt.return (Ok (Some raw, PACKEncoder.refill 0 (Cstruct.len raw) state))
             | None -> Lwt.return (Error (`Pack (PACKEncoder.Invalid_hash hash))))
          >>= function Ok (src, state) -> go src dst state t
                     | Error _ as err -> Lwt.return err
      in

      match r with
      | `ReadyPACK dst ->
        go src dst (PACKEncoder.flush 0 (Cstruct.len dst) state) t >>= (function
            | Ok (`Continue (state, src)) ->
              Client.run t.ctx (`SendPACK (PACKEncoder.used_out state))
              |> process t
              >>= pack src state t
            | Ok `Finish ->
              Client.run t.ctx `FinishPACK
              |> process t
              >>= pack src state t
            | Error (`Pack _ as err) -> Lwt.return (Error err))
      | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
    in

    let rec aux t refs commands = function
      | `Refs refs ->
        let capabilities =
          List.filter (function
              | `Report_status | `Delete_refs | `Ofs_delta | `Push_options | `Agent _ | `Side_band | `Side_band_64k -> true
              | _ -> false)
            Capabilities.default
        in

        (push git refs.Client.Decoder.refs >>= function
          | (_,  []) ->
            Client.run t.ctx `Flush
            |> process t
            >>= (function `Flush -> Lwt.return (Ok ())
                        | result -> Lwt.return (Error (`Push (err_unexpected_result result))))
          | (shallow, (x :: r as commands)) ->
            Client.run t.ctx (`UpdateRequest { Client.Encoder.shallow
                                             ; requests = Client.Encoder.L (x, r)
                                             ; capabilities })
            |> process t
            >>= aux t (Some refs.Client.Decoder.refs) (Some commands))
      | `ReadyPACK _ as result ->
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
            | Ok state -> pack None state t result
            | Error _ as err -> Lwt.return err)
      | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
    in

    aux t None None r

  let push git ~push ~packer ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `ReceivePack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= push_handler git ~push ~packer t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let ls git ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= ls_handler git t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let fetch git ?(shallow = []) ~notify ~negociate ~has ~want ?deepen ~thin ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state
    >>= fetch_handler git ~shallow ~notify ~negociate ~has ~want ?deepen ~thin t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  let clone git ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= clone_handler git t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v
end
