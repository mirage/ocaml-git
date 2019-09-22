module type NET = sig
  type endpoint
  type socket
  type error

  val pp_error : Format.formatter -> error -> unit
  val read : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val write : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val socket : endpoint -> socket Lwt.t
  val close : socket -> unit Lwt.t
end

open Lwt.Infix

let ( >>!= ) = Lwt_result.bind_lwt_err
let ( >>?= ) = Lwt_result.bind
let src = Logs.Src.create "git.tcp" ~doc:"logs git's tcp event"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (N : NET)
    (E : Sync.ENDPOINT with type t = N.endpoint)
    (G : Minimal.S) : Sync.S with module Store = G with module Endpoint = E =
struct
  module Store = G
  module Net = N
  module Client = Smart.Client (Store.Hash) (Store.Reference)
  module Encoder = Client.Encoder
  module Decoder = Client.Decoder
  module Hash = Store.Hash
  module Inflate = Store.Inflate
  module Deflate = Store.Deflate
  module Endpoint = E

  module Common :
    module type of Sync.Common (Store) with module Store = Store =
    Sync.Common (Store)

  type error =
    [ `Sync of string
    | `Store of Store.error
    | `Net of Net.error
    | `Smart of Decoder.error
    | `Not_found ]

  let pp_error ppf = function
    | `Sync err -> Helper.ppe ~name:"`Sync" Fmt.string ppf err
    | `Store err -> Helper.ppe ~name:"`Store" Store.pp_error ppf err
    | `Net err -> Helper.ppe ~name:"`Net" Net.pp_error ppf err
    | `Smart err -> Helper.ppe ~name:"`Smart" Decoder.pp_error ppf err
    | `Not_found -> Fmt.string ppf "`Not_found"

  (* XXX(dinosaure): we need to export some definitions about the Smart
     protocol to export a low level API (which let the end-user to choose
     negotiation engine). *)

  type command = Common.command

  let pp_command = Common.pp_command
  let pp_fetch_one = Common.pp_fetch_one

  type t =
    { socket: Net.socket
    ; input: Bytes.t
    ; output: Bytes.t
    ; ctx: Client.context
    ; capabilities: Capability.t list }

  let make ~socket ~ctx ~capabilities =
    { socket
    ; input= Bytes.create 65535
    ; output= Bytes.create 65535
    ; ctx
    ; capabilities }

  let err_unexpected_result result =
    let buf = Buffer.create 64 in
    let ppf = Fmt.with_buffer buf in
    Fmt.pf ppf "Unexpected result: %a%!" (Fmt.hvbox Client.pp_result) result ;
    Buffer.contents buf

  let rec process t result =
    match result with
    | `Read (buffer, off, len, continue) -> (
        Net.read t.socket t.input 0 len
        >>= function
        | Ok len ->
            Cstruct.blit_from_bytes t.input 0 buffer off len ;
            process t (continue len)
        | Error err -> Lwt.return_error (`Net err) )
    | `Write (buffer, off, len, continue) -> (
        Cstruct.blit_to_bytes buffer off t.output 0 len ;
        Net.write t.socket t.output 0 len
        >>= function
        | Ok n -> process t (continue n)
        | Error err -> Lwt.return_error (`Net err) )
    | `Error (err, buf, committed) ->
        let raw = Cstruct.sub buf committed (Cstruct.len buf - committed) in
        Log.err (fun l ->
            l "Retrieve an error (%a) on: %a." Client.Decoder.pp_error err
              (Fmt.hvbox
                 (Encore.Lole.pp_scalar ~get:Cstruct.get_char
                    ~length:Cstruct.len))
              raw ) ;
        Lwt.return_error (`Smart err)
        (* TODO *)
    | #Client.result as result -> Lwt.return_ok result

  module Pack = struct
    let default_stdout raw =
      Log.info (fun l -> l "%S" (Cstruct.to_string raw)) ;
      Lwt.return ()

    let default_stderr raw =
      Log.err (fun l -> l "%S" (Cstruct.to_string raw)) ;
      Lwt.return ()

    let populate git ?(stdout = default_stdout) ?(stderr = default_stderr) ctx
        first =
      let stream, push = Lwt_stream.create () in
      let cstruct_copy cs =
        let ln = Cstruct.len cs in
        let rs = Cstruct.create ln in
        Cstruct.blit cs 0 rs 0 ln ; rs
      in
      let rec dispatch ctx = function
        | `PACK (`Out raw) ->
            stdout raw
            >>= fun () ->
            Client.run ctx.ctx `ReceivePACK |> process ctx >>?= dispatch ctx
        | `PACK (`Err raw) ->
            stderr raw
            >>= fun () ->
            Client.run ctx.ctx `ReceivePACK |> process ctx >>?= dispatch ctx
        | `PACK (`Raw raw) ->
            (* XXX(dinosaure): we don't have the ownership on [raw] and need to
               copy it because it will be updated by [Client.run]. *)
            push (Some (cstruct_copy raw)) ;
            Client.run ctx.ctx `ReceivePACK |> process ctx >>?= dispatch ctx
        | `PACK `End -> push None ; Lwt.return (Ok ())
        | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))
      in
      dispatch ctx first
      >>?= fun () ->
      Store.Pack.from git (fun () -> Lwt_stream.get stream)
      >>!= fun err -> Lwt.return (`Store err)
  end

  let rec clone_handler git reference ?hash t r =
    match r with
    | `Negociation _ ->
        Client.run t.ctx `Done
        |> process t
        >>?= clone_handler git reference ?hash t
    | `NegociationResult _ -> (
        Client.run t.ctx `ReceivePACK
        |> process t
        >>?= Pack.populate git t
        >>= fun res ->
        match res, hash with
        | Ok (_, _), Some hash -> Lwt.return (Ok hash)
        | Ok (_, _), None ->
            assert false
            (* XXX(dinosaure): impossible to retrieve this state on
               `NegociationResult. *)
        | (Error _ as err), _ -> Lwt.return err )
    | `ShallowUpdate _ ->
        Client.run t.ctx (`Has Hash.Set.empty)
        |> process t
        >>?= clone_handler git reference ?hash t
    | `Refs refs -> (
      try
        let hash_head, _, _ =
          List.find
            (fun (_, reference', peeled) ->
              Store.Reference.(equal reference reference') && not peeled )
            refs.Client.Common.refs
        in
        Client.run t.ctx
          (`UploadRequest
             { Client.Common.want= hash_head, []
             (* XXX(dinosaure): why we need to put two times [hash_head]?
                [git] does not do that and we differ from [fetch] behavior. *)
             ; capabilities= t.capabilities
             ; shallow= []
             ; deep= None })
        |> process t
        >>?= clone_handler git reference ~hash:hash_head t
      with Not_found -> (
        Client.run t.ctx `Flush
        |> process t
        >>?= function
        | `Flush -> Lwt.return (Error `Not_found)
        | result -> Lwt.return (Error (`Sync (err_unexpected_result result))) )
      )
    | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))

  let ls_handler _ t r =
    match r with
    | `Refs refs -> (
        Client.run t.ctx `Flush
        |> process t
        >>?= function
        | `Flush -> Lwt.return (Ok refs.Client.Common.refs)
        | result -> Lwt.return (Error (`Sync (err_unexpected_result result))) )
    | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))

  let fetch_handler git ?(shallow = []) ~notify ~negociate:(fn, state) ~have
      ~want ?deepen t r =
    (* XXX(dinosaure): purpose of [asked]? TODO! *)
    let pack asked t =
      Client.run t.ctx `ReceivePACK
      |> process t
      >>?= Pack.populate git t
      >>= function
      | Ok (_, n) -> Lwt.return (Ok (asked, n))
      | Error err -> Lwt.return (Error err)
    in
    let rec aux t asked state = function
      | `ShallowUpdate shallow_update ->
          notify shallow_update
          >>= fun () ->
          Client.run t.ctx (`Has have) |> process t >>?= aux t asked state
      | `Negociation acks -> (
          Log.debug (fun l ->
              l "Retrieve the negotiation: %a."
                (Fmt.hvbox Client.Common.pp_acks)
                acks ) ;
          fn acks state
          >>= function
          | `Ready, _ ->
              Log.debug (fun l ->
                  l "Retrieve `Ready ACK from negotiation engine." ) ;
              Client.run t.ctx `Done |> process t >>?= aux t asked state
          | `Done, state ->
              Log.debug (fun l ->
                  l "Retrieve `Done ACK from negotiation engine." ) ;
              Client.run t.ctx `Done |> process t >>?= aux t asked state
          | `Again have, state ->
              Log.debug (fun l ->
                  l "Retrieve `Again ACK from negotiation engine." ) ;
              Client.run t.ctx (`Has have) |> process t >>?= aux t asked state
          )
      | `NegociationResult _ ->
          Log.debug (fun l -> l "Retrieve a negotiation result.") ;
          pack asked t
      | `Refs refs -> (
          want refs.Client.Common.refs
          >>= function
          | first :: rest ->
              Client.run t.ctx
                (`UploadRequest
                  { Client.Common.want= snd first, List.map snd rest
                  ; capabilities= t.capabilities
                  ; shallow
                  ; deep= deepen })
              |> process t
              >>?= aux t (first :: rest) state
          | [] -> (
              Client.run t.ctx `Flush
              |> process t
              >>?= function
              | `Flush -> Lwt.return (Ok ([], 0))
              (* XXX(dinosaure): better return? *)
              | result ->
                  Lwt.return (Error (`Sync (err_unexpected_result result))) ) )
      | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))
    in
    aux t [] state r

  let references_of_commands =
    List.fold_left (fun a -> function
        | `Create (_, r) -> Store.Reference.Set.add r a
        | `Delete (_, r) -> Store.Reference.Set.add r a
        | `Update (_, _, r) -> Store.Reference.Set.add r a)
      Store.Reference.Set.empty

  let push_handler git ~push t r =
    let send_pack refs stream t r =
      let rec go ?keep t r =
        let consume ?keep dst =
          match keep with
          | Some keep ->
              let n = min (Cstruct.len keep) (Cstruct.len dst) in
              Cstruct.blit keep 0 dst 0 n ;
              let keep = Cstruct.shift keep n in
              if Cstruct.len keep > 0 then
                Lwt.return (`Continue (Some keep, n))
              else Lwt.return (`Continue (None, n))
          | None -> (
              stream ()
              >>= function
              | Some keep ->
                  let n = min (Cstruct.len keep) (Cstruct.len dst) in
                  Cstruct.blit keep 0 dst 0 n ;
                  let keep = Cstruct.shift keep n in
                  if Cstruct.len keep > 0 then
                    Lwt.return (`Continue (Some keep, n))
                  else Lwt.return (`Continue (None, n))
              | None -> Lwt.return `Finish )
        in
        match r with
        | `ReadyPACK dst -> (
            consume ?keep dst
            >>= function
            | `Continue (keep, n) ->
                Client.run t.ctx (`SendPACK n) |> process t >>?= go ?keep t
            | `Finish -> Client.run t.ctx (`FinishPACK refs) |> process t >>?= go t )
        | `Nothing -> Lwt.return (Ok [])
        | `ReportStatus {Client.Common.unpack= Ok (); commands} ->
            Lwt.return (Ok commands)
        | `ReportStatus {Client.Common.unpack= Error err; _} ->
            Lwt.return (Error (`Sync err))
        | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))
      in
      go t r
    in
    let rec aux t refs commands = function
      | `Refs refs -> (
          Log.debug (fun l ->
              l "Receiving reference: %a."
                (Fmt.hvbox Client.Common.pp_advertised_refs)
                refs ) ;
          let capabilities =
            List.filter
              (function
                | `Report_status | `Delete_refs | `Ofs_delta | `Push_options
                 |`Agent _ | `Side_band | `Side_band_64k ->
                    true
                | _ -> false)
              t.capabilities
          in
          push refs.Client.Common.refs
          >>= function
          | _, [] -> (
              Client.run t.ctx `Flush
              |> process t
              >>?= function
              | `Flush -> Lwt.return_ok []
              | result ->
                  Lwt.return_error (`Sync (err_unexpected_result result)) )
          | shallow, commands ->
              Log.debug (fun l ->
                  l "Sending command(s): %a."
                    (Fmt.hvbox (Fmt.Dump.list pp_command))
                    commands ) ;
              let x, r =
                List.map
                  (function
                    | `Create (hash, r) -> Client.Common.Create (hash, r)
                    | `Delete (hash, r) -> Client.Common.Delete (hash, r)
                    | `Update (_of, _to, r) ->
                        Client.Common.Update (_of, _to, r))
                  commands
                |> fun commands -> List.hd commands, List.tl commands
              in
              Client.run t.ctx
                (`UpdateRequest
                  {Client.Common.shallow; requests= `Raw (x, r); capabilities})
              |> process t
              >>?= aux t (Some refs.Client.Common.refs) (Some (x :: r)) )
      | `ReadyPACK _ as result -> (
          Log.debug (fun l -> l "The server is ready to receive the PACK file.") ;
          let ofs_delta =
            List.exists (( = ) `Ofs_delta) (Client.capabilities t.ctx)
          in
          let commands =
            match commands with
            | Some commands -> commands
            | None -> assert false
          in
          let refs =
            match refs with Some refs -> refs | None -> assert false
          in
          (* XXX(dinosaure): in this case, we can use GADT to describe the
             protocol by the session-type (like, [`UpdateRequest] makes a [`]
             response). So, we can constraint some assertions about the context
             when we catch [`ReadyPACK].

             One of this assertion is about the [commands] variable, which one
             is previously specified. So, the [None] value can not be catch and
             it's why we have an [assert false]. It's the same for [refs]. *)
          List.map
            (function
              | Client.Common.Create (hash, refname) -> `Create (hash, refname)
              | Client.Common.Delete (hash, refname) -> `Delete (hash, refname)
              | Client.Common.Update (a, b, refname) -> `Update (a, b, refname))
            commands
          |> fun commands -> Common.packer git ~ofs_delta refs commands
          >>= function
          | Ok (stream, _) -> send_pack (references_of_commands commands) stream t result
          | Error err -> Lwt.return (Error (`Store err)) )
      | result -> Lwt.return (Error (`Sync (err_unexpected_result result)))
    in
    aux t None None r

  let uri x = Endpoint.uri x
  let path e = Uri.path (uri e)
  let port e = match Uri.port (uri e) with None -> 9418 | Some p -> p

  let host e =
    match Uri.host (uri e) with
    | Some h -> h
    | None -> Fmt.invalid_arg "missing host: %a" Uri.pp_hum (uri e)

  module N = Negociator.Make (G)

  let push git ~push ?(capabilities = Sync.Default.capabilities) endpoint =
    Net.socket endpoint
    >>= fun socket ->
    let ctx, state =
      Client.context
        { Client.Common.pathname= path endpoint
        ; host= Some (host endpoint, Some (port endpoint))
        ; request_command= `Receive_pack }
    in
    let t = make ~socket ~ctx ~capabilities in
    process t state
    >>?= push_handler git ~push t
    >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let ls git ?(capabilities = Sync.Default.capabilities) endpoint =
    Net.socket endpoint
    >>= fun socket ->
    let ctx, state =
      Client.context
        { Client.Common.pathname= path endpoint
        ; host= Some (host endpoint, Some (port endpoint))
        ; request_command= `Upload_pack }
    in
    let t = make ~socket ~ctx ~capabilities in
    process t state
    >>?= ls_handler git t
    >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let fetch git ?(shallow = []) ?(capabilities = Sync.Default.capabilities)
      ~notify ~negociate ~have ~want ?deepen endpoint =
    Net.socket endpoint
    >>= fun socket ->
    let ctx, state =
      Client.context
        { Client.Common.pathname= path endpoint
        ; host= Some (host endpoint, Some (port endpoint))
        ; request_command= `Upload_pack }
    in
    let t = make ~socket ~ctx ~capabilities in
    process t state
    >>?= fetch_handler git ~shallow ~notify ~negociate ~have ~want ?deepen t
    >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  (* XXX(dinosaure): replaced by [clone] below, this function handles only the
     Smart protocol, the next version set [reference] to the new hash. *)
  let clone git ?(reference = Store.Reference.master)
      ?(capabilities = Sync.Default.capabilities) endpoint =
    Net.socket endpoint
    >>= fun socket ->
    let ctx, state =
      Client.context
        { Client.Common.pathname= path endpoint
        ; host= Some (host endpoint, Some (port endpoint))
        ; request_command= `Upload_pack }
    in
    let t = make ~socket ~ctx ~capabilities in
    process t state
    >>?= clone_handler git reference t
    >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let fetch_and_set_references git ?capabilities ~choose ~references repository
      =
    N.find_common git
    >>= fun (have, state, continue) ->
    let continue {Sync.acks; shallow; unshallow} state =
      continue {Sync.acks; shallow; unshallow} state
    in
    let want_handler = Common.want_handler git choose in
    fetch git ?capabilities
      ~notify:(fun shallow_update ->
        Log.debug (fun l ->
            l "Notify %a."
              (Fmt.hvbox Client.Common.pp_shallow_update)
              shallow_update ) ;
        Lwt.return () )
      ~negociate:(continue, state) ~have ~want:want_handler repository
    >>?= fun (results, _) ->
    Common.update_and_create git ~references results
    >>!= fun err -> Lwt.return (`Store err)

  let fetch_all git ?capabilities ~references repository =
    let choose _ = Lwt.return true in
    fetch_and_set_references ~choose ?capabilities ~references git repository

  let fetch_some git ?capabilities ~references repository =
    let choose remote_ref =
      Lwt.return (Store.Reference.Map.mem remote_ref references)
    in
    fetch_and_set_references ~choose ?capabilities ~references git repository
    >>?= fun (updated, missed, downloaded) ->
    if Store.Reference.Map.is_empty downloaded then
      Lwt.return (Ok (updated, missed))
    else (
      Log.err (fun l ->
          l "This case should not appear, we download: %a."
            Fmt.Dump.(list (pair Store.Reference.pp Store.Hash.pp))
            (Store.Reference.Map.bindings downloaded) ) ;
      Lwt.return (Ok (updated, missed)) )

  let fetch_one git ?capabilities ~reference:(remote_ref, local_refs)
      repository =
    let references = Store.Reference.Map.singleton remote_ref local_refs in
    let choose remote_ref =
      Lwt.return (Store.Reference.Map.mem remote_ref references)
    in
    fetch_and_set_references ~choose ?capabilities ~references git repository
    >>?= fun (updated, missed, downloaded) ->
    if not (Store.Reference.Map.is_empty downloaded) then
      Log.err (fun l ->
          l "This case should not appear, we downloaded: %a."
            Fmt.Dump.(list (pair Store.Reference.pp Store.Hash.pp))
            (Store.Reference.Map.bindings downloaded) ) ;
    match Store.Reference.Map.(bindings updated, bindings missed) with
    | [], [_] -> Lwt.return (Ok `AlreadySync)
    | _ :: _, [] -> Lwt.return (Ok (`Sync updated))
    | [], missed ->
        Log.err (fun l ->
            l "This case should not appear, we missed too many references: %a."
              Fmt.Dump.(
                list (pair Store.Reference.pp (list Store.Reference.pp)))
              missed ) ;
        Lwt.return (Ok `AlreadySync)
    | _ :: _, missed ->
        Log.err (fun l ->
            l "This case should not appear, we missed too many references: %a."
              Fmt.Dump.(
                list (pair Store.Reference.pp (list Store.Reference.pp)))
              missed ) ;
        Lwt.return (Ok (`Sync updated))

  let clone t ?capabilities ~reference:(remote_ref, local_ref) endpoint =
    clone t ?capabilities ~reference:remote_ref endpoint
    >>?= function
    | hash' ->
        Log.debug (fun l ->
            l "Update reference %a to %a." Store.Reference.pp local_ref
              Store.Hash.pp hash' ) ;
        Store.Ref.write t local_ref (Store.Reference.Hash hash')
        >>?= (fun () ->
               Store.Ref.write t Store.Reference.head
                 (Store.Reference.Ref local_ref) )
        >>!= fun err -> Lwt.return (`Store err)

  let update_and_create git ?capabilities ~references repository =
    let push_handler remote_refs =
      Common.push_handler git references remote_refs
      >>= fun actions -> Lwt.return ([], actions)
    in
    push git ~push:push_handler ?capabilities repository
end
