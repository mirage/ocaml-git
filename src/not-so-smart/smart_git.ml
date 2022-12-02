include Smart_git_intf

module Verbose = struct
  type 'a fiber = 'a Lwt.t

  let succ () = Lwt.return_unit
  let print () = Lwt.return_unit
end

type handshake = uri0:Uri.t -> uri1:Uri.t -> Mimic.flow -> unit Lwt.t

let git_capabilities = Mimic.make ~name:"git-capabilities"
let git_scheme = Mimic.make ~name:"git-scheme"
let git_path = Mimic.make ~name:"git-path"
let git_hostname = Mimic.make ~name:"git-hostname"
let git_ssh_user = Mimic.make ~name:"git-ssh-user"
let git_port = Mimic.make ~name:"git-port"
let git_http_headers = Mimic.make ~name:"git-http-headers"

let git_transmission : [ `Git | `Exec | `HTTP of Uri.t * handshake ] Mimic.value
    =
  Mimic.make ~name:"git-transmission"

let git_uri = Mimic.make ~name:"git-uri"

module Endpoint = struct
  type t = {
    scheme :
      [ `SSH of string
      | `Git
      | `HTTP of (string * string) list
      | `HTTPS of (string * string) list
      | `Scheme of string ];
    port : int option;
    path : string;
    hostname : string;
  }

  let pp ppf edn =
    match edn with
    | { scheme = `SSH user; path; hostname; _ } ->
        Fmt.pf ppf "%s@%s:%s" user hostname path
    | { scheme = `Git; port; path; hostname } ->
        Fmt.pf ppf "git://%s%a/%s" hostname
          Fmt.(option (const string ":" ++ int))
          port path
    | { scheme = `HTTP _; path; port; hostname } ->
        Fmt.pf ppf "http://%s%a%s" hostname
          Fmt.(option (const string ":" ++ int))
          port path
    | { scheme = `HTTPS _; path; port; hostname } ->
        Fmt.pf ppf "https://%s%a%s" hostname
          Fmt.(option (const string ":" ++ int))
          port path
    | { scheme = `Scheme scheme; path; port; hostname } ->
        Fmt.pf ppf "%s://%s%a/%s" scheme hostname
          Fmt.(option (const string ":" ++ int))
          port path

  let headers_from_uri uri =
    match Uri.user uri, Uri.password uri with
    | Some user, Some password ->
        let raw = Base64.encode_exn (Fmt.str "%s:%s" user password) in
        [ "Authorization", Fmt.str "Basic %s" raw ]
    | _ -> []

  let of_string str =
    let open Rresult in
    let parse_ssh str =
      let len = String.length str in
      Emile.of_string_raw ~off:0 ~len str
      |> R.reword_error (R.msgf "%a" Emile.pp_error)
      >>= fun (consumed, m) ->
      match
        Astring.String.cut ~sep:":" (String.sub str consumed (len - consumed))
      with
      | Some ("", path) ->
          let local =
            List.map
              (function `Atom x -> x | `String x -> Fmt.str "%S" x)
              m.Emile.local
          in
          let user = String.concat "." local in
          let hostname =
            match fst m.Emile.domain with
            | `Domain vs -> String.concat "." vs
            | `Literal v -> v
            | `Addr (Emile.IPv4 v) -> Ipaddr.V4.to_string v
            | `Addr (Emile.IPv6 v) -> Ipaddr.V6.to_string v
            | `Addr (Emile.Ext (k, v)) -> Fmt.str "%s:%s" k v
          in
          R.ok { scheme = `SSH user; path; port = None; hostname }
      | _ -> R.error_msg "Invalid SSH pattern"
    in
    let parse_uri str =
      let uri = Uri.of_string str in
      let path = Uri.path uri in
      match Uri.scheme uri, Uri.host uri, Uri.port uri with
      | Some "git", Some hostname, port ->
          R.ok { scheme = `Git; path; port; hostname }
      | Some "http", Some hostname, port ->
          R.ok { scheme = `HTTP (headers_from_uri uri); path; port; hostname }
      | Some "https", Some hostname, port ->
          R.ok { scheme = `HTTPS (headers_from_uri uri); path; port; hostname }
      | Some scheme, Some hostname, port ->
          R.ok { scheme = `Scheme scheme; path; port; hostname }
      | _ -> R.error_msgf "Invalid uri: %a" Uri.pp uri
    in
    match parse_ssh str, parse_uri str with
    | Ok v, _ -> Ok v
    | _, Ok v -> Ok v
    | Error _, Error _ -> R.error_msgf "Invalid endpoint: %s" str

  let with_headers_if_http headers ({ scheme; _ } as edn) =
    match scheme with
    | `SSH _ | `Git | `Scheme _ -> edn
    | `HTTP _ -> { edn with scheme = `HTTP headers }
    | `HTTPS _ -> { edn with scheme = `HTTPS headers }

  let to_ctx edn ctx =
    let scheme =
      match edn.scheme with
      | `Git -> `Git
      | `SSH _ -> `SSH
      | `HTTP _ -> `HTTP
      | `HTTPS _ -> `HTTPS
      | `Scheme scheme -> `Scheme scheme
    in
    let headers =
      match edn.scheme with
      | `HTTP headers | `HTTPS headers -> Some headers
      | _ -> None
    in
    let ssh_user = match edn.scheme with `SSH user -> Some user | _ -> None in
    (* XXX(dinosaure): I don't like the reconstruction of the given [Uri.t] when we can miss some informations. *)
    let uri =
      match edn.scheme with
      | `HTTP _ ->
          Some
            (Uri.of_string
               (Fmt.str "http://%s%a%s" edn.hostname
                  Fmt.(option (const string ":" ++ int))
                  edn.port edn.path))
      | `HTTPS _ ->
          Some
            (Uri.of_string
               (Fmt.str "https://%s%a%s" edn.hostname
                  Fmt.(option (const string ":" ++ int))
                  edn.port edn.path))
      | _ -> None
    in
    ctx
    |> Mimic.add git_scheme scheme
    |> Mimic.add git_path edn.path
    |> Mimic.add git_hostname edn.hostname
    |> fun ctx ->
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_ssh_user v ctx) ssh_user
    |> fun ctx ->
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_port v ctx) edn.port
    |> fun ctx ->
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_uri v ctx) uri
    |> fun ctx ->
    Option.fold ~none:ctx
      ~some:(fun v -> Mimic.add git_http_headers v ctx)
      headers
end

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Pack : APPEND with type +'a fiber = 'a Lwt.t)
    (Index : APPEND with type +'a fiber = 'a Lwt.t)
    (Uid : UID)
    (Ref : Sigs.REF) =
struct
  let src = Logs.Src.create "git-fetch"

  module Log = (val Logs.src_log src : Logs.LOG)
  module Thin = Carton_lwt.Thin.Make (Uid)

  let fs =
    let open Rresult in
    let open Lwt.Infix in
    Thin.
      {
        create =
          (fun ?trunc t path ->
            Pack.create ?trunc ~mode:Pack.RdWr t path
            >|= R.reword_error (R.msgf "%a" Pack.pp_error));
        append = Pack.append;
        map = Pack.map;
        close =
          (fun t fd ->
            Pack.close t fd >|= R.reword_error (R.msgf "%a" Pack.pp_error));
      }

  (* XXX(dinosaure): abstract it? *)
  let digest :
      kind:[ `A | `B | `C | `D ] ->
      ?off:int ->
      ?len:int ->
      Bigstringaf.t ->
      Uid.t =
   fun ~kind ?(off = 0) ?len buf ->
    let len =
      match len with Some len -> len | None -> Bigstringaf.length buf - off
    in
    let ctx = Uid.empty in
    let feed_string ctx str =
      let off = 0 and len = String.length str in
      Uid.feed ctx (Bigstringaf.of_string ~off ~len str)
    in

    let ctx =
      match kind with
      | `A -> feed_string ctx (Fmt.str "commit %d\000" len)
      | `B -> feed_string ctx (Fmt.str "tree %d\000" len)
      | `C -> feed_string ctx (Fmt.str "blob %d\000" len)
      | `D -> feed_string ctx (Fmt.str "tag %d\000" len)
    in
    let ctx = Uid.feed ctx ~off ~len buf in
    Uid.get ctx

  let ( >>? ) = Lwt_result.bind

  module CartonSched = Carton.Make (Lwt)

  let finish_it t ~pack ~weight ~where offsets =
    let open Lwt.Infix in
    Log.debug (fun m -> m "Start to finish the canonicalized PACK file.");
    Pack.create ~trunc:false ~mode:Pack.Rd t pack >>? fun fd ->
    let zl_buffer = De.bigstring_create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    let pack =
      Carton.Dec.make fd ~allocate ~z:zl_buffer ~uid_ln:Uid.length
        ~uid_rw:Uid.of_raw_string (fun uid -> Hashtbl.find where uid)
    in
    let map fd ~pos len =
      let max = Int64.sub weight pos in
      let len = min max (Int64.of_int len) in
      let len = Int64.to_int len in
      Pack.map t fd ~pos len
    in
    let rec go entries = function
      | [] -> Lwt.return entries
      | (offset, crc) :: offsets ->
          Lwt.catch
            (fun () ->
              Log.debug (fun m -> m "Get object at %08Lx." offset);
              let weight =
                Carton.Dec.weight_of_offset ~map pack ~weight:Carton.Dec.null
                  offset
              in
              let raw = Carton.Dec.make_raw ~weight in
              let v = Carton.Dec.of_offset ~map pack raw ~cursor:offset in
              let kind = Carton.Dec.kind v in
              let raw = Carton.Dec.raw v in
              let len = Carton.Dec.len v in
              let uid = digest ~kind ~off:0 ~len raw in
              go ({ Carton.Dec.Idx.offset; crc; uid } :: entries) offsets)
            (fun exn ->
              Printexc.print_backtrace stdout;
              Lwt.fail exn)
    in
    go [] offsets >>= fun entries ->
    Pack.close t fd >>? fun () -> Lwt.return_ok entries

  let run_pck ?threads ~light_load ~heavy_load stream t ~src ~dst =
    let open Rresult in
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        Log.debug (fun m -> m "Start to verify the given stream.");
        Thin.verify ~digest ?threads t src fs stream)
      (function
        | Failure err -> Lwt.return_error (R.msg err)
        | Invalid_argument err -> Lwt.return_error (R.msg err)
        | exn ->
            Printexc.print_backtrace stdout;
            Lwt.return_error (`Exn exn))
    >>= function
    | Error _ as err -> Lwt.return err
    | Ok (_, [], [], entries, _weight, uid) ->
        Log.debug (fun m -> m "Given PACK file is not thin, move it!");
        Pack.move t ~src ~dst >|= R.reword_error (R.msgf "%a" Pack.pp_error)
        >>? fun () -> Lwt.return_ok (uid, Array.of_list entries)
    | Ok (n, uids, unresolveds, entries, weight, _uid) ->
        Log.debug (fun m -> m "Given PACK file is thin, canonicalize!");
        Thin.canonicalize ~light_load ~heavy_load ~src ~dst t fs n uids weight
        >>? fun (shift, weight, uid, entries') ->
        let where = Hashtbl.create 0x100 in
        let entries =
          let fold ({ Carton.Dec.Idx.offset; uid; _ } as entry) =
            let offset = Int64.add offset shift in
            Hashtbl.add where uid offset;
            { entry with Carton.Dec.Idx.offset }
          in
          List.map fold entries
        in
        List.iter
          (fun { Carton.Dec.Idx.offset; uid; _ } ->
            Hashtbl.add where uid offset)
          entries';
        let unresolveds =
          let fold (offset, crc) = Int64.add offset shift, crc in
          List.map fold unresolveds
        in
        finish_it ~pack:dst ~weight ~where t unresolveds
        >|= R.reword_error (R.msgf "%a" Pack.pp_error)
        >>? fun entries'' ->
        Log.debug (fun m -> m "PACK canonicalized.");
        let entries = List.rev_append entries' entries in
        let entries = List.rev_append entries'' entries in
        Lwt.return_ok (uid, Array.of_list entries)

  module Enc = Carton.Dec.Idx.N (Uid)

  let run_idx t ~dst ~pack entries =
    let open Lwt.Infix in
    let encoder = Enc.encoder `Manual ~pack entries in
    let buf = Bigstringaf.create De.io_buffer_size in
    Enc.dst encoder buf 0 (Bigstringaf.length buf);
    Index.create ~trunc:true ~mode:Index.Wr t dst >>? fun fd ->
    let rec go = function
      | `Partial ->
          let len = Bigstringaf.length buf - Enc.dst_rem encoder in
          Index.append t fd (Bigstringaf.substring buf ~off:0 ~len)
          >>= fun () ->
          Enc.dst encoder buf 0 (Bigstringaf.length buf);
          go (Enc.encode encoder `Await)
      | `Ok -> Lwt.return_ok ()
    in
    go (Enc.encode encoder `Await) >>? fun () -> Index.close t fd

  let run ?threads ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    run_pck ?threads ~light_load ~heavy_load stream t_pck ~src ~dst
    >>? fun (pack, entries) ->
    run_idx t_idx ~dst:idx ~pack entries
    >|= R.reword_error (R.msgf "%a" Index.pp_error)
    >>? fun () -> Lwt.return_ok pack

  module Flow = Unixiz.Make (Mimic)
  module Fetch = Nss.Fetch.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)
  module Push = Nss.Push.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)

  let fetch_v1 ?(uses_git_transport = false) ~push_stdout ~push_stderr
      ~capabilities path flow ?deepen ?want hostname store access fetch_cfg pack
      =
    let open Lwt.Infix in
    Lwt.try_bind
      (fun () ->
        Fetch.fetch_v1 ~uses_git_transport ~push_stdout ~push_stderr
          ~capabilities ?deepen ?want ~host:hostname path (Flow.make flow) store
          access fetch_cfg
        @@ fun (payload, off, len) ->
        let v = String.sub payload off len in
        pack (Some (v, 0, len)))
      (fun refs ->
        pack None;
        Mimic.close flow >>= fun () -> Lwt.return_ok refs)
    @@ fun exn ->
    pack None;
    Mimic.close flow >>= fun () -> Lwt.fail exn

  let default_capabilities =
    [
      `Side_band_64k;
      `Multi_ack_detailed;
      `Ofs_delta;
      `Thin_pack;
      `Report_status;
    ]

  type transmission = [ `Git | `Exec ]

  let rec get_transmission :
      Mimic.edn list -> [ `Git | `Exec | `HTTP of Uri.t * handshake ] option =
    function
    | Mimic.Edn (k, v) :: r -> (
        match Mimic.equal k git_transmission with
        | Some Mimic.Refl -> Some v
        | None -> get_transmission r)
    | [] -> None

  let add_unless lst k v =
    match List.assoc_opt (String.lowercase_ascii k) lst with
    | Some _ -> lst
    | None -> (String.lowercase_ascii k, v) :: lst

  let pp_version ppf = function
    | `V1 -> Fmt.pf ppf "1"
    | _ -> Fmt.pf ppf "unknown"

  let add_headers_for_fetching ?(version = `V1) ctx =
    let headers = Option.value ~default:[] (Mimic.get git_http_headers ctx) in
    let headers =
      add_unless headers "content-type" "application/x-git-upload-pack-request"
    in
    let headers =
      add_unless headers "accept" "application/x-git-upload-pack-result"
    in
    let headers =
      add_unless headers "git-protocol"
        (Fmt.str "version=%a" pp_version version)
    in
    Mimic.replace git_http_headers headers ctx

  let fetch ?(push_stdout = ignore) ?(push_stderr = ignore) ?threads ~ctx
      (access, light_load, heavy_load) store edn ?(version = `V1)
      ?(capabilities = default_capabilities) ?deepen want t_pck t_idx ~src ~dst
      ~idx =
    let open Rresult in
    let open Lwt.Infix in
    let hostname = edn.Endpoint.hostname in
    let path = edn.Endpoint.path in
    let stream, pusher = Lwt_stream.create () in
    let pusher_with_logging = function
      | Some (_, _, len) as v ->
          Log.debug (fun m -> m "Download %d byte(s) of the PACK file." len);
          pusher v
      | None ->
          Log.debug (fun m -> m "End of pack.");
          pusher None
    in
    let stream () = Lwt_stream.get stream in
    let ctx = Mimic.add git_capabilities `Rd (Endpoint.to_ctx edn ctx) in
    let ctx = add_headers_for_fetching ~version ctx in
    Lwt.catch (fun () ->
        Mimic.unfold ctx >>? fun ress ->
        Mimic.connect ress >>= fun flow ->
        match flow, get_transmission ress, version with
        | Ok flow, Some (#transmission as transmission), `V1 -> (
            let fetch_cfg = Nss.Fetch.configuration capabilities in
            let uses_git_transport =
              match transmission with `Git -> true | `Exec -> false
            in
            Lwt.both
              (fetch_v1 ~push_stdout ~push_stderr ~uses_git_transport
                 ~capabilities path flow ?deepen ~want hostname store access
                 fetch_cfg pusher_with_logging)
              (run ?threads ~light_load ~heavy_load stream t_pck t_idx ~src ~dst
                 ~idx)
            >>= fun (refs, idx) ->
            match refs, idx with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err)
        | Ok flow, Some (`HTTP (uri, handshake)), `V1 -> (
            let fetch_cfg =
              Nss.Fetch.configuration ~stateless:true capabilities
            in
            let uri0 =
              Fmt.str "%a/info/refs?service=git-upload-pack" Uri.pp uri
              |> Uri.of_string
            in
            let uri1 =
              Fmt.str "%a/git-upload-pack" Uri.pp uri |> Uri.of_string
            in
            Lwt.both
              ( handshake ~uri0 ~uri1 flow >>= fun () ->
                fetch_v1 ~push_stdout ~push_stderr ~capabilities path flow
                  ?deepen ~want hostname store access fetch_cfg
                  pusher_with_logging )
              (run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx)
            >>= fun (refs, idx) ->
            match refs, idx with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err)
        | Ok flow, Some _, _ ->
            Log.err (fun m -> m "The protocol version is uninmplemented.");
            Mimic.close flow >>= fun () ->
            Lwt.return_error (`Msg "Version protocol unimplemented")
        | Ok flow, None, _ ->
            Log.err (fun m ->
                m
                  "A flow was allocated but we can not recognize the \
                   transmission.");
            Mimic.close flow >>= fun () ->
            Lwt.return_error (`Msg "Unrecognized protocol")
        | Error err, _, _ ->
            Log.err (fun m -> m "The Git peer is not reachable.");
            Lwt.return_error err)
    @@ function
    | Failure err -> Lwt.return_error (R.msg err)
    | exn -> Lwt.return_error (`Exn exn)

  module Delta = Carton_lwt.Enc.Delta (Uid) (Verbose)

  let deltify ~light_load ~heavy_load ?(threads = 4) (uids : Uid.t list) =
    let open Lwt.Infix in
    let fold (uid : Uid.t) =
      light_load uid >|= fun (kind, length) ->
      Carton_lwt.Enc.make_entry ~kind ~length uid
    in
    Lwt_list.map_p fold uids >|= Array.of_list >>= fun entries ->
    Delta.delta
      ~threads:(List.init threads (fun _thread -> heavy_load))
      ~weight:10 ~uid_ln:Uid.length entries
    >>= fun targets -> Lwt.return (entries, targets)

  let header = Bigstringaf.create 12

  let pack ~(heavy_load : Uid.t Carton_lwt.Enc.load) stream targets =
    let open Lwt.Infix in
    let offsets = Hashtbl.create (Array.length targets) in
    let find uid =
      match Hashtbl.find offsets uid with
      | v -> Lwt.return_some v
      | exception Not_found -> Lwt.return_none
    in
    let uid =
      { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }
    in
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.Lz77.make_window ~bits:15;
      }
    in
    let ctx = ref Uid.empty in
    let cursor = ref 0 in
    Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12;
    stream (Some (Bigstringaf.to_string header));
    ctx := Uid.feed !ctx header ~off:0 ~len:12;
    cursor := !cursor + 12;
    let encode_targets targets =
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor;
        Carton_lwt.Enc.encode_target ~b ~find ~load:heavy_load ~uid
          targets.(idx) ~cursor:!cursor
        >>= fun (len, encoder) ->
        let rec go encoder =
          match Carton.Enc.N.encode ~o:b.o encoder with
          | `Flush (encoder, len) ->
              let payload = Bigstringaf.substring b.o ~off:0 ~len in
              stream (Some payload);
              ctx := Uid.feed !ctx b.o ~off:0 ~len;
              cursor := !cursor + len;
              let encoder =
                Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o)
              in
              go encoder
          | `End -> Lwt.return ()
        in
        let payload = Bigstringaf.substring b.o ~off:0 ~len in
        stream (Some payload);
        ctx := Uid.feed !ctx b.o ~off:0 ~len;
        cursor := !cursor + len;
        let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
        go encoder
      in
      let rec go idx =
        if idx < Array.length targets then
          encode_target idx >>= fun () -> go (succ idx)
        else Lwt.return ()
      in
      go 0
    in
    encode_targets targets >>= fun () ->
    let uid = Uid.get !ctx |> Uid.to_raw_string in
    stream (Some uid);
    stream None;
    Lwt.return_unit

  let pack ~light_load ~heavy_load uids =
    let open Lwt.Infix in
    let stream, pusher = Lwt_stream.create () in
    let fiber () =
      deltify ~light_load ~heavy_load uids >>= fun (_, targets) ->
      pack ~heavy_load pusher targets
    in
    let stream () = Lwt_stream.get stream in
    Lwt.async fiber;
    stream

  let push_v1 ?uses_git_transport flow ~capabilities path cmds hostname store
      access push_cfg pack =
    let open Lwt.Infix in
    Push.push ?uses_git_transport ~capabilities cmds ~host:hostname path
      (Flow.make flow) store access push_cfg pack
    >>= fun () ->
    Mimic.close flow >>= fun () -> Lwt.return_ok ()

  let add_headers_for_pushing ?(version = `V1) ctx =
    let headers = Option.value ~default:[] (Mimic.get git_http_headers ctx) in
    let headers =
      add_unless headers "content-type" "application/x-git-receive-pack-request"
    in
    let headers =
      add_unless headers "accept" "application/x-git-receive-pack-result"
    in
    let headers =
      add_unless headers "git-protocol"
        (Fmt.str "version=%a" pp_version version)
    in
    Mimic.replace git_http_headers headers ctx

  let push ~ctx (access, light_load, heavy_load) store edn ?(version = `V1)
      ?(capabilities = default_capabilities) cmds =
    let open Rresult in
    let open Lwt.Infix in
    let hostname = edn.Endpoint.hostname in
    let path = edn.Endpoint.path in
    let ctx = Mimic.add git_capabilities `Wr (Endpoint.to_ctx edn ctx) in
    let ctx = add_headers_for_pushing ~version ctx in
    Lwt.catch (fun () ->
        Mimic.unfold ctx >>? fun ress ->
        Mimic.connect ress >>= fun res ->
        match res, get_transmission ress, version with
        | Ok flow, Some (#transmission as transmission), `V1 ->
            let push_cfg = Nss.Push.configuration () in
            let uses_git_transport =
              match transmission with `Git -> true | `Exec -> false
            in
            push_v1 ~uses_git_transport flow ~capabilities path cmds hostname
              store access push_cfg
              (pack ~light_load ~heavy_load)
        | Ok flow, Some (`HTTP (uri, handshake)), `V1 ->
            let push_cfg = Nss.Push.configuration ~stateless:true () in
            let uri0 =
              Fmt.str "%a/info/refs?service=git-receive-pack" Uri.pp uri
              |> Uri.of_string
            in
            let uri1 =
              Fmt.str "%a/git-receive-pack" Uri.pp uri |> Uri.of_string
            in
            handshake ~uri0 ~uri1 flow >>= fun () ->
            push_v1 flow ~capabilities path cmds hostname store access push_cfg
              (pack ~light_load ~heavy_load)
        | Ok flow, Some _, _ ->
            Log.err (fun m -> m "The protocol version is uninmplemented.");
            Mimic.close flow >>= fun () ->
            Lwt.return_error (`Msg "Version protocol unimplemented")
        | Ok flow, None, _ ->
            Log.err (fun m ->
                m
                  "A flow was allocated but we can not recognize the \
                   transmission.");
            Mimic.close flow >>= fun () ->
            Lwt.return_error (`Msg "Unrecognized protocol")
        | Error err, _, _ ->
            Log.err (fun m -> m "The Git peer is not reachable.");
            Lwt.return_error err)
    @@ function
    | Failure err -> Lwt.return_error (R.msg err)
    | exn -> Lwt.return_error (`Exn exn)
end
