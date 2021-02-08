include Smart_git_intf

module Verbose = struct
  type 'a fiber = 'a Lwt.t

  let succ () = Lwt.return_unit
  let print () = Lwt.return_unit
end

let git_capabilities = Mimic.make ~name:"git-capabilities"
let git_scheme = Mimic.make ~name:"git-scheme"
let git_path = Mimic.make ~name:"git-path"
let git_host = Mimic.make ~name:"git-host"
let git_ssh_user = Mimic.make ~name:"git-ssh-user"
let git_port = Mimic.make ~name:"git-port"

module Endpoint = struct
  type t = {
    scheme :
      [ `SSH of string
      | `Git
      | `HTTP of (string * string) list
      | `HTTPS of (string * string) list ];
    port : int option;
    path : string;
    host : [ `Addr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ];
  }

  let pp ppf edn =
    let pp_host ppf = function
      | `Addr (Ipaddr.V4 v) -> Ipaddr.V4.pp ppf v
      | `Addr (Ipaddr.V6 v) -> Fmt.pf ppf "[IPv6:%a]" Ipaddr.V6.pp v
      | `Domain v -> Domain_name.pp ppf v
    in
    let pp_port ppf = function
      | Some port -> Fmt.pf ppf ":%d" port
      | None -> ()
    in
    match edn with
    | { scheme = `SSH user; path; host; _ } ->
        Fmt.pf ppf "%s@%a:%s" user pp_host host path
    | { scheme = `Git; port; path; host } ->
        Fmt.pf ppf "git://%a%a/%s" pp_host host pp_port port path
    | { scheme = `HTTP _; path; port; host } ->
        Fmt.pf ppf "http://%a%a/%s" pp_host host pp_port port path
    | { scheme = `HTTPS _; path; port; host } ->
        Fmt.pf ppf "https://%a%a/%s" pp_host host pp_port port path

  let ( <||> ) a b =
    match a with
    | Ok _ -> a
    | Error _ -> ( match b () with Ok _ as r -> r | Error _ -> a)

  let of_string str =
    let open Rresult in
    let parse_ssh x =
      let max = String.length x in
      Emile.of_string_raw ~off:0 ~len:max x
      |> R.reword_error (R.msgf "%a" Emile.pp_error)
      >>= fun (consumed, m) ->
      match
        Astring.String.cut ~sep:":" (String.sub x consumed (max - consumed))
      with
      | Some ("", path) ->
          let user =
            String.concat "."
              (List.map
                 (function `Atom x -> x | `String x -> Fmt.str "%S" x)
                 m.Emile.local)
          in
          (match fst m.Emile.domain with
          | `Domain vs -> (
              match
                ( Domain_name.(of_strings vs >>= host),
                  Ipaddr.V4.of_string (String.concat "." vs) )
              with
              | _, Ok ipv4 -> R.ok (`Addr (Ipaddr.V4 ipv4))
              | Ok v, _ -> R.ok (`Domain v)
              | (Error _ as err), _ -> err)
          | `Literal v ->
              Domain_name.of_string v >>= Domain_name.host >>| fun v ->
              `Domain v
          | `Addr (Emile.IPv4 v) -> R.ok (`Addr (Ipaddr.V4 v))
          | `Addr (Emile.IPv6 v) -> R.ok (`Addr (Ipaddr.V6 v))
          | v -> R.error_msgf "Invalid hostname: %a" Emile.pp_domain v)
          >>= fun host -> R.ok { scheme = `SSH user; path; port = None; host }
      | _ -> R.error_msg "invalid pattern"
    in
    let parse_uri x =
      let uri = Uri.of_string x in
      let path = Uri.path uri in
      let host str =
        (Domain_name.of_string str >>= Domain_name.host >>| fun x -> `Domain x)
        <||> fun () ->
        Ipaddr.of_string str >>| fun x -> `Addr x
      in
      match Uri.scheme uri, Uri.host uri, Uri.port uri with
      | Some "git", Some str, port ->
          host str >>= fun host -> R.ok { scheme = `Git; path; port; host }
      | Some "http", Some str, port ->
          host str >>= fun host -> R.ok { scheme = `HTTP []; path; port; host }
      | Some "https", Some str, port ->
          host str >>= fun host -> R.ok { scheme = `HTTPS []; path; port; host }
      | _ -> R.error_msgf "invalid uri: %a" Uri.pp uri
    in
    parse_ssh str
    <||> (fun () -> parse_uri str)
    |> R.reword_error (fun _ -> R.msgf "Invalid endpoint: %s" str)

  let with_headers_if_http headers ({ scheme; _ } as edn) =
    match scheme with
    | `SSH _ | `Git -> edn
    | `HTTP _ -> { edn with scheme = `HTTP headers }
    | `HTTPS _ -> { edn with scheme = `HTTPS headers }

  let to_ctx edn ctx =
    let scheme =
      match edn.scheme with
      | `Git -> `Git
      | `SSH _ -> `SSH
      | `HTTP _ -> `HTTP
      | `HTTPS _ -> `HTTPS
    in
    let ssh_user = match edn.scheme with `SSH user -> Some user | _ -> None in
    ctx
    |> Mimic.add git_scheme scheme
    |> Mimic.add git_path edn.path
    |> Mimic.add git_host edn.host
    |> fun ctx ->
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_ssh_user v ctx) ssh_user
    |> fun ctx ->
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_port v ctx) edn.port
end

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Pack : APPEND with type +'a fiber = 'a Lwt.t)
    (Index : APPEND with type +'a fiber = 'a Lwt.t)
    (HTTP : HTTP)
    (Uid : UID)
    (Ref : Sigs.REF) =
struct
  module Log = (val let src = Logs.Src.create "git-fetch" in
                    Logs.src_log src : Logs.LOG)

  module Thin = Carton_lwt.Thin.Make (Uid)

  let fs =
    let open Rresult in
    let open Lwt.Infix in
    let create t path =
      Pack.create ~mode:Pack.RdWr t path
      >|= R.reword_error (R.msgf "%a" Pack.pp_error)
    in
    let close t fd =
      Pack.close t fd >|= R.reword_error (R.msgf "%a" Pack.pp_error)
    in
    { Thin.create; append = Pack.append; map = Pack.map; close }

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
    Pack.create ~mode:Pack.Rd t pack >>? fun fd ->
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

  let run_pck ~light_load ~heavy_load stream t ~src ~dst =
    let open Rresult in
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        Log.debug (fun m -> m "Start to verify the given stream.");
        Thin.verify ~digest ~threads:1 t src fs stream)
      (function
        | Failure err -> Lwt.return_error (R.msg err)
        | Invalid_argument err -> Lwt.return_error (R.msg err)
        | exn -> Lwt.return_error (`Exn exn))
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
        let entries = List.rev_append entries' entries in
        let entries = List.rev_append entries'' entries in
        Lwt.return_ok (uid, Array.of_list entries)

  module Enc = Carton.Dec.Idx.N (Uid)

  let run_idx t ~dst ~pack entries =
    let open Lwt.Infix in
    let encoder = Enc.encoder `Manual ~pack entries in
    let buf = Bigstringaf.create De.io_buffer_size in
    Enc.dst encoder buf 0 (Bigstringaf.length buf);
    Index.create ~mode:Index.Wr t dst >>? fun fd ->
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

  let run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    run_pck ~light_load ~heavy_load stream t_pck ~src ~dst
    >>? fun (pack, entries) ->
    run_idx t_idx ~dst:idx ~pack entries
    >|= R.reword_error (R.msgf "%a" Index.pp_error)
    >>? fun () -> Lwt.return_ok pack

  module Flow = Unixiz.Make (Mimic)
  module Fetch = Nss.Fetch.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)
  module Fetch_v1 = Fetch.V1
  module Push = Nss.Push.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)

  (** [push_pack_str_alone push_pack (payload, off, len)] calls [push_pack] with
      [push_pack (Some (String.sub payload off len), 0, len)] *)
  let push_pack_new_str push_pack (payload, off, len) =
    let v = String.sub payload off len in
    push_pack (Some (v, 0, len))

  let fetch_v1 ?(uses_git_transport = false) ~push_stdout ~push_stderr
      ~capabilities path ~ctx ?deepen ?want host store access fetch_cfg
      push_pack =
    let open Lwt.Infix in
    Mimic.resolve ctx >>= function
    | Error _ as err ->
        let pp_host ppf = function
          | `Domain v -> Domain_name.pp ppf v
          | `Addr v -> Ipaddr.pp ppf v
        in
        Log.err (fun m -> m "%a not found" pp_host host);
        push_pack None;
        Lwt.return err
    | Ok flow ->
        Lwt.try_bind
          (fun () ->
            Fetch_v1.fetch ~uses_git_transport ~push_stdout ~push_stderr
              ~capabilities ?deepen ?want ~host path (Flow.make flow) store
              access fetch_cfg
              (push_pack_new_str push_pack))
          (fun refs ->
            push_pack None;
            Mimic.close flow >>= fun () -> Lwt.return_ok refs)
          (fun exn ->
            push_pack None;
            Mimic.close flow >>= fun () -> Lwt.fail exn)

  module Flow_http = struct
    type +'a fiber = 'a Lwt.t

    type t = {
      mutable ic : string;
      mutable oc : string;
      mutable pos : int;
      uri : Uri.t;
      headers : (string * string) list;
      ctx : Mimic.ctx;
    }

    type error = [ `Msg of string ]

    let pp_error = Rresult.R.pp_msg

    let send t raw =
      let oc = t.oc ^ Cstruct.to_string raw in
      t.oc <- oc;
      Lwt.return_ok (Cstruct.len raw)

    let rec recv t raw =
      if t.pos = String.length t.ic then (
        let open Lwt.Infix in
        (HTTP.post ~ctx:t.ctx ~headers:t.headers t.uri t.oc
        >|= Rresult.(R.reword_error (R.msgf "%a" HTTP.pp_error)))
        >>? fun (_resp, contents) ->
        t.ic <- t.ic ^ contents;
        recv t raw)
      else
        let len = min (String.length t.ic - t.pos) (Cstruct.len raw) in
        Cstruct.blit_from_string t.ic t.pos raw 0 len;
        t.pos <- t.pos + len;
        Lwt.return_ok (`Input len)
  end

  module Fetch_http = Nss.Fetch.Make (Scheduler) (Lwt) (Flow_http) (Uid) (Ref)
  module Fetch_v1_http = Fetch_http.V1

  let http_fetch_v1 ~push_stdout ~push_stderr ~capabilities ~ctx uri
      ?(headers = []) endpoint path ?deepen ?want store access fetch_cfg
      push_pack =
    let open Rresult in
    let open Lwt.Infix in
    let uri0 = Fmt.str "%a/info/refs?service=git-upload-pack" Uri.pp uri in
    let uri0 = Uri.of_string uri0 in
    Log.debug (fun m -> m "GET %a" Uri.pp uri0);
    HTTP.get ~ctx ~headers uri0 >|= R.reword_error (R.msgf "%a" HTTP.pp_error)
    >>? fun (_resp, contents) ->
    let uri1 = Fmt.str "%a/git-upload-pack" Uri.pp uri in
    let uri1 = Uri.of_string uri1 in
    let flow =
      { Flow_http.ic = contents; pos = 0; oc = ""; uri = uri1; headers; ctx }
    in
    Fetch_v1_http.fetch ~push_stdout ~push_stderr ~capabilities ?deepen ?want
      ~host:endpoint path flow store access fetch_cfg
      (push_pack_new_str push_pack)
    >>= fun refs ->
    push_pack None;
    Lwt.return_ok refs

  let default_capabilities =
    [
      `Side_band_64k; `Multi_ack_detailed; `Ofs_delta; `Thin_pack;
      `Report_status;
    ]

  module V2 = struct end

  let fetch ?(push_stdout = ignore) ?(push_stderr = ignore) ~ctx
      (access, light_load, heavy_load) store edn ?(version = `V1)
      ?(capabilities = default_capabilities) ?deepen want t_pck t_idx ~src ~dst
      ~idx =
    let open Rresult in
    let open Lwt.Infix in
    let host = edn.Endpoint.host in
    let path = edn.path in
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
    (* XXX(dinosaure): such trick is only about SSH. Indeed, when we use SSH, we
       should/must? know if we want to fetch or push. If we want to fetch, we
       will call git-upload-pack. To be able to pass this information to the
       "connect" function of SSH (whatever the implementation of SSH), we fill
       the given [ctx] with [`Rd]. *)
    let run =
      match version, edn.scheme with
      | `V1, ((`Git | `SSH _) as scheme) ->
          let fetch_cfg = Nss.Fetch.V1.configuration capabilities in
          let uses_git_transport =
            match scheme with `Git -> true | `SSH _ -> false
          in
          let run () =
            Lwt.both
              (fetch_v1 ~push_stdout ~push_stderr ~uses_git_transport
                 ~capabilities path ~ctx ?deepen ~want host store access
                 fetch_cfg pusher_with_logging)
              (run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx)
            >>= fun (refs, idx) ->
            match refs, idx with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err
          in
          run
      | `V1, ((`HTTP _ | `HTTPS _) as scheme) ->
          Log.debug (fun m -> m "Start an HTTP transmission.");
          let fetch_cfg =
            Nss.Fetch.V1.configuration ~stateless:true capabilities
          in
          let pp_host ppf = function
            | `Domain v -> Domain_name.pp ppf v
            | `Addr v -> Ipaddr.pp ppf v
          in
          let uri, headers =
            match scheme with
            | `HTTP headers ->
                ( Uri.of_string (Fmt.str "http://%a%s.git" pp_host host path),
                  headers )
            | `HTTPS headers ->
                ( Uri.of_string (Fmt.str "https://%a%s.git" pp_host host path),
                  headers )
          in
          let run () =
            Lwt.both
              (http_fetch_v1 ~push_stdout ~push_stderr ~capabilities ~ctx uri
                 ~headers host path ?deepen ~want store access fetch_cfg
                 pusher_with_logging)
              (run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx)
            >>= fun (refs, idx) ->
            match refs, idx with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err
          in
          run
      | _ -> assert false
    in
    Lwt.catch run (function
      | Failure err -> Lwt.return_error (R.msg err)
      | exn -> Lwt.return_error (`Exn exn))

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
        Carton.Enc.w = De.make_window ~bits:15;
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

  let push ?uses_git_transport ~ctx ~capabilities path cmds endpoint store
      access push_cfg pack =
    let open Lwt.Infix in
    (* XXX(dinosaure): see [fetch]. *)
    Mimic.resolve ctx >>? fun flow ->
    Push.push ?uses_git_transport ~capabilities cmds ~host:endpoint path
      (Flow.make flow) store access push_cfg pack
    >>= fun () ->
    Mimic.close flow >>= fun () -> Lwt.return_ok ()

  let push ~ctx (access, light_load, heavy_load) store edn ?(version = `V1)
      ?(capabilities = default_capabilities) cmds =
    let ctx = Mimic.add git_capabilities `Wr (Endpoint.to_ctx edn ctx) in
    let open Rresult in
    match version, edn.Endpoint.scheme with
    | `V1, ((`Git | `SSH _) as scheme) ->
        let uses_git_transport =
          match scheme with `Git -> true | `SSH _ -> false
        in
        let host = edn.host in
        let path = edn.path in
        let push_cfg = Nss.Push.configuration () in
        let run () =
          push ~uses_git_transport ~ctx ~capabilities path cmds host store
            access push_cfg
            (pack ~light_load ~heavy_load)
        in
        Lwt.catch run (function
          | Failure err -> Lwt.return_error (R.msgf "%s" err)
          | exn -> Lwt.return_error (`Exn exn))
    | _ -> assert false
end
