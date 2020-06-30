module type APPEND = sig
  type t

  type uid

  and fd

  type error

  type +'a fiber

  val pp_error : error Fmt.t

  val create : t -> uid -> (fd, error) result fiber

  val map : t -> fd -> pos:int64 -> int -> Bigstringaf.t fiber

  val append : t -> fd -> string -> unit fiber

  val move : t -> src:uid -> dst:uid -> (unit, error) result fiber

  val close : t -> fd -> (unit, error) result fiber
end

module type UID = sig
  include Carton.UID

  include Sigs.UID with type t := t

  val hash : t -> int
end

module Verbose = struct
  type 'a fiber = 'a Lwt.t

  let succ () = Lwt.return_unit

  let print () = Lwt.return_unit
end

module type HTTP = sig
  type error

  val pp_error : error Fmt.t

  val get :
    resolvers:Conduit.resolvers ->
    ?headers:(string * string) list ->
    Uri.t ->
    (unit * string, error) result Lwt.t

  val post :
    resolvers:Conduit.resolvers ->
    ?headers:(string * string) list ->
    Uri.t ->
    string ->
    (unit * string, error) result Lwt.t
end

let ( <.> ) f g x = f (g x)

type endpoint = {
  scheme :
    [ `SSH of string
    | `Git
    | `HTTP of (string * string) list
    | `HTTPS of (string * string) list ];
  path : string;
  domain_name : [ `host ] Domain_name.t;
}

let pp_endpoint ppf edn =
  match edn with
  | { scheme = `SSH user; path; domain_name } ->
      Fmt.pf ppf "%s@%a:%s" user Domain_name.pp domain_name path
  | { scheme = `Git; path; domain_name } ->
      Fmt.pf ppf "git://%a/%s" Domain_name.pp domain_name path
  | { scheme = `HTTP _; path; domain_name } ->
      Fmt.pf ppf "http://%a/%s" Domain_name.pp domain_name path
  | { scheme = `HTTPS _; path; domain_name } ->
      Fmt.pf ppf "https://%a/%s" Domain_name.pp domain_name path

let endpoint_of_string str =
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
               (function `Atom x -> x | `String x -> Fmt.strf "%S" x)
               m.Emile.local) in
        (match fst m.Emile.domain with
        | `Domain vs -> Domain_name.of_strings vs >>= Domain_name.host
        | `Literal v -> Domain_name.of_string v >>= Domain_name.host
        | `Addr _ -> R.error_msg "domain part must be a domain")
        >>= fun domain_name -> R.ok { scheme = `SSH user; path; domain_name }
    | _ -> R.error_msg "invalid pattern" in
  let parse_uri x =
    let uri = Uri.of_string x in
    match (Uri.scheme uri, Uri.host uri, Uri.path uri) with
    | Some "git", Some domain_name, path ->
        Domain_name.of_string domain_name >>= Domain_name.host
        >>= fun domain_name -> R.ok { scheme = `Git; path; domain_name }
    | Some "http", Some domain_name, path ->
        Domain_name.of_string domain_name >>= Domain_name.host
        >>= fun domain_name -> R.ok { scheme = `HTTP []; path; domain_name }
    | Some "https", Some domain_name, path ->
        Domain_name.of_string domain_name >>= Domain_name.host
        >>= fun domain_name -> R.ok { scheme = `HTTPS []; domain_name; path }
    | _ -> R.error_msgf "invalid uri: %a" Uri.pp uri in
  match (parse_ssh str, parse_uri str) with
  | Ok edn, _ -> R.ok edn
  | Error _, Ok edn -> R.ok edn
  | Error _, Error _ -> R.error_msgf "Invalid endpoint: %s" str

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Pack : APPEND with type +'a fiber = 'a Lwt.t)
    (Index : APPEND with type +'a fiber = 'a Lwt.t)
    (Conduit : Conduit.S
                 with type +'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (HTTP : HTTP)
    (Uid : UID)
    (Ref : Sigs.REF) =
struct
  let src = Logs.Src.create "git-fetch"

  module Log = (val Logs.src_log src : Logs.LOG)

  module Thin = Carton_lwt.Thin.Make (Uid)

  let fs =
    let open Rresult in
    let open Lwt.Infix in
    {
      Thin.create =
        (fun t path ->
          Pack.create t path >|= R.reword_error (R.msgf "%a" Pack.pp_error));
      Thin.append = Pack.append;
      Thin.map = Pack.map;
      Thin.close =
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
      Uid.feed ctx (Bigstringaf.of_string ~off ~len str) in

    let ctx =
      match kind with
      | `A -> feed_string ctx (Fmt.strf "commit %d\000" len)
      | `B -> feed_string ctx (Fmt.strf "tree %d\000" len)
      | `C -> feed_string ctx (Fmt.strf "blob %d\000" len)
      | `D -> feed_string ctx (Fmt.strf "tag %d\000" len) in
    let ctx = Uid.feed ctx ~off ~len buf in
    Uid.get ctx

  let ( >>? ) = Lwt_result.bind

  module CartonSched = Carton.Make (Lwt)

  let sched =
    let open Lwt.Infix in
    let open CartonSched in
    {
      Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Carton.return = (fun x -> inj (Lwt.return x));
    }

  let finish_it t ~pack ~weight ~where offsets =
    let open Lwt.Infix in
    Pack.create t pack >>? fun fd ->
    let zl_buffer = De.bigstring_create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    let pack =
      Carton.Dec.make fd ~allocate ~z:zl_buffer ~uid_ln:Uid.length
        ~uid_rw:Uid.of_raw_string (fun uid -> Hashtbl.find where uid) in
    let map fd ~pos len =
      let max = Int64.sub weight pos in
      let len = min max (Int64.of_int len) in
      let len = Int64.to_int len in
      CartonSched.inj (Pack.map t fd ~pos len) in
    let rec go entries = function
      | [] -> Lwt.return entries
      | (offset, crc) :: offsets ->
          Lwt.catch
            (fun () ->
              Carton.Dec.weight_of_offset sched ~map pack
                ~weight:Carton.Dec.null offset
              |> CartonSched.prj
              >>= fun weight ->
              let raw = Carton.Dec.make_raw ~weight in
              Carton.Dec.of_offset sched ~map pack raw ~cursor:offset
              |> CartonSched.prj
              >>= fun v ->
              let kind = Carton.Dec.kind v in
              let raw = Carton.Dec.raw v in
              let len = Carton.Dec.len v in
              let uid = digest ~kind ~off:0 ~len raw in
              go ({ Carton.Dec.Idx.offset; crc; uid } :: entries) offsets)
            (fun exn ->
              Printexc.print_backtrace stdout ;
              Lwt.fail exn) in
    go [] offsets >>= fun entries ->
    Pack.close t fd >>? fun () -> Lwt.return_ok entries

  let run_pck ~light_load ~heavy_load stream t ~src ~dst =
    let open Rresult in
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        Log.debug (fun m -> m "Start to verify the given stream.") ;
        Thin.verify ~digest ~threads:1 t src fs stream)
      (function
        | Failure err -> Lwt.return_error (R.msg err)
        | Invalid_argument err -> Lwt.return_error (R.msg err)
        | exn -> Lwt.return_error (`Exn exn))
    >>= function
    | Error _ as err -> Lwt.return err
    | Ok (_, [], [], entries, _weight, uid) ->
        Log.debug (fun m -> m "Given PACK file is not thin, move it!") ;
        Pack.move t ~src ~dst >|= R.reword_error (R.msgf "%a" Pack.pp_error)
        >>? fun () -> Lwt.return_ok (uid, Array.of_list entries)
    | Ok (n, uids, unresolveds, entries, weight, _uid) ->
        Log.debug (fun m -> m "Given PACK file is thin, canonicalize!") ;
        Thin.canonicalize ~light_load ~heavy_load ~src ~dst t fs n uids weight
        >>? fun (shift, weight, uid, entries') ->
        let where = Hashtbl.create 0x100 in
        let entries =
          let fold ({ Carton.Dec.Idx.offset; uid; _ } as entry) =
            let offset = Int64.add offset shift in
            Hashtbl.add where uid offset ;
            { entry with Carton.Dec.Idx.offset } in
          List.map fold entries in
        List.iter
          (fun { Carton.Dec.Idx.offset; uid; _ } ->
            Hashtbl.add where uid offset)
          entries' ;
        let unresolveds =
          let fold (offset, crc) = (Int64.add offset shift, crc) in
          List.map fold unresolveds in
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
    Enc.dst encoder buf 0 (Bigstringaf.length buf) ;
    Index.create t dst >>? fun fd ->
    let rec go = function
      | `Partial ->
          let len = Bigstringaf.length buf - Enc.dst_rem encoder in
          Index.append t fd (Bigstringaf.substring buf ~off:0 ~len)
          >>= fun () ->
          Enc.dst encoder buf 0 (Bigstringaf.length buf) ;
          go (Enc.encode encoder `Await)
      | `Ok -> Lwt.return_ok () in
    go (Enc.encode encoder `Await) >>? fun () -> Index.close t fd

  let run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    run_pck ~light_load ~heavy_load stream t_pck ~src ~dst
    >>? fun (pack, entries) ->
    run_idx t_idx ~dst:idx ~pack entries
    >|= R.reword_error (R.msgf "%a" Index.pp_error)
    >>? fun () -> Lwt.return_ok pack

  module Flow = struct
    type +'a fiber = 'a Lwt.t

    type t = Conduit.flow

    include Conduit
  end

  module Fetch = Nss.Fetch.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)
  module Push = Nss.Push.Make (Scheduler) (Lwt) (Flow) (Uid) (Ref)

  let fetch_v1 ?prelude ~capabilities path ~resolvers ?want domain_name store
      access fetch_cfg pack =
    let open Lwt.Infix in
    Conduit.resolve resolvers domain_name >>? fun flow ->
    Lwt.try_bind
      (fun () ->
        Fetch.fetch_v1 ?prelude ~capabilities ?want ~host:domain_name path flow
          store access fetch_cfg (fun (payload, off, len) ->
            let v = String.sub payload off len in
            pack (Some (v, 0, len))))
      (fun refs ->
        pack None ;
        Conduit.close flow >>? fun () -> Lwt.return_ok refs)
      (fun exn ->
        pack None ;
        Conduit.close flow >>= fun _ -> Lwt.fail exn)

  module Flow_http = struct
    type +'a fiber = 'a Lwt.t

    type t = {
      mutable ic : string;
      mutable oc : string;
      mutable pos : int;
      resolvers : Conduit.resolvers;
      uri : Uri.t;
      headers : (string * string) list;
    }

    type error = [ `Msg of string ]

    let pp_error = Rresult.R.pp_msg

    let send t raw =
      let oc = t.oc ^ Cstruct.to_string raw in
      t.oc <- oc ;
      Lwt.return_ok (Cstruct.len raw)

    let rec recv t raw =
      if t.pos = String.length t.ic
      then (
        let open Lwt.Infix in
        (HTTP.post ~resolvers:t.resolvers ~headers:t.headers t.uri t.oc
        >|= Rresult.(R.reword_error (R.msgf "%a" HTTP.pp_error)))
        >>? fun (_resp, contents) ->
        t.ic <- t.ic ^ contents ;
        recv t raw)
      else
        let len = min (String.length t.ic - t.pos) (Cstruct.len raw) in
        Cstruct.blit_from_string t.ic t.pos raw 0 len ;
        t.pos <- t.pos + len ;
        Lwt.return_ok (`Input len)
  end

  module Fetch_http = Nss.Fetch.Make (Scheduler) (Lwt) (Flow_http) (Uid) (Ref)

  let http_fetch_v1 ~capabilities uri ?(headers = []) domain_name path
      ~resolvers ?want store access fetch_cfg pack =
    let open Rresult in
    let open Lwt.Infix in
    let uri0 = Fmt.strf "%a/info/refs?service=git-upload-pack" Uri.pp uri in
    let uri0 = Uri.of_string uri0 in
    HTTP.get ~resolvers ~headers uri0
    >|= R.reword_error (R.msgf "%a" HTTP.pp_error)
    >>? fun (_resp, contents) ->
    let uri1 = Fmt.strf "%a/git-upload-pack" Uri.pp uri in
    let uri1 = Uri.of_string uri1 in
    let flow =
      {
        Flow_http.ic = contents;
        pos = 0;
        oc = "";
        resolvers;
        uri = uri1;
        headers;
      } in
    Fetch_http.fetch_v1 ~prelude:false ~capabilities ?want ~host:domain_name
      path flow store access fetch_cfg (fun (payload, off, len) ->
        let v = String.sub payload off len in
        pack (Some (v, 0, len)))
    >>= fun refs ->
    pack None ;
    Lwt.return_ok refs

  let fetch ~resolvers (access, light_load, heavy_load) store edn
      ?(version = `V1) ?(capabilities = []) want t_pck t_idx ~src ~dst ~idx =
    let open Rresult in
    let open Lwt.Infix in
    let domain_name = edn.domain_name in
    let path = edn.path in
    let stream, pusher = Lwt_stream.create () in
    let pusher = function
      | Some (_, _, len) as v ->
          Log.debug (fun m -> m "Download %d byte(s) of the PACK file." len) ;
          pusher v
      | None ->
          Log.debug (fun m -> m "End of pack.") ;
          pusher None in
    let stream () = Lwt_stream.get stream in
    let run =
      match (version, edn.scheme) with
      | `V1, ((`Git | `SSH _) as scheme) ->
          let fetch_cfg = Nss.Fetch.configuration capabilities in
          let prelude = match scheme with `Git -> true | `SSH _ -> false in
          (* XXX(dinosaure): [prelude] is the only tweak needed between git:// and SSH. *)
          let run () =
            Lwt.both
              (fetch_v1 ~prelude ~capabilities path ~resolvers ~want domain_name
                 store access fetch_cfg pusher)
              (run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx)
            >>= fun (refs, idx) ->
            match (refs, idx) with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err in
          run
      | `V1, ((`HTTP _ | `HTTPS _) as scheme) ->
          let fetch_cfg = Nss.Fetch.configuration ~stateless:true capabilities in
          let uri, headers =
            match scheme with
            | `HTTP headers ->
                ( Uri.of_string
                    (Fmt.strf "http://%a%s.git" Domain_name.pp domain_name path),
                  headers )
            | `HTTPS headers ->
                ( Uri.of_string
                    (Fmt.strf "https://%a%s.git" Domain_name.pp domain_name
                       path),
                  headers ) in
          let run () =
            Lwt.both
              (http_fetch_v1 ~capabilities uri ~headers domain_name path
                 ~resolvers ~want store access fetch_cfg pusher)
              (run ~light_load ~heavy_load stream t_pck t_idx ~src ~dst ~idx)
            >>= fun (refs, idx) ->
            match (refs, idx) with
            | Ok refs, Ok uid -> Lwt.return_ok (`Pack (uid, refs))
            | (Error _ as err), _ -> Lwt.return err
            | Ok [], _ -> Lwt.return_ok `Empty
            | Ok _refs, (Error _ as err) -> Lwt.return err in
          run
      | _ -> assert false in
    Lwt.catch run (function
      | Failure err -> Lwt.return_error (R.msg err)
      | exn -> Lwt.return_error (`Exn exn))

  module Delta = Carton_lwt.Enc.Delta (Uid) (Verbose)

  let deltify ~light_load ~heavy_load ?(threads = 4) (uids : Uid.t list) =
    let open Lwt.Infix in
    let fold (uid : Uid.t) =
      light_load uid >|= fun (kind, length) ->
      Carton_lwt.Enc.make_entry ~kind ~length uid in
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
      | exception Not_found -> Lwt.return_none in
    let uid =
      { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }
    in
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.make_window ~bits:15;
      } in
    let ctx = ref Uid.empty in
    let cursor = ref 0 in
    Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12 ;
    stream (Some (Bigstringaf.to_string header)) ;
    ctx := Uid.feed !ctx header ~off:0 ~len:12 ;
    cursor := !cursor + 12 ;
    let encode_targets targets =
      let encode_target idx =
        Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor ;
        Carton_lwt.Enc.encode_target ~b ~find ~load:heavy_load ~uid
          targets.(idx) ~cursor:!cursor
        >>= fun (len, encoder) ->
        let rec go encoder =
          match Carton.Enc.N.encode ~o:b.o encoder with
          | `Flush (encoder, len) ->
              let payload = Bigstringaf.substring b.o ~off:0 ~len in
              stream (Some payload) ;
              ctx := Uid.feed !ctx b.o ~off:0 ~len ;
              cursor := !cursor + len ;
              let encoder =
                Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
              go encoder
          | `End -> Lwt.return () in
        let payload = Bigstringaf.substring b.o ~off:0 ~len in
        stream (Some payload) ;
        ctx := Uid.feed !ctx b.o ~off:0 ~len ;
        cursor := !cursor + len ;
        let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
        go encoder in
      let rec go idx =
        if idx < Array.length targets
        then encode_target idx >>= fun () -> go (succ idx)
        else Lwt.return () in
      go 0 in
    encode_targets targets >>= fun () ->
    let uid = Uid.((to_raw_string <.> get) !ctx) in
    stream (Some uid) ;
    stream None ;
    Lwt.return_unit

  let pack ~light_load ~heavy_load uids =
    let open Lwt.Infix in
    let stream, pusher = Lwt_stream.create () in
    let fiber () =
      deltify ~light_load ~heavy_load uids >>= fun (_, targets) ->
      pack ~heavy_load pusher targets in
    let stream () = Lwt_stream.get stream in
    Lwt.async fiber ;
    stream

  let push ?prelude ~resolvers ~capabilities path cmds domain_name store access
      push_cfg pack =
    let open Lwt.Infix in
    Conduit.resolve resolvers domain_name >>? fun flow ->
    Push.push ?prelude ~capabilities cmds ~host:domain_name path flow store
      access push_cfg pack
    >>= fun () -> Conduit.close flow

  let push ~resolvers (access, light_load, heavy_load) store edn
      ?(version = `V1) ?(capabilities = []) cmds =
    let open Rresult in
    match (version, edn.scheme) with
    | `V1, ((`Git | `SSH _) as scheme) ->
        let prelude = match scheme with `Git -> true | `SSH _ -> false in
        let domain_name = edn.domain_name in
        let path = edn.path in
        let push_cfg = Nss.Push.configuration () in
        let run () =
          push ~prelude ~resolvers ~capabilities path cmds domain_name store
            access push_cfg
            (pack ~light_load ~heavy_load) in
        Lwt.catch run (function
          | Failure err -> Lwt.return_error (R.msgf "%s" err)
          | exn -> Lwt.return_error (`Exn exn))
    | _ -> assert false
end
