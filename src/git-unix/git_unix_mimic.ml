open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Happy_eyeballs = struct
  type t = Happy_eyeballs_lwt.t
  type flow = Lwt_unix.file_descr

  let happy_eyeballs = Mimic.make ~name:"happy-eyeballs-lwt"
  let resolve = Happy_eyeballs_lwt.connect
end

module TCP = struct
  type flow = Lwt_unix.file_descr
  type error = [ `Refused | `Timeout | `Error of Unix.error * string * string ]

  type write_error =
    [ `Refused | `Timeout | `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf = function
    | `Error (err, f, v) ->
        Fmt.pf ppf "%s(%s) : %s" f v (Unix.error_message err)
    | `Refused -> Fmt.pf ppf "Connection refused"
    | `Timeout -> Fmt.pf ppf "Connection timeout"

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.pf ppf "Connection closed by peer"

  let read fd =
    let tmp = Bytes.create 0x1000 in
    let process () =
      Lwt_unix.read fd tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_ok `Eof
      | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let write fd ({ Cstruct.len; _ } as cs) =
    let rec process buf off max =
      Lwt_unix.write fd buf off max >>= fun len ->
      if max - len = 0 then Lwt.return_ok ()
      else process buf (off + len) (max - len)
    in
    let buf = Cstruct.to_bytes cs in
    Lwt.catch (fun () -> process buf 0 len) @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let rec writev fd = function
    | [] -> Lwt.return_ok ()
    | x :: r -> write fd x >>? fun () -> writev fd r

  let close fd = Lwt_unix.close fd

  type endpoint = Lwt_unix.sockaddr

  let connect sockaddr =
    let process () =
      let domain = Unix.domain_of_sockaddr sockaddr in
      let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket sockaddr >>= fun () -> Lwt.return_ok socket
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  (* fake *)
  type listener = {
    process : flow -> unit Lwt.t;
    keepalive : Mirage_protocols.Keepalive.t option;
  }

  type t = |
  type ipaddr = Ipaddr.t

  let disconnect _ = assert false
  let dst _ = assert false
  let write_nodelay _ _ = assert false
  let writev_nodelay _ _ = assert false
  let create_connection ?keepalive:_ _ _ = assert false
  let input _ ~src:_ ~dst:_ _ = assert false
  let listen _ = assert false
  let unlisten _ = assert false
end

module FIFO = struct
  open Lwt.Infix

  let ( >>? ) = Lwt_result.bind

  type flow = Lwt_unix.file_descr * Lwt_unix.file_descr
  type endpoint = Fpath.t
  type error = [ `Error of Unix.error * string * string ]
  type write_error = [ `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf (`Error (err, f, v)) =
    Fmt.pf ppf "%s(%s) : %s" f v (Unix.error_message err)

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.pf ppf "Closed by peer"

  let read (ic, _) =
    let tmp = Bytes.create 0x1000 in
    let process () =
      Lwt_unix.read ic tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_ok `Eof
      | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn

  let write (_, oc) ({ Cstruct.len; _ } as cs) =
    let rec process buf off max =
      Lwt_unix.write oc buf off max >>= fun len ->
      if max - len = 0 then Lwt.return_ok ()
      else process buf (off + len) (max - len)
    in
    let buf = Cstruct.to_bytes cs in
    Lwt.catch (fun () -> process buf 0 len) @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn

  let rec writev fd = function
    | [] -> Lwt.return_ok ()
    | x :: r -> write fd x >>? fun () -> writev fd r

  let close (ic, oc) = Lwt_unix.close ic >>= fun () -> Lwt_unix.close oc

  let connect fpath =
    let process () =
      Lwt_unix.openfile (Fpath.to_string fpath ^ "-ic") Unix.[ O_RDONLY ] 0o644
      >>= fun ic ->
      Lwt_unix.openfile (Fpath.to_string fpath ^ "-oc") Unix.[ O_WRONLY ] 0o644
      >>= fun oc -> Lwt.return_ok (ic, oc)
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn
end

let fifo_endpoint, _ = Mimic.register ~name:"fifo" (module FIFO)

module A = Git_mirage_tcp.Make (TCP) (Happy_eyeballs)
module B = Git_mirage_ssh.Make (Mclock) (TCP) (Happy_eyeballs)
module C = Git_mirage_http.Make (OS.Time) (Pclock) (TCP) (Happy_eyeballs)

let ctx happy_eyeballs =
  let ctx =
    Mimic.add Happy_eyeballs.happy_eyeballs happy_eyeballs Mimic.empty
  in
  A.connect ctx >>= fun ctx ->
  B.connect ctx >>= fun ctx ->
  C.connect ctx >>= fun ctx ->
  let k1 git_transmission git_scheme git_hostname =
    match git_transmission, git_scheme, Fpath.of_string git_hostname with
    | `Exec, `Scheme "fifo", Ok fpath -> Lwt.return_some fpath
    | _ -> Lwt.return_none
  in
  let k2 git_scheme =
    match git_scheme with
    | `Scheme "fifo" -> Lwt.return_some `Exec
    | _ -> Lwt.return_none
  in
  let ctx =
    Mimic.fold fifo_endpoint
      Mimic.Fun.
        [
          req Smart_git.git_transmission; req Smart_git.git_scheme;
          req Smart_git.git_hostname;
        ]
      ~k:k1 ctx
  in
  let ctx =
    Mimic.fold Smart_git.git_transmission
      Mimic.Fun.[ req Smart_git.git_scheme ]
      ~k:k2 ctx
  in
  Lwt.return ctx
