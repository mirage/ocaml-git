let src = Logs.Src.create "git-tcp.net" ~doc:"logs git's net I/O event"

module Log = (val Logs.src_log src : Logs.LOG)

type endpoint = {
  uri    : Uri.t;
  headers: Cohttp.Header.t;
}

type socket = Lwt_unix.file_descr
type error = [`Unix of exn]

let pp_error ppf = function
  | `Unix (Unix.Unix_error (errno, fn, parameter)) ->
      Fmt.pf ppf "(Unix_error@ @[(%s, %s, %S)@])" (Unix.error_message errno) fn
        parameter
  | `Unix _ -> assert false

let read socket buf off len =
  Lwt.try_bind
    (fun () -> Lwt_unix.read socket buf off len)
    (fun ln -> Lwt.return_ok ln)
    (fun exn -> Lwt.return_error (`Unix exn))

let write socket buf off len =
  Lwt.try_bind
    (fun () -> Lwt_unix.write socket buf off len)
    (fun ln -> Lwt.return_ok ln)
    (fun exn -> Lwt.return_error (`Unix exn))

let close = Lwt_unix.close
let port uri = match Uri.port uri with None -> 9418 | Some p -> p

let host uri =
  match Uri.host uri with
  | Some h -> h
  | None ->
      Fmt.kstrf failwith "Expected a git url with host: %a." Uri.pp_hum uri

let socket (e : endpoint) =
  let open Lwt.Infix in
  let uri = e.uri in
  Log.debug (fun l ->
      l ~header:"socket" "Start to open connection on %a." Uri.pp_hum uri ) ;
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname (host uri)
  >>= fun he ->
  Lwt_unix.connect socket (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port uri))
  >|= fun () -> socket
