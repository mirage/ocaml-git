let src = Logs.Src.create "git-tcp.net" ~doc:"logs git's net I/O event"

module Log = (val Logs.src_log src : Logs.LOG)

type socket = Lwt_unix.file_descr

let read = Lwt_unix.read
let write = Lwt_unix.write
let close = Lwt_unix.close
let port uri = match Uri.port uri with None -> 9418 | Some p -> p

let host uri =
  match Uri.host uri with
  | Some h -> h
  | None ->
      Fmt.kstrf failwith "Expected a git url with host: %a." Uri.pp_hum uri

let socket uri =
  let open Lwt.Infix in
  Log.debug (fun l ->
      l ~header:"socket" "Start to open connection on %a." Uri.pp_hum uri ) ;
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname (host uri)
  >>= fun he ->
  Lwt_unix.connect socket (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port uri))
  >|= fun () -> socket
