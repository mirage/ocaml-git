type socket = Lwt_unix.file_descr

let read = Lwt_unix.read
let write = Lwt_unix.write
let close = Lwt_unix.close

let socket host port =
  let open Lwt.Infix in

  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect socket (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >|= fun () -> socket
