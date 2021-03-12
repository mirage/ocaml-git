type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a

type 'a mode =
  | Rd : < rd : unit > mode
  | Wr : < wr : unit > mode
  | RdWr : < rd : unit ; wr : unit > mode

type t = Fpath.t
type 'a fd = Lwt_unix.file_descr
type uid = Fpath.t
type error = [ `Msg of string ]
type +'a fiber = 'a Lwt.t

open Lwt.Infix

let pp_error = Rresult.R.pp_msg

let create ?(trunc = true) ~mode:_ t path =
  let path = Fpath.(t // path) in
  let flags =
    match trunc with
    | true -> Unix.[ O_CREAT; O_RDWR; O_TRUNC ]
    | false -> Unix.[ O_CREAT; O_RDWR ]
  in
  Lwt_unix.openfile (Fpath.to_string path) flags 0o644 >>= Lwt.return_ok

let map _ fd ~pos len =
  let res =
    Mmap.V1.map_file
      (Lwt_unix.unix_file_descr fd)
      ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray res

let append _ fd str =
  let rec go off len =
    Lwt_unix.write_string fd str off len >>= fun len' ->
    if len - len' <= 0 then Lwt.return_unit else go (off + len') (len - len')
  in
  go 0 (String.length str)

let move t ~src ~dst =
  let src = Fpath.(t // src) in
  let dst = Fpath.(t // dst) in
  Lwt_unix.rename (Fpath.to_string src) (Fpath.to_string dst) >>= fun () ->
  Lwt.return_ok ()

let close _ fd = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
