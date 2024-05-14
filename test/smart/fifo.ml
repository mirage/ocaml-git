open Lwt.Infix

let src = Logs.Src.create "FIFO" ~doc:"logs FIFO event"

module Log = (val Logs.src_log src : Logs.LOG)

type flow = {
  ic : Lwt_unix.file_descr;
  oc : Lwt_unix.file_descr;
  linger : Bytes.t;
  mutable closed : bool;
}

type endpoint = Fpath.t * Fpath.t
type error = |
type write_error = [ `Closed ]

let pp_error : error Fmt.t = fun _ppf -> function _ -> .
let closed_by_peer = "Closed by peer"
let pp_write_error ppf = function `Closed -> Fmt.string ppf closed_by_peer
let io_buffer_size = 65536

let connect (ic, oc) =
  let open Lwt.Infix in
  Log.debug (fun m -> m "Start to recv and send over named pipes.");
  Lwt_unix.openfile (Fpath.to_string ic) Unix.[ O_RDONLY ] 0o600 >>= fun ic ->
  Lwt_unix.openfile (Fpath.to_string oc) Unix.[ O_WRONLY ] 0o600 >>= fun oc ->
  Lwt.return_ok { ic; oc; linger = Bytes.create io_buffer_size; closed = false }

let read { ic; linger; closed; _ } =
  if closed then Lwt.return_ok `Eof
  else
    Lwt_unix.read ic linger 0 (Bytes.length linger) >>= function
    | 0 -> Lwt.return_ok `Eof
    | len -> Lwt.return_ok (`Data (Cstruct.of_bytes linger ~off:0 ~len))

let write { oc; closed; _ } cs =
  if closed then Lwt.return_error `Closed
  else
    let rec go ({ Cstruct.buffer; off; len } as cs) =
      if len = 0 then Lwt.return_ok ()
      else
        Lwt_bytes.write oc buffer off len >>= fun len ->
        go (Cstruct.shift cs len)
    in
    go cs

let writev t css =
  let rec go = function
    | [] -> Lwt.return_ok ()
    | hd :: tl -> (
        write t hd >>= function
        | Ok () -> go tl
        | Error _ as err -> Lwt.return err)
  in
  go css

let close t = Lwt_unix.close t.ic >>= fun () -> Lwt_unix.close t.oc
let shutdown t _ = close t
