let src = Logs.Src.create "FIFO" ~doc:"logs FIFO event"

module Log = (val Logs.src_log src : Logs.LOG)

type input = Cstruct.t
type output = Cstruct.t
type +'a io = 'a Lwt.t

type flow = {
  ic : Lwt_unix.file_descr;
  oc : Lwt_unix.file_descr;
  linger : Bytes.t;
  mutable closed : bool;
}

type endpoint = Fpath.t * Fpath.t
type error = [ `Closed | `Unix_error of Unix.error ]

let closed_by_peer = "Closed by peer"

let pp_error ppf = function
  | `Closed -> Fmt.string ppf closed_by_peer
  | `Unix_error err -> Fmt.pf ppf "fifo: %s" (Unix.error_message err)

let io_buffer_size = 65536

let connect (ic, oc) =
  let open Lwt.Infix in
  Log.debug (fun m -> m "Start to recv and send over named pipes.");
  Lwt_unix.openfile (Fpath.to_string ic) Unix.[ O_RDONLY ] 0o600 >>= fun ic ->
  Lwt_unix.openfile (Fpath.to_string oc) Unix.[ O_WRONLY ] 0o600 >>= fun oc ->
  Lwt.return_ok { ic; oc; linger = Bytes.create io_buffer_size; closed = false }

let recv { ic; linger; closed; _ } raw =
  if closed then Lwt.return_ok `End_of_flow
  else
    let rec process filled raw =
      let open Lwt.Infix in
      let max = Cstruct.len raw in
      Log.debug (fun m -> m "Start to recv over the input named pipe.");
      Lwt_unix.read ic linger 0 (min max (Bytes.length linger)) >>= fun len ->
      Log.debug (fun m -> m "Get %d byte(s)." len);
      if len = 0 then
        Lwt.return_ok (if filled = 0 then `End_of_flow else `Input filled)
      else (
        Cstruct.blit_from_bytes linger 0 raw 0 len;
        if len = Bytes.length linger && max > Bytes.length linger then
          if Lwt_unix.readable ic then
            process (filled + len) (Cstruct.shift raw len)
          else
            Lwt.return_ok
              (if filled + len = 0 then `End_of_flow else `Input (filled + len))
        else
          Lwt.return_ok
            (if filled + len = 0 then `End_of_flow else `Input (filled + len)) )
    in
    Lwt.catch (fun () -> process 0 raw) @@ function
    | Unix.Unix_error (err, _, _) -> Lwt.return_error (`Unix_error err)
    | exn -> Lwt.fail exn

let rec send ({ oc; closed; linger; _ } as t) raw =
  if closed then Lwt.return_error `Closed
  else (
    Log.debug (fun m ->
        m "Start to send over the output named pipe (%d byte(s))."
          (Cstruct.len raw));
    let max = Cstruct.len raw in
    let len0 = min (Bytes.length linger) max in
    Cstruct.blit_to_bytes raw 0 linger 0 len0;
    let process () =
      let open Lwt.Infix in
      Lwt_unix.write oc linger 0 len0 >>= fun len1 ->
      if len1 = len0 then
        if max > len0 then send t (Cstruct.shift raw len0)
        else Lwt.return_ok max
      else Lwt.return_ok len1
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (err, _, _) -> Lwt.return_error (`Unix_error err)
    | exn -> Lwt.fail exn )

let close t =
  let process () =
    let open Lwt.Infix in
    if not t.closed then (
      Lwt_unix.close t.ic >>= fun () ->
      Lwt_unix.close t.oc >>= fun () ->
      t.closed <- true;
      Lwt.return_ok () )
    else Lwt.return_ok ()
  in
  Lwt.catch process @@ function
  | Unix.Unix_error (err, _, _) -> Lwt.return_error (`Unix_error err)
  | exn -> Lwt.fail exn
