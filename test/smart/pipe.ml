type flow = { process : Lwt_process.process_full; buffer : Bytes.t }

let io_buffer_size = 65536

type endpoint = {
  cmd : string;
  args : string array;
  env : string array;
  cwd : string option;
}

type error = |
type write_error = [ `Couldn't_connect | `Closed ]

let pp_error : error Fmt.t = fun _ppf -> function _ -> .
let closed_by_peer = "Closed by peer"

let pp_write_error ppf = function
  | `Closed -> Fmt.string ppf closed_by_peer
  | `Couldn't_connect -> Fmt.string ppf "Couldn't connect"

let connect ({ cmd; args; env; cwd } : endpoint) =
  (try
     let process = Lwt_process.open_process_full ?cwd ~env (cmd, args) in
     let buffer = Bytes.create io_buffer_size in
     Ok { process; buffer }
   with _exn -> Error `Couldn't_connect)
  |> Lwt.return

let read { process; buffer } =
  match process#state with
  | Exited _ -> Lwt.return_ok `Eof
  | Running -> (
      let open Lwt.Syntax in
      let+ len =
        Lwt_io.read_into process#stdout buffer 0 (Bytes.length buffer)
      in
      match len with
      | 0 -> Ok `Eof
      | len -> Ok (`Data (Cstruct.of_bytes buffer ~off:0 ~len)))

let write { process; _ } cs =
  match process#state with
  | Exited _ -> Lwt.return_error `Closed
  | Running ->
      let rec loop ({ Cstruct.buffer; off; len } as cs) =
        if len = 0 then Lwt.return_ok ()
        else
          let open Lwt.Syntax in
          let* len = Lwt_io.write_from_bigstring process#stdin buffer off len in
          Cstruct.shift cs len |> loop
      in
      loop cs

let writev t css =
  let open Lwt.Infix in
  let rec go = function
    | [] -> Lwt.return_ok ()
    | hd :: tl -> (
        write t hd >>= function
        | Ok () -> go tl
        | Error _ as err -> Lwt.return err)
  in
  go css

let close { process; _ } =
  let open Lwt.Syntax in
  let+ (_ : Unix.process_status) = process#close in
  ()

let shutdown t _ = close t
