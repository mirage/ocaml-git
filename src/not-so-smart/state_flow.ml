open Sigs

module Log =
  (val let src = Logs.Src.create "state-flow" in
    Logs.src_log src
    : Logs.LOG)

let io_buffer_size = 65536

type ('a, 's) raise = exn -> ('a, 's) io

let run :
    type fl s err.
    s scheduler ->
    ('a, s) raise ->
    err Fmt.t ->
    (fl, 'error, s) flow ->
    fl ->
    ('res, [ `Protocol of err ]) State.t ->
    ('res, s) io =
 fun scheduler io_raise pp_error flow_ops flow state ->
  let { bind; return } = scheduler in
  let ( >>= ) = bind in

  let failwithf fmt =
    Format.kasprintf (fun err -> io_raise (Failure err)) fmt
  in

  let cbuff = Cstruct.create io_buffer_size in

  let rec unwrap = function
    | State.Return v ->
        Log.debug (fun m -> m "got return ");
        return v
    | Error (`Protocol err) ->
        Log.err (fun m -> m "Got a protocol error: %a." pp_error err);
        failwithf "%a" pp_error err
    | Read { k; buffer; off; len; eof } -> (
        let rd_n_bytes = min (Cstruct.length cbuff) len in
        Log.debug (fun m -> m "Start to read %d byte(s)." rd_n_bytes);
        flow_ops.recv flow (Cstruct.sub cbuff 0 rd_n_bytes) >>= function
        | Ok `End_of_flow ->
            Log.debug (fun m -> m "Got end of input.");
            unwrap (eof ())
        | Ok (`Input len) ->
            Log.debug (fun m -> m "Got %d/%d byte(s)." len rd_n_bytes);
            Cstruct.blit_to_bytes cbuff 0 buffer off len;
            unwrap (k len)
        | Error err ->
            Log.err (fun m -> m "Got an error: %a." flow_ops.pp_error err);
            failwithf "%a" flow_ops.pp_error err)
    | Write { k; buffer; off; len } ->
        (* TODO: almost always we can write in one go instead of calling a loop,
                 so we should try writing and call loop if we aren't done *)
        let rec loop tmp =
          if Cstruct.is_empty tmp then unwrap (k len)
          else
            flow_ops.send flow tmp >>= function
            | Ok shift ->
                Log.debug (fun m ->
                    m "Wrote %d byte(s). %s" shift (Cstruct.to_string tmp));
                loop (Cstruct.shift tmp shift)
            | Error err -> failwithf "%a" flow_ops.pp_error err
        in
        Cstruct.of_string buffer ~off ~len |> loop
  in

  unwrap state
