open Sigs

module Log =
  (val let src = Logs.Src.create "smart_flow" in
       Logs.src_log src
      : Logs.LOG)

let io_buffer_size = 65536

type ('a, 's) raise = exn -> ('a, 's) io

let run :
    type fl s.
    s scheduler ->
    ('a, s) raise ->
    (fl, 'error, s) flow ->
    fl ->
    ('res, [ `Protocol of Smart.error ]) Smart.t ->
    ('res, s) io =
 fun { bind; return } raise { recv; send; pp_error } flow fiber ->
  let ( >>= ) = bind in
  let tmp = Cstruct.create io_buffer_size in
  let failwithf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt in
  let rec go = function
    | Smart.Read { k; buffer; off; len; eof } -> (
        let max = min (Cstruct.length tmp) len in
        Log.debug (fun m -> m "Start to read %d byte(s)." max);
        recv flow (Cstruct.sub tmp 0 max) >>= function
        | Ok `End_of_flow ->
            Log.debug (fun m -> m "Got end of input.");
            go (eof ())
        | Ok (`Input len) ->
            Log.debug (fun m -> m "Got %d/%d byte(s)." len max);
            Cstruct.blit_to_bytes tmp 0 buffer off len;
            go (k len)
        | Error err ->
            Log.err (fun m -> m "Got an error: %a." pp_error err);
            failwithf "%a" pp_error err)
    | Smart.Write { k; buffer; off; len } ->
        let rec loop tmp =
          if Cstruct.length tmp = 0 then go (k len)
          else
            send flow tmp >>= function
            | Ok shift -> loop (Cstruct.shift tmp shift)
            | Error err -> failwithf "%a" pp_error err
        in
        Log.debug (fun m -> m "Write %d byte(s)." len);
        loop (Cstruct.of_string buffer ~off ~len)
    | Smart.Return v -> return v
    | Smart.Error (`Protocol err) ->
        Log.err (fun m -> m "Got a protocol error: %a." Smart.pp_error err);
        failwithf "%a" Smart.pp_error err
  in
  go fiber
