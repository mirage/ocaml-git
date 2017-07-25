type t =
  { state          : Zlib.stream
  ; used_in        : int
  ; used_out       : int
  ; in_pos         : int
  ; in_len         : int
  ; out_pos        : int
  ; out_len        : int
  ; want_to_finish : bool }
and error = Error

let pp_error fmt Error = Format.fprintf fmt "(Deflate_error #zlib)" (* lazy *)

let empty_in = Bytes.create 0
let empty_out = Bytes.create 0

let default level =
  { state = Zlib.deflate_init level true
  ; used_in = 0
  ; used_out = 0
  ; in_pos = 0
  ; in_len = 0
  ; out_pos = 0
  ; out_len = 0
  ; want_to_finish = false }

let used_in { used_in; _ }   = used_in
let used_out { used_out; _ } = used_out

let finish t =
  { t with want_to_finish = true
         ; in_pos = 0
         ; in_len = 0
         ; used_in = 0 }

let no_flush off len t =
  { t with in_pos = off; in_len = len; used_in = 0 }

let sync_flush off len t =
  { t with in_pos = off; in_len = len; used_in = 0 }

let flush off len t =
  { t with out_pos = off; out_len = len; used_out = 0 }

let eval src' dst' t =
  if t.want_to_finish = false && t.in_len - t.used_in = 0
  then `Await t
  else if t.out_len - t.used_out = 0
  then `Flush t
  else begin
    let src = Bytes.create t.in_len in
    let dst = Bytes.create t.out_len in

    Cstruct.blit_to_bytes src' t.in_pos src 0 t.in_len;

    let (finished, used_in, used_out) =
      Zlib.deflate t.state src
        (if t.want_to_finish then 0 else t.used_in)
        (if t.want_to_finish then 0 else t.in_len - t.used_in)
        dst
        t.used_out
        (t.out_len - t.used_out)
        (if t.want_to_finish then Zlib.Z_FINISH else Zlib.Z_NO_FLUSH)
    in

    Cstruct.blit_from_bytes dst t.used_out dst' (t.out_pos + t.used_out) used_out;

    if finished
    then
      `End { t with used_in = t.used_in + used_in
                  ; used_out = t.used_out + used_out }
    else
      (if t.used_out + used_out = t.out_len
        then
          `Flush { t with used_in = t.used_in + used_in
                        ; used_out = t.used_out + used_out }
        else
          `Await { t with used_in = t.used_in + used_in
                        ; used_out = t.used_out + used_out })
  end
