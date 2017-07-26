type t =
  { state          : Zlib.stream
  ; used_in        : int
  ; used_out       : int
  ; in_pos         : int
  ; in_len         : int
  ; out_pos        : int
  ; out_len        : int
  ; write          : int
  ; finish         : bool }
and error = Error
and window = unit

let window () = ()
let window_reset () = ()

let pp_error fmt Error = Format.fprintf fmt "(Inflate_error #zlib)"
let pp fmt _ = Format.fprintf fmt "#inflateState"

let default level =
  { state = Zlib.inflate_init true
  ; used_in = 0
  ; used_out = 0
  ; in_pos = 0
  ; in_len = 0
  ; out_pos = 0
  ; out_len = 0
  ; write = 0
  ; finish = false }

let used_in { used_in; _ }   = used_in
let used_out { used_out; _ } = used_out
let write { write; _ }       = write

let refill off len t =
  { t with in_pos = off; in_len = len; used_in = 0 }

let flush off len t =
  { t with out_pos = off; out_len = len; used_out = 0 }

let eval src' dst' t =
  if t.finish
  then `End t
  else if t.in_len - t.used_in = 0
  then `Await t
  else if t.out_len - t.used_out = 0
  then `Flush t
  else begin
    let src = Bytes.create t.in_len in
    let dst = Bytes.create t.out_len in

    Cstruct.blit_to_bytes src' t.in_pos src 0 t.in_len;

    let (finished, used_in, used_out) =
      Zlib.inflate t.state src
        (if t.finish then 0 else t.used_in)
        (if t.finish then 0 else t.in_len - t.used_in)
        dst
        t.used_out
        (t.out_len - t.used_out)
        Zlib.Z_SYNC_FLUSH
    in

    Cstruct.blit_from_bytes dst t.used_out dst' (t.out_pos + t.used_out) used_out;

    if finished
    then begin
      Zlib.inflate_end t.state;

      `End { t with used_in = t.used_in + used_in
                  ; used_out = t.used_out + used_out
                  ; write = t.write + used_out
                  ; finish = true }
    end else
      (if t.used_out + used_out = t.out_len
        then
          `Flush { t with used_in = t.used_in + used_in
                        ; used_out = t.used_out + used_out
                        ; write = t.write + used_out }
        else
          `Await { t with used_in = t.used_in + used_in
                        ; used_out = t.used_out + used_out
                        ; write = t.write + used_out })
  end
