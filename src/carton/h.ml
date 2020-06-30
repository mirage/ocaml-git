module Bigarray = Bigarray_compat

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let bigstring_length x = Bigarray.Array1.dim x [@@inline]

let bigstring_sub x off len = Bigarray.Array1.sub x off len [@@inline]

let bigstring_blit a b = Bigarray.Array1.blit a b [@@inline]

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"

external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"

external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"

external unsafe_set_char : bigstring -> int -> char -> unit = "%caml_ba_set_1"

let unsafe_blit src src_off dst dst_off len =
  let a = bigstring_sub src src_off len in
  let b = bigstring_sub dst dst_off len in
  bigstring_blit a b

let unsafe_blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1 do
    unsafe_set_char dst (dst_off + i) src.[src_off + i]
  done

let invalid_bounds off len =
  Fmt.invalid_arg "Out of bounds (off: %d, len: %d)" off len

let invalid_encode () = Fmt.invalid_arg "expected `Await encode"

let bigstring_to_string v =
  let len = bigstring_length v in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (unsafe_get_char v i)
  done ;
  Bytes.unsafe_to_string res

let output_bigstring oc buf off len =
  (* XXX(dinosaure): stupidly slow! *)
  let v = Bigarray.Array1.sub buf off len in
  let v = bigstring_to_string v in
  output_string oc v

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
 fun buf off -> Char.code (Bytes.get buf off)

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in

  let len0 = res land 3 in
  let len1 = res asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bytes_unsafe_get_uint32 tmp i in
    unsafe_set_uint32 buf (off + i) v
  done ;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bytes_unsafe_get_uint8 tmp i in
    unsafe_set_uint8 buf (off + i) v
  done ;
  res

let slow_blit src src_off dst dst_off len =
  for i = 0 to len - 1 do
    let v = unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 dst (dst_off + i) v
  done

let blit src src_off dst dst_off len =
  if dst_off - src_off < 4
  then slow_blit src src_off dst dst_off len
  else
    let len0 = len land 3 in
    let len1 = len asr 2 in

    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = unsafe_get_uint32 src (src_off + i) in
      unsafe_set_uint32 dst (dst_off + i) v
    done ;

    for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = unsafe_get_uint8 src (src_off + i) in
      unsafe_set_uint8 dst (dst_off + i) v
    done

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

let bigstring_of_string v =
  let len = String.length v in
  let res = bigstring_create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = string_unsafe_get_uint32 v i in
    unsafe_set_uint32 res i v
  done ;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = string_unsafe_get_uint8 v i in
    unsafe_set_uint8 res i v
  done ;
  res

let io_buffer_size = 65536

external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( <= ) : 'a -> 'a -> bool = "%lessequal"

external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]

let ( < ) (x : int) y = x < y [@@inline]

let ( <= ) (x : int) y = x <= y [@@inline]

module M = struct
  type src = [ `Channel of in_channel | `Manual | `String of string ]

  type decode = [ `Await | `Header of int * int | `End | `Malformed of string ]

  type decoder = {
    mutable source : bigstring;
    src : src;
    mutable dst : bigstring;
    mutable i : bigstring;
    mutable i_pos : int;
    mutable i_len : int;
    mutable t_len : int;
    mutable t_need : int;
    t_tmp : bigstring;
    mutable o_pos : int;
    mutable src_len : int;
    mutable dst_len : int;
    mutable s : state;
    mutable k : decoder -> ret;
  }

  and ret = Await | Stop | End | Malformed of string

  and state = Header | Postprocess | Cmd | Cp of int | It of int

  let variable_length buf off top =
    let p = ref off in
    let i = ref 0 in
    let len = ref 0 in

    while
      let cmd = unsafe_get_uint8 buf !p in
      incr p ;
      len := !len lor ((cmd land 0x7f) lsl !i) ;
      i := !i + 7 ;
      cmd land 0x80 != 0 && !p <= top
    do
      ()
    done ;
    (!p - off, !len)
    [@@inline]

  let eoi d =
    d.i <- bigstring_empty ;
    d.i_pos <- 0 ;
    d.i_len <- min_int

  let i_rem d = d.i_len - d.i_pos + 1 [@@inline]

  let src_rem = i_rem

  let dst_rem d = bigstring_length d.dst - d.o_pos

  let src_len { src_len; _ } = src_len

  let dst_len { dst_len; _ } = dst_len

  let malformedf fmt = Fmt.kstrf (fun s -> Malformed s) fmt

  let t_need d n =
    d.t_len <- 0 ;
    d.t_need <- n

  let src d s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l ;
    if l == 0
    then eoi d
    else (
      d.i <- s ;
      d.i_pos <- j ;
      d.i_len <- j + l - 1)

  let dst d s j l =
    match d.s with
    | Postprocess ->
        if j < 0 || l < 0 || j + l > bigstring_length s
        then invalid_bounds j l
        else if l < d.dst_len
        then Fmt.invalid_arg "Invalid destination"
        else (
          d.dst <- s ;
          d.o_pos <- j ;
          if bigstring_length d.source >= d.src_len then d.s <- Cmd)
    | _ -> Fmt.invalid_arg "Invalid call of dst"

  let pp_state ppf = function
    | Header -> Fmt.string ppf "Header"
    | Postprocess -> Fmt.string ppf "Postprocess"
    | Cmd -> Fmt.string ppf "Cmd"
    | Cp _ -> Fmt.string ppf "Cd"
    | It _ -> Fmt.string ppf "It"

  let source d src =
    match d.s with
    | Postprocess ->
        if bigstring_length src < d.src_len
        then Fmt.invalid_arg "Invalid source"
        else d.source <- src
    | _ -> Fmt.invalid_arg "Invalid call of source (state: %a)" pp_state d.s

  (* get new input in [d.i] and [k]ontinue. *)
  let refill k d =
    match d.src with
    | `String _ ->
        eoi d ;
        k d
    | `Channel ic ->
        let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
        src d d.i 0 res ;
        k d
    | `Manual ->
        d.k <- k ;
        Await

  let rec t_fill k d =
    let blit d len =
      unsafe_blit d.i d.i_pos d.t_tmp d.t_len len ;
      d.i_pos <- d.i_pos + len ;
      d.t_len <- d.t_len + len in
    let rem = i_rem d in
    if rem < 0
    then k d
    else
      let need = d.t_need - d.t_len in
      if rem < need
      then (
        blit d rem ;
        refill (t_fill k) d)
      else (
        blit d need ;
        k d)

  let required =
    let a = [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4 |] in
    fun x -> a.(x land 0xf) + a.(x lsr 4)

  let enough d =
    match d.s with
    | Cp cmd -> i_rem d >= required (cmd land 0x7f)
    | It len -> i_rem d >= len
    | _ -> assert false

  (* XXX(dinosaure): [enough] is called only after a [d.s <- (It _ | Cp _)]. *)

  let need d =
    match d.s with
    | Cp cmd -> required (cmd land 0x7f)
    | It len -> len
    | _ -> assert false

  (* XXX(dinosaure): [flambda] is able to optimize [let rec a .. and b .. and c ..]
     instead [match .. with A -> .. | B -> .. | C -> ..]. *)

  let rec cp d =
    let[@warning "-8"] (Cp command) = d.s in
    let p = ref (if d.t_len > 0 then 0 else d.i_pos) in
    let i = if d.t_len > 0 then d.t_tmp else d.i in
    let cp_off = ref 0 in
    let cp_len = ref 0 in

    if command land 0x01 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_off := v ;
      incr p) ;
    if command land 0x02 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_off := !cp_off lor (v lsl 8) ;
      incr p) ;
    if command land 0x04 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_off := !cp_off lor (v lsl 16) ;
      incr p) ;
    if command land 0x08 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_off := !cp_off lor (v lsl 24) ;
      incr p) ;
    if command land 0x10 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_len := v ;
      incr p) ;
    if command land 0x20 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_len := !cp_len lor (v lsl 8) ;
      incr p) ;
    if command land 0x40 != 0
    then (
      let v = unsafe_get_uint8 i !p in
      cp_len := !cp_len lor (v lsl 16) ;
      incr p) ;
    if !cp_len == 0 then cp_len := 0x10000 ;

    blit d.source !cp_off d.dst d.o_pos !cp_len ;
    if d.t_len > 0 then d.t_len <- 0 else d.i_pos <- !p ;
    d.o_pos <- d.o_pos + !cp_len ;
    d.s <- Cmd ;
    d.k <- decode_k ;
    decode_k d

  and it d =
    let[@warning "-8"] (It len) = d.s in

    if d.t_len > 0
    then (
      blit d.t_tmp 0 d.dst d.o_pos len ;
      d.t_len <- 0 ;
      d.o_pos <- d.o_pos + len ;
      d.s <- Cmd ;
      d.k <- decode_k ;
      decode_k d)
    else (
      blit d.i d.i_pos d.dst d.o_pos len ;
      d.i_pos <- d.i_pos + len ;
      d.o_pos <- d.o_pos + len ;
      d.s <- Cmd ;
      d.k <- decode_k ;
      decode_k d)

  and cmd d =
    let c = unsafe_get_uint8 d.i d.i_pos in

    if c == 0
    then malformedf "Invalid delta code (%02x)" c
    else (
      d.s <- (if c land 0x80 != 0 then Cp c else It c) ;
      d.i_pos <- d.i_pos + 1 ;

      if enough d
      then if c land 0x80 != 0 then cp d else it d
      else (
        t_need d (need d) ;
        t_fill (if c land 0x80 != 0 then cp else it) d))

  and decode_k d =
    let rem = i_rem d in

    if rem <= 0
    then if rem < 0 then End else refill decode_k d
    else
      match d.s with
      | Header ->
          if rem < 2 then Fmt.invalid_arg "Not enough space" ;
          (* TODO: [malformedf]? *)
          let x, src_len = variable_length d.i d.i_pos d.i_len in
          let y, dst_len = variable_length d.i (d.i_pos + x) d.i_len in

          (* XXX(dinosaure): ok, this code can only work if the first given buffer
             is large enough to store header. In the case of [carton], output buffer
             of [zlib]/input buffer of [h] is [io_buffer_size]. *)
          d.i_pos <- d.i_pos + x + y ;
          d.src_len <- src_len ;
          d.dst_len <- dst_len ;

          d.s <- Postprocess ;
          Stop
      | Postprocess -> Stop
      | Cmd -> cmd d
      | Cp cmd ->
          if required (cmd land 0x7f) <= rem
          then cp d
          else (
            t_need d (need d) ;
            t_fill cp d)
      | It len ->
          if len <= rem
          then it d
          else (
            t_need d (need d) ;
            t_fill it d)

  let decode d =
    match d.k d with
    | Await -> `Await
    | Stop -> `Header (d.src_len, d.dst_len)
    | End -> `End
    | Malformed err -> `Malformed err

  let decoder ?(source = bigstring_empty) src =
    let i, i_pos, i_len =
      match src with
      | `Manual -> (bigstring_empty, 1, 0)
      | `String x -> (bigstring_of_string x, 0, String.length x - 1)
      | `Channel _ -> (bigstring_create io_buffer_size, 1, 0) in
    {
      src;
      source;
      dst = bigstring_empty;
      i;
      i_pos;
      i_len;
      t_len = 0;
      t_need = 0;
      t_tmp = bigstring_create 128;
      o_pos = 0;
      src_len = 0;
      dst_len = 0;
      s = Header;
      k = decode_k;
    }
end

module N = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type encode = [ `Await | `Copy of int * int | `Insert of string | `End ]

  type encoder = {
    dst : dst;
    src_len : int;
    dst_len : int;
    mutable o : bigstring;
    mutable o_pos : int;
    mutable o_max : int;
    t : bigstring;
    (* XXX(dinosaure): [bytes]? *)
    mutable t_pos : int;
    mutable t_max : int;
    mutable s : s;
    mutable k : encoder -> encode -> [ `Ok | `Partial ];
  }

  and s = Header | Contents

  let o_rem e = e.o_max - e.o_pos + 1 [@@inline]

  let dst e s j l =
    if j < 0 || l < 0 || j + l > bigstring_length s then invalid_bounds j l ;
    e.o <- s ;
    e.o_pos <- j ;
    e.o_max <- j + l - 1

  let dst_rem encoder = o_rem encoder

  let partial k e = function
    | `Await -> k e
    | `Copy _ | `Insert _ | `End -> invalid_encode ()

  let flush k e =
    match e.dst with
    | `Manual ->
        e.k <- partial k ;
        `Partial
    | `Channel oc ->
        output_bigstring oc e.o 0 e.o_pos ;
        e.o_pos <- 0 ;
        k e
    | `Buffer b ->
        (* XXX(dinosaure): optimize it! *)
        for i = 0 to e.o_pos - 1 do
          Buffer.add_char b (unsafe_get_char e.o i)
        done ;
        e.o_pos <- 0 ;
        k e

  let cmd off len =
    let cmd = ref 0 in

    if off land 0x000000ff <> 0 then cmd := !cmd lor 0x01 ;
    if off land 0x0000ff00 <> 0 then cmd := !cmd lor 0x02 ;
    if off land 0x00ff0000 <> 0 then cmd := !cmd lor 0x04 ;
    if off land 0x7f000000 <> 0 then cmd := !cmd lor 0x08 ;

    if len land 0x0000ff <> 0 then cmd := !cmd lor 0x10 ;
    if len land 0x00ff00 <> 0 then cmd := !cmd lor 0x20 ;
    if len land 0xff0000 <> 0 then cmd := !cmd lor 0x40 ;

    !cmd
    [@@inline]

  let t_range e max =
    e.t_pos <- 0 ;
    e.t_max <- max

  let rec t_flush k e =
    let blit e l =
      unsafe_blit e.t e.t_pos e.o e.o_pos l ;
      e.o_pos <- e.o_pos + l ;
      e.t_pos <- e.t_pos + l in
    let rem = o_rem e in
    let len = e.t_max - e.t_pos + 1 in

    if rem < len
    then (
      blit e rem ;
      flush (t_flush k) e)
    else (
      blit e len ;
      k e)

  let rec encode_contents e v =
    let k e =
      e.k <- encode_contents ;
      `Ok in
    match v with
    | `Await -> k e
    | `Copy (off, len) ->
        let rem = o_rem e in
        let cmd = cmd off len in
        let required = 1 + M.required cmd in

        let s, j, k =
          if rem < required
          then (
            t_range e (required - 1) ;
            (e.t, 0, t_flush k))
          else
            let j = e.o_pos in
            e.o_pos <- e.o_pos + required ;
            (e.o, j, k) in

        unsafe_set_uint8 s j (cmd lor 0x80) ;
        let pos = ref (j + 1) in
        let off = ref off in
        while !off <> 0 do
          if !off land 0xff != 0
          then (
            unsafe_set_uint8 s !pos !off ;
            incr pos) ;
          off := !off asr 8
        done ;
        let len = ref len in
        while !len <> 0 do
          if !len land 0xff != 0
          then (
            unsafe_set_uint8 s !pos !len ;
            incr pos) ;
          len := !len asr 8
        done ;
        k e
    | `Insert p ->
        let rem = o_rem e in
        let required = 1 + String.length p in

        let s, j, k =
          if rem < required
          then (
            t_range e (required - 1) ;
            (e.t, 0, t_flush k))
          else
            let j = e.o_pos in
            e.o_pos <- e.o_pos + required ;
            (e.o, j, k) in

        unsafe_set_uint8 s j (String.length p) ;
        unsafe_blit_from_string p 0 s (j + 1) (String.length p) ;
        k e
    | `End -> flush k e

  let store_variable_length buf off length =
    let l = ref length in
    let off = ref off in
    while !l >= 0x80 do
      unsafe_set_uint8 buf !off (!l lor 0x80 land 0xff) ;
      incr off ;
      l := !l asr 7
    done ;
    unsafe_set_uint8 buf !off !l

  let needed length =
    let l = ref length in
    let o = ref 0 in
    while !l >= 0x80 do
      incr o ;
      l := !l asr 7
    done ;
    incr o ;
    !o
    [@@inline]

  let encode_header e v =
    let k e =
      e.k <- encode_contents (* XXX(dinosaure): short-cut [encode]. *) ;
      e.s <- Contents ;
      e.k e v in
    let ndd = needed e.src_len + needed e.dst_len in
    let rem = o_rem e in
    (* assert (ndd <= 10) ; *)
    if rem >= ndd
    then (
      store_variable_length e.o e.o_pos e.src_len ;
      store_variable_length e.o (e.o_pos + needed e.src_len) e.dst_len ;
      e.o_pos <- e.o_pos + ndd ;
      k e)
    else (
      t_range e ndd ;
      store_variable_length e.t 0 e.src_len ;
      store_variable_length e.t (needed e.src_len) e.dst_len ;
      t_flush k e)

  let encode e v = e.k e v

  let encoder dst ~src_len ~dst_len =
    let o, o_pos, o_max =
      match dst with
      | `Manual -> (bigstring_empty, 1, 0)
      | `Buffer _ | `Channel _ ->
          (bigstring_create io_buffer_size, 0, io_buffer_size - 1) in
    {
      dst;
      src_len;
      dst_len;
      o;
      o_pos;
      o_max;
      t = bigstring_create 128;
      t_pos = 1;
      t_max = 0;
      s = Header;
      k = encode_header;
    }
end
