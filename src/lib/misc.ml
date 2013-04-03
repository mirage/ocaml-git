(* From OCaml's stdlib. See [Digest.to_hex] *)
let hex_encode s =
  let n = String.length s in
  let result = String.create (n*2) in
  for i = 0 to n-1 do
    String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result

(* From OCaml's stdlib. See [Digest.from_hex] *)
let hex_decode h =
  let n = String.length h in
  if n mod 2 <> 0 then (
    let msg =
      Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
    raise (Invalid_argument msg)
  );
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | c ->
      let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
      raise (Invalid_argument msg) in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = String.create (n / 2) in
  for i = 0 to n/2 - 1 do
    result.[i] <- Char.chr (byte (2 * i));
  done;
  result

(* From OPAM's [OpamMisc.cut_at] *)
let cut_at_aux fn s sep =
  try
    let i = fn s sep in
    let name = String.sub s 0 i in
    let version = String.sub s (i+1) (String.length s - i - 1) in
    Some (name, version)
  with _ ->
    None

let cut_at = cut_at_aux String.index

let rcut_at = cut_at_aux String.rindex

let split s c =
  Re_pcre.split ~rex:(Re_perl.compile (Re.char c)) s

(* From Zlib *)
let buffer_size = 1024
let uncompress ?(header = true) incr_used_in refill flush =
  let inbuf = String.create buffer_size
  and outbuf = String.create buffer_size in
  let zs = Zlib.inflate_init header in
  let rec uncompr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then uncompr_finish true else uncompr 0 incount
    end else begin
      let (finished, used_in, used_out) =
        Zlib.inflate zs inbuf inpos inavail outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
      incr_used_in used_in;
      flush outbuf used_out;
      if not finished then uncompr (inpos + used_in) (inavail - used_in)
    end
  and uncompr_finish first_finish =
    (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
       after the compressed stream in order to complete decompression
       and return finished = true. *)
    let dummy_byte = if first_finish && not header then 1 else 0 in
    let (finished, used_in, used_out) =
      Zlib.inflate zs inbuf 0 dummy_byte outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
    incr_used_in used_in;
    flush outbuf used_out;
    if not finished then uncompr_finish false
  in
  uncompr 0 0;
  Zlib.inflate_end zs

let refill_string input =
  let n = String.length input in
  let toread = ref n in
  fun buf ->
    let m =
      if !toread <= String.length buf then !toread
      else String.length buf in
    String.blit input (n - !toread) buf 0 m;
    toread := !toread - m;
    m

let flush_string output buf len =
  Buffer.add_substring output buf 0 len

let deflate_string input =
  let output = Buffer.create 1024 in
  Zlib.compress (refill_string input) (flush_string output);
  Buffer.contents output

let inflate_string input =
  let output = Buffer.create 1024 in
  let used_in = ref 0 in
  let incr_used_in n =
    used_in := !used_in + n in
  uncompress incr_used_in (refill_string input) (flush_string output);
  !used_in, Buffer.contents output

let string_of_in_channel ic =
  let buffer_size = 1024 in
  let tmp = String.create buffer_size in
  let buf = Buffer.create buffer_size in
  let rec read () =
    let len = input ic tmp 0 buffer_size in
    if len = 0 then Buffer.contents buf
    else (
      Buffer.add_substring buf tmp 0 len;
      read ()
    ) in
  read ()
