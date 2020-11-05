type decoder = { buffer : Bytes.t; mutable pos : int; mutable max : int }

let io_buffer_size = 65536
let create () = { buffer = Bytes.create io_buffer_size; pos = 0; max = 0 }

let decoder_from x =
  let max = String.length x in
  let buffer = Bytes.of_string x in
  { buffer; pos = 0; max }

type error =
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  | `Expected_eol
  | `Expected_eol_or_space
  | `No_enough_space
  | `Unexpected_end_of_input
  | `Assert_predicate of char -> bool
  | `Invalid_pkt_line ]

let pp_error ppf = function
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Expected_char chr -> Fmt.pf ppf "Expected char: %02x" (Char.code chr)
  | `Unexpected_char chr -> Fmt.pf ppf "Unexpected char: %02x" (Char.code chr)
  | `Expected_string s -> Fmt.pf ppf "Expected_string: %s" s
  | `Expected_eol -> Fmt.string ppf "Expected end-of-line"
  | `Expected_eol_or_space -> Fmt.string ppf "Expected end-of-line or space"
  | `No_enough_space -> Fmt.string ppf "No enough space"
  | `Unexpected_end_of_input -> Fmt.string ppf "Unexpected end of input"
  | `Assert_predicate _ -> Fmt.string ppf "Assert predicate"
  | `Invalid_pkt_line -> Fmt.string ppf "Invalid PKT-line"

type 'err info = { error : 'err; buffer : Bytes.t; committed : int }

type ('v, 'err) state =
  | Done of 'v
  | Read of {
      buffer : Bytes.t;
      off : int;
      len : int;
      continue : int -> ('v, 'err) state;
      eof : unit -> ('v, 'err) state;
    }
  | Error of 'err info

exception Leave of error info

let return (type v) (v : v) _ : (v, 'err) state = Done v

let safe :
    (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state
    =
 fun k decoder ->
  try k decoder
  with Leave { error = #error as error; buffer; committed } ->
    Error { error = (error :> 'err); buffer; committed }

let end_of_input decoder = decoder.max

let peek_char decoder =
  if decoder.pos < end_of_input decoder then
    Some (Bytes.unsafe_get decoder.buffer decoder.pos)
  else None

(* XXX(dinosaure): in [angstrom] world, [peek_char] should try to read input
    again. However, SMTP is a line-directed protocol where we can ensure to
    have the full line at the top (with a queue) instead to have a
    systematic check (which slow-down the process). *)

let leave_with (decoder : decoder) error =
  raise (Leave { error; buffer = decoder.buffer; committed = decoder.pos })

let fail (decoder : decoder) error =
  Error { error; buffer = decoder.buffer; committed = decoder.pos }

let string str decoder =
  let idx = ref 0 in
  let len = String.length str in
  while
    decoder.pos + !idx < end_of_input decoder
    && !idx < len
    && Char.equal
         (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
         (String.unsafe_get str !idx)
  do
    incr idx
  done;
  if !idx = len then decoder.pos <- decoder.pos + len
  else leave_with decoder (`Expected_string str)

let junk_char decoder =
  if decoder.pos < end_of_input decoder then decoder.pos <- decoder.pos + 1
  else leave_with decoder `End_of_input

let while1 predicate decoder =
  let idx = ref decoder.pos in
  while
    !idx < end_of_input decoder
    && predicate (Bytes.unsafe_get decoder.buffer !idx)
  do
    incr idx
  done;
  if !idx - decoder.pos = 0 then
    leave_with decoder (`Assert_predicate predicate);
  let sub = decoder.buffer, decoder.pos, !idx - decoder.pos in
  (* XXX(dinosaure): avoid sub-string operation. *)
  decoder.pos <- !idx;
  sub

let at_least_one_line decoder =
  let pos = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while
    !pos < decoder.max
    &&
    (chr := Bytes.unsafe_get decoder.buffer !pos;
     not (!chr = '\n' && !has_cr))
  do
    has_cr := !chr = '\r';
    incr pos
  done;
  !pos < decoder.max && !chr = '\n' && !has_cr

let digit = function
  | 'a' .. 'f' as chr -> 10 + Char.code chr - Char.code 'a'
  | 'A' .. 'F' as chr -> 10 + Char.code chr - Char.code 'A'
  | '0' .. '9' as chr -> Char.code chr - Char.code '0'
  | _ -> invalid_arg "invalid digit"

let to_int ~base ~off ~len buf =
  let code = ref 0 in
  for i = 0 to len - 1 do
    let v = digit (Bytes.get buf (off + i)) in
    assert (v < base);
    code := (base * !code) + v
  done;
  !code

let at_least_one_pkt decoder =
  let len = decoder.max - decoder.pos in
  if len >= 4 then
    let pkt_len = to_int ~base:16 ~off:decoder.pos ~len:4 decoder.buffer in
    len - pkt_len >= 0
  else false

(* no header *)

let get_pkt_len decoder =
  let len = decoder.max - decoder.pos in
  if len >= 4 then
    let pkt_len = to_int ~base:16 ~off:decoder.pos ~len:4 decoder.buffer in
    Some pkt_len
  else None

(* XXX(dinosaure): to be able to do a /gentle close/, we do a hack.
   It seems that [git] is a bit /obtuse/ when it receives something
   which is not expected.

   For example:
   C> 0009done\n
   C> 0000

   Where [git] expects only:
   C> 0009done\n

   seems to cause a drastic connection close by the server when we want to
   download the PACK file. In such case, our decoder will be stick on the loop
   and waiting more where it received a partial chunk of the current /PKT/.

   So we provide an [eof] function which will (depends on [strict]):
   - return an error [`End_of_input] as usual
   - reformat the current /PKT/ to be able to emit the partial chunk
     to another process.

   The second case, we are able to unlock the ability to properly close the
   connection and to the other process (eg. [carton]) that we can not have more
   that what we have (more precisely, from a given /pusher/ to the stream used
   by the other process, we are able to do [pusher None]). By this way, we are
   able to unlock the /waiting-state/ of the other process. Then, in our side,
   we properly call [Flow.close].

   However, the error is a protocol error. The second branch [reliable_pkt] should
   never appear! It permits for us to gently close the connection and fallback
   the protocol error to another layer (eg. [carton] when it received finally a
   __not-full__ PACK file). The goal is to be more resilient at this layer. *)

let error_end_of_input decoder () = fail decoder `End_of_input

let reliable_pkt k decoder () =
  match get_pkt_len decoder with
  | Some _len ->
      let hdr = Fmt.strf "%04X" (decoder.max - decoder.pos) in
      Bytes.blit_string hdr 0 decoder.buffer decoder.pos 4;
      (* unsafe! *)
      k decoder
  | None ->
      Bytes.blit_string "0000" 0 decoder.buffer decoder.pos 4;
      decoder.max <- decoder.pos + 4;
      k decoder

let prompt :
    ?strict:bool ->
    (decoder -> ('v, ([> error ] as 'err)) state) ->
    decoder ->
    ('v, 'err) state =
 fun ?(strict = true) k decoder ->
  if decoder.pos > 0 then (
    (* XXX(dinosaure): compress *)
    let rest = decoder.max - decoder.pos in
    Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest;
    decoder.max <- rest;
    decoder.pos <- 0);
  let rec go off =
    try
      if
        off = Bytes.length decoder.buffer
        && decoder.pos > 0
        && not (at_least_one_pkt { decoder with max = off })
      then
        Error
          {
            error = `No_enough_space;
            buffer = decoder.buffer;
            committed = decoder.pos;
          }
      else if
        not (at_least_one_pkt { decoder with max = off })
        (* XXX(dinosaure): we make a new decoder here and we did __not__ set
           [decoder.max] owned by end-user, and this is exactly what we want. *)
      then
        Read
          {
            buffer = decoder.buffer;
            off;
            len = Bytes.length decoder.buffer - off;
            continue = (fun len -> go (off + len));
            eof =
              (if strict then error_end_of_input decoder (* fail *)
              else (
                decoder.max <- off;
                reliable_pkt k decoder));
          }
      else (
        decoder.max <- off;
        safe k decoder)
    with
    | _exn (* XXX(dinosaure): [at_least_one_pkt] can raise an exception. *) ->
      Error
        {
          error = `Invalid_pkt_line;
          buffer = decoder.buffer;
          committed = decoder.pos;
        }
  in
  go decoder.max

let peek_pkt decoder =
  let len = to_int ~base:16 ~off:decoder.pos ~len:4 decoder.buffer in
  if len >= 4 then decoder.buffer, decoder.pos + 4, len - 4
  else decoder.buffer, decoder.pos + 4, 0

let junk_pkt decoder =
  let len = to_int ~base:16 ~off:decoder.pos ~len:4 decoder.buffer in
  if len < 4 then decoder.pos <- decoder.pos + 4
  else decoder.pos <- decoder.pos + len

let peek_while_eol decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while
    !idx < end_of_input decoder
    &&
    (chr := Bytes.unsafe_get decoder.buffer !idx;
     not (!chr == '\n' && !has_cr))
  do
    has_cr := !chr == '\r';
    incr idx
  done;

  if !idx < end_of_input decoder && !chr == '\n' && !has_cr then (
    assert (!idx + 1 - decoder.pos > 1);
    decoder.buffer, decoder.pos, !idx + 1 - decoder.pos)
  else leave_with decoder `Expected_eol

let peek_while_eol_or_space decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while
    !idx < end_of_input decoder
    &&
    (chr := Bytes.unsafe_get decoder.buffer !idx;
     (not (!chr = '\n' && !has_cr)) && !chr <> ' ')
  do
    has_cr := !chr = '\r';
    incr idx
  done;

  if !idx < end_of_input decoder && ((!chr = '\n' && !has_cr) || !chr = ' ')
  then decoder.buffer, decoder.pos, !idx + 1 - decoder.pos
  else leave_with decoder `Expected_eol_or_space
