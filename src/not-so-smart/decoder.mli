(** Module for decoding Git pkt lines, as specified at
    https://github.com/git/git/blob/master/Documentation/technical/protocol-common.txt

    We define a "packet line" (aka a "packet") as

      |   4 bytes   || (enc-pkt-len)-4 |
      [ enc-pkt-len ][   pkt-content   ]
      |-------    pkt-len        ------|

    Example: "0009done\n" where [enc-pkt-len = 4] and [pkt-content = "done"] given we
    usually trim LF ("\n").

    "Encoded" packet length, [enc-pkt-len], is the first 4 bytes in the packet
    that encode the length of the packet in hex. It can have specific values of 0, 1, 2
    to encode flush, delimiter, and message (response end) packets respectively.
    Otherwise, it should be >= 4, i.e., 4 length bytes + the length of the packet content.

    In the docs, we define [min_pkt_len = 4] as in specs. *)

type decoder = { buffer : bytes; mutable pos : int; mutable max : int }

val io_buffer_size : int
val create : unit -> decoder
val of_string : string -> decoder
val end_of_input : decoder -> int

type error =
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  | `Expected_eol
  | `Expected_eol_or_space
  | `Unexpected_end_of_input
  | `No_enough_space
  | `Assert_predicate of char -> bool
  | `Invalid_pkt_line of string ]

val pp_error : error Fmt.t

type 'err info = { error : 'err; buffer : bytes; committed : int }

exception Leave of error info

type ('v, 'err) state =
  | Done of 'v
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      continue : int -> ('v, 'err) state;
      eof : unit -> ('v, 'err) state;
    }
  | Error of 'err info

val return : 'v -> decoder -> ('v, 'err) state
val bind : ('a, 'b) state -> f:('a -> ('c, 'b) state) -> ('c, 'b) state
val ( >>= ) : ('a, 'b) state -> ('a -> ('c, 'b) state) -> ('c, 'b) state

val leave_with : decoder -> error -> 'never
(** [leave_with d error] raises [Leave { error; buffer = d.buffer; committed = d.pos }]

  @raise Leave *)

val safe :
  (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state
(** [safe k decoder] wraps a call [k decoder] in a try-with block;
    if exception [Leave err] is raised, the function returns [Error of err] *)

val fail : decoder -> ([> error ] as 'err) -> ('v, 'err) state
val peek_char : decoder -> char option
val string : string -> decoder -> unit
val junk_char : decoder -> unit

val while1 : (char -> bool) -> decoder -> bytes * int * int
(** @return [decoder.buffer], updated [decoder.pos], # of bytes read *)

val at_least_one_pkt : decoder -> bool
(** returns true if [decoder.max - decoder.pos] is [>= min_pkt_len] and [>= pkt_len],
    where [pkt_len] is the length of a pkt line starting at [decoder.pos]. *)

val at_least_one_line : decoder -> bool

val prompt :
  ?strict:bool ->
  (decoder -> ('v, ([> error ] as 'err)) state) ->
  decoder ->
  ('v, 'err) state

val peek_while_eol : decoder -> bytes * int * int
val peek_while_eol_or_space : decoder -> bytes * int * int
val peek_pkt : decoder -> bytes * int * int

val junk_pkt : decoder -> unit
(** increase [decoder.pos] by [max min_pkt_len pkt_len], where [pkt_len] is the length
    of the pkt line starting at the current value of [decoder.pos] (before increasing) and
    [min_pkt_len = 4].

    @raise Invalid_argument if there aren't 4 bytes representing the length *)

val prompt_pkt :
  ?strict:bool ->
  (decoder -> ('a, ([> error ] as 'b)) state) ->
  decoder ->
  ('a, 'b) state

(**/*)

val pkt_len_unsafe : decoder -> int
