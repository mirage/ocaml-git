(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  val inflate: ?output_size:int -> Mstruct.t -> Mstruct.t option
  val deflate: ?level:int -> Cstruct.t -> Cstruct.t
end

module None: S

module type ZLIB = sig
  exception Error of string * string
  val compress:
    ?level: int -> ?header: bool ->
    (bytes -> int) -> (bytes -> int -> unit) -> unit
  type stream
  type flush_command =
    | Z_NO_FLUSH
    | Z_SYNC_FLUSH
    | Z_FULL_FLUSH
    | Z_FINISH
  val inflate_init: bool -> stream
  val inflate:
    stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
    -> bool * int * int
  val inflate_end: stream -> unit
end

module Make (Z: ZLIB): S
