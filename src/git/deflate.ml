(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

open Decompress
module Deflate = Zlib_deflate

type t = (B.bs, B.bs) Deflate.t
and error = Deflate.error

let pp_error: error Fmt.t = Deflate.pp_error

let default level: t = Deflate.default ~proof:B.proof_bigstring level

let finish: t -> t = Deflate.finish
let used_in: t -> int = Deflate.used_in
let used_out: t -> int = Deflate.used_out
let no_flush: int -> int -> t -> t = Deflate.no_flush
let flush: int -> int -> t -> t = Deflate.flush

let eval ~src:src' ~dst:dst' t:
  [ `Await of t
  | `Flush of t
  | `Error of t * error
  | `End of t ]
  =
  let src = B.from_bigstring @@ Cstruct.to_bigarray src' in
  let dst = B.from_bigstring @@ Cstruct.to_bigarray dst' in

  Deflate.eval src dst t
