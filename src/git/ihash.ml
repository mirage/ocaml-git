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

module type FDIGEST =
sig
  type t
  type buffer

  type 'a digest = 'a -> t

  val cstruct : Cstruct.t digest
  val string  : string digest
  val bytes   : Bytes.t digest
  val digest  : buffer digest
  val digestv : buffer list digest

  val length  : int
end

module type IDIGEST =
sig
  type t
  type ctx
  type buffer

  val init   : unit -> ctx
  val feed   : ctx -> buffer -> unit
  val get    : ctx -> t

  val length : int
end

module type DIGEST =
sig
  type t
  type hash

  val digest : t -> hash
end
