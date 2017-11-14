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

type tz_offset =
  { sign    : [ `Plus | `Minus ]
  ; hours   : int
  ; minutes : int }

type t =
  { name  : string
  ; email : string
  ; date  : int64 * tz_offset option }

include S.BASE
  with type t := t

module A
  : S.ANGSTROM
    with type t = t
module F
  : S.FARADAY
    with type t = t
module M
  : S.MINIENC
    with type t = t
module D
  : S.DECODER
    with type t = t
     and type raw = Cstruct.t
     and type init = Cstruct.t
     and type error = [ `Decoder of string ]


