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

module type STORE = sig
  module Hash : S.HASH
  module Inflate : S.INFLATE
  module Deflate : S.DEFLATE

  module Value :
    Value.S
    with module Hash := Hash
     and module Inflate := Inflate
     and module Deflate := Deflate

  module PEnc : Pack.P with module Hash := Hash and module Deflate := Deflate

  type t
  type error = private [> `Delta of PEnc.Delta.error]
  type kind = [`Commit | `Tree | `Tag | `Blob]

  val pp_error : error Fmt.t
  val read_inflated : t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
end

module Make (S : STORE) : sig
  type stream = unit -> Cstruct.t option Lwt.t

  val make_stream :
       S.t
    -> ?window:[`Memory of int | `Object of int]
    -> ?depth:int
    -> S.Value.t list
    -> (stream * (Crc32.t * int64) S.Hash.Map.t Lwt_mvar.t, S.error) result
       Lwt.t
end
