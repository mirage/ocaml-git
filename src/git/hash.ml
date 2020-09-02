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

module Make (Digestif : Digestif.S) :
  S.HASH
    with type t = Digestif.t
     and type ctx = Digestif.ctx
     and type kind = Digestif.kind = struct
  include Digestif

  module Ordered = struct
    type t = Digestif.t

    let equal = Digestif.equal
    let hash = Hashtbl.hash
    let compare = Digestif.unsafe_compare
  end

  let equal = Ordered.equal
  let hash = Ordered.hash
  let compare = Ordered.compare
  let read h i = Char.code @@ (Digestif.to_raw_string h).[i]
  let null = Digestif.digest_string ""
  let length = Digestif.digest_size
  let feed ctx ?off ?len buf = Digestif.feed_bigstring ctx ?off ?len buf

  module Set : Set.S with type elt = t = Set.Make (Ordered)
  module Map : Map.S with type key = t = Map.Make (Ordered)

  let feed_cstruct t s = Digestif.feed_bigstring t (Cstruct.to_bigarray s)
end
