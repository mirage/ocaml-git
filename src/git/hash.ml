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

type hex = string

module Make (D: Digestif.S): S.HASH = struct
  type t = D.t
  type nonrec hex = hex

  module Digest = struct
    type nonrec t = t
    type ctx = D.ctx

    let init () = D.init ()
    let feed ctx cs = D.feed_bigstring ctx (Cstruct.to_bigarray cs)
    let get ctx = D.get ctx

    let length = D.digest_size
  end

  external unsafe_of_string : string -> t = "%identity"
  external unsafe_to_string : t -> string = "%identity"

  let of_string = unsafe_of_string
  let to_string = unsafe_to_string

  let get : t -> int -> char = fun t i -> String.get (t:>string) i

  let of_hex : hex -> t = fun buf -> D.of_hex buf
  let to_hex : t -> hex = fun x -> D.to_hex x

  (* XXX(dinosaure): Git need a lexicographical comparison function. *)
  let compare : t -> t -> int = fun a b -> String.compare (a:>string) (b:>string)
  let hash : t -> int = fun t -> Hashtbl.hash (t:>string)

  (* XXX(dinosaure): we use safe equal function provided by digestif. *)
  let equal = D.eq
  let pp = D.pp

  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
end

module SHA1 =  Make(Digestif.SHA1)
