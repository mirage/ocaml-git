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

type t = Bytes.t
type hex = string

module Make (D: Digestif_sig.S) = struct
  type nonrec t = t
  type nonrec hex = hex
  module Digest = struct
    type t = Bytes.t
    type ctx = D.Bytes.ctx
    type buffer = Cstruct.t

    let init () = D.Bytes.init ()
    let feed ctx cs = D.Bytes.feed_bigstring ctx (Cstruct.to_bigarray cs)
    let get ctx = D.Bytes.get ctx

    let length = D.digest_size
  end

  let of_string = Bytes.of_string
  let to_string = Bytes.to_string
  let get = Bytes.get

  let of_hex buf = D.Bytes.of_hex (Bytes.unsafe_of_string buf)
  let to_hex x = D.Bytes.to_hex x |> Bytes.to_string

  module Map = Map.Make(Bytes)
  module Set = Set.Make(Bytes)

  let compare = Bytes.compare
  let equal = Bytes.equal
  let hash : Bytes.t -> int = Hashtbl.hash
  let pp ppf hash = Fmt.string ppf (to_hex hash)
end

(* XXX: this breaks as jbuilder doesn't the linking trick yet *)
(* module SHA1 =  Make(Digestif.SHA1) *)
