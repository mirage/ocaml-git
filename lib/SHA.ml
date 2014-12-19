(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "sha1" end)

module type S = sig
  include Object.S
  val of_string: string -> t
  val of_cstruct: Cstruct.t -> t
  val to_raw: t -> string
  val of_raw: string -> t
  val to_hex: t -> string
  val of_hex: string -> t
  val input_hex: Mstruct.t -> t
  val add_hex: Buffer.t -> t -> unit
  val zero: t
  module Set: Misc.Set with type elt = t
  module Map: Misc.Map with type key = t
end

module SHA1_String = struct

  type t = string

  let equal x y = (x=y)

  let hash x = Hashtbl.hash x

  let compare x y = String.compare x y

  let to_raw x = x

  let of_raw x = x

  let of_string str =
    Cstruct.of_string str
    |> Nocrypto.Hash.SHA1.digest
    |> Cstruct.to_string

  let of_cstruct c =
    Nocrypto.Hash.SHA1.digest c
    |> Cstruct.to_string

  let to_hex t =
    let `Hex h = Hex.of_string t in
    h

  let of_hex h =
    Hex.to_string (`Hex h)

  let zero =
    of_hex (String.make 40 '0')

  let pretty = to_hex

  let input buf =
    Mstruct.get_string buf 20

  let add buf t =
    Buffer.add_string buf t

  let input_hex buf =
    of_hex (Mstruct.get_string buf (Mstruct.length buf))

  let add_hex buf t =
    Buffer.add_string buf (to_hex t)

  module X = struct
    type t = string
    let compare = String.compare
    let pretty = pretty
  end
  module Map = Misc.Map(X)
  module Set = Misc.Set(X)
end

include (SHA1_String: S)

module Commit: S = SHA1_String
module Tree: S = SHA1_String
module Blob: S = SHA1_String

let of_commit c = of_raw (Commit.to_raw c)
let to_commit n = Commit.of_raw (to_raw n)
let of_tree t = of_raw (Tree.to_raw t)
let to_tree n = Tree.of_raw (to_raw n)
let of_blob b = of_raw (Blob.to_raw b)
let to_blob n = Blob.of_raw (to_raw n)
