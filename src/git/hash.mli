(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include S.S
  val to_raw: t -> string
  val of_raw: string -> t
  val to_hex: t -> string
  val hex_length: t -> int
  val lt: t -> t -> bool
  val is_prefix: t -> t -> bool
  module Set: S.Set with type elt = t
  module Map: S.Map with type key = t
end

include S

exception Ambiguous of string

module Commit: S
module Tree: S
module Blob: S

val of_commit: Commit.t -> t
val to_commit: t -> Commit.t
val of_tree: Tree.t -> t
val to_tree: t -> Tree.t
val of_blob: Blob.t -> t
val to_blob: t -> Blob.t

module type H = sig
  include S
  include S.IO with type t := t
  val of_hex: string -> t
  val of_short_hex: string -> t
  val input_hex: Mstruct.t -> t
  val add_hex: Buffer.t -> t -> unit
  val is_short: t -> bool
  val zero: t
end

module type IO = sig
  include H with type t = t
  module Blob: H with type t = Blob.t
  module Tree: H with type t = Tree.t
  module Commit: H with type t = Commit.t
end

type 'a digest = 'a -> t

module type DIGEST = sig
  val cstruct: Cstruct.t digest
  val string: string digest
  val length: int
end

module IO (D: DIGEST): IO

module Array (D: DIGEST): sig
  val get: Cstruct.t -> int -> t
  val sub: Cstruct.t -> int -> int -> Cstruct.t
  val to_list: Cstruct.t -> t list
  val length: Cstruct.t -> int
  val linear_search: Cstruct.t -> t -> int option
  val binary_search: Cstruct.t -> t ->  int option
end
