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

type t =
  | Blob   of Git_blob.t
  | Commit of Git_commit.t
  | Tag    of Git_tag.t
  | Tree   of Git_tree.t

include Git_s.S with type t := t

val type_of: t -> Git_object_type.t
val type_of_inflated: Mstruct.t -> Git_object_type.t
val commit: Git_commit.t -> t
val blob: Git_blob.t -> t
val tree: Git_tree.t -> t
val tag: Git_tag.t -> t

type read = Git_hash.t -> t option Lwt.t
type read_inflated = Git_hash.t -> string option Lwt.t
type write = t -> Git_hash.t Lwt.t
type write_inflated = string -> Git_hash.t Lwt.t

module Cache: sig
  val set_size: int -> unit
  val clear: unit -> unit
  val find: Git_hash.t -> t option
  val find_inflated: Git_hash.t -> string option
  val add: Git_hash.t -> t -> unit
  val add_inflated: Git_hash.t -> string -> unit
end

module type IO = sig
  include Git_s.IO with type t = t
  val name: t -> Git_hash.t
  val add_header: Buffer.t -> Git_object_type.t -> int -> unit
  val add_inflated: Buffer.t -> t -> unit
  val input_inflated: Mstruct.t -> t
end

module IO (D: Git_hash.DIGEST) (I: Git_inflate.S): IO
