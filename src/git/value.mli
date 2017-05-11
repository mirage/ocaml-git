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

type t =
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tag    of Tag.t
  | Tree   of Tree.t

include S.S with type t := t

val type_of: t -> Object_type.t
val type_of_inflated: Mstruct.t -> Object_type.t
val commit: Commit.t -> t
val blob: Blob.t -> t
val tree: Tree.t -> t
val tag: Tag.t -> t

type read = Hash.t -> t option Lwt.t
type read_inflated = Hash.t -> string option Lwt.t
type write = t -> Hash.t Lwt.t
type write_inflated = string -> Hash.t Lwt.t

module Cache: sig
  val set_size: int -> unit
  val clear: unit -> unit
  val find: Hash.t -> t option
  val find_inflated: Hash.t -> string option
  val add: Hash.t -> t -> unit
  val add_inflated: Hash.t -> string -> unit
end

module type IO = sig
  include S.IO with type t = t
  val name: t -> Hash.t
  val add_header: Buffer.t -> Object_type.t -> int -> unit
  val add_inflated: Buffer.t -> t -> unit
  val input_inflated: Mstruct.t -> t
end

module IO (D: Hash.DIGEST) (I: Inflate.S): IO
