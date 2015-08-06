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

(** Loose Git objects. *)

type t =
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tag    of Tag.t
  | Tree   of Tree.t
(** The type for loose Git objects. *)

include Object.S with type t := t

val type_of: t -> Object_type.t
(** Return the object type. *)

val type_of_inflated: Mstruct.t -> Object_type.t
(** Return the type of the inflated object stored in the given
    buffer. *)

(** {2 Constructors} *)

val commit: Commit.t -> t
(** Cast a commit to an object. *)

val blob: Blob.t -> t
(** Cast a blob to an object. *)

val tree: Tree.t -> t
(** Cast a tree to an object. *)

val tag: Tag.t -> t
(** Cast a tag to an object. *)

(** {1 IO types} *)

type read = SHA.t -> t option Lwt.t
(** The type for functions reading values. *)

type read_inflated = SHA.t -> string option Lwt.t
(** The type for functions reading inflated values. *)

type write = t -> SHA.t Lwt.t
(** The type for functions writing values. *)

type write_inflated = string -> SHA.t Lwt.t
(** The type for functions writing raw values. *)

module Cache: sig

  (** {1 Global cache of values} *)

  val set_size: int -> unit
  (** Empty the current LRU cache, and create a new one with the given
      size. *)

  val clear: unit -> unit
  (** Empty the cache. *)

  val find: SHA.t -> t option
  (** Cache an inflated values. This is used by various operations, so
      it could be useful to look into it to speed-up operations which
      needs to search a pack file. *)

  val find_inflated: SHA.t -> string option
  (** Same as {!find} but store the inflated representation of the
      value. *)

  val add: SHA.t -> t -> unit
  (** Cache a value. *)

  val add_inflated: SHA.t -> string -> unit
  (** Cache an inflated value. *)

end

module type IO = sig

  include Object.IO with type t = t

  (** {2 Inflated values} *)

  val sha1: t -> SHA.t
  (** Return the SHA of the serialized contents. *)

  val add_header: Buffer.t -> Object_type.t -> int -> unit
  (** Append the given object header to a buffer.  *)

  val add_inflated: Buffer.t -> t -> unit
  (** Append the inflated serialization of an object to a buffer.
      Similar to [add], but without deflating the contents. *)

  val input_inflated: Mstruct.t -> t
  (** Build a value from an inflated contents. *)

end

module IO (D: SHA.DIGEST) (I: Inflate.S): IO
