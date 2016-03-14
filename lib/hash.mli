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

(** Hashing.

    This module handles both usual hashes and {i short} hashes, which
    are shorter sequences of bits with a valid hexadecimal
    representation. The only way to create short hashes is to use
    {!of_hex} with the [strict] argument set to false.

    When short hashes are used in normal Git operations, they can
    raise {!Ambiguous}.

    This module define various abstraction to distinguish between
    general, commit, node and blob hashes. It's just an abstraction
    layer, at runtine they will all be similar.
*)

module type S = sig

  (** {1 Signature for hash values} *)

  include Object.S
  (** The usual compare functions on hashes, but can raise
      {!Ambiguous} if one is short hash and is prefix to the other. *)

  val to_raw: t -> string
  (** Raw hash value. *)

  val of_raw: string -> t
  (** Abstract a raw hash value. *)

  val to_hex: t -> string
  (** [to_hex h] is [h]s' hex encoding. *)

  val hex_length: t -> int
  (** The number of hex digits in the hash. *)

  val lt: t -> t -> bool
  (** (<) relation between hash. *)

  val is_prefix: t -> t -> bool
  (** Check if a hash is a prefix of another hash. *)

  module Set: Misc.Set with type elt = t
  module Map: Misc.Map with type key = t

end

include S

exception Ambiguous of string
(** Exception raised when using short and ambiguous hashes. *)

module Commit: S
(** Commit nodes. *)

module Tree: S
(** Treee nodes. *)

module Blob: S
(** Blob nodes. *)

val of_commit: Commit.t -> t
(** A commit node is also a node. *)

val to_commit: t -> Commit.t
(** A node might be a commit. *)

val of_tree: Tree.t -> t
(** A tree node is also a node. *)

val to_tree: t -> Tree.t
(** A node might be a node. *)

val of_blob: Blob.t -> t
(** A blob node is also a node. *)

val to_blob: t -> Blob.t
  (** A node might be a blob node. *)

module type H = sig

  include S
  include Object.IO with type t := t

  val of_hex: string -> t
  (** Convert an hex-encoded string into a hash value. Raise
      {!Ambiguous} if the hash is short; in that case use
      {!of_short_hex}. *)

  val of_short_hex: string -> t
  (** Same as {!of_hex} but allow values shorter than 20
      characters. Such hash values are called {i short} hashes and can
      cause some functions to raise {!Ambiguous}. *)

  val input_hex: Mstruct.t -> t
  (** Read an hex-encoded hash value. *)

  val add_hex: Buffer.t -> t -> unit
  (** Add the hex-encoding of the hash value to the buffer. *)

  val is_short: t -> bool
  (** Check if the hash is short. *)

  val zero: t
  (** A hash full of zero. Useful for padding. *)

end

module type IO = sig
  include H with type t = t
  module Blob: H with type t = Blob.t
  module Tree: H with type t = Tree.t
  module Commit: H with type t = Commit.t
end

type 'a digest = 'a -> t
(** The type for digest functions. *)

(** The signature to compute hash digests. *)
module type DIGEST = sig
  val cstruct: Cstruct.t digest
  val string: string digest
  val length: int
end

module IO (D: DIGEST): IO

module Array (D: DIGEST): sig
  (** {1 Arrays of hashes}

      Similar to [Cstruct.t] but where the unit of offsets and length
      is the number of hash values instead of the number of bytes.*)

  val get: Cstruct.t -> int -> t
  (** [get buf n] is the [n]-th hash in the buffer [buf]. *)

  val sub: Cstruct.t -> int -> int -> Cstruct.t
  (** Same as [Cstruct.sub] but where [offset] and [length] as hash
      offsets. *)

  val to_list: Cstruct.t -> t list
  (** [to_list t] is the list of elements of [t]. *)

  val length: Cstruct.t -> int
  (** [length v] is the number of hashes store in [t]. *)

  val linear_search: Cstruct.t -> t -> int option
  (** [linear_search buf h] iterates through the hashes stored in the
      buffer [buf]. Return the indice of the first hash equals to
      [h]. Can raise {!Ambiguous} if [h] is short and more than one
      hash are similar.*)

  val binary_search: Cstruct.t -> t ->  int option
  (** [binary_search buf h] binary searches through the sorted array
      of hashes stored in [buf]. Return the indice of the first hash
      equal to [h]. Can raise {!Ambiguous} if [h] is short and more
      than one hash are similar. *)

end
