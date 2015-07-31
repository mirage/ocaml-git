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

(** SHA1 hashes.

    This module handles both usual SHA1 hashes (20 bits) and {i short}
    hashes, which are shorter sequences of bits with a valid
    hexadecimal representation. The only way to create short hashes is
    to use {!of_hex} with the [strict] argument set to false.

    When short hashes are used in normal Git operations, they can
    raise {Ambiguous}.

    This module define various abstraction to distinguish between
    general, commit, node and blob hashes. It's just an abstraction
    layer, at runtine they will all be similar.
*)


module type S = sig

  (** {1 Signature for SHA1 values} *)

  include Object.S
  (** The usual compare functions on hashes, but can raise
      {!Ambiguous} if one is short hash and is prefix to the other. *)

  val of_string: string -> t
  (** Build a hash from a string. *)

  val of_cstruct: Cstruct.t -> t
  (** Build a hash from a cstruct. *)

  val to_raw: t -> string
  (** Raw SHA1 value. *)

  val of_raw: string -> t
  (** Abstract a raw SHA1 value. *)

  val to_hex: t -> string
  (** Display the hex encoding of the SHA1 hash. *)

  val of_hex: ?strict:bool -> string -> t
  (** Convert an hex-encoded string into a sha1 value.  If [strict] is
      not set (by default it is), we allow values shorter than 20
      characters. Such hash values are called {i short} hashes and can
      cause some functions to raise {!Ambiguous}. Can raise
      {!Ambiguous} if the hash is short but [strict] is set. *)

  val input_hex: Mstruct.t -> t
  (** Read an hex-encode SHA1 value. *)

  val add_hex: Buffer.t -> t -> unit
  (** Add the hex-encoding of the SHA1 value to the buffer. *)

  val zero: t
  (** A SHA1 full of zero. Useful for padding. *)

  val hex_length: t -> int
  (** The number of hex digits in the SHA1. *)

  val is_short: t -> bool
  (** Check if the SHA1 is abbreviated. *)

  val lt: t -> t -> bool
  (** (<) relation between SHA1s. *)

  val is_prefix: t -> t -> bool
  (** Check if a SHA1 is a prefix of another SHA1. *)

  module Set: Misc.Set with type elt = t
  module Map: Misc.Map with type key = t

end

(** Unique object identifiers using SHA1. *)

include S

module Commit: S
(** Commit nodes. *)

val of_commit: Commit.t -> t
(** A commit node is also a node. *)

val to_commit: t -> Commit.t
(** A node might be a commit. *)

module Tree: S
(** Treee nodes. *)

val of_tree: Tree.t -> t
(** A tree node is also a node. *)

val to_tree: t -> Tree.t
(** A node might be a node. *)

module Blob: S
(** Blob nodes. *)

val of_blob: Blob.t -> t
(** A blob node is also a node. *)

val to_blob: t -> Blob.t
(** A node might be a blob node. *)

exception Ambiguous of string
(** Exception raised when using short and ambiguous hashes. *)

module Array: sig
  (** {1 Arrays of SHA1s}

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
  (** [linear_search buf sha1] iterates through the hashes stored in
      the buffer [buf]. Return the indice of the first hash equals to
      [sha1]. Can raise {!Ambiguous} if [sha1] is short and more than
      one hash are similar.*)

  val binary_search: Cstruct.t -> t ->  int option
  (** [binary_search buf shat1] binary searches through the sorted
      array of hashes stored in [buf]. Return the indice of the first
      hash equal to [sha1]. Can raise {!Ambiguous} if [sha1] is short
      and more than one hash are similar. *)

end
