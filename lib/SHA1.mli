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

open Core_kernel.Std

module type S = sig


  (** Signature for SHA1 values *)

  include Object.S

  val create: string -> t
  (** Build a node from a raw bigstring. *)

  val to_hex: t -> string
  (** Display the hex encoding of the SHA1 hash. *)

  val of_hex: string -> t
  (** Convert an hex-encoded string into a sha1 value. *)

  val input_hex: Mstruct.t -> t
  (** Read an hex-encode SHA1 value. *)

  val add_hex: Bigbuffer.t -> t -> unit
  (** Add the hex-encoding of the SHA1 value to the buffer. *)

  val zero: t
  (** A SHA1 full of zero. Useful for padding. *)

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
