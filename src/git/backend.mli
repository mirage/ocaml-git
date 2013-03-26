(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Git serialization backend. *)

(** In this module, we explain how to serialize/de-serialize git
    files. All the input and output function takes buffers as parameters,
    so at one point one needs to map the file system to these buffer. This
    is done in [Store]. *)

open Lib

(** File contents. *)
module type VALUE = sig
  type t

  (** Dump the contents to stderr. *)
  val dump: t -> unit

  (** Output the file in a buffer. *)
  val output: Buffer.t -> t -> unit

  (** Read the contents from an input buffer. *)
  val input: IO.buffer -> t

end

(** Blob objects. *)
module Blob: VALUE with type t := Model.blob

(** Commit objects. *)
module Commit: VALUE with type t := Model.commit

(** Tree objects. *)
module Tree: VALUE with type t := Model.tree

(** Tag objects. *)
module Tag: VALUE with type t := Model.tag

(** An object is either a tag/tree/commit/blob. *)
module Value: sig
  include VALUE with type t := Model.value
  val input: IO.buffer -> Model.value
  val input_inflated: IO.buffer -> Model.value
  val output_inflated: Model.value -> string
  val output: Model.value -> string
end

(** Pack indexes. *)
module Idx: VALUE with type t := Model.pack_index

(** Pack objects. *)
module PackedValue2: VALUE with type t := Model.packed_value

(** Pack objects. *)
module PackedValue3: VALUE with type t := Model.packed_value

(** Packs. *)
module Pack: sig

  include VALUE with type t := Model.pack

  (** Read a pack, given the associated pack index. *)
  val input: Model.pack_index -> IO.buffer -> Model.pack

  (** Apply a series of hunks to a base object. *)
  val apply_delta: IO.buffer Model.delta -> Model.value

end
