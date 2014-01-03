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

open GitTypes

(** File contents. *)
module type VALUE = sig
  type t

  (** Dump the contents to stderr. *)
  val dump: t -> unit

  (** Output the file in a buffer. *)
  val output: Buffer.t -> t -> unit

  (** Read the contents from an input buffer. *)
  val input: Mstruct.t -> t

end

(** Blob objects. *)
module Blob: VALUE with type t := blob

(** Commit objects. *)
module Commit: VALUE with type t := commit

(** Tree objects. *)
module Tree: VALUE with type t := tree

(** Tag objects. *)
module Tag: VALUE with type t := tag

(** An object is either a tag/tree/commit/blob. *)
module Value: sig
  include VALUE with type t := value
  val input: Mstruct.t -> value
  val input_inflated: Mstruct.t -> value
  val output_inflated: value -> string
  val output: value -> string
end

(** Pack indexes. *)
module Idx: VALUE with type t := pack_index

(** Pack objects. *)
module PackedValue2: VALUE with type t := packed_value

(** Pack objects. *)
module PackedValue3: VALUE with type t := packed_value

(** Packs. *)
module Pack: sig

  include VALUE with type t := pack

  (** Read a pack, given the associated pack index. *)
  val input: pack_index -> Mstruct.t -> pack

  (** Unpack a packed value. The [read] and [write] function are used
      to read and write node contents (to the disk or in memory, depending
      on their implementation).

      Usage: [unpack ~read ~write idx offset packed_value]

      [offset] is the position of [packed_value] into the pack file:
      this is useful of delta offsets.  *)
  val unpack:
    read:(node -> Mstruct.t) ->
    write:(value -> node) -> int Node.Map.t ->
    int -> packed_value -> node

  (** Unpack a whole pack file. Useful when we have no index file,
      eg. after a fetch. *)
  val unpack_all:
    read:(node -> Mstruct.t) ->
    write:(value -> node) ->
    Mstruct.t -> node list

end
