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

(** Git serialization protocol *)

(** In this module, we explain how to serialize/de-serialize git
    files. All the input and output function takes buffers as parameters,
    so we do not make any assumptions on where the data comes from. *)

open GitTypes

module type CONTENTS = sig

  (** Serialization of file contents. *)

  type t
  (** The file contents. *)

  (** Dump the contents to stderr. *)
  val dump: t -> unit

  (** Output the file in a buffer. *)
  val output: Buffer.t -> t -> unit

  (** Read the contents from an input buffer. *)
  val input: Mstruct.t -> t

end

module Blob: CONTENTS with type t := blob
(** Blob objects. *)

module Commit: CONTENTS with type t := commit
(** Commit objects. *)

module Tree: CONTENTS with type t := tree
(** Tree objects. *)

module Tag: CONTENTS with type t := tag
(** Tag objects. *)

module Value: sig

  (** An object is either a tag/tree/commit/blob. *)

  include CONTENTS with type t := value
  val input: Mstruct.t -> value
  val input_inflated: Mstruct.t -> value
  val output_inflated: value -> string
  val output: value -> string
end

module Idx: CONTENTS with type t := pack_index
(** Pack indexes. *)

module PackedValue2: CONTENTS with type t := packed_value
(** Pack objects. *)

module PackedValue3: CONTENTS with type t := packed_value
(** Pack objects. *)

(** Packs. *)
module Pack: sig

  include CONTENTS with type t := pack

  (** Read a pack, given the associated pack index. *)
  val input: pack_index -> Mstruct.t -> pack

  val unpack:
    read:(sha1 -> Mstruct.t) ->
    write:(value -> sha1) -> int SHA1.Map.t ->
    int -> packed_value -> sha1
  (** Unpack a packed value. The [read] and [write] function are used
      to read and write object contents (to the disk or in memory,
      depending on their implementation).

      Usage: [unpack ~read ~write idx offset packed_value]

      [offset] is the position of [packed_value] into the pack file:
      this is useful of delta offsets.  *)

  val unpack_all:
    read:(sha1 -> Mstruct.t) ->
    write:(value -> sha1) ->
    Mstruct.t -> sha1 list
  (** Unpack a whole pack file. Useful when we have no index file,
      eg. after a fetch. *)

end
