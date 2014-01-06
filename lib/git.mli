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

module type SERIALIZABLE = sig

  (** Serialization of file contents. *)

  type t
  (** The file contents. *)

  val dump: t -> unit
  (** Dump the contents to stderr. *)

  val output: Buffer.t -> t -> unit
  (** Output the file in a buffer. *)

  val input: Mstruct.t -> t
  (** Read the contents from an input buffer. *)

end

module Blob: SERIALIZABLE with type t := blob
(** Blob objects. *)

module Commit: SERIALIZABLE with type t := commit
(** Commit objects. *)

module Tree: SERIALIZABLE with type t := tree
(** Tree objects. *)

module Tag: SERIALIZABLE with type t := tag
(** Tag objects. *)

include SERIALIZABLE with type t := value
(** All the objects kinds can be serializable. *)

val type_of_inflated: Mstruct.t -> [`Blob|`Commit|`Tag|`Tree]
(** Return the type of the inflated object stored in the given
    buffer. *)

val input_inflated: Mstruct.t -> value
(** Unmarshal an inflated string to a value. *)

val output_inflated: Buffer.t -> value -> unit
(** Marshal a value to a buffer, without deflating it. *)

(** {2 Packed Objects} *)

module Idx: SERIALIZABLE with type t := pack_index
(** Pack indexes. *)

module PackedValue2: SERIALIZABLE with type t := packed_value
(** Pack objects. *)

module PackedValue3: SERIALIZABLE with type t := packed_value
(** Pack objects. *)

(** Packs. *)
module Pack: sig

  include SERIALIZABLE with type t := pack

  (** Read a pack, given the associated pack index. *)
  val input: pack_index -> Mstruct.t -> pack

  val unpack:
    read_inflated:(sha1 -> Mstruct.t Lwt.t) ->
    write:(value -> sha1 Lwt.t) ->
    idx:int SHA1.Map.t ->
    offset:int ->
    packed_value -> sha1 Lwt.t
  (** Unpack a packed value. The [read_inflated] and [write] function
      are used to read and write object contents (to the disk or in
      memory, depending on their implementation).

      Usage: [unpack ~read_inflated ~write ~idx ~offset packed_value]

      [offset] is the position of [packed_value] into the pack file:
      this is useful of delta offsets. *)

  val unpack_all:
    read_inflated:(sha1 -> Mstruct.t Lwt.t) ->
    write:(value -> sha1 Lwt.t) ->
    Mstruct.t -> sha1 list Lwt.t
    (** Unpack a whole pack file. Useful when we have no index file,
        eg. after a fetch. *)

end
