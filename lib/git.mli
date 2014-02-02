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

module type ISERIALIZABLE = sig

  (** Serialization of inflated contents. *)

  type t
  (** The file contents. *)

  val pretty: t -> string
  (** Almost as [to_string] but prettier (if possible). *)

  val dump: t -> unit
  (** Dump the contents to stderr. *)

  val output_inflated: Buffer.t -> t -> unit
  (** Output the inflated contents in a buffer. *)

  val input_inflated: Mstruct.t -> t
  (** Build a value from an inflated contents. *)

end

module type SERIALIZABLE = sig

  (** Serialization of full contents. *)

  type t
  (** The file contents. *)

  val pretty: t -> string
  (** Almost as [to_string] but prettier (if possible). *)

  val dump: t -> unit
  (** Dump the contents to stderr. *)

  val output: t -> Cstruct.buffer list
  (** Output the inflated contents in a list of buffers. *)

  val input: Mstruct.t -> t
  (** Build a value from an inflated contents. *)

end

module Blob: ISERIALIZABLE with type t := blob
(** Blob objects. *)

module Commit: ISERIALIZABLE with type t := commit
(** Commit objects. *)

module Tree: ISERIALIZABLE with type t := tree
(** Tree objects. *)

module Tag: ISERIALIZABLE with type t := tag
(** Tag objects. *)

include SERIALIZABLE with type t := value
(** All the objects kinds can be serializable. *)

include ISERIALIZABLE with type t := value
(** All the objects kinds can be serializable. *)

val sha1: value -> SHA1.t
(** Return the SHA1 of the serialized contents. *)

val type_of_inflated: Mstruct.t -> [`Blob|`Commit|`Tag|`Tree]
(** Return the type of the inflated object stored in the given
    buffer. *)

(** {2 Packed Objects} *)

module Idx: SERIALIZABLE with type t := pack_index
(** Pack indexes. *)

module PackedValue2: ISERIALIZABLE with type t := packed_value
(** Pack objects version 2. *)

module PackedValue3: ISERIALIZABLE with type t := packed_value
(** Pack objects version 3. *)

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

(** Cache *)

module Cache: SERIALIZABLE with type t := Cache.t

(** {2 Navigation through the filesystem} *)

val mem: succ:(sha1 -> successor list Lwt.t) -> sha1 -> string list -> bool Lwt.t
(** [mem t sha1 path] check wether we can go from the object named
    [sha1] to an other object following the [path] labels. *)

val find: succ:(sha1 -> successor list Lwt.t) -> sha1 -> string list -> sha1 option Lwt.t
(** [find t sha1 path] returns (if it exists) the object named [sha1]
    to an other object following the [path] labels. *)

val find_exn: succ:(sha1 -> successor list Lwt.t) -> sha1 -> string list -> sha1 Lwt.t
(** [find t sha1 path] returns (if it exists) the object named [sha1]
    to an other object following the [path] labels. Raise [Not_found]
    if no such object exist.*)
