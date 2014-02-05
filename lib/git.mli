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
open Core_kernel.Std

module type ISERIALIZABLE = sig

  (** Serialization of inflated contents. *)

  type t
  (** The file contents. *)

  val pretty: t -> string
  (** Almost as [to_string] but prettier (if possible). *)

  val dump: t -> unit
  (** Dump the contents to stderr. *)

  val output_inflated: Bigbuffer.t -> t -> unit
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

module Blob: ISERIALIZABLE with type t = blob
(** Blob objects. *)

module Commit: ISERIALIZABLE with type t = commit
(** Commit objects. *)

module Tree: ISERIALIZABLE with type t = tree
(** Tree objects. *)

module Tag: ISERIALIZABLE with type t = tag
(** Tag objects. *)

include SERIALIZABLE with type t = value
(** All the objects kinds can be serializable. *)

val output_inflated: t -> Bigstring.t
(** Output the inflated contents in a buffer. *)

val input_inflated: Mstruct.t -> t
(** Build a value from an inflated contents. *)

val sha1: value -> SHA1.t
(** Return the SHA1 of the serialized contents. *)

val type_of_inflated: Mstruct.t -> object_type
(** Return the type of the inflated object stored in the given
    buffer. *)

val add_header: object_type -> Bigstring.t -> Bigstring.t
(** Add the headers for the given object type. *)

(** {2 Packed Objects} *)

module Pack_index: sig

  (** Pack indexes. *)

  include SERIALIZABLE with type t = pack_index

  val of_pack: pack -> t
  (** Create an index from a pack. *)

  val of_raw_pack: Bigstring.t -> t
  (** Same as [of_pack] but create an index from a raw pack instead
      (such raw packs are received on fetch/clone) *)

end

module PackedValue2: ISERIALIZABLE with type t = packed_value
(** Pack objects version 2. *)

module PackedValue3: ISERIALIZABLE with type t = packed_value
(** Pack objects version 3. *)

(** Packs. *)
module Pack: sig

  include SERIALIZABLE with type t = pack

  val read_packed_value: Bigstring.t -> pack_index -> sha1 -> packed_value
  (** Read a packed value inside a raw pack file. *)

  val unpack_inflated:
    read_inflated:(sha1 -> Bigstring.t Lwt.t) ->
    index:pack_index ->
    offset:int ->
    packed_value ->
    Bigstring.t Lwt.t
  (** [unpack ~read_inflated ~index ~offset p] unpack the packed value
      [p].

      The [read_inflated] function is used to read object contents
      from the disk or from memory, depending on the backend. [index]
      is the pack index and [offset] is the position of [p] into the
      pack file (this is useful of delta offsets). *)

  val unpack:
    read_inflated:(sha1 -> Bigstring.t Lwt.t) ->
    index:pack_index ->
    offset:int ->
    packed_value ->
    value Lwt.t
  (** Same as [unpack_inflated] but return a value instead of a raw buffer. *)

  val unpack_all:
    read_inflated:(sha1 -> Bigstring.t Lwt.t) ->
    write:(value -> sha1 Lwt.t) ->
    Bigstring.t ->
    pack_index Lwt.t
    (** Unpack a whole pack file. *)

end

(** Cache *)

module Cache: SERIALIZABLE with type t = Cache.t

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
