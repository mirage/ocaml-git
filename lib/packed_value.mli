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

(** Packed values. *)

type copy = { copy_offset: int; copy_length: int; }
(** The type for [Copy]'s {!hunk} arguments. *)

type hunk = Insert of string | Copy of copy
(** The type for {!delta} hunks. A delta hunk can either insert a
      string of copy the contents of a base object. *)

type 'a delta = {
  source       : 'a;
  source_length: int;
  result_length: int;
  hunks        : hunk list;
}
(** The type for delta values. *)

type kind =
  | Raw_value of string
  | Ref_delta of SHA.t delta
  | Off_delta of int delta
(** The type for packed values kind. *)

val pp_kind: Format.formatter -> kind -> unit
(** Pretty-print packed values' kind. *)

type t = { kind: kind; offset: int }
(** The type for packed values. *)

val pp: Format.formatter -> t -> unit
(** Human readable representation of a packed value. *)

val result_length: t -> int
(** Return the lenght of the result object. *)

val source_length: t -> int
(** Return the lenght of the base (source) object. *)

module type S = sig
  (** The signature for packed values implementations. *)

  include Object.S with type t = kind

  val crc32: t -> int32
  (** Return the CRC-32 of the packed value. Useful when creating pack
      index files. *)

end

module V2: S
(** Packed values version 2. *)

module V3: S
(** Packed values version 3. *)

(** {2 Conversion to values} *)


val add_hunk: Buffer.t -> source:string -> pos:int -> hunk -> unit
(** Append a hunk to a buffer. [source] is the original object the
    hunk refers to (with the given offset). *)

val add_delta: Buffer.t -> string delta -> unit
(** Append a delta to a buffer. *)

val add_inflated_value: read:Value.read_inflated -> offsets:(int -> SHA.t option) ->
  Buffer.t -> t -> unit Lwt.t
(** Append the inflated representation of a packed value to a given
    buffer. *)

val to_value: index:Pack_index.f -> read:Value.read_inflated -> version:int ->
  ba:Cstruct.buffer -> t -> Value.t Lwt.t
(** Unpack the packed value using the provided indexes. *)

(** {2 Position independant packed values} *)

module PIC: sig

  (** Position-independant packed values. *)

  type kind = Raw of string | Link of t delta
  (** The type for position-independent packed values' kind. *)

  and t = { kind: kind; sha1: SHA.t; mutable raw: string option; }
  (** The type for postition-independant packed values. *)

  val pp: Format.formatter -> t -> unit
  (** Human readable representation. *)

  val pretty: t -> string
  (** Pretty-print the value. *)

  val to_value: t -> Value.t
  (** [to_value p] unpacks the packed position-independant value
      [p]. *)

  val raw: SHA.t -> Cstruct.t -> t
  (** Build a raw value. *)

  val unpack_kind: kind -> string
  (** Unpack a PIC kind into a string. *)

  val unpack: t -> string
  (** Unpack a PICK value into a string. *)

  module Map: Map.S with type key = t

end

val to_pic: read:Value.read_inflated ->
  offsets:(int -> PIC.t option) -> sha1s:(SHA.t -> PIC.t option) ->
  ?sha1:SHA.t -> t -> PIC.t Lwt.t
(** [to_pic t] is the position-independant representation of the
    packed value [t]. *)

val of_pic: index:Pack_index.f -> offset:int -> PIC.t -> t
(** Position dependent packed value. Convert a [PIC.Link] into to the
    corresponding [Off_delta] and [Ref_delta], using the provided
    indexes. *)
