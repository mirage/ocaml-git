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

(** {1 Packed values} *)

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

type t = { kind: kind; offset: int }
(** The type for packed values. *)

val shallow: SHA.Set.t -> t -> bool
(** [shallow p t] checks whether the SHAs appearing in [t] also appear
    in the pack file [p]. *)

val create: offset:int -> kind:kind -> t
(** Create a packed value. *)

val kind: t -> kind
(** [kind t] is [t]'s kind. *)

val offset: t -> int
(** [offset t] is [t]'s offset. *)

val pp_kind: Format.formatter -> kind -> unit
(** Pretty-print packed values' kind. *)

val is_delta: t -> bool
(** Check if a packed value is a delta (either a [Ref_delta] or an
    [Off_delta]). *)

val result_length: t -> int
(** Return the lenght of the result object. *)

val source_length: t -> int
(** Return the lenght of the base (source) object. *)

include Object.S with type t := t

(** {1 Positition-independant packed values} *)

module PIC: sig

  type kind = Raw of string | Link of t delta
  (** The type for position-independent packed values' kind. *)

  and t = {
    kind: kind;
    sha1: SHA.t;
    shallow: bool;
    mutable raw: string option;
  }
  (** The type for postition-independant packed values. See {!S.PIC}. *)

  include Object.S with type t := t

  val create: ?raw:string -> ?shallow:bool -> SHA.t -> kind -> t
  (** Create a position-independent packed value. By default,
      [shallow] is [false]. *)

  val of_raw: ?shallow:bool ->  SHA.t -> string -> t
  (** [of_raw sha1 raw] is the position-independant packed value built
      by parsing [raw]. By default [shallow] is [false]. *)

  val kind: t -> kind
  (** [kind t] is [t]'s kind. *)

  val sha1: t -> SHA.t
  (** [sha1 t] is [t]'s SHA1. *)

  val raw: t -> string option
  (** [raw t] is [t]'s raw represation. *)

  val shallow: t -> bool
  (** [shallow t] is true iff [t] is not included in the pack file. *)

  val unpack_kind: kind -> string
  (** Unpack a PIC kind into a string. *)

  val unpack: t -> string
  (** Unpack a PICK value into a string. *)

end

type pic = PIC.t
(** The type for position-independant packked values. *)

module IO (D: SHA.DIGEST) (I: Inflate.S): sig

  module type IO = Object.IO with type t = kind

  module V2: IO
  (** Packed values version 2. *)

  module V3: IO
  (** Packed values version 3. *)

  (** {2 Conversion to values} *)

  val add_inflated_value:
    read:Value.read_inflated -> offsets:(int -> SHA.t option) ->
    Buffer.t -> t -> unit Lwt.t
  (** Append the inflated representation of a packed value to a given
      buffer. *)

  val to_value:
    index:Pack_index.f -> read:Value.read_inflated -> version:int ->
    ba:Cstruct.buffer -> t -> Value.t Lwt.t
  (** Unpack the packed value using the provided indexes. *)

  (** {2 Position independant packed values} *)

  val unpack:
    index:Pack_index.f -> read:Value.read_inflated -> version:int ->
    ba:Cstruct.buffer -> t -> string Lwt.t
  (** Same as {!to_value} but for inflated raw buffers. *)

  val value_of_pic: pic -> Value.t
  (** [to_value p] unpacks the packed position-independant value
      [p]. *)

  val to_pic: read:Value.read_inflated ->
    offsets:(int -> pic option) -> sha1s:(SHA.t -> pic option) ->
    t -> pic Lwt.t
  (** [to_pic t] is the position-independant representation of the
      packed value [t]. *)

  val of_pic: index:Pack_index.f -> offset:int -> pic -> t
  (** Position dependent packed value. Convert a [PIC.Link] into to the
      corresponding [Off_delta] and [Ref_delta], using the provided
      indexes. *)

end
