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

type copy = {
  offset: int;
  length: int;
}
(** Copy arguments. *)

type hunk =
  | Insert of string
  | Copy of copy
  (** A delta hunk can either insert a string of copy the contents of a
      base object. *)

type 'a delta = {
  source       : 'a;
  source_length: int;
  result_length: int;
  hunks        : hunk list;
}
(** Delta objects. *)

type t =
  | Raw_value of string
  | Ref_delta of SHA.t delta
  | Off_delta of int delta
  (** Packed values. *)

val pp_hum: Format.formatter -> t -> unit
(** Human readable representation of a packed value. *)

module V2: sig

  include Object.S with type t := t

  val crc32: t -> int32
  (** Return the CRC-32 of the packed value. Useful when creating pack
      index files. *)

end

module V3: sig

  include Object.S with type t := t

  val crc32: t -> int32
  (** Return the CRC-32 of the packed value. Useful when creating pack
      index files. *)

end

val result_length: t -> int
(** Return the lenght of the result object. *)

val source_length: t -> int
(** Return the lenght of the base (source) object. *)

(** {2 Conversion to values} *)

val add_hunk: Buffer.t -> source:string -> pos:int -> hunk -> unit
(** Append a hunk to a buffer. [source] is the original object the
    hunk refers to (with the given offset). *)

val add_delta: Buffer.t -> string delta -> unit
(** Append a delta to a buffer. *)

val add_inflated_value:
  read:(SHA.t -> string Lwt.t) ->
  offsets:SHA.t Misc.IntMap.t ->
  pos:int ->
  Buffer.t -> t -> unit Lwt.t
(** Append the inflated representation of a packed value to a given
    buffer. Use the same paramaters as [to_value]. *)

val add_inflated_value_sync:
  read:(SHA.t -> string) ->
  offsets:SHA.t Misc.IntMap.t ->
  pos:int ->
  Buffer.t -> t -> unit
(** Same as [add_inflated_value] but with a synchronous read
    function. *)

(** {2 Position independant packed values} *)

module PIC: sig

  (** Position-independant packed values. *)

  type kind =
    | Raw of string
    | Link of t delta

  and t = {
    kind: kind;
    sha1: SHA.t;
  }

  val pp_hum: Format.formatter -> t -> unit
  (** Human readable representation. *)

  val to_value: t -> Value.t
  (** [to_value p] unpacks the packed position-independant value
      [p]. *)

  val raw: SHA.t -> Cstruct.t -> t
  (** Build a raw value. *)

  module Map: Map.S with type key = t

end

val to_pic: read:(SHA.t -> Value.t option Lwt.t) -> PIC.t Misc.IntMap.t ->
  PIC.t SHA.Map.t -> (int * SHA.t * t) -> PIC.t Lwt.t
(** Position-independant packed value. Convert [Off_delta] and
    [Ref_delta] to [PIC.Link] using the provided indexes. *)

val of_pic: int PIC.Map.t -> pos:int -> PIC.t -> t
(** Position dependent packed value. Convert a [PIC.Link] into to the
    corresponding [Off_delta], using the provided indexes. *)

val to_value: version:int -> index:Pack_index.t -> ba:Cstruct.buffer -> (int * t) -> Value.t
(** Unpack the packed value using the provided indexes. *)
