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

(** Packed values. *)

open Core_kernel.Std

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

type pic =
  [ `Raw_value of Bigstring.t
  | `Ref_delta of SHA1.t delta ]
with bin_io, compare, sexp

type t =
  [ pic | `Off_delta of int delta ]
(** Packed values. *)

val pretty: t -> string
(** Human readable representation of a packed value. *)

include Identifiable.S with type t := t

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

val add_hunk: source:Bigstring.t -> Bigbuffer.t -> hunk -> unit
(** Append a hunk to a buffer. [source] is the original object the
    hunk refers to. *)

val add_delta: Bigbuffer.t -> Bigstring.t delta -> unit
(** Append a delta to a buffer. *)

val to_value:
  read:(SHA1.t -> Bigstring.t Lwt.t) ->
  index:Pack_index.t ->
  pos:int ->
  t -> Value.t Lwt.t
(** [to_value ~read index ~pos p] unpacks the packed value [p].

    The [read] function is used to read object contents from the disk
    or from memory, depending on the backend. [index] is the pack
    index and [pos] is the current position of [p] into the pack
    file (this is useful to process delta offsets). *)

val add_inflated_value:
  read:(SHA1.t -> Bigstring.t Lwt.t) ->
  index:Pack_index.t ->
  pos:int ->
  Bigbuffer.t -> t -> unit Lwt.t
(** Append the inflated representation of a value to a given
    buffer. Use the same paramaters as [to_value]. *)

val add_inflated_value_sync:
  read:(SHA1.t -> Bigstring.t) ->
  index:Pack_index.t ->
  pos:int ->
  Bigbuffer.t -> t -> unit
(** Same as [add_inflated_value] but with a synchronous read
    function. *)

(** {2 Position independant packed values} *)

val pic: Pack_index.t -> pos:int -> t -> pic
(** Position-independant packed value. Convert an [Off_delta] packed
    value into [Ref_delta] using the provided pack index. *)

val unpic: Pack_index.t -> pos:int -> pic -> t
(** Position dependent packed value. Convert a [Ref_delta] to the
    corresponding [Off_delta], using the provided pack index. *)
