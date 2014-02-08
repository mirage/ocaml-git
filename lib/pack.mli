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

(** Pack files. *)

open Core_kernel.Std

type t
(** Pack value. *)

include Object.S with type t := t

val input: Mstruct.t -> Pack_index.t -> t
(** Return the list of offsets of the packed values. *)

val values: t -> Packed_value.pic list
(** Get the pack values. *)

val create: Packed_value.pic list -> t
(** Create a pack file from a list of PIC packed values. *)

val packed_value: index:Pack_index.t -> key:SHA1.t ->
  Bigstring.t -> Packed_value.t
(** Read a packed value inside a raw pack file. *)

val unpack:
  read:(SHA1.t -> Bigstring.t Lwt.t) ->
  write:(Value.t -> SHA1.t Lwt.t) ->
  Bigstring.t -> Pack_index.t Lwt.t
(** Unpack a whole pack file. [read] should return the inflated
    contents of the value having the given SHA1. [write] should
    returns the SHA1 of the marshaled value. *)

val to_index: t -> Pack_index.t
(** Build a pack index from a pack file. Similar to
    [Pack_index.of_raw_pack] but works on structured pack values
    instead of a raw buffer. *)

module Raw: sig

  include Object.S
  (** Raw pack file, with position-dependant deltas. *)

  val to_index: t -> Pack_index.t
  (** Same a [index] but using a raw pack file. Useful in
      fetch/clone. *)

end

val pic: Pack_index.t -> Raw.t -> t
(** Transform a raw pack file into a position independant pack
    file. *)
