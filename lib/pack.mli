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

type t = {
  index : Pack_index.t;
  values: Packed_value.pic list;
}

include Object.S with type t := t

type raw  = {
  checksum  : SHA1.t;
  raw_values:  (int * Packed_value.t) list;
}

val pic: Pack_index.t -> raw -> t
(** Return position-independant packed values. Convert all [Off_delta]
    into [Ref_delta] using the provided pack index. *)

val input: Mstruct.t -> raw
(** Return the list of offsets of the packed values. *)

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

val index_of_raw: Bigstring.t -> Pack_index.t
(** Same a [index] but using a raw pack file. Useful in
    fetch/clone. *)
