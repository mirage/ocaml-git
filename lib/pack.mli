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

type t = (SHA1.t * Packed_value.PIC.t) list
(** A pack value is an ordered list of position-independant packed
    values and the SHA1 of the corresponding inflated objects. *)

include Object.S with type t := t

val input: Mstruct.t -> index:Pack_index.t option -> t
(** The usual [Object.S.input] function, but with an additional
    [index] argument. When [index] is [None], recompute the whole
    index: that's very costly so provide the index when possible. *)

val keys: t -> SHA1.Set.t
(** Return the keys present in the pack. *)

val read: t -> SHA1.t -> Value.t option
(** Return the value stored in the pack file. *)

val read_exn: t -> SHA1.t -> Value.t
(** Return the value stored in the pack file. *)

val unpack: write:(Value.t -> SHA1.t Lwt.t) ->
  Bigstring.t -> SHA1.Set.t Lwt.t
(** Unpack a whole pack file. [write] should returns the SHA1 of the
    marshaled value. Return the IDs of the written objects. *)

module Raw: sig

  (** Raw pack file: they contains a pack index and a list of
      position-dependant deltas. *)

  include Object.S

  val input: Mstruct.t -> index:Pack_index.t option -> t
  (** Same as the top-level [input] function but for raw packs. *)

  val sha1: t -> SHA1.t
  (** Return the name of the pack. *)

  val index: t -> Pack_index.t
  (** Return the pack index. *)

  val keys: t -> SHA1.Set.t
  (** Return the keys present in the raw pack. *)

end

val to_pic: Raw.t -> t
(** Transform a raw pack file into a position-independant pack
    file. *)

val of_pic: t -> Raw.t
(** Transform a position-independant pack file into a raw one. *)
