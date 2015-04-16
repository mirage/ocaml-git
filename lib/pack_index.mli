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

(** Pack indexes. *)

type t = {
  offsets      : int SHA.Map.t;
  crcs         : int32 SHA.Map.t;
  pack_checksum: SHA.t;
}
(** [offsests] is the positions of the SHA objects in the
    corresponding raw pack file.

    [crcs] contains the CRC-32 value of the packed object data.

    [pack_checksum] is the corresponding pack file checksums, value
    which is needed when writing the pack index file to disk. *)

include Object.S with type t := t

val keys: Mstruct.t -> SHA.Set.t
(** Read only the keys contained in the index. *)

val lengths: t -> int option SHA.Map.t
(** [lengths] returns the difference between two consecutive offsets
    (appart for the last elements, where the lenght can be [None] is
    the index file is build from a raw pack file). *)

val empty: ?pack_checksum:SHA.t -> unit -> t
(** The empty pack index. *)

type c_t
(** Abstract type of pack index. *)

val create: ?cache_size:int -> Cstruct.buffer -> c_t
(** Create a pack index object. The results of [find_offset] are cached
    for upto [cache_size] SHA objects *)

val find_offset: ?scan_thresh:int -> c_t -> SHA.t -> int option
(** [find_offset] searches the index for the offset of the SHA object.
    Binary search is performed until the candidates are narrowed down to
    [scan_thresh] or less. *)

val mem: c_t -> SHA.t -> bool
(** [mem] checks if the SHA object is contained in the index object *)
