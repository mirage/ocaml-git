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

(** Pack indexes. *)

open Core_kernel.Std

type t = {
  offsets      : int SHA1.Map.t;
  crcs         : int32 SHA1.Map.t;
  pack_checksum: SHA1.t;
}
(** [offsests] is the positions of the SHA1 objects in the
    corresponding raw pack file.

    [crcs] contains the CRC-32 value of the packed object data.

    [pack_checksum] is the corresponding pack file checksums, value
    which is needed when writing the pack index file to disk. *)

include Object.S with type t := t

val lengths: t -> int option SHA1.Map.t
(** [lengths] returns the difference between two consecutive offsets
    (appart for the last elements, where the lenght can be [None] is
    the index file is build from a raw pack file). *)

val empty: pack_checksum:SHA1.t -> t
(** The empty pack index. *)
