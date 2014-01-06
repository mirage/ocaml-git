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

(** Store Git objects on the local filesystem. *)

open GitTypes

val create: ?root:string -> unit -> t
(** Create a cache of objects for the given path. If [root] is not
    set, use the current directory. *)

val dump: t -> unit Lwt.t
(** Dump the state of cache to stderr. *)

val read: t -> sha1 -> value option Lwt.t
(** Return the contents of the object having the given SHA1 file. The
    result can be either a normal value or a packed value. If the
    object is already in the cache, use it directly. Otherwise,
    populate the cache to speed up the next disk accesses. *)

val type_of: t -> sha1 -> [ `Blob | `Commit | `Tag | `Tree ] option Lwt.t
(** Return the object type corresponding to the given SHA1. *)

val references: t -> (string * sha1) list Lwt.t
(** Return the list of references (stored in {i ./git/refs/}). *)

val list: t -> sha1 list Lwt.t
(** List of SHA1. *)

val succ: t -> sha1 -> ([ `Parent | `Tag of string | `File of string ] * sha1) list Lwt.t
(** Successors (with labels). *)

val write: t -> value -> sha1 Lwt.t
(** Write a value and return the SHA1 of the object. *)

val write_and_check_inflated: t -> sha1 -> string -> unit Lwt.t
(** Compress a binary value and write it in the store. Check that the
    SHA1 of the uncompressed value is the one expected. *)

val write_reference: t -> string -> sha1 -> unit Lwt.t
(** Write a reference. *)

val read_inflated: t -> sha1 -> Mstruct.t option Lwt.t
(** Return the inflated contents of the given SHA1 file. *)

val write_filesystem: t -> SHA1.Commit.t -> unit Lwt.t
(** Expand the filesystem corresponding to the given revision. *)

(** {2 Flushing} *)

val flush: t -> unit Lwt.t
(** Flush the in-memory cache to disk. *)

val set_auto_flush: bool -> unit
(** Set the flushing behavior. When set to [false], no contents is
    modified on the disk. *)

val get_auto_flush: unit -> bool
(** Get the current flush behavior. *)
