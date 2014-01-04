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
(** Read a complete repository state. Note: we only load the object
    references, not their contents. This is because we don't really want
    to store all the state in memory, especially when you can *huge* pack
    files. if [root] is not set, use the current directory. *)

val dump: t -> unit
(** Dump the state to stderr. This function is in this module because
    we need to be aware of the mapping model/filesystem to load file
    contents on demand. *)

val read: t -> sha1 -> value option
(** Read the contents of an object. The result can be either a normal
    value or a packed value. *)

val references: t -> (string * sha1) list
(** Return the list of references (stored in {i ./git/refs/}). *)

val list: t -> sha1 list
(** List of SHA1. *)

val succ: t -> sha1 -> ([`parent|`tag of string|`file of string] * sha1) list
(** Successors (with labels). *)

val write: t -> value -> sha1
(** Write a value and return the SHA1 of the object. *)

val write_and_check_inflated: t -> sha1 -> string -> unit
(** Compress a binary value and write it in the store. Check that the
    SHA1 of the uncompressed value is the one expected. *)

val write_reference: t -> string -> sha1 -> unit
(** Write a reference. *)
