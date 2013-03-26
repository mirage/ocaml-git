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

(** Mapping of git concepts into the filesystem. *)

open Lib
open Model

(** Read a complete repository state. Note: we only load the object
    references, not their contents. This is because we don't really want
    to store all the state in memory, especially when you can *huge* pack
    files. *)
val create: File.Dirname.t -> t

(** Dump the state to stderr. This function is in this module because
    we need to be aware of the mapping model/filesystem to load file
    contents on demand. *)
val dump: t -> unit

(** Read the contents of a node. The result can be either a normal
    value or a packed value. *)
val read: t -> node -> value option

(** Expand a packed object as a loose object. *)
val expand: t -> node -> value -> unit

(** Return the references. *)
val refs: t -> (string * node) list

(** List of nodes. *)
val nodes: t -> node list

(** Successors. *)
val succ: t -> node -> node list
