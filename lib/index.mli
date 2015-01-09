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

(** Implementation of the V2 Git Index format (as V1 is deprecated). *)

type time = {

  lsb32: Int32.t;
  (** binary integer containg the lower 32 bits of the entry (file
      or symbolic link) timestamp. *)

  nsec : Int32.t;
  (** binary integer containg the lower 32 bits of the entry (file or
      symbolic link) more precise timestamp, if available. *)
}

type mode =
  [ `Normal
  | `Exec
  | `Link
  | `Gitlink ]
(** Permission for files in the index file. *)

val pretty_mode: mode -> string
(** Pretty print file modes. *)

type stat_info = {
  ctime: time;
  mtime: time;
  dev  : Int32.t;
  inode: Int32.t;

  mode : mode;
  (** binary integer containg the lower 32 bits of the entry (file
      or symbolic link) file system entity type and permissions. *)

  uid  : Int32.t;
  gid  : Int32.t;

  size : Int32.t;
  (** binary integer containg the lower 32 bits of the entry
      (file or symbolic link) size. *)
}
(** These fields are used as a part of a heuristic to determine if the
    file system entity associated with this entry has changed. The
    names are very *nix centric but the exact contents of each field
    have no meaning to Git, besides exact match, except for the [mode]
    and [size] fields. *)

val pretty_stats: stat_info -> string
(** Pretty-print file stats. *)

type entry = {
  stats : stat_info;
  id    : SHA.Blob.t;
  stage : int;
  name  : string;
}

val pretty_entry: entry -> string
(** Human-readable representation of an index entry. *)

type extension_kind =
  [ `Tree (** Cached tree *)
  | `Reuc (** Reuse undo *)
  | `Link (** split index *)
  | `Other of string ]

type extension = {
  kind: extension_kind;
  payload: string;
}

val pretty_extension: extension -> string
(** Human-readable representation of the extension. *)

type t = {
  entries   : entry list;
  extensions: extension list;
}
(** Index entries are sorted by the byte sequence that comprises the
    entry [name]; with a secondary comparison of the [stage] bits if
    the entry name byte sequences are identical *)

include Object.S with type t := t

val empty: t
(** The empty index file. *)
