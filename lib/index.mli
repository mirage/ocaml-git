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

(** Implementation of the V2 Git Index format (as V1 is deprecated).

    The index file contains information used by Git to manage the
    state of working directory contents.
*)

type time = {

  lsb32: Int32.t;
  (** binary integer containg the lower 32 bits of the entry (file
      or symbolic link) timestamp. *)

  nsec : Int32.t;
  (** binary integer containg the lower 32 bits of the entry (file or
      symbolic link) more precise timestamp, if available. *)
}
(** The type for time values. *)

type mode =
  [ `Normal
  | `Exec
  | `Link
  | `Gitlink ]
(** The type for files' permission in the index file. *)

val pp_mode: Format.formatter -> mode -> unit
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

val pp_stats: Format.formatter -> stat_info -> unit
(** Pretty-print file stats. *)

type entry = {
  stats : stat_info;
  id    : Hash.Blob.t;
  stage : int;
  name  : string;
}

val pp_entry: Format.formatter -> entry -> unit
(** Human-readable representation of an index entry. *)

type extension_kind = [ `Tree | `Reuc | `Link | `Other of string ]
(** The type for extension kinds.

    {ul
    {- [Tree] is for cached tree}
    {- [Reuc] is for reuse undo}
    {- [Link] is for split index}
    {- [Other] is for other extension kinds}
    }
*)

type extension = {
  kind: extension_kind;
  payload: string;
}
(** The type for extension payload. *)

val pp_extension: Format.formatter -> extension -> unit
(** Human-readable representation of the extension. *)

type t = private {
  entries   : entry list;
  extensions: extension list;
}
(** Index entries are sorted by the byte sequence that comprises the
    entry [name]; with a secondary comparison of the [stage] bits if
    the entry name byte sequences are identical *)

val create: ?extensions:extension list -> entry list -> t
(** Create an index. *)

val empty: t
(** The empty index file. *)

include Object.S with type t := t

module type IO = Object.IO with type t = t

module IO (D: Hash.DIGEST): IO
