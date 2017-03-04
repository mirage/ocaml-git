(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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


(** The type for a time represented by its [lsb32] and [nsec] parts. *)
type time = {
  lsb32: Int32.t;
  nsec : Int32.t;
}

type mode =
  [ `Normal
  | `Exec
  | `Link
  | `Gitlink ]

val pp_mode: Format.formatter -> mode -> unit
(** [pp_mode] is the pretty-printer for modes. *)

(** The type for file-system stat information. *)
type stat_info = {
  ctime: time;
  mtime: time;
  dev  : Int32.t;
  inode: Int32.t;
  mode : mode;
  uid  : Int32.t;
  gid  : Int32.t;
  size : Int32.t;
}

val pp_stats: Format.formatter -> stat_info -> unit
(** [pp_stats] is the pretty-printer for stat_info. *)

(** The type for a Git index entry. *)
type entry = {
  stats : stat_info;
  id    : Git_hash.Blob.t;
  stage : int;
  name  : string;
}

val pp_entry: Format.formatter -> entry -> unit
(** [pp_entry] is the pretty-printer for entry. *)

type extension_kind = [ `Tree | `Reuc | `Link | `Other of string ]

type extension = {
  kind: extension_kind;
  payload: string;
}

val pp_extension: Format.formatter -> extension -> unit
(** [pp_extension] is the pretty-printer for extension. *)

type t = private {
  entries   : entry list;
  extensions: extension list;
}

val create: ?extensions:extension list -> entry list -> t

val empty: t

include Git_s.S with type t := t

module type IO = Git_s.IO with type t = t

module IO (D: Git_hash.DIGEST): IO
