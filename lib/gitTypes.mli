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

(** Git types. *)

open Core_kernel.Std

module SHA1: sig

  (** Unique object identifiers using SHA1. *)

  include Identifiable.S

  (** Commit nodes. *)
  module Commit: Identifiable.S

  (** Treee nodes. *)
  module Tree: Identifiable.S

  (** Blob nodes. *)
  module Blob: Identifiable.S

  (** A commit node is also a node. *)
  val commit: Commit.t -> t

  (** A node can be casted to a commit. WARNING: this is not
      type-safe, use it carrefuly! *)
  val to_commit: t -> Commit.t

  (** A tree node is also a node. *)
  val tree: Tree.t -> t

  (** A blob node is also a node. *)
  val blob: Blob.t -> t

  (** Build a node from a raw string. *)
  val sha1: string -> t

end

(** Short-cut to generic object SHA1. *)
type sha1 = SHA1.t

(** Git user. *)
type user = {
  name : string;
  email: string;
  date : string;
}

(** Blob files are leafs. For us, this is just a raw string but is
   could be arbitrary absract types. *)
module Blob: Identifiable.S

(** Shorcut to blob types. *)
type blob = Blob.t

(** A commit is a tree snapshot, with some credentials (eg. we can
    find who created the initial snapshot, and who added it to to
    store) and a message explaining what the snapshot contains. *)
type commit = {
  tree     : SHA1.Tree.t;
  parents  : SHA1.Commit.t list;
  author   : user;
  committer: user;
  message  : string;
}

(** A tree entry. This is either a directory or a file. As this is
    supposed to model a filesystem, directory does not contain
    data. *)
type entry = {
  perm: [`normal|`exec|`link|`dir];
  file: string;
  node: sha1;
}

(** A tree is an hierarchical data-store. NB: data (eg. blobs) are
    only carried on the leafs. *)
type tree = entry list

(** A tag is bookmark to a previous commit. *)
type tag = {
  commit     : SHA1.Commit.t;
  tag        : string;
  tagger     : user;
  tag_message: string;
}

(** Loose git objects. *)
type value =
  | Blob   of blob
  | Commit of commit
  | Tag    of tag
  | Tree   of tree

(** {2 Packs} *)

(** A delta hunk can either insert a string of copy the contents of a
    base object. *)
type hunk =
  | Insert of string
  | Copy of int * int

(** Delta objects. *)
type 'a delta = {
  source       : 'a;
  source_length: int;
  result_length: int;
  hunks        : hunk list;
}

(** Packed values. *)
type packed_value =
  | Value     of value
  | Ref_delta of sha1 delta
  | Off_delta of int delta

(** A pack file. Pack files can become quite big, so we fully parse
    the index file (which says which objects are stored in there) and
    we lazily parse the contents (eg. we fetch the contents on
    demand). *)
type pack = sha1 -> packed_value

(** Pack indexes. *)
type pack_index = {
  offsets: int SHA1.Map.t;
  lengths: int option SHA1.Map.t;
}

(** {2 Casts} *)

(** Cast a commit to an object. *)
val commit: commit -> value

(** Cast a blob to an object. *)
val blob: blob -> value

(** Cast a tree to an object. *)
val tree: tree -> value

(** Cast a tag to an object. *)
val tag: tag -> value

(** Packed value. *)
val value: value -> packed_value

(** Cast reference-delta values. *)
val ref_delta: sha1 delta -> packed_value

(** Cast offset-delta values. *)
val off_delta: int delta -> packed_value

(** {2 Repositories} *)

(** Git repository. *)
type t = {
  root   : string;                                       (** Git root *)
  buffers: (sha1, Mstruct.t) Hashtbl.t; (** Cache of inflated buffers *)
  packs  : (sha1, pack) Hashtbl.t;           (** Cache of pack files  *)
  indexes: (sha1, pack_index) Hashtbl.t;     (** Cache of index files *)
}
