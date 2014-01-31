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

type object_type =
  [ `Blob
  | `Commit
  | `Tag
  | `Tree ]
(** Values representing object types. *)

module type SHA1 = sig

  (** Signature for SHA1 values *)

  include Identifiable.S

  val create: string -> t
  (** Build a node from a raw string. *)

  val to_hex: t -> string
  (** Display the hex encoding of the SHA1 hash. *)

  val of_hex: string -> t
  (** Convert an hex-encoded string into a sha1 value. *)

end


module SHA1: sig

  (** Unique object identifiers using SHA1. *)

  include SHA1

  module Commit: SHA1
  (** Commit nodes. *)

  val of_commit: Commit.t -> t
  (** A commit node is also a node. *)

  val to_commit: t -> Commit.t
  (** A node might be a commit. *)

  module Tree: SHA1
  (** Treee nodes. *)

  val of_tree: Tree.t -> t
  (** A tree node is also a node. *)

  val to_tree: t -> Tree.t
  (** A node might be a node. *)

  module Blob: SHA1
  (** Blob nodes. *)

  val of_blob: Blob.t -> t
  (** A blob node is also a node. *)

  val to_blob: t -> Blob.t
  (** A node might be a blob node. *)

end

type sha1 = SHA1.t

module Blob: Identifiable.S
(** Blob files are leafs. For us, this is just a raw string but is
   could be arbitrary absract types. *)

type blob = Blob.t

module User: sig

  (** Git user actions. *)

  type t = {
    name : string;
    email: string;
    date : string;
  }
  (** A user action has a (user) name, an (user) email and an (action)
      date. *)

  include Identifiable.S with type t := t

end

module Commit: sig

  (** Git commits. *)

  type t = {
    tree     : SHA1.Tree.t;
    parents  : SHA1.Commit.t list;
    author   : User.t;
    committer: User.t;
    message  : string;
  }
  (** A commit is a tree snapshot, with some credentials (eg. we can
      find who created the initial snapshot, and who added it to to
      store) and a message explaining what the snapshot contains. *)

  include Identifiable.S with type t := t

end

type commit = Commit.t

type perm = [`normal|`exec|`link|`dir]
(** File permission. *)

module Tree: sig

  (** Git trees. *)

  type entry = {
    perm: perm;
    name: string;
    node: SHA1.t;
  }
  (** A tree entry. This is either a directory or a file. As this is
      supposed to model a filesystem, directory does not contain
      data. *)

  type t
  (** A tree is an hierarchical data-store. NB: data (eg. blobs) are
      only carried on the leafs. *)

  val create: entry list -> t
  (** Order the entries by name. *)

  val entries: t -> entry list
  (** Return the tree entries. *)

  include Identifiable.S with type t := t

end

type tree = Tree.t

module Tag: sig

  (** Git tags. *)

  type t = {
    sha1   : SHA1.t;
    typ    : object_type;
    tag    : string;
    tagger : User.t;
    message: string;
  }
  (** A tag is bookmark to a previous commit. *)

  include Identifiable.S with type t := t

end

type tag = Tag.t

module Value: sig

  (** Git objects. *)

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tag    of Tag.t
    | Tree   of Tree.t
    (** Loose git objects. *)

  include Identifiable.S with type t := t

end

type value = Value.t

(** {2 Cache (index file)} *)

module Cache: sig

  (** Implementation of the V2 cache format (as V1 is deprecated). *)

  module Entry: sig

    type time = {

      lsb32: Int32.t;
      (** binary integer containg the lower 32 bits of the entry (file
	  or symbolic link) timestamp. *)

      nsec : Int32.t;
      (** binary integer containg the lower 32 bits of the entry (file or
	  symbolic link) more precise timestamp, if available. *)
    } with bin_io, compare, sexp

    type stat_info = {
      ctime: time;
      mtime: time;
      dev  : Int32.t;
      inode: Int32.t;
      uid  : Int32.t;
      gid  : Int32.t;

      mode : Int32.t;
      (** binary integer containg the lower 32 bits of the entry (file
	  or symbolic link) file system entity type and permissions. *)

      size : Int32.t;
      (** binary integer containg the lower 32 bits of the entry
	  (file or symbolic link) size. *)
    } with bin_io, compare, sexp
    (** These fields are used as a part of a heuristic to determine if the
	file system entity associated with this entry has changed. The
	names are very *nix centric but the exact contents of each
	field have no meaning to Git, besides exact match, except for
	the [mode] and [size] fields. *)

    type t = {
      stats : stat_info;
      id    : SHA1.t;
      stage : int;
      name  : string;
    } with bin_io, compare, sexp

  end

  type t = {
    entries   : Entry.t list;
    extensions: (Int32.t * string) list;
  }
  (** Index entries are sorted by the byte sequence that comprises the
      entry [name]; with a secondary comparison of the [stage] bits if
      the entry name byte sequences are identical *)

  include Identifiable.S with type t := t

end

type cache = Cache.t

(** {2 Packs} *)

module Packed_value: sig

  (** Packed values. *)

  type hunk =
    | Insert of string
    | Copy of int * int
    (** A delta hunk can either insert a string of copy the contents of a
        base object. *)

  type 'a delta = {
    source       : 'a;
    source_length: int;
    result_length: int;
    hunks        : hunk list;
  }
  (** Delta objects. *)

  type t =
    | Value     of Value.t
    | Ref_delta of SHA1.t delta
    | Off_delta of int delta
  (** Packed values. *)

  include Identifiable.S with type t := t

end

type packed_value = Packed_value.t

type pack = SHA1.t -> packed_value
(** A pack file. Pack files can become quite big, so we fully parse
    the index file (which says which objects are stored in there) and
    we lazily parse the contents (eg. we fetch the contents on
    demand). *)

type pack_index = {
  offsets: int SHA1.Map.t;
  lengths: int option SHA1.Map.t;
}
(** Pack indexes. *)

(** {2 Casts} *)

val commit: commit -> value
(** Cast a commit to an object. *)

val blob: blob -> value
(** Cast a blob to an object. *)

val tree: tree -> value
(** Cast a tree to an object. *)

val tag: tag -> value
(** Cast a tag to an object. *)

val value: value -> packed_value
(** Packed value. *)

val ref_delta: sha1 Packed_value.delta -> packed_value
(** Cast reference-delta values. *)

val off_delta: int Packed_value.delta -> packed_value
(** Cast offset-delta values. *)

(** {2 References} *)

module Reference: Identifiable.S
(** Branch references. *)

type reference = Reference.t

(** {2 Successors} *)

type successor =
  [ `Commit of sha1
  | `Tag of string * sha1
  | `Tree of string * sha1 ]
(** Value representing object successors:

    - blobs have no successors;
    - commits might have commit successors (in this case, 'ancestors');
    - tags have commit successors;
    - trees have trees and blobs successors.
*)

val succ: successor -> sha1
(** Return the sha1 of a successor object. *)

(** {2 Backends} *)

module type S = sig

  (** Signature for Git stores. *)

  type t
  (** Abstract value for Git store handlers. *)

  val create: ?root:string -> unit -> t Lwt.t
  (** Create a store handler for the given path. If [root] is not set,
      use the current directory. *)

  val root: t -> string
  (** The state root (or any other meaningful name to be displayed to
      the user). *)

  val dump: t -> unit Lwt.t
  (** Dump the store contents of cache to stderr. *)

  (** {2 Objects} *)

  val read: t -> sha1 -> value option Lwt.t
  (** Return the object having the given SHA1 name. *)

  val read_exn: t -> sha1 -> value Lwt.t
  (** Same as [read] but raises [Not_found] if no object with the given
      SHA1 is found. *)

  val mem: t -> sha1 -> bool Lwt.t
  (** Check whether a key belongs to the store. *)

  val read_inflated: t -> sha1 -> Mstruct.t option Lwt.t
  (** Return the inflated contents of object having the given SHA1
      name. *)

  val list: t -> sha1 list Lwt.t
  (** Return the list of SHA1 names. *)

  val write: t -> value -> sha1 Lwt.t
  (** Write a value and return the SHA1 of its serialized contents. *)

  val write_and_check_inflated: t -> sha1 -> string -> unit Lwt.t
  (** Compress a binary value and write it in the store. Check that
      the SHA1 of the uncompressed contents is the one that is
      expected. *)

  (** {2 References} *)

  val references: t -> reference list Lwt.t
  (** Return the list of references (ie. tags and branches). *)

  val mem_reference: t -> reference -> bool Lwt.t
  (** Check if a reference exists. *)

  val read_reference: t -> reference -> sha1 option Lwt.t
  (** Read a given reference. *)

  val read_reference_exn: t -> reference -> sha1 Lwt.t
  (** Read a given reference. *)

  val write_reference: t -> reference -> sha1 -> unit Lwt.t
  (** Write a reference. *)

  val remove_reference: t -> reference -> unit Lwt.t
  (** Remove a refernce. *)

  (** {2 Type Inspection} *)

  val type_of: t -> sha1 -> object_type option Lwt.t
  (** Return the object type corresponding to the given SHA1. *)

  val succ: t -> sha1 -> successor list Lwt.t
  (** Return the list of [successors] of a given object. *)

  (** {2 Filesystem} *)

  val iter_blobs: t ->
    f:(string list -> perm -> blob -> unit Lwt.t) ->
    init:SHA1.Commit.t ->
    unit Lwt.t
    (** Apply a function on all the blobs corresponding to the given
        commit. This is useful to create a new filesystem. *)

  val cache: t -> cache Lwt.t
  (** Return the cache of files. *)

end
