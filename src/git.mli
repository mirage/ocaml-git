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

(** Implementation of the Git protocol in pure OCaml

    %%VERSION%%
 *)

(** {1 Common signatures to all objects} *)


(** General functions. *)
module type S = sig

  type t
  (** The type for the given Git object. *)

  val equal: t -> t -> bool
  (** Are two objects equal? *)

  val hash: t -> int
  (** Hash an object. *)

  val compare: t -> t -> int
  (** Compare two objects. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for values of type [t]. *)

end

(** Extended sets *)
module type Set = sig
  include Set.S
  val pp: t Fmt.t
  val to_list: t -> elt list
  val of_list: elt list -> t
end

(** Extended maps *)
module type Map = sig
  include Map.S
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val keys: 'a t -> key list
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val add_multi: key -> 'a -> 'a list t -> 'a list t
end

module type IO = sig

  (** {1 Input/output functions} *)

  include S

  val input: Mstruct.t -> t
  (** Build a value from an inflated contents. *)

  val add: Buffer.t -> ?level:int -> t -> unit
  (** Add the serialization of the value to an already existing
      buffer.

      The compression [level] must be between 0 and 9: 1 gives best
      speed, 9 gives best compression, 0 gives no compression at all
      (the input data is simply copied a block at a time). The default
      value (currently equivalent to level 6) requests a default
      compromise between speed and compression. *)

end

(** {1 Base objects} *)

(** Hashing.

    This module handles both usual hashes and {i short} hashes, which
    are shorter sequences of bits with a valid hexadecimal
    representation. The only way to create short hashes is to use
    {!of_hex} with the [strict] argument set to false.

    When short hashes are used in normal Git operations, they can
    raise {!Ambiguous}.

    This module define various abstraction to distinguish between
    general, commit, node and blob hashes. It's just an abstraction
    layer, at runtine they will all be similar.
*)
module Hash: sig

  module type S = sig

    (** {1 Signature for hash values} *)

    include S
    (** The usual compare functions on hashes, but can raise
        {!Ambiguous} if one is short hash and is prefix to the other. *)

    val to_raw: t -> string
    (** Raw hash value. *)

    val of_raw: string -> t
    (** Abstract a raw hash value. *)

    val to_hex: t -> string
    (** [to_hex h] is [h]s' hex encoding. *)

    val hex_length: t -> int
    (** The number of hex digits in the hash. *)

    val lt: t -> t -> bool
    (** (<) relation between hash. *)

    val is_prefix: t -> t -> bool
    (** Check if a hash is a prefix of another hash. *)

    module Set: Set with type elt = t
    module Map: Map with type key = t

  end

  include S

  exception Ambiguous of string
  (** Exception raised when using short and ambiguous hashes. *)

  module Commit: S
  (** Commit nodes. *)

  module Tree: S
  (** Treee nodes. *)

  module Blob: S
  (** Blob nodes. *)

  val of_commit: Commit.t -> t
  (** A commit node is also a node. *)

  val to_commit: t -> Commit.t
  (** A node might be a commit. *)

  val of_tree: Tree.t -> t
  (** A tree node is also a node. *)

  val to_tree: t -> Tree.t
  (** A node might be a node. *)

  val of_blob: Blob.t -> t
  (** A blob node is also a node. *)

  val to_blob: t -> Blob.t
  (** A node might be a blob node. *)

  module type H = sig

    include S
    include IO with type t := t

    val of_hex: string -> t
    (** Convert an hex-encoded string into a hash value. Raise
        {!Ambiguous} if the hash is short; in that case use
        {!of_short_hex}. *)

    val of_short_hex: string -> t
    (** Same as {!of_hex} but allow values shorter than 20
        characters. Such hash values are called {i short} hashes and can
        cause some functions to raise {!Ambiguous}. *)

    val input_hex: Mstruct.t -> t
    (** Read an hex-encoded hash value. *)

    val add_hex: Buffer.t -> t -> unit
    (** Add the hex-encoding of the hash value to the buffer. *)

    val is_short: t -> bool
    (** Check if the hash is short. *)

    val zero: t
    (** A hash full of zero. Useful for padding. *)

  end

  module type IO = sig
    include H with type t = t
    module Blob: H with type t = Blob.t
    module Tree: H with type t = Tree.t
    module Commit: H with type t = Commit.t
  end

  type 'a digest = 'a -> t
  (** The type for digest functions. *)

  (** The signature to compute hash digests. *)
  module type DIGEST = sig
    val cstruct: Cstruct.t digest
    val string: string digest
    val length: int
  end

  module IO (D: DIGEST): IO

  module Array (D: DIGEST): sig
    (** {1 Arrays of hashes}

        Similar to [Cstruct.t] but where the unit of offsets and length
        is the number of hash values instead of the number of bytes.*)

    val get: Cstruct.t -> int -> t
    (** [get buf n] is the [n]-th hash in the buffer [buf]. *)

    val sub: Cstruct.t -> int -> int -> Cstruct.t
    (** Same as [Cstruct.sub] but where [offset] and [length] as hash
        offsets. *)

    val to_list: Cstruct.t -> t list
    (** [to_list t] is the list of elements of [t]. *)

    val length: Cstruct.t -> int
    (** [length v] is the number of hashes store in [t]. *)

    val linear_search: Cstruct.t -> t -> int option
    (** [linear_search buf h] iterates through the hashes stored in the
        buffer [buf]. Return the indice of the first hash equals to
        [h]. Can raise {!Ambiguous} if [h] is short and more than one
        hash are similar.*)

    val binary_search: Cstruct.t -> t ->  int option
    (** [binary_search buf h] binary searches through the sorted array
        of hashes stored in [buf]. Return the indice of the first hash
        equal to [h]. Can raise {!Ambiguous} if [h] is short and more
        than one hash are similar. *)

  end

end

(** Binary blobs *)
module Blob: sig

  include IO
  val to_raw: t -> string
  val of_raw: string -> t

end

(** Filesystem trees. *)
module Tree: sig

  type perm =
    [ `Normal
    | `Exec
    | `Link
    | `Dir
    | `Commit ]
  (** File permission. *)

  val pretty_perm: perm -> string
  (** Pretty printing of tree permissions. *)

  val string_of_perm : perm -> string
  (** Raw represention of a permission, using the [Git] format. *)

  val fixed_length_string_of_perm : perm -> string
  (** Fixed-length raw represention of a permission, using the [Git]
      format (6 characters long). *)

  type entry = {
    perm: perm;
    name: string;
    node: Hash.t;
  }
  (** A tree entry. This is either a directory or a file. As this is
      supposed to model a filesystem, directory does not contain
      data. *)

  type t = entry list
  (** A tree is an hierarchical data-store. NB: data (eg. blobs) are
      only carried on the leafs. *)

  include S with type t := t

  module IO (D: Hash.DIGEST): IO with type t = t

end

(** User actions. *)
module User: sig

  type tz_offset = {
    sign: [`Plus | `Minus];
    hours: int;
    min: int;
  }
  (** Signed offset of time zone from UTC. *)

  type t = {
    name : string;
    email: string;
    date : int64 * tz_offset option;
  }
  (** A user action has a (user) name, an (user) email and an (action)
      date. The date is the number of seconds since 12:00 midnight
      January 1, 1970, UTC without accounting for leap seconds with an
      optional timezone info. *)

  include IO with type t := t

end

(** Commits. *)
module Commit: sig

  type t = {
    tree     : Hash.Tree.t;
    parents  : Hash.Commit.t list;
    author   : User.t;
    committer: User.t;
    message  : string;
  }
  (** A commit is a tree snapshot, with some credentials (eg. we can
      find who created the initial snapshot, and who added it to to
      store) and a message explaining what the snapshot contains. *)

  include S with type t := t

  module IO (D: Hash.DIGEST): IO with type t = t

end

(** Object types. *)
module Object_type: sig

  type t =
    | Blob
    | Commit
    | Tag
    | Tree

  val to_string: t -> string

  val of_string: string -> t option

end

(** Annotated tags. *)
module Tag: sig

  type t = {
    obj    : Hash.t;
    typ    : Object_type.t;
    tag    : string;
    tagger : User.t;
    message: string;
  }
  (** A tag is bookmark to a previous commit. *)

  include S with type t := t

  module IO (D: Hash.DIGEST): IO with type t = t

end


(** References. *)
module Reference: sig

  include IO

  val to_raw: t -> string
  val of_raw: string -> t

  module Map: Map.S with type key = t
  (** A map of references. *)

  val head: t
  (** The repository HEAD. *)

  val master: t
  (** The master branch. *)

  val is_head: t -> bool
  (** Is the given reference the HEAD ? *)

  type head_contents =
    | Hash of Hash.Commit.t
    | Ref of t
    (** The possible HEAD contents. *)

  val head_contents_of_string: of_hex:(string -> Hash.t) -> string -> head_contents
  (** Parse the contents of HEAD *)

  val head_contents_of_commit: Hash.Commit.t Map.t -> Hash.Commit.t -> head_contents
  (** Build a head contents from a commit hash. The result is either the
      hex representation of the SHA or something like {i ref: <ref>} if
      the SHA has already a reference pointing to it. *)

  val pp_head_contents: Format.formatter -> head_contents -> unit
  (** Pretty-print head contents. *)

  val equal_head_contents: head_contents -> head_contents -> bool
  (** Compare head contents. *)

  val is_valid: t -> bool
  (** Check if a reference can be stored on disk properly. *)

end

(** Zlib Compression. *)
module Inflate: sig

  module type S = sig

    val inflate: ?output_size:int -> Mstruct.t -> Mstruct.t option
    (** Inflate an mstruct. *)

    val deflate: ?level:int -> Cstruct.t -> Cstruct.t
    (** Deflate an cstruct. *)

  end

  module None: S
  (** No compression. *)

  (** Minimaal signature provided by Zlib. *)
  module type ZLIB = sig
    exception Error of string * string
    val compress:
      ?level: int -> ?header: bool ->
      (bytes -> int) -> (bytes -> int -> unit) -> unit
    type stream
    type flush_command =
      | Z_NO_FLUSH
      | Z_SYNC_FLUSH
      | Z_FULL_FLUSH
      | Z_FINISH
    val inflate_init: bool -> stream
    val inflate:
      stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
      -> bool * int * int
    val inflate_end: stream -> unit
  end

  module Make (Z: ZLIB): S

end

(** Git values *)
module Value: sig

  (** Loose objects. *)

  (** The type for loose Git objects. *)
  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tag    of Tag.t
    | Tree   of Tree.t

  include S with type t := t

  val type_of: t -> Object_type.t
  (** Return the object type. *)

  val type_of_inflated: Mstruct.t -> Object_type.t
  (** Return the type of the inflated object stored in the given
      buffer. *)

  (** {2 Constructors} *)

  val commit: Commit.t -> t
  (** Cast a commit to an object. *)

  val blob: Blob.t -> t
  (** Cast a blob to an object. *)

  val tree: Tree.t -> t
  (** Cast a tree to an object. *)

  val tag: Tag.t -> t
  (** Cast a tag to an object. *)

  (** {1 IO types} *)

  type read = Hash.t -> t option Lwt.t
  (** The type for functions reading values. *)

  type read_inflated = Hash.t -> string option Lwt.t
  (** The type for functions reading inflated values. *)

  type write = t -> Hash.t Lwt.t
  (** The type for functions writing values. *)

  type write_inflated = string -> Hash.t Lwt.t
  (** The type for functions writing raw values. *)

  module Cache: sig

    (** {1 Global cache of values} *)

    val set_size: int -> unit
    (** Empty the current LRU cache, and create a new one with the given
        size. *)

    val clear: unit -> unit
    (** Empty the cache. *)

    val find: Hash.t -> t option
    (** Cache an inflated values. This is used by various operations, so
        it could be useful to look into it to speed-up operations which
        needs to search a pack file. *)

    val find_inflated: Hash.t -> string option
    (** Same as {!find} but store the inflated representation of the
        value. *)

    val add: Hash.t -> t -> unit
    (** Cache a value. *)

    val add_inflated: Hash.t -> string -> unit
    (** Cache an inflated value. *)

  end

  module type IO = sig

    include IO with type t = t

    (** {2 Inflated values} *)

    val name: t -> Hash.t
    (** Return the hash of the serialized contents. *)

    val add_header: Buffer.t -> Object_type.t -> int -> unit
    (** Append the given object header to a buffer.  *)

    val add_inflated: Buffer.t -> t -> unit
    (** Append the inflated serialization of an object to a buffer.
        Similar to [add], but without deflating the contents. *)

    val input_inflated: Mstruct.t -> t
    (** Build a value from an inflated contents. *)

  end

  module IO (D: Hash.DIGEST) (I: Inflate.S): IO

end

(** {1 Pack objects} *)

(** Pack indexes. *)
module Pack_index: sig

  type t
  (** Abstract type of pack index. *)

  type f = Hash.t -> int option
  (** The type of functions associating Hash1 keys to offset in the pack
      file. *)

  type raw = {
    offsets      : int Hash.Map.t;
    crcs         : int32 Hash.Map.t;
    pack_checksum: Hash.t;
  }
  (** [offsests] is the positions of the Hash objects in the
      corresponding raw pack file.

      [crcs] contains the CRC-32 value of the packed object data.

      [pack_checksum] is the corresponding pack file checksums, value
      which is needed when writing the pack index file to disk. *)

  module Raw: sig

    include S with type t = raw

    val keys: t -> Hash.Set.t
    (** Read only the keys contained in the index. *)

    val find_offset: t -> Hash.t -> int option
    (** Same as {!find_offset} but for raw packs. *)

    val lengths: t -> int option Hash.Map.t
    (** [lengths] returns the difference between two consecutive offsets
        (appart for the last elements, where the lenght can be [None] is
        the index file is build from a raw pack file). *)

  end

  module Make (D: Hash.DIGEST): sig

    val input: ?cache_size:int -> Cstruct.buffer -> t
    (** Create a pack index object. The results of [find_offset] are
        cached for upto [cache_size] Hash objects *)

    val find_offset: t -> Hash.t -> int option
    (** [find_offset] searches the index for the offset of the Hash object.
        Binary search is performed until the candidates are narrowed down to
        [scan_thresh] or less. *)

    val mem: t -> Hash.t -> bool
    (** [mem] checks if the Hash object is contained in the index object *)

    val keys: t -> Hash.t list
    (** [keys t] is the list of hash keys present in [t]. *)

    module Raw: IO with type t = raw

  end

end

(** Packed values. *)
module Packed_value: sig

  (** {1 Packed values} *)

  type copy = { copy_offset: int; copy_length: int; }
  (** The type for [Copy]'s {!hunk} arguments. *)

  type hunk = Insert of string | Copy of copy
  (** The type for {!delta} hunks. A delta hunk can either insert a
        string of copy the contents of a base object. *)

  type 'a delta = {
    source       : 'a;
    source_length: int;
    result_length: int;
    hunks        : hunk list;
  }
  (** The type for delta values. *)

  type kind =
    | Raw_value of string
    | Ref_delta of Hash.t delta
    | Off_delta of int delta
    (** The type for packed values kind. *)

  type t = { kind: kind; offset: int }
  (** The type for packed values. *)

  val shallow: Hash.Set.t -> t -> bool
  (** [shallow p t] checks whether the Hashs appearing in [t] also appear
      in the pack file [p]. *)

  val create: offset:int -> kind:kind -> t
  (** Create a packed value. *)

  val kind: t -> kind
  (** [kind t] is [t]'s kind. *)

  val offset: t -> int
  (** [offset t] is [t]'s offset. *)

  val pp_kind: Format.formatter -> kind -> unit
  (** Pretty-print packed values' kind. *)

  val is_delta: t -> bool
  (** Check if a packed value is a delta (either a [Ref_delta] or an
      [Off_delta]). *)

  val result_length: t -> int
  (** Return the lenght of the result object. *)

  val source_length: t -> int
  (** Return the lenght of the base (source) object. *)

  include S with type t := t

  (** {1 Positition-independant packed values} *)

  module PIC: sig

    type kind = Raw of string | Link of t delta
    (** The type for position-independent packed values' kind. *)

    and t = {
      kind   : kind;
      hash   : Hash.t;
      shallow: bool;
      mutable raw: string option;
    }
    (** The type for postition-independant packed values. See {!S.PIC}. *)

    include S with type t := t

    val create: ?raw:string -> ?shallow:bool -> Hash.t -> kind -> t
    (** Create a position-independent packed value. By default,
        [shallow] is [false]. *)

    val of_raw: ?shallow:bool ->  Hash.t -> string -> t
    (** [of_raw sha1 raw] is the position-independant packed value built
        by parsing [raw]. By default [shallow] is [false]. *)

    val kind: t -> kind
    (** [kind t] is [t]'s kind. *)

    val name: t -> Hash.t
    (** [name t] is [t]'s name. *)

    val raw: t -> string option
    (** [raw t] is [t]'s raw represation. *)

    val shallow: t -> bool
    (** [shallow t] is true iff [t] is not included in the pack file. *)

    val unpack_kind: kind -> string
    (** Unpack a PIC kind into a string. *)

    val unpack: t -> string
    (** Unpack a PICK value into a string. *)

  end

  type pic = PIC.t
  (** The type for position-independant packked values. *)

  module IO (D: Hash.DIGEST) (I: Inflate.S): sig

    module type IO = sig
      include IO with type t = kind

      val size: Mstruct.t -> int
      (** Get size of the value that [input] would return, without actually
          decompressing the value. *)
    end

    module V2: IO
    (** Packed values version 2. *)

    module V3: IO
    (** Packed values version 3. *)

    (** {2 Conversion to values} *)

    val add_inflated_value:
      read:Value.read_inflated -> offsets:(int -> Hash.t option) ->
      Buffer.t -> t -> unit Lwt.t
    (** Append the inflated representation of a packed value to a given
        buffer. *)

    val to_value:
      index:Pack_index.f ->
      read:Value.read_inflated -> write:Value.write_inflated -> version:int ->
      ba:Cstruct.buffer -> t -> Value.t Lwt.t
    (** Unpack the packed value using the provided indexes. *)

    (** {2 Position independant packed values} *)

    val unpack:
      index:Pack_index.f ->
      read:Value.read_inflated -> write:Value.write_inflated -> version:int ->
      ba:Cstruct.buffer -> t -> string Lwt.t
    (** Same as {!to_value} but for inflated raw buffers. *)

    val value_of_pic: pic -> Value.t
    (** [to_value p] unpacks the packed position-independant value
        [p]. *)

    val to_pic: read:Value.read_inflated ->
      offsets:(int -> pic option) -> hashes:(Hash.t -> pic option) ->
      t -> pic Lwt.t
    (** [to_pic t] is the position-independant representation of the
        packed value [t]. *)

    val of_pic: index:Pack_index.f -> offset:int -> pic -> t
    (** Position dependent packed value. Convert a [PIC.Link] into to the
        corresponding [Off_delta] and [Ref_delta], using the provided
        indexes. *)

  end

end

(** Pack files. *)
module Pack: sig

  type t = Packed_value.pic list
  (** A pack value is an ordered list of position-independant packed
      values and the keys of the corresponding inflated objects. *)

  val keys: t -> Hash.Set.t
  (** Return the keys present in the pack. *)

  include S with type t := t

  type raw
  (** The type for raw packs. *)

  module Raw: sig

    include S with type t = raw

    val index: t -> Pack_index.Raw.t
    (** Get the raw index asoociated to the raw pack. *)

    val name: t -> Hash.t
    (** Get the name of the pack. *)

    val keys: t -> Hash.Set.t
    (** Get the keys present in the raw pack. *)

    val buffer: t -> Cstruct.t
    (** Get the pack buffer. *)

    val shallow: t -> bool
    (** [shallow t] is true if all the Hash references appearing in [t]
        corresponds to objects also in [t]. *)

    val input_header: Mstruct.t -> [`Version of int] * [`Count of int]
    (** [input_head buf] reads the pack [version] number (could be 2 or
        3) and the [count] of packed values in the pack file. *)

  end

  module type IO = sig

    include S with type t = t

    val add: ?level:int -> t -> Pack_index.raw * Cstruct.t
    (** Serialize a pack file into a list of buffers. Return the
        corresponding raw index. *)

    val input: ?progress:(string -> unit) ->
      index:Pack_index.f -> keys:Hash.Set.t -> read:Value.read_inflated ->
      Mstruct.t -> t Lwt.t
    (** The usual [Object.S.input] function, but with additionals [index]
        and [keys] arguments to speed-up ramdom accesses and [read] to
        read shallow objects external to the pack file. *)

    val read: t -> Hash.t -> Value.t option
    (** Return the value stored in the pack file. *)

    val read_exn: t -> Hash.t -> Value.t
    (** Return the value stored in the pack file. *)

    val create: (Hash.t * Value.t) list -> t
    (** Create a (not very well compressed) pack file. *)

    module Raw: sig

      (** Raw pack file: they contains a pack index and a list of
          position-dependant deltas. *)

      include S with type t = raw

      val add: [`Not_defined]
      (** [Pack.Raw.add] is not defined. Use {!buffer} instead. *)

      val input: ?progress:(string -> unit) -> read:Value.read_inflated ->
        Mstruct.t -> t Lwt.t
      (** [input ~read buf] is the raw pack and raw index obtained by
          reading the buffer [buf]. External (shallow) Hash1 references are
          resolved using the [read] function; these references are needed
          to unpack the list of values stored in the pack file, to then
          compute the full raw index. *)

      val unpack: ?progress:(string -> unit) -> write:Value.write_inflated ->
        t -> Hash.Set.t Lwt.t
      (** Unpack a whole pack file on disk (by calling [write] on every
          values) and return the Hash1s of the written objects. *)

      val read: index:Pack_index.f ->
        read:Value.read_inflated -> write:Value.write_inflated ->
        Mstruct.t -> Hash.t -> Value.t option Lwt.t
      (** Same as the top-level [read] function but for raw packs. *)

      val read_inflated: index:Pack_index.f ->
        read:Value.read_inflated -> write:Value.write_inflated ->
        Mstruct.t -> Hash.t -> string option Lwt.t
      (** Same as {!read} but for inflated values. *)

      val size: index:Pack_index.f -> Mstruct.t -> Hash.t -> int option Lwt.t
      (** [size ~index buf h] is the (uncompressed) size of [h]. *)
    end

    type raw = Raw.t
    (** The type for raw packs. *)

    val of_raw: ?progress:(string -> unit) -> Raw.t -> t Lwt.t
    (** Transform a raw pack file into a position-independant pack
        file. *)

    val to_raw: t -> Raw.t
    (** Transform a position-independant pack file into a raw pack and
        index files. *)

  end

  module IO (D: Hash.DIGEST) (I: Inflate.S): IO

end

(** Packed references. *)
module Packed_refs: sig

  (** The type for packed reference entries. *)
  type entry = [
    | `Newline
    | `Comment of string
    | `Entry of (Hash.t * Reference.t)
  ]

  include S with type t = entry list

  val find: t -> Reference.t -> Hash.t option
  (** [find t r] is [r]'s hash in [t]. *)

  val references: t -> Reference.t list
  (** [references t] is the list of [t]'s references. *)

  module IO (D: Hash.DIGEST): IO with type t = t

end

(** Implementation of the V2 Git Index format (as V1 is deprecated).

    The index file contains information used by Git to manage the
    state of working directory contents.
*)
module Index: sig

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

  include S with type t := t

  module type IO = IO with type t = t

  module IO (D: Hash.DIGEST): IO

end

(** {1 Git Stores} *)

(** General signature for Git stores. *)
module Store: sig

  module type S = sig

    type t
    (** Git store handlers. *)

    val create: ?root:string -> ?dot_git:string -> ?level:int ->
      unit -> t Lwt.t
    (** Create a store handler for the given path. See {!root} and
        {!level}.*)

    val dot_git: t -> string
    (** The location of the [.git] directory. By defaut it is
        {!root}[/.git]. *)

    val root: t -> string
    (** The location of the repository root (or any other meaningful
        name to be displayed to the user). By default, it is the current
        directory. *)

    val level: t -> int
    (** The compression [level] used when creating new Git objects. must
        be between 0 and 9: 1 gives best speed, 9 gives best
        compression, 0 gives no compression at all (the input data is
        simply copied a block at a time). The default value (currently
        equivalent to level 6) requests a default compromise between
        speed and compression. *)

    val dump: t -> unit Lwt.t
    (** Dump the store contents to stderr. *)

    val contents: t -> (Hash.t * Value.t) list Lwt.t
    (** Get the full store contents. *)

    (** {2 Objects} *)

    val size: t -> Hash.t -> int option Lwt.t
    (** Return the size of the blob having the given Hash name. *)

    val read: t -> Hash.t -> Value.t option Lwt.t
    (** Return the object having the given Hash name. *)

    val read_exn: t -> Hash.t -> Value.t Lwt.t
    (** Same as [read] but raises [Not_found] if no object with the given
        Hash is found. *)

    val mem: t -> Hash.t -> bool Lwt.t
    (** Check whether a key belongs to the store. *)

    val list: t -> Hash.t list Lwt.t
    (** Return the list of Hash names. *)

    val write: t -> Value.t -> Hash.t Lwt.t
    (** Write a value and return the Hash of its serialized contents. *)

    val write_pack: t -> Pack.raw -> Hash.Set.t Lwt.t
    (** Write a raw pack file and the corresponding index. Return the
        objects IDs which have been written. *)

    (** {2 References} *)

    val references: t -> Reference.t list Lwt.t
    (** Return the list of references (ie. tags and branches). *)

    val mem_reference: t -> Reference.t -> bool Lwt.t
    (** Check if a reference exists. *)

    val read_reference: t -> Reference.t -> Hash.t option Lwt.t
    (** Read a given reference. *)

    val read_reference_exn: t -> Reference.t -> Hash.t Lwt.t
    (** Read a given reference. *)

    val write_head: t -> Reference.head_contents -> unit Lwt.t
    (** Write the HEAD. *)

    val read_head: t -> Reference.head_contents option Lwt.t
    (** Read the head contents. *)

    val write_reference: t -> Reference.t -> Hash.t -> unit Lwt.t
    (** Write a reference. *)

    val remove_reference: t -> Reference.t -> unit Lwt.t
    (** Remove a refernce. *)

    (** {2 Git index files} *)

    val read_index: t -> Index.t Lwt.t
    (** Return the index file. *)

    val write_index: t -> ?index:Index.t -> Hash.Commit.t -> unit Lwt.t
    (** Update the index file for the given revision. A side-effect of
        this operation is that the blobs are expanded into the
        filesystem.

        {b Note:} It is the user responsability to ensure that filenames
        are valid. No sanitazition is done by the library -- the Git
        format does not impose a filename format as this is a constraint
        of the underlying filesystem.

        If [index] is not set, read the current index and update it with
        the current state of the filesystem. *)

    (** {2 Backend kind} *)

    val kind: [`Memory | `Disk]
    (** The kind of backend. *)

    (** {2 Raw values} *)

    val read_inflated: t -> Hash.t -> string option Lwt.t
    (** Read a raw buffer from the store. *)

    val write_inflated: t -> string -> Hash.t Lwt.t
    (** Write a raw buffer in the store. *)

    module Digest: Hash.DIGEST
    (** Digest functions that the store is using. *)

    module Inflate: Inflate.S
    (** Inflate functions that the store is using. *)

  end

end


(** {1 Wire protocol} *)

(** Git ressource identifiers (GRI).

    GRIs are similar to URIs, but Git also allows some blend of URL
    and SSH remote files such as:

      git@github.com:samoht/ocaml-git.git

*)
module Gri: sig

  type t
  (** Git ressource identifier values. *)

  val of_string: string -> t
  (** Create a GRI from a string. *)

  val to_string: t -> string
  (** Convert a string to a GRI. *)

  val to_uri: t -> Uri.t
  (** Cast to [Uri.t]. *)

  val of_uri: Uri.t -> t
  (** Cast from [Uri.t]. *)

end

(** Clone/Fecth/Push protocol *)
module Sync: sig

  type protocol = [ `SSH | `Git | `Smart_HTTP ]
  (** The type for the different Git protocols. *)

  val protocol: Uri.t -> [`Ok of protocol | `Not_supported of string | `Unknown]
  (** [protocol uri] is the Git protocol associated to [uri]. *)

  type capability =
    [ `Multi_ack
    | `Thin_pack
    | `No_thin
    | `Side_band
    | `Side_band_64k
    | `Ofs_delta
    | `Shallow
    | `No_progress
    | `Include_tag
    | `Report_status
    | `Delete_refs
    | `Allow_reachable_sha1_in_want (* in Git 2.5 only *)
    | `Agent of string
    | `Other of string ]

  val pp_capability: capability Fmt.t
  (** [pp_capability] it the pretty-printer for values of type
      {!pp_capability}. *)

  module Result: sig

    type fetch
    (** The resulting hashes and references. *)

    val head_contents: fetch -> Reference.head_contents option
    (** [head_contents f] is [f]'s head contents (the value of the
        remote {i .git/HEAD}). *)

    val head: fetch -> Hash.Commit.t option
    (** [head f] is [f]'s head commit. *)

    val references: fetch -> Hash.t Reference.Map.t
    (** [references f] are [f]'s references. *)

    val hashes: fetch -> Hash.Set.t
    (** [hashes f] are [f]'s object hashes. *)

    val pp_fetch: fetch Fmt.t
    (** [pp_fetch] is the pretty-printer for {!fetch} values. *)

    type ok_or_error = [`Ok | `Error of string]

    type push = {
      result  : ok_or_error;
      commands: (Reference.t * ok_or_error) list;
    }
    (** The result of a push operation. *)

    val pp_push: push Fmt.t
    (** [pp_push] is the pretty-printer for {!push} values. *)

  end

  type want = [ `Ref of Reference.t | `Commit of Hash.Commit.t ]
  (** The type for values wanted. See {!fetch} for details. *)

  val pp_want: want Fmt.t
  (** [pp_want] is the pretty-printer for values of type {!want}. *)

  module type S = sig

    type t
    (** Abstract value for stores. *)

    type ctx
    (** Connection context *)

    (** {1 The base Git protocol and Git+SSH} *)

    val ls: ?ctx:ctx -> t -> Gri.t -> Hash.t Reference.Map.t Lwt.t
    (** List the references of the remote repository. *)

    val push: ?ctx:ctx -> t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
    (** Push a local branch to a remote store. *)

    val fetch:
      ?ctx:ctx ->
      ?deepen:int ->
      ?unpack:bool ->
      ?capabilities:capability list ->
      ?wants:want list ->
      ?progress:(string -> unit) ->
      t -> Gri.t -> Result.fetch Lwt.t
    (** [fetch t uri] fetches the contents of [uri] into the store [t].

        By default, all the remote references are updated. This behavior
        can be changed by using the [wants] parameter:

        {ul
        {- if [wants] is not specified, the objects corresponding to {b
           all} the remote references are downloaded. This is useful
           when cloning a new repository as the remote references are
           not yet known.}
        {- If a reference (e.g. a [`Ref] {!want} value) appears in the
           list, the object corresponding to that remote reference are
           fetched. This works only if the server exports the
           "allow-reachable-sha1-in-want" (available in Git 2.5.0)}
        {- If a commit hash (e.g. a [`Commit] {!want} value) in the
           list, the objects corresponding to the that remote commits
           are fetched..}  }

        {b Note:} the local HEAD is not modified when doing a fetch. To do so
        (for instance when doing a clone) do the following:

        {[
          fetch t gri >>= fun r ->
          match Result.head_contents r with
          | Some h -> Store.write_head t h
          | None   -> Lwt.return_unit
        ]}
    *)

    (** Similar to {!fetch} but also initialise [HEAD] and a the
        specified [branch]. If [branch] is not specified, initialise all
        the references that the remote repository exposes. *)
    val clone:
      ?ctx:ctx ->
      ?deepen:int ->
      ?unpack:bool ->
      ?capabilities:capability list ->
      ?branch:want ->
      ?progress:(string -> unit) ->
      t -> checkout:bool -> Gri.t -> Result.fetch Lwt.t

  end

  (** {2 Constructor} *)

  module type IO = sig

    (** Channel abstraction. *)

    type ic
    (** Type for input channels. *)

    type oc
    (** Type for output channels. *)

    type ctx
    (** Connection context. *)

    val with_connection: ?ctx:ctx -> Uri.t -> ?init:string ->
      (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
    (** Connect to a remote server, get the corresponding input and
        output channels and apply a function on them. Close the channel
        once the function returns. The [init] corresponds to an optional
        first message sent on the connection to set-it up. *)

    val read_all: ic -> string list Lwt.t
    (** Read all the channel contents (until the channel is closed by
        the other side). *)

    val read_exactly: ic -> int -> string Lwt.t
    (** Read a given number of bytes. *)

    val write: oc -> string -> unit Lwt.t
    (** Write a string on a channel. *)

    val flush: oc -> unit Lwt.t
    (** Flush the channel. *)

  end

  module Make (IO: IO) (S: Store.S): S with type t = S.t and type ctx = IO.ctx

end


(** Store Git objects in memory. *)
module Memory: sig

  module Make (D: Hash.DIGEST) (I: Inflate.S): sig

    include Store.S

    val clear: ?root:string -> unit -> unit
    (** Remove all the contents store in memory for the given root. Use
        the default root if the optional argument is not provided. *)

    val clear_all: unit -> unit
    (** Remove all the contents store in memory for all roots. *)

  end

end

(** Store Git objects on the local filesystem. *)
module FS: sig

  module type S = sig

    include Store.S

    val remove: t -> unit Lwt.t
    (** Remove all the contents of the store. *)

    val create_file: t -> string -> Tree.perm -> Blob.t -> unit Lwt.t
    (** Create a file on the filesystem, with the given mode. *)

    val entry_of_file: t -> Index.t ->
      string -> Tree.perm -> Hash.Blob.t -> Blob.t -> Index.entry option Lwt.t
    (** Generate a cache entry for the file. Create a fresh file if it
        does not already exist. If [root] is not set, use the current
        working directory as repository root. *)

    val clear: unit -> unit
    (** Clear the LRU caches. *)

  end

  (** {2 Constructor} *)

  module type IO = sig

    val getcwd: unit -> string Lwt.t
    (** Get the current working directory. *)

    val realpath: string -> string Lwt.t
    (** Very dumb real-path. Only works for existing directories. *)

    val mkdir: string -> unit Lwt.t
    (** Create a directory (and the parent dirs if needed). *)

    val remove: string -> unit Lwt.t
    (** Remote a file or a directory (even if non-empty). *)

    val file_exists: string -> bool Lwt.t
    (** Does the given file exists? See {!Sys.file_exists}*)

    val directories: string -> string list Lwt.t
    (** List the subdirs. *)

    val files: string -> string list Lwt.t
    (** List the subfiles. *)

    val rec_files: string -> string list Lwt.t
    (** [rec_files dir] is the list of the files recursively present in
        [dir] and all of its sub-directories. Return filenames prefixed
        by [dir]. *)

    val read_file: string -> Cstruct.t Lwt.t
    (** Read a file and return a mutable C-like structure with its
        contents. *)

    val write_file: string -> ?temp_dir:string -> Cstruct.t -> unit Lwt.t
    (** Write a bigarray to a file. Atomicity is guaranteed by writing
        the file first in [temp_dir] and then moving the file to the
        right location. By defaut, [temp_dir] is
        [Filename.get_temp_dir_name]. *)

    val chmod: string -> int -> unit Lwt.t
    (** Change the file mode. *)

    val stat_info: string -> Index.stat_info
    (** Return the stats of the given file. *)

  end

  module Make (IO: IO) (D: Hash.DIGEST) (I: Inflate.S): S
  (** Create an on-disk store implementation. *)

end

(** {1 Higher level read-only functions} *)

(** Search over the graph of Git objects. *)
module Search: sig

  type pred = [
    | `Commit of Hash.t
    | `Tag of string * Hash.t
    | `Tree of string * Hash.t
    | `Tree_root of Hash.t
  ]
  (** The type for {!Value.t} predecessors. *)

  module Make (S: Store.S): sig

    val pred: S.t -> ?full:bool -> Hash.t -> pred list Lwt.t
    (** [pred t s] is the list of [s]'s predecessors in the graph
        [t]. If [full] is not set (by default it is) only consider
        commits and their history relation. *)

    type path = [
      | `Tag of string * path
      | `Commit of path
      | `Path of string list
    ]
    (** The type for path values. See {!find} for details. *)

    val mem: S.t -> Hash.t -> path -> bool Lwt.t
    (** [mem t s p] check wether there exists a path [p] from [s] in
        [t]. *)

    val find: S.t -> Hash.t -> path -> Hash.t option Lwt.t
    (** [find t s p] follows the path [p] from [s] in [t]. *)

  end

end

(** Functions to manipulate the graph of Git objects. *)
module Graph: sig

  module K: Graph.Sig.I with type V.t = Hash.t
  (** The graph of Git keys. *)

  module Make (S: Store.S): sig

    val keys: K.t -> Hash.t list
    (** [keys g] is a topological sort of [g]'s keys. *)

    val of_keys: S.t -> K.t Lwt.t
    (** Return the graph of keys of the given Git store. *)

    val closure: ?full:bool -> S.t -> min:Hash.Set.t -> max:Hash.Set.t -> K.t Lwt.t
    (** Return a consistent cut of the graph of keys, where no elements
        are stricly lesser than the ones in [min] and none are bigger
        than the ones in [max]. Elements of [min] and [max] are in the
        closure. If [full] is not set (it is by default), return only
        the graph of commits. *)

    val pack: S.t -> min:Hash.Set.t -> max:Hash.Set.t -> (Hash.t * Value.t) list Lwt.t
    (** Return a packed (closed) collection of objects. *)

    val to_dot: S.t -> Buffer.t -> unit Lwt.t
    (** [to_dot g buffer] fille [buffer] with the `.dot` representation
        of the Git graph. *)

  end

end
