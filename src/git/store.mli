(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

(** Implementation of a Git stores.

    This implementation is more complete than the memory back-end
    because firstly, Git was think to be use on a file-system. Then,
    because for each operations, we let the client to control the
    memory consumption.

    So we provide a more powerful API which let the user to notice
    aready allocated buffers outside this scope and process some I/O
    operations on pools. *)

module type LOOSE = sig

  type t
  (** The type of the {i loosed} Git object. *)

  type state
  (** The type for the state of a Git store. *)

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]
  (** Kind of the {i loosed} Git object. *)

  module Hash: S.HASH
  (** The [Hash] module used to make the implementation. *)

  module FS: S.FS
  (** The [FS] module used to make the implementation. *)

  module Inflate: S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate: S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t
  (** The Value module, which represents the Git object. *)

  type error

  val lookup: state -> Hash.t -> Hash.t option Lwt.t
  (** [lookup state hash] is the object associated with the hash
      [hash]. The result is [None] if the Git object does not exist or
      is not stored as a {i loose} object. *)

  val mem: state -> Hash.t -> bool Lwt.t
  (** [mem state hash] is true iff there is an object such that
      [digest(object) = hash]. This function is the same as [lookup
      state hash <> None]. *)

  val list: state -> Hash.t list Lwt.t
  (** [list state] is the list of all the available git {i loose}
      objects in [state]. The list returned does not contain all git
      objects available in your repository but only which {i loosed}
      one. *)

  val read: state -> Hash.t -> (t, error) result Lwt.t
  (** [read state hash] is the git {i loose} object in [state] such
      that [digest(result) = hash].

      Can return an {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object does not respect the Git format.}} *)

  val size: state -> Hash.t -> (int64, error) result Lwt.t
  (** [size t hash] is the size of the git {i loose} object which
      respects the predicate [digest(object) = hash]. The size is how
      many byte(s) are needed to store the serialized (but not
      inflated) git object in bytes (without the header).

      As {!read}, {!size} can return an {!error}. *)

  val write: state -> t -> (Hash.t * int, error) result Lwt.t
  (** [write state v] writes [v] as a {i loose} git object in the
      file-system.

      This function does not allocate any buffer and uses only the
      specified buffers in [state]'s buffer. Once the write is done,
      [v] becomes available by [read state digest(v)].

      Can return an {!error}:

      {ul
      {- {!FS.File.error} when we can not create a new file in
      the file-system.}
      {- {!Value.E.error} when we can not serialize xor deflate the
      requested git {i loose} object. This kind of error should be
      never happen.}}

      The current implementation does not limit the memory consumption
      of the deflate computation (i.e. {i zlib} and the flush method).
      Depending of the object [v], the process can consume a lot of
      memory. *)

  val write_inflated: state -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This
      function does not verify if the raw data is well-defined (and
      respects the Git format). Then, this function returns the hash
      produced from the kind and the inflated raw to let the user to
      retrieve it.

      If we retrieve any error error while the I/O operations, we {b
      raise} by {!Lwt.fail} a [Failure] which describe in a [string]
      the error encountered. *)

  val read_inflated: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [read_inflated state hash] can retrieve a git {i loose} object
      from the file-system. However, this function {b does not}
      de-serialize the git object and returns the kind of the object
      and the {i raw-data} inflated. Precisely, it decodes only the
      header.

      This function allocate only one buffer in the major-heap and
      uses only the specified buffers to compute the {i raw-data} in
      the allocated {!Cstruct.t}. The {i raw-data} respects the
      predicate [digest(header + raw-data) = hash].

      Can return an {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object has a wrong header or it is corrupted.}} *)

  val read_inflated_wa:
    Cstruct.t -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [read_inflated_wa result state h] is the Git {i loose} object
      with the hash [h]. However, this function {b does not}
      de-serialize the git object and returns the kind of the object
      and the {i raw-data} inflated. Precisely, it decodes only the
      header. This function needs some buffers, provided by [decoder]
      as well as [result], a buffer to store the result.

      The suffix [wa] refers to: Without Allocation. Indeed, this
      function allocates {b only} any data in the minor-heap. All
      other needed OCaml objects must be noticed by the client.
      However, the client needs to provide a well sized [result]
      (otherwise, the client can retrieve an error). He can get this
      information by {!size}.

      The {i raw-data} respects the predicate [digest(header +
      raw-data) = h].

      Can return an {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object has a wrong header or it is corrupted.}}

      In a server context, this function should be used to limit the
      allocation. *)

  module D: S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = [ Error.Decoder.t0 | `Inflate of Inflate.error ]
  (** The decoder of the Git object. We constraint the input to be an
      {!Inflate.window} and a {Cstruct.t} which used by the {Inflate}
      module and an other {Cstruct.t} as an internal buffer.

      All error from the {!Inflate} module is relayed to the
      [`Inflate] error value.

      The decoder includes an header process. This decoder does not
      correspond directly to a de-serialized Git object but a
      de-serialized Git {i loose} object. *)

  module E: S.ENCODER
    with type t = t
     and type init = int * t * int * Cstruct.t
     and type error = [ `Deflate of Deflate.error ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git object.
      We constraint the output to be a {Cstruct.t}. This encoder needs
      the level of the compression, the value {!t}, the memory
      consumption of the encoder (in bytes - must be a power of two)
      and an internal buffer between the compression and the encoder.

      All error from the {!Deflate} module is relayed to the
      [`Deflate] error value.

      The encoder includes an header process. This encoder does not
      correspond directly to a serialized Git object but a serialized
      Git {i loose} object. That means we put in front of the Git
      object an header:

      > kind length\000 ... *)
end

module type PACK = sig

  type t
  (** The type of the {i packed} Git object. *)

  type state
  (** The type for the state of a Git store. *)

  type value
  (** The type of the Git values. *)

  module Hash: S.HASH
  (** The [Hash] module used to make the implementation. *)

  module FS: S.FS
  (** The [FS] module used to make the implementation. *)

  module Inflate: S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate: S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module HDec: Unpack.H with module Hash = Hash

  module PDec: Unpack.P
    with module Hash = Hash
     and module Inflate = Inflate
     and module HunkDecoder := HDec

  module RPDec: Unpack.D
    with module Hash = Hash
     and module Inflate = Inflate
     and module HunkDecoder := HDec
     and module PackDecoder := PDec

  module PEnc: Pack.P
    with module Hash = Hash
     and module Deflate = Deflate

  module IDec: Index_pack.LAZY
    with module Hash = Hash

  module IEnc: Index_pack.ENCODER
    with module Hash = Hash

  module PInfo: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec

  type error

  val lookup: state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  (** [lookup state hash] tries to find the object associated by the
      hash [hash] in all {i IDX} files available in the current git
      repository [state]. This function returns [None] if the Git
      object does not exist in any {i IDX} files or it does not exists
      in the current repository. *)

  val mem: state -> Hash.t -> bool Lwt.t
  (** [mem state hash] is true iff there is an object such that
      [digest(object) = hash]. This function is the same as [lookup
      state hash <> None]. *)

  val list: state -> Hash.t list Lwt.t
  (** [list state] is the list of all the {i packed} Git object
      noticed by all {i IDX} files available in the current git
      repository [state]. Errors are ignored and skipped. *)

  val read: state -> Hash.t -> (t, error) result Lwt.t
  (** [read state hash] can retrieve a git {i packed} object from any
      {i PACK} files available in the current git repository
      [state]. It just inflates the git object and informs some
      meta-data (like kind, CRC-32 check-sum, length, etc.) about
      it. Then, the client can use the related decoder to get the
      OCaml value.

      This function allocates 2 {Cstruct.t} needed to re-construct the
      git {i packed} object in any case (if it is delta-ified or not)
      and a certain amount of little buffer (sized by 0x7F bytes) to
      help the construction only when the requested object is
      delta-ified.

      Can return an {!error}:

      {ul
      {- {!FS.File.error} or {!FS.Dir.error} or
      {!FS.Mapper.error} when we retrieve a file-system error}
      {- {!PACKDecoder.error} when we retrieve an error about the
      decoding of the {i packed} git object in the founded {i PACK}i
      file}
      {- {!IDXDecoder.error} when we retrieve an error about the
      decoding of an {i IDX} file}
      {- [`Not_found] when the requested object is not {i packed}}} *)

  val size: state -> Hash.t -> (int, error) result Lwt.t
  (** [size state hash] returns the size of the git {i packed} object
      which respects the predicate [digest(object) = hash]. The size
      is how many byte(s) are needed to store the serialized (but not
      inflated) git object in bytes (without the header).

      As for {!read}, {!size} can return an {!error}. *)

  type stream = unit -> Cstruct.t option Lwt.t
  (** The stream contains the PACK flow. *)

  module Graph: Map.S with type key = Hash.t

  val from: state -> stream -> (Hash.t * int, error) result Lwt.t
  (** [from git stream] populates the Git repository [git] from the
      PACK flow [stream]. If any error is encountered, any Git objects
      of the PACK flow are not added in the Git repository. *)

  val make: state
    -> ?window:[ `Object of int | `Memory of int ]
    -> ?depth:int
    -> value list
    -> (stream * (Crc32.t * int64) Graph.t Lwt_mvar.t, error) result Lwt.t
  (** [make ?window ?depth values] makes a PACK stream from a list of
      {!Value.t}.

      [?window] specifies the weight of the window while the
      delta-ification. The client can limit the weight by the number
      of objects in the windoz (by default, is 10) or by the weight in
      byte of the window in your memory.

      [depth] (default is [50]) limits the depth of the
      delta-ification.

      Then, the function returns a stream and a protected variable
      which contains a representation of the associated IDX file of
      the PACK stream. This protected variable is available
      ([Lwt_mvar.take]) only {b at the end} of the PACK stream. That
      means, the client needs to consume all of the stream and only
      then he can take the [Graph.t] associated. *)
end

module type S = sig

  type t
  (** The type of the git repository. *)

  module Hash: S.HASH
  (** The [Digest] module used to make the implementation. *)

  module Inflate: S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate: S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module FS : S.FS
  (** The [FS] module used to make the implementation. *)

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t
  (** The Value module, which represents the Git object. *)

  module Reference: Reference.IO
    with module Hash = Hash
     and module FS = FS
  (** The Reference module, which represents the Git reference. *)

  module HDec: Unpack.H
    with module Hash = Hash

  module PDec: Unpack.P
    with module Hash = Hash
     and module Inflate = Inflate
     and module HunkDecoder := HDec

  module RPDec: Unpack.D
    with module Hash = Hash
     and module Inflate = Inflate
     and module HunkDecoder := HDec
     and module PackDecoder := PDec

  module PEnc: Pack.P
    with module Hash = Hash
     and module Deflate = Deflate

  module IDec: Index_pack.LAZY
    with module Hash = Hash

  module IEnc: Index_pack.ENCODER
    with module Hash = Hash

  module PInfo: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec

  module Packed_refs: Packed_refs.S
    with module Hash = Hash
     and module FS = FS

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]
  (** Kind of the {i packed} Git object. *)

  (** The type error. *)
  type error =
    [ Error.Decoder.t2
    | Error.Decoder.t0
    | Error.Angstrom.t2
    | Error.Angstrom.t0
    | `Close             of Fpath.t * FS.error
    | `Create            of Fpath.t * FS.error
    | `Delete            of Fpath.t * FS.error
    | `Length            of Fpath.t * FS.error
    | `Map               of Fpath.t * FS.error
    | `Move              of Fpath.t * Fpath.t * FS.error
    | `Open              of Fpath.t * FS.error
    | `Read              of Fpath.t * FS.error
    | `Write             of Fpath.t * FS.error
    | `Stack             of Fpath.t
    | `Deflate_file      of Fpath.t * Deflate.error
    | `Inflate_file      of Fpath.t * Inflate.error
    | `Delta             of PEnc.Delta.error
    | `Pack_decoder      of RPDec.error
    | `Pack_encoder      of PEnc.error
    | `Pack_info         of PInfo.error
    | `Idx_decoder       of IDec.error
    | `Idx_encoder       of IEnc.error
    | `Integrity         of string
    | `Invalid_hash      of Hash.t
    | `Invalid_reference of Reference.t
    | `Not_found
    | `SystemDir         of FS.error
    | `SystemFile        of FS.error
    | `SystemMap         of FS.error ]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  module Loose: LOOSE
    with type t = Value.t
     and type state = t
     and type error = error
     and module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module FS = FS
  (** The [Loose] module which represents any {i loose} git object
      available in git repository. *)

  module Pack: PACK
    with type t = RPDec.Object.t
     and type value = Value.t
     and type state = t
     and type error = error
     and module Hash = Hash
     and module FS = FS
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec
     and module RPDec := RPDec
  (** The [Pack] module which represents any {i packed} git object
      available in the git repository. *)

  type buffer
  (** The type for buffers. *)

  val default_buffer: unit -> buffer

  val buffer:
    ?ztmp:Cstruct.t ->
    ?dtmp:Cstruct.t ->
    ?raw:Cstruct.t ->
    ?window:Inflate.window ->
    unit -> buffer
  (** Build a buffer to read and write a Git object.

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      If not specified the cstruct are created with a size of 4 MiB.

      Store functions can be used in a concurrency context only if the
      specified buffers are not used by another process. The
      deserialisation functions does not allocate any buffer and uses
      only the specified buffers to construct the OCaml value. *)

  val create :
       ?root:Fpath.t
    -> ?dotgit:Fpath.t
    -> ?compression:int
    -> ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t)
    -> unit -> (t, error) result Lwt.t
  (** [create ?root ?dotgit ?compression ?with_buffer ()] creates a
      new store represented by the path [root] (default is ["."]),
      where the Git objects are located in [dotgit] (default is [root
      / ".git"] and when Git objects are compressed by the [level]
      (default is [4]). If [with_buffer] is not set, use a [Lwt_pool]
      of {!default_buffer} of size 4. *)

  val dotgit: t -> Fpath.t
  (** [dotgit state] is the current [".git"] path used. *)

  val root: t -> Fpath.t
  (** [root state] is the current path of the repository. *)

  val compression: t -> int
  (** [compression state] is the current level of the compression used
      to write a git object. *)

  val mem: t -> Hash.t -> bool Lwt.t
  (** [mem state hash] is true if one object of the current repository
      [state] satisfies the predicate [digest(object) = hash]. *)

  val list: t -> Hash.t list Lwt.t
  (** [list state] is the list of all git objects available in the
      current repository [state]. *)

  val read: t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read state hash] is the Git object with hash [hash] from the
      current repository [state]. It de-serializes the git object to
      an OCaml value. This function needs some buffers, provided [t]'s
      buffer function.

      This function follows the same scheme of allocation of
      {!Loose.read} if the requested git object is a {i loose} git
      object or {!Pack.read} if the requested git object is a {i
      packed} git object.  Otherwise, return an {!error}. *)

  val read_exn: t -> Hash.t -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an
      exception (instead to return a {!result}) if the git object
      requested does not exist or if we catch any other errors. *)

  val write: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write state v] writes as a {b loose} git object [v] in the
      file-system. It serializes and deflates the value to a new
      file. which respect the internal structure of a git repository.

      This function does not allocate any buffer and uses only the
      specified buffers to store the OCaml value to the
      file-system. Then, the OCaml value will be available by [read
      state digest(v)]. Otherwise, return an {!error}:

      {ul
      {- {!FS.File.error} when we can not create a new file in
      the file-system.}
      {- {!Value.E.error} when we can not serialize xor deflate the
      requested git {i loose} object. This kind of error should be
      never happen.}} *)

  val size: t -> Hash.t -> (int64, error) result Lwt.t
  (** [size state hash] is the size of the git object such that
      [digest(object) = hash]. The size is how many byte(s) are needed
      to store the serialized (but not inflated) git object in bytes
      (without the header).

      As {!read}, {!size} can return an {!error}. *)

  val read_inflated: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** [read_inflated state hash] is the non-compressed representaton
      of a Git object in the git repository xs[state]. However, this
      function {b does not} de-serialize the git object and returns
      the kind of the object and the {i raw-data} inflated.
      Precisely, it decodes only the header.

      This function follows the same scheme of allocation of
      {!Loose.read_inflated} if the requested git object is a {i
      loose} git object or {!Pack.read} if the requested git object is
      a {i packed} git object.  Otherwise, return an {!error}. *)

  val write_inflated: t -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This
      function does not verify if the raw data is well-defined (and
      respects the Git format). Then, this function returns the hash
      produced from the kind and the inflated raw to let the user to
      retrieve it.

      If we retrieve any error error while the I/O operations, we {b
      raise} 9by {!Lwt.fail} a [Failure] which describe in a [string]
      the error encountered. *)

  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and
      its bind git object. This list contains all git objects
      available in the current git repository [state]. *)

  val fold:
       t
    -> ('a -> ?name:Fpath.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t)
    -> path:Fpath.t
    -> 'a
    -> Hash.t
    -> 'a Lwt.t
  (** [fold state f ~path acc hash] iters on any git objects reachable
      by the git object [hash] which located in [path] (for example,
      if you iter on a commit, [path] should be ["."] - however, if
      you iter on a tree, [path] should be the directory path
      represented by your tree). For each git objects, we notice the
      path [name] (derived from [path]) if the object is a Blob or a
      Tree, the [length] or the git object (see {!size}), the [hash]
      and the [value].

      If the [hash] points to:

      {ul
      {- {!Value.Blob.t}: [f] is called only one time with the OCaml
      value of the {i blob}.}
      {- {!Value.Tree.t}: [f] is called firstly with the Ocaml value
      of the pointed {i tree} by the hash [hash]. Then, we {i iter}
      (and call [f] for each iteration) in the list of entries of the
      {i tree}. Finally, we retrieve recursively all sub-tree objects
      and do an ascending walk. [f] is never called more than one time
      for each hash.}
      {- {!Value.Commit.t}: [f] is called firstly with the OCaml value
      of the pointed {i commit} by the hash [hash]. Then, it follozs
      recursively all parents of the current commit, Finallym it
      starts a [fold] inside the pointed root {i tree} git object of
      each {i commit} previously retrieved. [f] never called more than
      one time for each hash.}
      {- {!Value.Tag.t}: [f] is called firstly with the OCaml value of
      the pointed {i tag} by the hash [hash]. Then, it follows the git
      object pointed by the {i tag}.}}

      Any retrieved {!error} is skipped. *)

  module Ref: sig

    val mem: t -> Reference.t -> bool Lwt.t
    (** [mem state ref] is [true] iff [ref] exists in [state],
        otherwise it is [false]. *)

    val graph: t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph state] is the graph containing all the references and
        their relationship in the current git repository [state]. *)

    val normalize:
      Hash.t Reference.Map.t -> Reference.head_contents ->
      (Hash.t, error) result Lwt.t
    (** [normalize graph ref] is the final hash pointed by the
        reference [ref]. This function can return an error:

        {ul
        {- [`Invalid_reference ref] if, from the graph [graph], we
        don't find the final pointed hash. This case can appear if the
        graph is not complete or if the link is broken.}} *)

    val list: t -> (Reference.t * Hash.t) list Lwt.t
    (** [list state] is the list of references and their associated
        hash. This function is the same than:

        > graph state >|= Reference.Map.fold (fun x acc -> x :: acc)

        Finally, as for {!graph}, if we encountered any {!error}, we
        make it silent and continue the process. *)


    val remove: t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference]
        from the git repository [state]. *)

    val read: t -> Reference.t ->
      ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read state reference] is the value contained in the reference
        [reference] (available in the git repository [state]). *)

    val write: t -> Reference.t -> Reference.head_contents ->
      (unit, error) result Lwt.t
   (** [write state reference value] writes the value [value] in
       the the [reference] in the git repository [state]. *)

  end

  val clear_caches: t -> unit Lwt.t
  (** [clear_caches t] drops all values stored in the internal caches
      binded with the git repository [t]. *)

  val reset: t -> (unit, error) result Lwt.t
  (** [reset t] removes all things of the git repository [t] and
      ensures it will be empty. *)

  val has_global_watches: bool
  val has_global_checkout: bool
end

module FS
    (H: S.HASH)
    (FS: S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
  : S with module Hash = H
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS
