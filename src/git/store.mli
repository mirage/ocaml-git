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

    This implementation is more complete than the memory back-end because
    firstly, Git was think to be use on a file-system. Then, because for each
    operations, we let the client to control the memory consumption.

    So we provide a more powerful API which let the user to notice aready
    allocated buffers outside this scope and process some I/O operations on
    pools. *)


  type t

  type error

module type S = sig
  type t
  (** The type of the git repository. *)

  module FS : S.FS
  (** The [FS] module used to make the implementation. *)

  module Hash : S.HASH
  (** The [Digest] module used to make the implementation. *)

  module Inflate : S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate : S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  (** The Value module, which represents the Git object. *)
  module Value :
    Value.S
      with module Hash := Hash
       and module Inflate := Inflate
       and module Deflate := Deflate
       and module Blob = Blob.Make(Hash)
       and module Commit = Commit.Make(Hash)
       and module Tree = Tree.Make(Hash)
       and module Tag = Tag.Make(Hash)
       and type t = Value.Make(Hash)(Inflate)(Deflate).t

  (** The Reference module, which represents the Git reference. *)
  module Reference : Reference.IO with module Hash := Hash and module FS := FS

  module HDec : Unpack.H with module Hash := Hash

  module PDec :
    Unpack.P
      with module Hash := Hash
       and module Inflate := Inflate
       and module Hunk := HDec

  module RPDec :
    Unpack.D
      with module Hash := Hash
       and module Inflate := Inflate
       and module Hunk := HDec
       and module Pack := PDec
       and module Mapper := FS.Mapper

  module PEnc : Pack.P with module Hash := Hash and module Deflate := Deflate
  module IDec : Index_pack.LAZY with module Hash := Hash
  module IEnc : Index_pack.ENCODER with module Hash := Hash

  module PInfo :
    Pack_info.S
      with module Hash := Hash
       and module Inflate := Inflate
       and module HDec := HDec
       and module PDec := PDec

  module Packed_refs :
    Packed_refs.S with module Hash := Hash and module FS := FS

  type kind = [ `Commit | `Tree | `Tag | `Blob ]
  (** Kind of the {i packed} Git object. *)

  type error =
    [ `Delta of PEnc.Delta.error
    | `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | `Invalid_hash of Hash.t
    | `Invalid_reference of Reference.t
    | Error.Decoder.t
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Deflate.error Error.Def.t
    | Error.not_found ]
  (** The type error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** The [Loose] module which represents any {i loose} git object available in
      git repository. *)
  module Loose :
    LOOSE
      with type t = Value.t
       and type state = t
       and type error = error
       and module Hash := Hash
       and module Inflate := Inflate
       and module Deflate := Deflate
       and module FS := FS

  (** The [Pack] module which represents any {i packed} git object available in
      the git repository. *)
  module Pack :
    PACK
      with type t = RPDec.Object.t
       and type value = Value.t
       and type state = t
       and type error = error
       and module Hash := Hash
       and module FS := FS
       and module Inflate := Inflate
       and module HDec := HDec
       and module PDec := PDec
       and module RPDec := RPDec

  type buffer
  (** The type for buffers. *)

  val default_buffer : unit -> buffer

  val buffer :
    ?ztmp:Cstruct.t ->
    ?etmp:Cstruct.t ->
    ?dtmp:Cstruct.t ->
    ?raw:Cstruct.t ->
    ?window:Inflate.window ->
    unit ->
    buffer
  (** Build a buffer to read and write a Git object.

      {ul

      {- [window] is a buffer used by the {!Inflate} module}

      {- [ztmp] is a buffer used to store the inflated flow}

      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
     keep it for an alteration)}

      {- [raw] is a buffer used to store the input flow}}

      If not specified the cstruct are created with a size of 4 MiB.

      Store functions can be used in a concurrency context only if the specified
     buffers are not used by another process. The deserialisation functions does
     not allocate any buffer and uses only the specified buffers to construct
     the OCaml value. *)

  val dotgit : t -> Fpath.t
  (** [dotgit state] is the current [".git"] path used. *)

  val root : t -> Fpath.t
  (** [root state] is the current path of the repository. *)

  val compression : t -> int
  (** [compression state] is the current level of the compression used to write
      a git object. *)

  val mem : t -> Hash.t -> bool Lwt.t
  (** [mem state hash] is true if one object of the current repository [state]
      satisfies the predicate [digest(object) = hash]. *)

  val list : t -> Hash.t list Lwt.t
  (** [list state] is the list of all git objects available in the current
      repository [state]. *)

  val read : t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read state hash] is the Git object with hash [hash] from the current
      repository [state]. It de-serializes the git object to an OCaml value.
      This function needs some buffers, provided [t]'s buffer function.

      This function follows the same scheme of allocation of {!Loose.read} if
      the requested git object is a {i loose} git object or {!Pack.read} if the
      requested git object is a {i packed} git object. Otherwise, return an
      {!error}. *)

  val read_exn : t -> Hash.t -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an exception
      (instead to return a {!result}) if the git object requested does not
      exist or if we catch any other errors. *)

  val write : t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write state v] writes as a {b loose} git object [v] in the file-system.
     It serializes and deflates the value to a new file. which respect the
     internal structure of a git repository.

      This function does not allocate any buffer and uses only the specified
     buffers to store the OCaml value to the file-system. Then, the OCaml value
     will be available by [read state digest(v)]. Otherwise, return an {!error}:

      {ul

      {- {!FS.File.error} when we can not create a new file in the file-system.}

      {- {!Value.E.error} when we can not serialize xor deflate the requested
     git {i loose} object. This kind of error should be never happen.}} *)

  val size : t -> Hash.t -> (int64, error) result Lwt.t
  (** [size state hash] is the size of the git object such that [digest(object)
      = hash]. The size is how many byte(s) are needed to store the serialized
      (but not inflated) git object in bytes (without the header).

      As {!read}, {!size} can return an {!error}. *)

  val read_inflated : t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** [read_inflated state hash] is the non-compressed representaton of a Git
      object in the git repository xs[state]. However, this function {b does
      not} de-serialize the git object and returns the kind of the object and
      the {i raw-data} inflated. Precisely, it decodes only the header.

      This function follows the same scheme of allocation of
      {!Loose.read_inflated} if the requested git object is a {i loose} git
      object or {!Pack.read} if the requested git object is a {i packed} git
      object. Otherwise, return an {!error}. *)

  val write_inflated : t -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This function
      does not verify if the raw data is well-defined (and respects the Git
      format). Then, this function returns the hash produced from the kind and
      the inflated raw to let the user to retrieve it.

      If we retrieve any error error while the I/O operations, we {b raise} 9by
      {!Lwt.fail} a [Failure] which describe in a [string] the error
      encountered. *)

  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and its bind
      git object. This list contains all git objects available in the current
      git repository [state]. *)

  val fold :
    t ->
    ('a -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t) ->
    path:Path.t ->
    'a ->
    Hash.t ->
    'a Lwt.t
  (** [fold state f ~path acc hash] iters on any git objects reachable by the
     git object [hash] which located in [path] (for example, if you iter on a
     commit, [path] should be ["."] - however, if you iter on a tree, [path]
     should be the directory path represented by your tree). For each git
     objects, we notice the path [name] (derived from [path]) if the object is a
     Blob or a Tree, the [length] or the git object (see {!size}), the [hash]
     and the [value].

      If the [hash] points to:

      {ul

      {- {!Value.Blob.t}: [f] is called only one time with the OCaml value of
     the {i blob}.}

      {- {!Value.Tree.t}: [f] is called firstly with the Ocaml value of the
     pointed {i tree} by the hash [hash]. Then, we {i iter} (and call [f] for
     each iteration) in the list of entries of the {i tree}. Finally, we
     retrieve recursively all sub-tree objects and do an ascending walk. [f] is
     never called more than one time for each hash.}

      {- {!Value.Commit.t}: [f] is called firstly with the OCaml value of the
     pointed {i commit} by the hash [hash]. Then, it follozs recursively all
     parents of the current commit, Finallym it starts a [fold] inside the
     pointed root {i tree} git object of each {i commit} previously retrieved.
     [f] never called more than one time for each hash.}

      {- {!Value.Tag.t}: [f] is called firstly with the OCaml value of the
     pointed {i tag} by the hash [hash]. Then, it follows the git object pointed
     by the {i tag}.}}

      Any retrieved {!error} is skipped. *)

  val iter : t -> (Hash.t -> Value.t -> unit Lwt.t) -> Hash.t -> unit Lwt.t

  module Ref : sig
    val mem : t -> Reference.t -> bool Lwt.t
    (** [mem state ref] is [true] iff [ref] exists in [state], otherwise it is
        [false]. *)

    val graph : t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph state] is the graph containing all the references and their
        relationship in the current git repository [state]. *)

    val normalize :
      Hash.t Reference.Map.t ->
      Reference.head_contents ->
      (Hash.t, error) result Lwt.t
    (** [normalize graph ref] is the final hash pointed by the reference [ref].
       This function can return an error:

        {ul

        {- [`Invalid_reference ref] if, from the graph [graph], we don't find
       the final pointed hash. This case can appear if the graph is not complete
       or if the link is broken.}} *)

    val list : t -> (Reference.t * Hash.t) list Lwt.t
    (** [list state] is the list of references and their associated hash. This
        function is the same than:

        > graph state >|= Reference.Map.fold (fun x acc -> x :: acc)

        Finally, as for {!graph}, if we encountered any {!error}, we make it
        silent and continue the process. *)

    val remove : t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference] from the git
        repository [state]. *)

    val read : t -> Reference.t -> (Reference.head_contents, error) result Lwt.t
    (** [read state reference] is the value contained in the reference
        [reference] (available in the git repository [state]). *)

    val resolve : t -> Reference.t -> (Hash.t, error) result Lwt.t

    val write :
      t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    (** [write state reference value] writes the value [value] in the the
        [reference] in the git repository [state]. *)
  end

  val clear_caches : t -> unit Lwt.t
  (** [clear_caches t] drops all values stored in the internal caches binded
      with the git repository [t]. *)

  val reset : t -> (unit, error) result Lwt.t
  (** [reset t] removes all things of the git repository [t] and ensures it
      will be empty. *)

  val has_global_watches : bool
  val has_global_checkout : bool
end

module Make
    (H : Digestif.S)
    (FS : S.FS)
    (Inflate : S.INFLATE)
    (Deflate : S.DEFLATE) : sig
  include
    S
      with module Hash = Hash.Make(H)
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS

  val v :
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t) ->
    FS.t ->
    Fpath.t ->
    (t, error) result Lwt.t
  (** [create ?dotgit ?compression ?buffer fs root] creates a new store
      represented by the path [root] (default is ["."]), where the Git objects
      are located in [dotgit] (default is [root / ".git"] and when Git objects
      are compressed by the [level] (default is [4]). If [buffer] is not set,
      use a [Lwt_pool] of {!default_buffer} of size 4. [fs] is the storage
      backend state. *)
end
