module type S = sig

  module Hash: S.HASH
  module Lock: S.LOCK
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t

  module Reference: Reference.S with module Hash = Hash

  (** The type of the git repository. *)
  type t

  (** The type error. *)
  type error = private [> `Not_found]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  val create: ?root:Fpath.t ->
    ?dotgit:Fpath.t ->
    ?compression:int ->
    unit -> (t, error) result Lwt.t
  (** [create ?root ?dotgit ?compression ()] creates a new store
      represented by the path [root] (default is ["."]), where the Git
      objects are located in [dotgit] (default is [root / ".git"] and when
      Git objects are compressed by the [level] (default is [4]). *)

  val dotgit: t -> Fpath.t
  (** [dotgit state] returns the current [".git"] path used - eg. the
      default [?dotgit] value of {!create} if the client does not
      notice a specific value. *)

  val root: t -> Fpath.t
  (** [root state] returns the current path of the repository. eg. the
      default value [?root] value of {!create} if the client does not
      notice a specific value. *)

  val compression: t -> int
  (** [compression state] returns the current level of the compression
      used to write a Git object - eg. the default value
      [?compression] value of {!create} if the client does not notice
      a specific value. *)

  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and
      its bind git object. This list contains all git objects
      available in the current git repository [state]. *)

  val size: t -> Hash.t -> (int64, error) result Lwt.t
  (** [size state hash] returns the size of the git object which
      respects the predicate [digest(object) = hash]. The size is how
      many byte(s) are needed to store the serialized (but not
      deflated) git object in bytes (without the header). *)

  val read: t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read state hash] can retrieve a git object from the current
      repository [state]. It de-serializes the git object to an OCaml
      value. *)

  val read_exn: t -> Hash.t -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an
      exception (instead to return a {!result}) if the git object
      requested does not exist or when we catch any others errors. *)

  val mem: t -> Hash.t -> bool Lwt.t
  (** [mem state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. *)

  val list: t -> Hash.t list Lwt.t
  (** [list state] lists all git objects available in the current
      git repository [state]. *)

  val write: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write state v] writes the value [v] in the git repository
      [state]. *)

  val fold :
       t
    -> ('acc -> ?name:Fpath.t -> length:int64 -> Hash.t -> Value.t -> 'acc Lwt.t)
    -> path:Fpath.t
    -> 'acc
    -> Hash.t
    -> 'acc Lwt.t
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

      Any retrieved {!error} is missed. *)

  module Pack:  sig
    type stream = unit -> Cstruct.t option Lwt.t
    (** The stream contains the PACK flow. *)

    module Graph: Map.S with type key = Hash.t

    (** The type error. *)
    type nonrec error

    val pp_error: error Fmt.t
    (** Pretty-printer of {!error}. *)

    val from: t -> stream -> (Hash.t * int, error) result Lwt.t
    (** [from git stream] populates the Git repository [git] from the
        PACK flow [stream]. If any error is encountered, any Git
        objects of the PACK flow are not added in the Git
        repository. *)

    val make: t
      -> ?window:[ `Object of int | `Memory of int ]
      -> ?depth:int
      -> Value.t list
      -> (stream * (Crc32.t * int64) Graph.t Lwt_mvar.t, error) result Lwt.t
    (** [make ?window ?depth values] makes a PACK stream from a list
        of {!Value.t}.

        [?window] specifies the weight of the window while the
        delta-ification. The client can limit the weight by the number
        of objects in the windoz (by default, is 10) or by the weight
        in byte of the window in your memory.

        [depth] (default is [50]) limits the depth of the
        delta-ification.

        Then, the function returns a stream and a protected variable
        which contains a representation of the associated IDX file of
        the PACK stream. This protected variable is available
        ([Lwt_mvar.take]) only {b at the end} of the PACK stream. That
        means, the client needs to consume all of the stream and only
        then he can take the [Graph.t] associated. *)
  end

  module Ref: sig

    (** The type error. *)
    type error = private [> `Not_found]

    val pp_error: error Fmt.t
    (** Pretty-printer of {!error}. *)

    val list: ?locks:Lock.t -> t -> (Reference.t * Hash.t) list Lwt.t
    (** [list state] returns an associated list between reference and
        its bind hash. *)

    val mem: t -> Reference.t -> bool Lwt.t
    (** [mem state ref] returns [true] iff [ref] exists in [state],
        otherwise returns [false]. *)

    val read: ?locks:Lock.t -> t -> Reference.t ->
      ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read state reference] returns the value contains in the
        reference [reference] (available in the git repository
        [state]). *)

    val write: t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents
      -> (unit, error) result Lwt.t
    (** [write state ?locks reference value] writes the value [value]
        in the mutable representation of the [reference] in the git
        repository [state]. The [?lock] avoids the race condition if
        it's specified. *)

    val remove: t -> ?locks:Lock.t -> Reference.t ->
      (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference]
        from the git repository [state]. The [?lock] avoids the race
        condition if it's specified. *)
  end

  val reset: ?locks:Lock.t -> t ->
    (unit, [ `Store of error | `Ref of Ref.error ]) result Lwt.t
  (** [reset ?locks t] removes all things of the git repository [t]
      and ensures it will be empty. *)

  val clear_caches: ?locks:Lock.t -> t -> unit Lwt.t
  (** [clear_caches ?locks t] drops all values stored in the internal
      caches binded with the git repository [t]. *)

  val read_inflated: t -> Hash.t ->
    ([ `Commit | `Tag | `Blob | `Tree ] * Cstruct.t) option Lwt.t
  (** [read_inflated state hash] returns the inflated git object which
      respect the predicate [digest(value) = hash]. We return the kind of
      the object and the inflated value as a {!Cstruct.t} (which the
      client can take the ownership). *)

  val write_inflated: t -> kind:[ `Commit | `Tree | `Blob | `Tag ] ->
    Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This
      function does not verify if the raw data is well-defined (and
      respects the Git format). Then, this function returns the hash
      produced from the kind and the inflated raw to let the user to
      retrieve it. *)


  (** {1 Backend Features} *)

  val has_global_watches: bool
  val has_global_checkout: bool
end
