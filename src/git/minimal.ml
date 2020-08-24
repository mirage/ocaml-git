module type S = sig
  type t
  (** The type of the git repository. *)

  (** The type error. *)
  type hash

  type error =
    private
    [> `Not_found of hash
    | `Reference_not_found of Reference.t
    | `Msg of string ]

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  module Hash : S.HASH with type t = hash

  module Value : Value.S with type hash = hash

  module Reference : Reference.S with type hash = hash

  val dotgit : t -> Fpath.t
  (** [dotgit state] returns the current [".git"] path used - eg. the default
      [?dotgit] value of {!create} if the client does not notice a specific
      value. *)

  val root : t -> Fpath.t
  (** [root state] returns the current path of the repository. eg. the default
      value [?root] value of {!create} if the client does not notice a specific
      value. *)

  (*
  val compression : t -> int
  (** [compression state] returns the current level of the compression used to
      write a Git object - eg. the default value [?compression] value of
      {!create} if the client does not notice a specific value. *)
  *)

  val contents : t -> (hash * Value.t) list Lwt.t
  (** [contents state] returns an associated list between the hash and its bind
      git object. This list contains all git objects available in the current
      git repository [state]. *)

  val size : t -> hash -> (int64, error) result Lwt.t
  (** [size state hash] returns the size of the git object which respects the
      predicate [digest(object) = hash]. The size is how many byte(s) are
      needed to store the serialized (but not deflated) git object in bytes
      (without the header). *)

  val read : t -> hash -> (Value.t, error) result Lwt.t
  (** [read state hash] can retrieve a git object from the current repository
      [state]. It de-serializes the git object to an OCaml value. *)

  val read_exn : t -> hash -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an exception
      (instead to return a {!result}) if the git object requested does not
      exist or when we catch any others errors. *)

  val mem : t -> hash -> bool Lwt.t
  (** [mem state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. *)

  (** [list state] lists all git objects available in the current git
      repository [state]. *)
  val list : t -> hash list Lwt.t

  val write : t -> Value.t -> (hash * int, error) result Lwt.t
  (** [write state v] writes the value [v] in the git repository [state]. *)

  val batch_write :
    t ->
    hash ->
    pck:(unit -> string option Lwt.t) ->
    idx:(unit -> string option Lwt.t) ->
    (unit, error) result Lwt.t

  val fold :
    t ->
    ('acc -> ?name:Fpath.t -> length:int64 -> hash -> Value.t -> 'acc Lwt.t) ->
    path:Fpath.t ->
    'acc ->
    hash ->
    'acc Lwt.t
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

      Any retrieved {!error} is missed. *)

  val iter : t -> (hash -> Value.t -> unit Lwt.t) -> hash -> unit Lwt.t

  module Ref : sig
    val list : t -> (Reference.t * hash) list Lwt.t
    (** [list state] returns an associated list between reference and its bind
        hash. *)

    val mem : t -> Reference.t -> bool Lwt.t
    (** [mem state ref] returns [true] iff [ref] exists in [state], otherwise
        returns [false]. *)

    val read : t -> Reference.t -> (Reference.contents, error) result Lwt.t
    (** [read state reference] returns the value contains in the reference
        [reference] (available in the git repository [state]). *)

    val resolve : t -> Reference.t -> (hash, error) result Lwt.t
    (** [resolve state reference] returns endpoint of [reference] (available in
        the git repository [state]). *)

    val write :
      t -> Reference.t -> Reference.contents -> (unit, error) result Lwt.t
    (** [write state reference value] writes the value [value] in the mutable
        representation of the [reference] in the git repository [state]. *)

    val remove : t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference] from the git
        repository [state]. *)
  end

  val reset : t -> (unit, error) result Lwt.t
  (** [reset t] removes all things of the git repository [t] and ensures it
      will be empty. *)


  val read_inflated :
    t -> hash -> ([ `Commit | `Tag | `Blob | `Tree ] * Cstruct.t) option Lwt.t
  (** [read_inflated state hash] returns the inflated git object which respect
      the predicate [digest(value) = hash]. We return the kind of the object
      and the inflated value as a {!Cstruct.t} (which the client can take the
      ownership). *)

  val write_inflated :
    t -> kind:[ `Commit | `Tree | `Blob | `Tag ] -> Cstruct.t -> hash Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This function
      does not verify if the raw data is well-defined (and respects the Git
      format). Then, this function returns the hash produced from the kind and
      the inflated raw to let the user to retrieve it. *)

  (** {1 Backend Features} *)

  val has_global_watches : bool

  val has_global_checkout : bool
end
