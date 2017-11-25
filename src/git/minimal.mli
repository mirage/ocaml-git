module type S =
sig
  module Hash
    : S.HASH
      with type Digest.buffer = Cstruct.t
       and type hex = string

  module Path
    : S.PATH

  module Lock
    : S.LOCK

  module Inflate
    : S.INFLATE

  module Deflate
    : S.DEFLATE

  module Value
    : Value.S
      with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module Blob = Blob.Make(Hash)
       and module Commit = Commit.Make(Hash)
       and module Tree = Tree.Make(Hash)
       and module Tag = Tag.Make(Hash)
       and type t = Value.Make(Hash)(Inflate)(Deflate).t

  module Reference
    : Reference.S
      with module Hash = Hash
       and module Path = Path

  type t

  type error

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val create : ?root:Path.t ->
    ?dotgit:Path.t ->
    ?compression:int ->
    unit -> (t, error) result Lwt.t

  val dotgit : t -> Path.t
  (** [dotgit state] returns the current [".git"] path used - eg. the
      default [?dotgit] value of {!create} if the client does not
      notice a specific value. *)

  val root : t -> Path.t
  (** [root state] returns the current path of the repository. eg. the
      default value [?root] value of {!create} if the client does not
      notice a specific value. *)

  val compression : t -> int
  (** [compression state] returns the current level of the compression
      used to write a Git object - eg. the default value
      [?compression] value of {!create} if the client does not notice
      a specific value. *)

  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and
      its bind git object. This list contains all git objects
      available in the current git repository [state]. *)

  val size : t -> Hash.t -> (int64, error) result Lwt.t
  (** [size state hash] returns the size of the git object which
      respects the predicate [digest(object) = hash]. The size is how
      many byte(s) are needed to store the serialized (but not
      inflated) git object in bytes (without the header). *)

  val read : t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read state hash] can retrieve a git object from the current
      repository [state]. It de-serializes the git object to an OCaml
      value. *)

  val read_exn : t -> Hash.t -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an
      exception (instead to return a {!result}) if the git object
      requested does not exist or we catch any others errors. *)

  val exists : t -> Hash.t -> bool Lwt.t
  (** [exists state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. *)

  val list : t -> Hash.t list Lwt.t
  (** [list state] lists all git objects available in the current
      repository [state]. *)

  val write : t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write state v] writes the value [v] in the git repository
      [state]. *)

  val fold :
       t
    -> ('acc -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'acc Lwt.t)
    -> path:Path.t
    -> 'acc
    -> Hash.t
    -> 'acc Lwt.t

  module Pack :
  sig
    type stream = unit -> Cstruct.t option Lwt.t
     (** The stream contains the PACK flow. *)

    module Graph : Map.S with type key = Hash.t

    type nonrec error

    val pp_error : error Fmt.t
    (** Pretty-printer of {!error}. *)

    val from : t -> stream -> (Hash.t * int, error) result Lwt.t
    (** [from git stream] populates the Git repository [git] from the
        PACK flow [stream]. If any error is encountered, any Git
        objects of the PACK flow are not added in the Git
        repository. *)

    val make : t
      -> ?window:[ `Object of int | `Memory of int ]
      -> ?depth:int
      -> Value.t list
      -> (stream * (Crc32.t * int64) Graph.t Lwt_mvar.t, error) result Lwt.t
  end

  module Ref :
  sig
    type nonrec error

    val pp_error : error Fmt.t
    (** Pretty-printer of {!error}. *)

    val list : ?locks:Lock.t -> t -> (Reference.t * Hash.t) list Lwt.t
    (** [list state] returns an associated list between reference and
        its bind hash. *)

    val exists : t -> Reference.t -> bool Lwt.t
    (** [exists state ref] returns [true] iff [ref] exists in [state],
        otherwise returns [false]. *)

    val read : ?locks:Lock.t -> t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read state reference] returns the value contains in the
        reference [reference] (available in the git repository
        [state]). *)

    val write : t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    (** [write state ?locks reference value] writes the value [value]
        in the mutable representation of the [reference] in the git
        repository [state]. The [?lock] avoids the race condition if
        it's specified. *)

    val test_and_set : t ->
         ?locks:Lock.t
      -> Reference.t
      -> test:Reference.head_contents option
      -> set:Reference.head_contents option
      -> (bool, error) result Lwt.t
    (** [test_and_set state ?locks reference ~test ~set] is an atomic
        update of the reference [reference] in the git repository
        [state].

        {ul
        {- If [test = None], we set [reference] to [set] if
        [reference] does not exist.}
        {- If [test = Some v], we set [reference] to [set] only if
        [reference] exists and [reference = v].
        {- If [set = None], we ensure than [reference] will no longer
        exists iff [test] returns [true].}
        {- If [set = Some v], we ensure than [reference] will be equal
        to [v] iff [test] returns [true].}} *)

    val remove : t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference]
        from the git repository [state]. The [?lock] avoids the race
        condition if it's specified. *)
  end

  val reset : ?locks:Lock.t -> t -> (unit, [ `Store of error | `Ref of Ref.error ]) result Lwt.t
  (** [reset ?locks t] removes all things of the git repository [t]
      and ensures it will be empty. *)

  val clear_caches : ?locks:Lock.t -> t -> unit Lwt.t
  (** [clear_caches ?locks t] drops all values stored in the internal
      caches binded with the git repository [t]. *)

  val read_inflated : t -> Hash.t -> ([ `Commit | `Tag | `Blob | `Tree ] * Cstruct.t) option Lwt.t
  val write_inflated : t -> kind:[ `Commit | `Tree | `Blob | `Tag ] -> Cstruct.t -> Hash.t Lwt.t
end
