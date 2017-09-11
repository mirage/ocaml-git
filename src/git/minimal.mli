module type S =
sig
  module Hash
    : Ihash.S

  module Path
    : Path.S

  module Lock
    : Lock.S

  module Inflate
    : S.INFLATE

  module Deflate
    : S.DEFLATE

  module Value
    : Value.S
      with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate

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
      default [?dotgit] value of {!create} if the client does not notice a
      specific value. *)

  val root : t -> Path.t
  (** [root state] returns the current path of the repository. eg. the
      default value [?root] value of {!create} if the client does not
      notice a specific value. *)

  val compression : t -> int
  (** [compression state] returns the current level of the compression
      used to write a Git object - eg. the default value [?compression]
      value of {!create} if the client does not notice a specific
      value. *)

  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and
      its bind git object. This list contains all git objects available in
      the current git repository [state]. *)

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

  module Ref :
  sig
    type nonrec error

    val pp_error : error Fmt.t
    (** Pretty-printer of {!error}. *)

    val list : t -> (Reference.t * Hash.t) list Lwt.t
    (** [list state] returns an associated list between reference and its
        bind hash. *)

    val read : t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read state reference] returns the value contains in the
        reference [reference] (available in the git repository
        [state]). *)

    val write : t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    (** [write state ?lock reference value] writes the value [value]
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
end
