(** Decoder of a PACK file.

    Along this module, the type [('a, 's) io] with a ['s scheduler] is needed
    for some operations (which use a {i syscall}). To be able to use them, the
    use must create a new type ['s] which represents the scheduler. To do that
    with LWT for example:

    {[
      module Lwt_scheduler = Make (Lwt)

      let scheduler =
        let open Lwt.Infix in
        let open Lwt_scheduler in
        {
          bind = (fun x f -> inj (x >>= fun x -> prj (f x)));
          return = (fun x -> inj x);
        }
    ]}

    The produced module has 2 functions [inj] and [prj] to pass from or to an
    LWT value. The user can use these functions like:

    {[
      let fiber =
        let ( >>= ) = scheduler.bind in
        let return = scheduler.return in

        weight_of_offset scheduler ~map t ~weight:null 0L >>= fun weight ->
        let raw = make_raw ~weight in
        of_offset scheduler ~map t raw ~cursor:0L in
      prj fiber ;;
      - : (Carton.v, [> error ]) Lwt.t = <abstr>
    ]} *)

open Sigs

(** {1 {i Memoization} of [map].}

    Instead to systematically call [map] to load {i memory-page}, we {i memoize}
    the call by a simple internal table of {i weak pointers}. *)
module W : sig
  type 'fd t

  and slice = { offset : int64; length : int; payload : Bigstringaf.t }

  and ('fd, 's) map = 'fd -> pos:int64 -> int -> (Bigstringaf.t, 's) io

  val length : int64
  val reset : 'fd t -> unit
  val make : 'fd -> 'fd t

  val load :
    's scheduler -> map:('fd, 's) map -> 'fd t -> int64 -> (slice option, 's) io
end

type weight = private int
(** Type of [weight]. [weight] is {b not} {i length} of object but bytes needed
    to extract it. *)

val null : weight
(** {i zero} weight. *)

val weight_of_int_exn : int -> weight
(** [weight_of_int_exn n] is the weight of [n]. *)

type ('fd, 's) read = 'fd -> bytes -> off:int -> len:int -> (int, 's) io
(** Type of read {i syscall}. *)

module Idx = Idx

(** {1 First-pass of a PACK file.}

    From a {i stream}, it is possible to infer information needed then to apply
    a second analyse to extract all objects of a the given PACK file. From Git,
    this {i pass} appears when the client [clone]/[fetch] and the program counts
    how many objects the PACK file has.

    {[
      $ git clone ...
      remote: Enumerating objects: 105, done.
      remote: Counting objects: 100% (105/105), done.
      remote: Compressing objects: 100% (81/81), done.
      remote: Total 305 (delta 41), reused 75 (delta 23), pack-reused 200
      Receiving objects: 100% (305/305), 104.46 KiB | 0 bytes/s, done. # first pass
    ]} *)
module Fp (Uid : UID) : sig
  type optint = Optint.t

  type kind =
    | Base of [ `A | `B | `C | `D ]
    | Ofs of { sub : int; source : weight; target : weight }
    | Ref of { ptr : Uid.t; source : weight; target : weight }
        (** Type of PACK objects. *)

  type entry = {
    offset : int64;  (** Absolute offset into the given PACK file. *)
    kind : kind;  (** Kind of the object. *)
    size : weight;  (** Length of the inflated object. *)
    consumed : int;
        (** Length of the deflated object (as it is into the PACK file). *)
    crc : optint;
        (** Check-sum of the entry (header plus the deflated object). *)
  }
  (** Type of a PACK entry. *)

  val check_header :
    's scheduler -> ('fd, 's) read -> 'fd -> (int * string * int, 's) io

  type decoder
  (** The type for decoders. *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
      provide input with {!src}. *)

  type decode =
    [ `Await of decoder
    | `Peek of decoder
    | `Entry of entry * decoder
    | `End of Uid.t
    | `Malformed of string ]

  type header = Consumed of Bigstringaf.t | None

  val decoder : o:Bigstringaf.t -> allocate:(int -> De.window) -> src -> decoder
  val decode : decoder -> decode
  val number : decoder -> int
  val version : decoder -> int
  val count : decoder -> int
  val src_rem : decoder -> int
  val src : decoder -> Bigstringaf.t -> int -> int -> decoder
end

type ('fd, 'uid) t
(** Type of state used to access to any objects into a [Carton] file. *)

(**/*)

val header_of_entry :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  int64 ->
  W.slice ->
  (int * int * int * W.slice, 's) io

(**/*)

val with_z : Bigstringaf.t -> ('fd, 'uid) t -> ('fd, 'uid) t
(** [with_z new t] replaces the used temporary buffer by [t] by [new]. Indeed,
    when the user wants to extract an object, the internal temporary buffer is
    used to store the inflated object. By this way, a parallel/concurrent
    computation of 2 extractions with the same [t] is unsafe.

    So, this function allows the user to create a {i new} [t] with a new
    dedicated temporary buffer (physically different from the old one) to be
    able to start a parallel/concurrent process. *)

val with_w : 'fd W.t -> ('fd, 'uid) t -> ('fd, 'uid) t
(** [with_w w t] replaces the used table {!W.t} by [w]. As {!with_z}, the
    purpose of this function is to be able to {i parallelize} multiple {!t}. *)

val with_allocate :
  allocate:(int -> De.window) -> ('fd, 'uid) t -> ('fd, 'uid) t
(** [with_allocate allocate t] replaces the function to allocate the window
    needed to inflate objects by [allocate]. As {!with_z}, the purpose of this
    function is to be able to {i parallelize} multiple [t]. *)

val fd : ('fd, 'uid) t -> 'fd
(** [fd t] returns the underlying used [fd] resource to map memory parts of it.
    On [Unix], even if a mapped memory part can live if [fd] is the close, the
    resource should be open as long as the user extracts objects. *)

type raw
(** Type of a [Carton] object as is into a [Carton] file. *)

val make_raw : weight:weight -> raw
(** [make_raw ~weight] allocates a raw. *)

val weight_of_raw : raw -> weight

type v
(** Type of values. *)

val v : kind:kind -> ?depth:int -> Bigstringaf.t -> v
(** [v ~kind ?depth raw] is a value [raw] typed by [kind]. [?depth] is an
    optional value to know at which depth the object exists into the PACK file
    it came from (default to [1]). *)

val kind : v -> kind
(** [kind v] is the type of the object [v]. *)

val raw : v -> Bigstringaf.t
(** [raw v] is the contents of the object [v].

    {b Note.} The {!Bigstringaf.t} can be larger (and contain extra contents)
    than [len v] (see {!len}). The user should {!Bigstringaf.sub} it with the
    real length of the object. *)

val len : v -> int
(** [len v] is the length of the object [v]. *)

val depth : v -> int
(** [depth v] is the depth of the object into the PACK file it came from. *)

val make :
  'fd ->
  z:Zl.bigstring ->
  allocate:(int -> Zl.window) ->
  uid_ln:int ->
  uid_rw:(string -> 'uid) ->
  ('uid -> int64) ->
  ('fd, 'uid) t
(** [make fd ~z ~allocate ~uid_ln ~uid_rw where] returns a state associated to
    [fd] which is the user-defined representation of a [Carton] file. Some
    informations are needed:

    - [z] is an underlying buffer used to {i inflate} an object.
    - [allocate] is an {i allocator} of underlying {i window} used to
      {i inflate} an object.
    - [uid_ln] is the length of {i raw} representation of user-defined {i uid}.
    - [uid_rw] is the {i cast-function} from a string to user-defined {i uid}.
    - [where] is the function to associate an {i uid} to an {i offset} into the
      associated [Carton] file.

    Each argument depends on what the user wants. For example, if [t] is used by
    {!Verify.verify}, [allocate] {b must} be thread-safe according to {!IO}.
    [where] is not used by {!Verify.verify}. [uid_ln] and [uid_rw] depends on
    the [Carton] file associated by [fd]. Each functions available below
    describes precisely what they do on [t]. *)

(** {3 Weight of object.}

    Before to extract an object, we must know resources needed to extract it.
    [weight_of_offset]/[weight_of_uid] do an simple analyse and return the
    larger length needed to store the requested object such as:

    {[
      weight_of_offset unix ~map t ~weight:null 0L >>= fun weight ->
      assert ((null :> int) <= (weight :> int)) ;
      Fmt.epr "Object at %08Lx needs %d byte(s).\n%!" 0L (weight :> int) ;
      let resource = make_raw ~weight in
      ...
    ]}

    An object can need an other object (see [OBJ_OFS_DELTA] and
    [OBJ_REF_DELTA]). In this case, the resource needed must be larger/enough to
    store both objects. So the analyse is recursive over the {i delta-chain}.

    {b Note.} If the given PACK file represented by [t] is bad, [Cycle] is
    raised. It means that an object A refers to an object B which refers to our
    last object A.

    {b Note.} This process is not {i tail-rec} and discover at each step if it
    needs to continue the {i delta-chain} or not. *)

exception Cycle

val weight_of_offset :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  weight:weight ->
  ?visited:int64 list ->
  int64 ->
  (weight, 's) io
(** [weight_of_offset sched ~map t ~weight offset] returns the [weight] of the
    given object available at [offset] into [t]. This function assumes:

    {[
      weight_of_offset sched ~map t ~weight:a offset >>= fun b ->
      assert ((a :> int) <= (b :> int))
    ]}

    {b Note.} This function can try to partially inflate objects. So, this
    function can use internal buffers and it is not {i thread-safe}.

    {b Note.} This function can try to {i look-up} an other object if it
    extracts an [OBJ_REF_DELTA] object. However, if we suppose that we process a
    PACKv2, an [OBJ_REF_DELTA] {i usually} points to an external object (see
    {i thin}-pack). *)

val weight_of_uid :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  weight:weight ->
  ?visited:int64 list ->
  'uid ->
  (weight, 's) io
(** [weight_of_offset sched ~map t ~weight uid] returns the [weight] of the
    given object identified by [uid] into [t]. This function assumes the same
    assumption as {!weight_of_offset}.

    {b Note.} As {!weight_of_offset}, this function can inflate objects and use
    internal buffers and it is not {i thread-safe}.

    {b Note.} Despite {!weight_of_offset}, this function {b look-up} the object
    from the given reference. *)

(** {3 Value of object.} *)

val of_offset :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  raw ->
  cursor:int64 ->
  (v, 's) io
(** [of_offset sched ~map raw ~cursor] is the object at the offset [cursor] into
    [t]. The function is not {i tail-recursive}. It discovers at each step if
    the object depends on another one (see [OBJ_REF_DELTA] or [OBJ_OFS_DELTA]).

    {b Note.} This function does not allocate larges resources (or, at least,
    only the given [allocate] function to {!t} is able to allocate a large
    resource). [raw] (which should be created with the associated {!weight}
    given by {!weight_of_offset}) is enough to extract the object. *)

val of_uid :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  raw ->
  'uid ->
  (v, 's) io
(** As {!of_offset}, [of_uid sched ~map raw uid] is the object identified by
    [uid] into [t]. *)

(** {3 Path of object.}

    Due to the fact that {!of_offset}/{!of_uid} are not {i tail-rec}, an other
    solution exists to extract an object from the PACK file. However, this
    solution requires a {i meta-data} {!path} to be able to extract an object.

    A {!path} is the {i delta-chain} of the object. It assumes that a
    {i delta-chain} can not be larger than [60] (see Git assumptions). From it,
    the way to construct an object is well-know and the step to discover if an
    object depends on an other one is deleted - and we ensure that the
    reconstruction is bound over our {!path}.

    This solution fits well when we want to {i memoize} the extraction. *)

type path
(** The type of paths. *)

val path_to_list : path -> int64 list
(** [path_to_list path] returns the {i delta-chain} of the given [path]. *)

val kind_of_path : path -> [ `A | `B | `C | `D ]
(** [kind_of_path path] returns the kind of the object associated to the given
    [path]. An assumption exists about PACK format, a {i delta-chain} refers to
    several objects which must have the same type/kind. *)

val path_of_offset :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  cursor:int64 ->
  (path, 's) io
(** [path_of_offset sched ~map t ~cursor] is that {!path} of the given object
    available at [cursor].

    {b Note.} This function can try to partially inflate objects. So, this
    function can use internal buffers and it is not {i thread-safe}.

    {b Note.} This function can try to {i look-up} an other object if it
    extracts an [OBJ_REF_DELTA] object. However, if we suppose that we process a
    PACKv2, an [OBJ_REF_DELTA] {i usually} points to an external object (see
    {i thin}-pack). *)

val path_of_uid :
  's scheduler -> map:('fd, 's) W.map -> ('fd, 'uid) t -> 'uid -> (path, 's) io
(** [path_of_uid sched ~map t uid] is the {!path} of the given object identified
    by [uid] into [t].

    {b Note.} As {!weight_of_offset}, this function can inflate objects and use
    internal buffers and it is not {i thread-safe}.

    {b Note.} Despite {!weight_of_offset}, this function {b look-up} the object
    from the given reference. *)

val of_offset_with_path :
  's scheduler ->
  map:('fd, 's) W.map ->
  ('fd, 'uid) t ->
  path:path ->
  raw ->
  cursor:int64 ->
  (v, 's) io
(** [of_offset_with_path sched ~map t ~path raw ~cursor] is the object available
    at [cursor] into [t]. This function is {i tail-recursive} and bound to the
    given [path]. *)

(** {3 Uid of object.}

    Unique identifier of objects is a user-defined type which is not described
    by the format of the PACK file. By this fact, the way to {i digest} an
    object is at the user's discretion. For example, Git {i prepends} the value
    by an header such as:

    {[
      let digest v =
        let kind = match kind v with
          | `A -> "commit"
          | `B -> "tree"
          | `C -> "blob"
          | `D -> "tag" in
        let hdr = Fmt.strf "%s %d\000" kind (len v) int
        let ctx = Digest.empty in
        feed_string ctx hdr ;
        feed_bigstring ctx (Bigstringaf.sub (raw v) 0 (len v)) ;
        finalize ctx
    ]}

    Of course, the user can decide how to digest a value (see {!digest}).
    However, 2 objects with the same contents but different types should have
    different unique identifier. *)

type 'uid digest = kind:kind -> ?off:int -> ?len:int -> Bigstringaf.t -> 'uid

val uid_of_offset :
  's scheduler ->
  map:('fd, 's) W.map ->
  digest:'uid digest ->
  ('fd, 'uid) t ->
  raw ->
  cursor:int64 ->
  (kind * 'uid, 's) io

val uid_of_offset_with_source :
  's scheduler ->
  map:('fd, 's) W.map ->
  digest:'uid digest ->
  ('fd, 'uid) t ->
  kind:kind ->
  raw ->
  depth:int ->
  cursor:int64 ->
  ('uid, 's) io

type 'uid children = cursor:int64 -> uid:'uid -> int64 list
type where = cursor:int64 -> int

type 'uid oracle = {
  digest : 'uid digest;
  children : 'uid children;
  where : where;
  weight : cursor:int64 -> weight;
}

(** {3 Verify.}

    When the user get a PACK file, he must generate an IDX file (see {!Idx})
    from it - to be able to look-up objects from their [uid]. [Verify] is a
    process which try to create an OCaml representation of the IDX file. This
    process requires some information (see {!oracle}) which can be collected by
    a first analyse (see {!Fp}). Then, the process wants to take the opportunity
    to {i parallelize} extraction (depending on the {!IO} implementation). *)

module Verify
    (Uid : UID)
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s) : sig
  val s : Scheduler.t scheduler

  type status

  val pp : Format.formatter -> status -> unit
  val is_resolved : status -> bool
  val uid_of_status : status -> Uid.t
  val kind_of_status : status -> kind
  val depth_of_status : status -> int
  val source_of_status : status -> Uid.t option
  val offset_of_status : status -> int64
  val unresolved_base : cursor:int64 -> status
  val unresolved_node : status

  val verify :
    threads:int ->
    map:('fd, Scheduler.t) W.map ->
    oracle:Uid.t oracle ->
    ('fd, Uid.t) t ->
    matrix:status array ->
    unit IO.t
end

module Ip
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Uid : UID) : sig
  val iter :
    threads:'a list ->
    f:('a -> uid:Uid.t -> offset:int64 -> crc:Idx.optint -> unit IO.t) ->
    Uid.t Idx.idx ->
    unit IO.t
end
