type lwt

val lwt : lwt Carton.scheduler
external inj : 'a Lwt.t -> ('a, lwt) Carton.io = "%identity"
external prj : ('a, lwt) Carton.io -> 'a Lwt.t = "%identity"

module Scheduler : Carton.SCHEDULER with type +'a s = 'a Lwt.t and type t = lwt

module Dec : sig
  module W : sig
    type 'fd t

    and slice = Carton.Dec.W.slice = {
      offset : int64;
      length : int;
      payload : Bigstringaf.t;
    }

    and 'fd map = 'fd -> pos:int64 -> int -> Bigstringaf.t

    val make : 'fd -> 'fd t
  end

  type weight = Carton.Dec.weight
  type 'fd read = 'fd -> bytes -> off:int -> len:int -> int Lwt.t

  module Idx = Carton.Dec.Idx

  module Fp (Uid : Carton.UID) : sig
    type optint = Optint.t

    type kind =
      | Base of [ `A | `B | `C | `D ]
      | Ofs of { sub : int; source : weight; target : weight }
      | Ref of { ptr : Uid.t; source : weight; target : weight }

    type entry = {
      offset : int64;
      kind : kind;
      size : weight;
      consumed : int;
      crc : optint;
    }

    val check_header : 'fd read -> 'fd -> (int * string * int) Lwt.t

    type decoder
    type src = [ `Channel of in_channel | `String of string | `Manual ]

    type decode =
      [ `Await of decoder
      | `Peek of decoder
      | `Entry of entry * decoder
      | `End of Uid.t
      | `Malformed of string ]

    type header = Consumed of Bigstringaf.t | None

    val decoder :
      o:Bigstringaf.t -> allocate:(int -> De.window) -> src -> decoder

    val decode : decoder -> decode
    val number : decoder -> int
    val version : decoder -> int
    val count : decoder -> int
    val src_rem : decoder -> int
    val src : decoder -> Bigstringaf.t -> int -> int -> decoder
  end

  type ('fd, 'uid) t = ('fd, 'uid) Carton.Dec.t

  val with_z : Bigstringaf.t -> ('fd, 'uid) t -> ('fd, 'uid) t
  val with_w : 'fd W.t -> ('fd, 'uid) t -> ('fd, 'uid) t

  val with_allocate :
    allocate:(int -> De.window) -> ('fd, 'uid) t -> ('fd, 'uid) t

  val fd : ('fd, 'uid) t -> 'fd

  type raw = Carton.Dec.raw

  val make_raw : weight:weight -> raw

  type v = Carton.Dec.v

  val v : kind:Carton.kind -> ?depth:int -> Bigstringaf.t -> v
  val kind : v -> Carton.kind
  val raw : v -> Bigstringaf.t
  val len : v -> int
  val depth : v -> int

  val make :
    'fd ->
    z:Zl.bigstring ->
    allocate:(int -> Zl.window) ->
    uid_ln:int ->
    uid_rw:(string -> 'uid) ->
    ('uid -> int64) ->
    ('fd, 'uid) t

  val weight_of_offset :
    map:'fd W.map -> ('fd, 'uid) t -> weight:weight -> int64 -> weight

  val weight_of_uid :
    map:'fd W.map -> ('fd, 'uid) t -> weight:weight -> 'uid -> weight

  val of_offset : map:'fd W.map -> ('fd, 'uid) t -> raw -> cursor:int64 -> v
  val of_uid : map:'fd W.map -> ('fd, 'uid) t -> raw -> 'uid -> v

  type path = Carton.Dec.path

  val path_to_list : path -> int64 list
  val kind_of_path : path -> [ `A | `B | `C | `D ]
  val path_of_offset : map:'fd W.map -> ('fd, 'uid) t -> cursor:int64 -> path
  val path_of_uid : map:'fd W.map -> ('fd, 'uid) t -> 'uid -> path

  val of_offset_with_path :
    map:'fd W.map -> ('fd, 'uid) t -> path:path -> raw -> cursor:int64 -> v

  type 'uid digest = 'uid Carton.Dec.digest

  val uid_of_offset :
    map:'fd W.map ->
    digest:'uid digest ->
    ('fd, 'uid) t ->
    raw ->
    cursor:int64 ->
    Carton.kind * 'uid

  val uid_of_offset_with_source :
    map:'fd W.map ->
    digest:'uid digest ->
    ('fd, 'uid) t ->
    kind:Carton.kind ->
    raw ->
    depth:int ->
    cursor:int64 ->
    'uid

  type 'uid oracle = 'uid Carton.Dec.oracle

  module Verify (Uid : Carton.UID) : sig
    type status

    val pp : Format.formatter -> status -> unit
    val is_resolved : status -> bool
    val uid_of_status : status -> Uid.t
    val kind_of_status : status -> Carton.kind
    val depth_of_status : status -> int
    val source_of_status : status -> Uid.t option
    val offset_of_status : status -> int64
    val unresolved_base : cursor:int64 -> status
    val unresolved_node : status

    val verify :
      threads:int ->
      map:'fd W.map ->
      oracle:Uid.t oracle ->
      verbose:(unit -> unit) ->
      ('fd, Uid.t) t ->
      matrix:status array ->
      unit Lwt.t
  end

  module Ip (Uid : Carton.UID) : sig
    val iter :
      threads:'a list ->
      f:('a -> uid:Uid.t -> offset:int64 -> crc:Idx.optint -> unit Lwt.t) ->
      Uid.t Idx.idx ->
      unit Lwt.t
  end
end

module Enc : sig
  type 'uid entry = 'uid Carton.Enc.entry
  type 'uid delta = 'uid Carton.Enc.delta = From of 'uid | Zero

  val make_entry :
    kind:Carton.kind ->
    length:int ->
    ?preferred:bool ->
    ?delta:'uid delta ->
    'uid ->
    'uid entry

  val length : 'uid entry -> int

  type 'uid q = 'uid Carton.Enc.q
  type 'uid p = 'uid Carton.Enc.p
  type 'uid patch = 'uid Carton.Enc.patch
  type 'uid load = 'uid -> Dec.v Lwt.t
  type 'uid find = 'uid -> int option Lwt.t

  type 'uid uid = 'uid Carton.Enc.uid = {
    uid_ln : int;
    uid_rw : 'uid -> string;
  }

  val target_to_source : 'uid q -> 'uid p
  val target_uid : 'uid q -> 'uid
  val entry_to_target : load:'uid load -> 'uid entry -> 'uid q Lwt.t

  val apply :
    load:'uid load -> uid_ln:int -> source:'uid p -> target:'uid q -> unit Lwt.t

  module type VERBOSE = Carton.Enc.VERBOSE with type 'a fiber = 'a Lwt.t
  module type UID = Carton.Enc.UID

  module Delta (Uid : UID) (Verbose : VERBOSE) : sig
    val delta :
      threads:Uid.t load list ->
      weight:int ->
      uid_ln:int ->
      Uid.t entry array ->
      Uid.t q array Lwt.t
  end

  module N : sig
    type encoder = Carton.Enc.N.encoder

    type b = Carton.Enc.N.b = {
      i : Bigstringaf.t;
      q : De.Queue.t;
      w : De.Lz77.window;
    }

    val encoder : b:b -> load:'uid load -> 'uid q -> encoder Lwt.t
    val encode : o:Bigstringaf.t -> encoder -> [ `Flush of encoder * int | `End ]
    val dst : encoder -> Bigstringaf.t -> int -> int -> encoder
  end

  type b = Carton.Enc.b = {
    i : Bigstringaf.t;
    q : De.Queue.t;
    w : De.Lz77.window;
    o : Bigstringaf.t;
  }

  val header_of_pack : length:int -> Bigstringaf.t -> int -> int -> unit

  val encode_target :
    ?level:int ->
    b:b ->
    find:'uid find ->
    load:'uid load ->
    uid:'uid uid ->
    'uid q ->
    cursor:int ->
    (int * N.encoder) Lwt.t
end

module Thin : sig
  type 'uid light_load = 'uid -> (Carton.kind * int) Lwt.t
  type 'uid heavy_load = 'uid -> Carton.Dec.v Lwt.t
  type optint = Optint.t

  module Make (Uid : Carton.UID) : sig
    type ('t, 'path, 'fd, 'error) fs = {
      create : ?trunc:bool -> 't -> 'path -> ('fd, 'error) result Lwt.t;
      append : 't -> 'fd -> string -> unit Lwt.t;
      map : 't -> 'fd -> pos:int64 -> int -> Bigstringaf.t;
      close : 't -> 'fd -> (unit, 'error) result Lwt.t;
    }

    val verify :
      ?threads:int ->
      digest:Uid.t Carton.Dec.digest ->
      't ->
      'path ->
      ('t, 'path, 'fd, ([> `Msg of string ] as 'error)) fs ->
      (unit -> (string * int * int) option Lwt.t) ->
      ( int
        * Uid.t list
        * (int64 * optint) list
        * Uid.t Dec.Idx.entry list
        * int64
        * Uid.t,
        'error )
      result
      Lwt.t

    val canonicalize :
      light_load:Uid.t light_load ->
      heavy_load:Uid.t heavy_load ->
      src:'path ->
      dst:'path ->
      't ->
      ('t, 'path, 'fd, ([> `Msg of string ] as 'error)) fs ->
      int ->
      Uid.t list ->
      int64 ->
      (int64 * int64 * Uid.t * Uid.t Dec.Idx.entry list, 'error) result Lwt.t
  end
end
