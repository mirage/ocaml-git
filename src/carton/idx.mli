type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'uid idx
type optint = Optint.t

val make :
  bigstring ->
  uid_ln:int ->
  uid_rw:('uid -> string) ->
  uid_wr:(string -> 'uid) ->
  'uid idx

val find : 'uid idx -> 'uid -> (optint * int64) option

val iter :
  f:(uid:'uid -> offset:int64 -> crc:optint -> unit) -> 'uid idx -> unit

val map :
  f:(uid:'uid -> offset:int64 -> crc:optint -> 'a) -> 'uid idx -> 'a list

val exists : 'uid idx -> 'uid -> bool
val max : 'uid idx -> int
val get_uid : 'uid idx -> int -> 'uid
val get_offset : 'uid idx -> int -> int64
val get_crc : 'uid idx -> int -> optint

module type UID = sig
  type t
  type ctx

  val empty : ctx
  val feed : ctx -> ?off:int -> ?len:int -> bigstring -> ctx
  val get : ctx -> t
  val compare : t -> t -> int
  val length : int
  val to_raw_string : t -> string
  val pp : t Fmt.t
end

type 'uid entry = { crc : optint; offset : int64; uid : 'uid }

module N (Uid : UID) : sig
  type encoder
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  val encoder : dst -> pack:Uid.t -> Uid.t entry array -> encoder
  val encode : encoder -> [ `Await ] -> [ `Partial | `Ok ]
  val dst_rem : encoder -> int
  val dst : encoder -> Bigstringaf.t -> int -> int -> unit
end

(** Memory safe IDX decoder.

    The goal of this library is to provide a way to decode the IDX file as a
    stream. *)

module Device : sig
  type t
  type uid

  val device : unit -> t
  val create : t -> uid
  val project : t -> uid -> Bigstringaf.t
end

module M (IO : sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end) (Uid : sig
  include UID

  val of_raw_string : string -> t
  val null : t
end) : sig
  type t = Device.t
  type uid = Device.uid
  type fd
  type error

  val pp_error : error Fmt.t
  val create : t -> uid -> (fd, error) result IO.t
  val append : t -> fd -> string -> unit IO.t
  val close : t -> fd -> (unit, error) result IO.t
end
