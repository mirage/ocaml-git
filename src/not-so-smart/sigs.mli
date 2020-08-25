type ('a, 's) io

type ('k, 'v, 's) store

type 's scheduler = {
  bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io;
  return : 'a. 'a -> ('a, 's) io;
}

type ('flow, 'error, 's) flow = {
  recv :
    'flow ->
    Cstruct.t ->
    (([ `End_of_flow | `Input of int ], 'error) result, 's) io;
  send : 'flow -> Cstruct.t -> ((int, 'error) result, 's) io;
  pp_error : Format.formatter -> 'error -> unit;
}

type ('uid, 'ref, 'v, 'g, 's) access = {
  get : 'uid -> ('uid, 'v, 'g) store -> ('v option, 's) io;
  parents : 'uid -> ('uid, 'v, 'g) store -> ('v list, 's) io;
  deref : ('uid, 'v, 'g) store -> 'ref -> ('uid option, 's) io;
  locals : ('uid, 'v, 'g) store -> ('ref list, 's) io;
}

module type SCHED = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module type STORE = sig
  type ('a, 'b) s

  type t

  external inj : ('a, 'b) s -> ('a, 'b, t) store = "%identity"

  external prj : ('a, 'b, t) store -> ('a, 'b) s = "%identity"
end

module Make_sched (T : sig
  type +'a t
end) : SCHED with type +'a s = 'a T.t

module Make_store (T : sig
  type ('k, 'v) t
end) : STORE with type ('k, 'v) s = ('k, 'v) T.t

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  val async : (unit -> unit t) -> unit
end

module type UID = sig
  type t

  val of_hex : string -> t

  val to_hex : t -> string

  val compare : t -> t -> int
end

module type REF = sig
  type t

  val v : string -> t

  val equal : t -> t -> bool

  val to_string : t -> string
end

module type FLOW = sig
  type +'a fiber

  type t

  type error

  val recv :
    t -> Cstruct.t -> ([ `End_of_flow | `Input of int ], error) result fiber

  val send : t -> Cstruct.t -> (int, error) result fiber

  val pp_error : error Fmt.t
end
