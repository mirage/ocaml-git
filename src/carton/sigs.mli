module type FUNCTOR = sig
  type +'a t
end

type (+'a, 's) io

type 's scheduler = {
  bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io;
  return : 'a. 'a -> ('a, 's) io;
}

module type SCHEDULER = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) io = "%identity"

  external prj : ('a, t) io -> 'a s = "%identity"
end

module type MUTEX = sig
  type +'a fiber

  type t

  val create : unit -> t

  val lock : t -> unit fiber

  val unlock : t -> unit
end

module type FUTURE = sig
  type +'a fiber

  type 'a t

  val wait : 'a t -> 'a fiber

  val peek : 'a t -> 'a option
end

module type CONDITION = sig
  type +'a fiber

  type mutex

  type t

  val create : unit -> t

  val wait : t -> mutex -> unit fiber

  val signal : t -> unit

  val broadcast : t -> unit
end

module type IO = sig
  type +'a t

  module Future : FUTURE with type 'a fiber = 'a t

  module Mutex : MUTEX with type 'a fiber = 'a t

  module Condition :
    CONDITION with type 'a fiber = 'a t and type mutex = Mutex.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val nfork_map : 'a list -> f:('a -> 'b t) -> 'b Future.t list t

  val all_unit : unit t list -> unit t
end

module Make (T : FUNCTOR) : SCHEDULER with type 'a s = 'a T.t

module type UID = sig
  type t

  type ctx

  val empty : ctx

  val get : ctx -> t

  val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val length : int

  val of_raw_string : string -> t

  val to_raw_string : t -> string

  val pp : t Fmt.t

  val null : t
end

type kind = [ `A | `B | `C | `D ]

val _max_depth : int
