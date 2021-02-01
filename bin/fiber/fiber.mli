type +'a t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( >>> ) : unit t -> unit t -> unit t
val both : 'a t -> 'b t -> ('a * 'b) t
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
val fork_and_join_unit : (unit -> unit t) -> (unit -> unit t) -> unit t
val parallel_map : f:('a -> 'b t) -> 'a list -> 'b list t
val parallel_iter : f:('a -> unit t) -> 'a list -> unit t
val detach : (unit -> 'a) -> 'a t
val run : 'a t -> 'a
val set_concurrency : int -> unit
val get_concurrency : unit -> int

module Mutex : sig
  type +'a fiber = 'a t
  type t

  val create : unit -> t
  val lock : t -> unit fiber
  val unlock : t -> unit
end

module Condition : sig
  type +'a fiber = 'a t
  type mutex = Mutex.t
  type t

  val create : unit -> t
  val wait : t -> mutex -> unit fiber
  val signal : t -> unit
  val broadcast : t -> unit
end
