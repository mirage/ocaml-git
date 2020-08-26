open Loose

module type STORE = sig
  type t
  type uid
  type error
  type +'a fiber

  val pp_error : error Fmt.t
  val exists : t -> uid -> bool fiber
  val length : t -> uid -> (int64, error) result fiber
  val map : t -> uid -> pos:int64 -> int -> Bigstringaf.t fiber
  val append : t -> uid -> Bigstringaf.t -> (unit, error) result fiber
  val appendv : t -> uid -> Bigstringaf.t list -> (unit, error) result fiber
  val list : t -> uid list fiber
  val reset : t -> (unit, error) result fiber
end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Make
    (Scheduler : Carton.SCHEDULER)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Store : STORE with type +'a fiber = 'a Scheduler.s)
    (Uid : UID with type t = Store.uid) : sig
  val exists : Store.t -> Uid.t -> bool IO.t
  val list : Store.t -> Uid.t list IO.t

  val atomic_add :
    Store.t ->
    buffers ->
    Carton.Dec.v ->
    (Uid.t * int, [> `Store of Store.error | `Non_atomic ]) result IO.t

  val add :
    Store.t ->
    buffers ->
    [ `Blob | `Commit | `Tag | `Tree ] * int64 ->
    (unit -> string option IO.t) ->
    (Uid.t * int, [> `Store of Store.error ]) result IO.t

  val atomic_get :
    Store.t -> buffers -> Uid.t -> (Carton.Dec.v, [> `Non_atomic ]) result IO.t

  val size_and_kind :
    Store.t -> buffers -> Uid.t -> (int64 * kind, [> `Malformed ]) result IO.t

  val get :
    Store.t ->
    buffers ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string ]) result IO.t
end
