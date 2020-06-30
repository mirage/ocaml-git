module type STORE = sig
  type t

  type uid

  and fd

  type error

  type +'a fiber

  val pp_error : error Fmt.t

  val create : t -> uid -> (fd, error) result fiber

  val map : t -> fd -> pos:int64 -> int -> Bigstringaf.t fiber

  val close : t -> fd -> (unit, error) result fiber

  val list : t -> uid list fiber

  val length : fd -> int64 fiber
end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

type ('path, 'fd, 'uid) t

type 'fd buffers = {
  z : Bigstringaf.t;
  allocate : int -> De.window;
  w : 'fd Carton.Dec.W.t;
}

module Make
    (Scheduler : Carton.SCHEDULER)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Store : STORE with type +'a fiber = 'a Scheduler.s)
    (Uid : Carton.UID) : sig
  val make :
    Store.t ->
    idx:(Store.uid -> Store.uid) ->
    (Store.uid, Store.fd, Uid.t) t IO.t

  val add :
    Store.t ->
    (Store.uid, Store.fd, Uid.t) t ->
    idx:(Store.uid -> Store.uid) ->
    Store.uid ->
    (unit, Store.error) result IO.t

  val get :
    Store.t ->
    resources:
      (Store.fd * int64 ->
      ((Store.fd * int64) buffers -> Carton.Dec.v IO.t) ->
      Carton.Dec.v IO.t) ->
    (Store.uid, Store.fd, Uid.t) t ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string | `Not_found of Uid.t ]) result IO.t

  val exists : Store.t -> (Store.uid, Store.fd, Uid.t) t -> Uid.t -> bool

  val list : Store.t -> (Store.uid, Store.fd, Uid.t) t -> Uid.t list

  val fds : (Store.uid, Store.fd, Uid.t) t -> (Store.fd * int64) list
end
