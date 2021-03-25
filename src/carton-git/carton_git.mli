module type STORE = sig
  type 'a rd = < rd : unit ; .. > as 'a
  type 'a wr = < wr : unit ; .. > as 'a

  type 'a mode =
    | Rd : < rd : unit > mode
    | Wr : < wr : unit > mode
    | RdWr : < rd : unit ; wr : unit > mode

  type t
  type uid
  type 'a fd
  type error
  type +'a fiber

  val pp_error : error Fmt.t

  val create :
    ?trunc:bool -> mode:'a mode -> t -> uid -> ('a fd, error) result fiber

  val map : t -> 'm rd fd -> pos:int64 -> int -> Bigstringaf.t
  val close : t -> 'm fd -> (unit, error) result fiber
  val list : t -> uid list fiber
  val length : 'm fd -> int64 fiber
end

module type IO = sig
  type +'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
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
    uid_of_major_uid:(Store.uid -> 'uid) ->
    idx_major_uid_of_uid:(Store.t -> 'uid -> Store.uid) ->
    (Store.uid, < rd : unit > Store.fd, Uid.t) t IO.t

  val add :
    Store.t ->
    (Store.uid, < rd : unit > Store.fd, Uid.t) t ->
    idx:Store.uid ->
    Store.uid ->
    (< rd : unit > Store.fd * int64, Store.error) result IO.t

  val get :
    Store.t ->
    resources:
      (< rd : unit > Store.fd * int64 ->
      ((< rd : unit > Store.fd * int64) buffers -> Carton.Dec.v IO.t) ->
      Carton.Dec.v IO.t) ->
    (Store.uid, < rd : unit > Store.fd, Uid.t) t ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string | `Not_found of Uid.t ]) result IO.t

  val exists : Store.t -> (Store.uid, 'm Store.fd, Uid.t) t -> Uid.t -> bool
  val list : Store.t -> (Store.uid, 'm Store.fd, Uid.t) t -> Uid.t list
  val fds : (Store.uid, 'm Store.fd, Uid.t) t -> ('m Store.fd * int64) list
end
