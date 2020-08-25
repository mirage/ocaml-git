open Loose

type error = |

module type UID = sig
  include Loose.UID

  val of_hex : string -> t
end

module Make (Uid : UID) : sig
  val exists : Fpath.t -> Uid.t -> bool Lwt.t

  val atomic_add :
    Fpath.t ->
    buffers ->
    Carton.Dec.v ->
    (Uid.t * int, [> `Store of error | `Non_atomic ]) result Lwt.t

  val add :
    Fpath.t ->
    buffers ->
    [ `Blob | `Commit | `Tag | `Tree ] * int64 ->
    (unit -> string option Lwt.t) ->
    (Uid.t * int, [> `Store of error ]) result Lwt.t

  val atomic_get :
    Fpath.t -> buffers -> Uid.t -> (Carton.Dec.v, [> `Non_atomic ]) result Lwt.t

  val size_and_kind :
    Fpath.t -> buffers -> Uid.t -> (int64 * kind, [> `Malformed ]) result Lwt.t

  val get :
    Fpath.t ->
    buffers ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string ]) result Lwt.t
end
