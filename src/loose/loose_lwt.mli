open Loose

type lwt = Carton_lwt.lwt

module Make (Uid : Loose.UID) : sig
  val exists : 't -> ('t, Uid.t, _, lwt) store -> Uid.t -> bool Lwt.t

  val atomic_add :
    't ->
    buffers ->
    ('t, Uid.t, 'error, lwt) store ->
    hdr:(buffer:Cstruct.t -> Carton.Dec.v -> Cstruct.t) ->
    Carton.Dec.v ->
    (Uid.t * int, [> `Store of 'error | `Non_atomic ]) result Lwt.t

  val add :
    't ->
    buffers ->
    ('t, Uid.t, 'error, lwt) store ->
    hdr:Cstruct.t ->
    (unit -> string option Lwt.t) ->
    (Uid.t * int, [> `Store of 'error ]) result Lwt.t

  val atomic_get :
    't ->
    buffers ->
    ('t, Uid.t, 'error, lwt) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    (Carton.Dec.v, [> `Non_atomic ]) result Lwt.t

  val size_and_kind :
    't ->
    buffers ->
    ('t, Uid.t, 'error, lwt) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    (int64 * kind, [> `Malformed ]) result Lwt.t

  val get :
    't ->
    buffers ->
    ('t, Uid.t, 'error, lwt) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string ]) result Lwt.t
end
