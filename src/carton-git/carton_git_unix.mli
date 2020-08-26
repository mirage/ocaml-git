module Store :
  Carton_git.STORE
    with type t = Fpath.t
     and type uid = Fpath.t
     and type 'a fd = Lwt_unix.file_descr
     and type error = [ `Not_found of Fpath.t ]
     and type +'a fiber = 'a Lwt.t

open Carton_git

module Make (Uid : sig
  include Carton.UID

  val of_hex : string -> t

  val to_hex : t -> string
end) : sig
  val make : Store.t -> (Fpath.t, Lwt_unix.file_descr, Uid.t) t Lwt.t

  val add :
    Store.t ->
    (Fpath.t, Lwt_unix.file_descr, Uid.t) t ->
    idx:Fpath.t ->
    Fpath.t ->
    (unit, Store.error) result Lwt.t

  val get :
    Store.t ->
    resources:
      (Lwt_unix.file_descr * int64 ->
      ((Lwt_unix.file_descr * int64) buffers -> Carton.Dec.v Lwt.t) ->
      Carton.Dec.v Lwt.t) ->
    (Fpath.t, Lwt_unix.file_descr, Uid.t) t ->
    Uid.t ->
    (Carton.Dec.v, [> `Msg of string | `Not_found of Uid.t ]) result Lwt.t

  val list : Store.t -> (Fpath.t, Lwt_unix.file_descr, Uid.t) t -> Uid.t list

  val fds :
    (Fpath.t, Lwt_unix.file_descr, Uid.t) t ->
    (Lwt_unix.file_descr * int64) list
end
