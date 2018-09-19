(** This interface describes the minimal I/O operations to a git repository. *)
module type NET = sig
  type endpoint
  type socket
  type error

  val pp_error : Format.formatter -> error -> unit
  val read : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val write : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val socket : endpoint -> socket Lwt.t
  val close : socket -> unit Lwt.t
end

module Make (N : NET with type endpoint = Gri.t) (G : Minimal.S) :
  Sync.S with module Store = G and module Endpoint = Gri
