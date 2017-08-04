module type S =
sig
  type t

  val unlock    : t -> unit Lwt.t
  val lock      : t -> unit Lwt.t
  val with_lock : t option -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end
