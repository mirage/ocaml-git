module type S =
sig
  type key
  type elt
  type t

  val unlock    : elt -> unit Lwt.t
  val lock      : elt -> unit Lwt.t
  val with_lock : elt option -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val make      : t -> key -> elt
end
