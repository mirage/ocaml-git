type 'a t

val exists : 'a t -> equal:('a -> 'a -> bool) -> 'a -> bool Lwt.t
val get : 'a t -> 'a list Lwt.t
val append : 'a t -> 'a -> unit Lwt.t
val remove : 'a t -> equal:('a -> 'a -> bool) -> 'a -> unit Lwt.t
val make : 'a list -> 'a t
