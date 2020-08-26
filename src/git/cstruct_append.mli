include Smart_git.APPEND with type +'a fiber = 'a Lwt.t

val device : unit -> t
val key : t -> uid
val project : t -> uid -> Cstruct.t
