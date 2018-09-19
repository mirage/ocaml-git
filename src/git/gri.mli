type t = private Uri.t

val host : t -> string
val path : t -> string
val make : Uri.t -> t
val of_string : string -> t
val pp : t Fmt.t
