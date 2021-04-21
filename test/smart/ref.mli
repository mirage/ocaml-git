type t = private string

val v : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val pp : t Fmt.t
val segs : t -> string list
