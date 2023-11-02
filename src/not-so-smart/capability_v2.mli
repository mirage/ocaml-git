type t =
  [ `Atom of string
  | `Key_value of string * string
  | `Command_features of string * string list ]

val pp : t Fmt.t
val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
