type t = Digestif.SHA1.t

include module type of Digestif.SHA1 with type t := t

val length : int
val compare : t -> t -> int
val hash : t -> int
val null : t
val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx
