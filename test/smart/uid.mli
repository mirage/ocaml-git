include module type of Digestif.SHA1

val length : int
val compare : t -> t -> int
val hash : t -> int
val null : t
val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx
