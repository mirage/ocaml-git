type t =
  { mutable buf : Cstruct.t
  ; mutable pos : int
  ; mutable len : int
  ; init : Cstruct.t }

type raw = string
type fixe = Cstruct.t

val create : int -> t
val contents : t -> string
val unsafe_contents : t -> Cstruct.t
val has : t -> int
val resize : t -> int -> unit
val add : t -> Cstruct.t -> unit
val clear : t -> unit
val reset : t -> unit
