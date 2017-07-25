type tz_offset =
  { sign    : [ `Plus | `Minus ]
  ; hours   : int
  ; minutes : int }

type t =
  { name  : string
  ; email : string
  ; date  : int64 * tz_offset option }

include Common.BASE with type t := t

module A
  : Common.ANGSTROM with type t = t
module F
  : Common.FARADAY with type t = t
module M
  : Common.MINIENC with type t = t
module D
  : Common.DECODER  with type t = t
                     and type raw = Cstruct.t
                     and type init = Cstruct.t
                     and type error = [ `Decoder of string ]


