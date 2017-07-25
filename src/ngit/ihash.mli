module type FDIGEST =
sig
  type t
  type buffer

  type 'a digest = 'a -> t

  val cstruct : Cstruct.t digest
  val string  : string digest
  val bytes   : Bytes.t digest
  val digest  : buffer digest
  val digestv : buffer list digest

  val length  : int
end

module type IDIGEST =
sig
  type t
  type ctx
  type buffer

  val init   : unit -> ctx
  val feed   : ctx -> buffer -> unit
  val get    : ctx -> t

  val length : int
end

module type DIGEST =
sig
  type t
  type hash

  val digest : t -> hash
end
