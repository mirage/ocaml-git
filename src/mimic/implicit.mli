module type KEY_INFO = sig
  type 'a t
end

module Make (Key_info : KEY_INFO) : sig
  type t = private ..
  type 'a key = 'a Key_info.t

  module type WITNESS = sig
    type a
    type t += T of a

    val key : a key
  end

  type 'a witness = (module WITNESS with type a = 'a)
  type pack = Key : 'a key -> pack
  type value = Value : 'a * 'a key -> value

  val inj : 'a key -> 'a witness
  val prj : t -> value
  val bindings : unit -> pack list
end
