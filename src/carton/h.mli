type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val io_buffer_size : int
val bigstring_create : int -> bigstring
val bigstring_empty : bigstring

module M : sig
  type src = [ `Channel of in_channel | `Manual | `String of string ]
  type decode = [ `Await | `Header of int * int | `End | `Malformed of string ]
  type decoder

  val src : decoder -> bigstring -> int -> int -> unit
  val dst : decoder -> bigstring -> int -> int -> unit
  val source : decoder -> bigstring -> unit
  val src_rem : decoder -> int
  val dst_rem : decoder -> int
  val src_len : decoder -> int
  val dst_len : decoder -> int
  val decode : decoder -> decode
  val decoder : ?source:bigstring -> src -> decoder
end

module N : sig
  type dst = [ `Channel of out_channel | `Manual | `Buffer of Buffer.t ]
  type encode = [ `Await | `Copy of int * int | `Insert of string | `End ]
  type encoder

  val dst : encoder -> bigstring -> int -> int -> unit
  val dst_rem : encoder -> int
  val encoder : dst -> src_len:int -> dst_len:int -> encoder
  val encode : encoder -> encode -> [ `Ok | `Partial ]
end
