module BaseBigstring
  : Common.BASE with type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module BaseBytes :
sig
  include Common.BASE with type t = Bytes.t

  val to_hex : t -> Bytes.t
  val of_hex : Bytes.t -> t
end

module MakeDecoder (A : Common.ANGSTROM)
  : Common.DECODER with type t = A.t
                    and type raw = Cstruct.t
                    and type init = Cstruct.t
                    and type error = [ `Decoder of string ]

module MakeInflater (Z : Common.INFLATE) (A : Common.ANGSTROM)
  : Common.DECODER with type t = A.t
                    and type raw = Cstruct.t
                    and type init = Z.window * Cstruct.t * Cstruct.t
                    and type error = [ `Decoder of string | `Inflate of Z.error ]

module MakeEncoder (M : Common.MINIENC)
  : Common.ENCODER with type t = M.t
                    and type raw = Cstruct.t
                    and type init = int * M.t
                    and type error = [ `Never ]

module MakeDeflater (Z : Common.DEFLATE) (M : Common.MINIENC)
  : Common.ENCODER with type t = M.t
                    and type raw = Cstruct.t
                    and type init = int * M.t * int * Cstruct.t
                    and type error = [ `Deflate of Z.error ]

val digest :
  (module Ihash.IDIGEST with type buffer = Cstruct.t
                         and type t = 'hash) ->
  (module Common.FARADAY with type t = 't) -> kind:string -> 't -> 'hash

val fdigest :
  (module Ihash.IDIGEST with type buffer = Cstruct.t
                         and type t = 'hash) ->
  (module Common.ENCODER with type t = 't
                          and type raw = Cstruct.t
                          and type init = int * 't
                          and type error = [ `Never ]) ->
  ?capacity:int -> tmp:Cstruct.t -> kind:string -> length:('t -> int64) -> 't -> 'hash

module type ENCODER =
sig
  type state
  type raw
  type result
  type error

  val raw_length : raw -> int
  val raw_blit   : raw -> int -> raw -> int -> int -> unit

  val eval  : raw -> state -> [ `Flush of state | `End of (state * result) | `Error of (state * error) ] Lwt.t
  val used  : state -> int
  val flush : int -> int -> state -> state
end

type ('state, 'raw, 'result, 'error) encoder =
  (module ENCODER with type state  = 'state
                   and type raw    = 'raw
                   and type result = 'result
                   and type error  = 'error)
and ('fd, 'raw, 'error) writer = 'raw -> ?off:int -> ?len:int -> 'fd -> (int, 'error) result Lwt.t

val safe_encoder_to_file :
  limit:int ->
  ('state, 'raw, 'res, 'err_encoder) encoder ->
  ('fd, 'raw, 'err_writer) writer ->
  'fd -> 'raw -> 'state ->
  ('res, [ `Stack | `Encoder of 'err_encoder | `Writer of 'err_writer ]) result Lwt.t
