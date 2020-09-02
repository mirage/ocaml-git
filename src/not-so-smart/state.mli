type ('a, 'err) t =
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      k : int -> ('a, 'err) t;
      eof : unit -> ('a, 'err) t;
    }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

module type CONTEXT = sig
  type t
  type encoder
  type decoder

  val pp : t Fmt.t
  val encoder : t -> encoder
  val decoder : t -> decoder
  val shared : Capability.t -> t -> bool
end

module type S = sig
  type 'a send
  type 'a recv
  type error
  type encoder
  type decoder

  val encode : encoder -> 'a send -> 'a -> (unit, error) t
  val decode : decoder -> 'a recv -> ('a, error) t
end

module Context : sig
  include
    CONTEXT
      with type encoder = Encoder.encoder
       and type decoder = Decoder.decoder

  val make : Capability.t list -> t
  val capabilities : t -> Capability.t list * Capability.t list
  val update : t -> Capability.t list -> unit
  val shared : Capability.t -> t -> bool
end

module Scheduler
    (Context : CONTEXT)
    (Value : S
               with type encoder = Context.encoder
                and type decoder = Context.decoder) : sig
  type error = Value.error

  val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

  val encode :
    Context.t ->
    'a Value.send ->
    'a ->
    (Context.t -> ('b, ([> `Protocol of error ] as 'err)) t) ->
    ('b, 'err) t

  val decode :
    Context.t ->
    'a Value.recv ->
    (Context.t -> 'a -> ('b, ([> `Protocol of error ] as 'err)) t) ->
    ('b, 'err) t

  val send :
    Context.t -> 'a Value.send -> 'a -> (unit, [> `Protocol of error ]) t

  val recv : Context.t -> 'a Value.recv -> ('a, [> `Protocol of error ]) t
  val return : 'v -> ('v, 'err) t
  val fail : 'err -> ('v, 'err) t
  val reword_error : ('err0 -> 'err1) -> ('v, 'err0) t -> ('v, 'err1) t

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a
end
