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

(** minimal interface that contains [encoder] and [decoder] states *)
module type CONTEXT = sig
  type t
  type encoder
  type decoder

  val pp : t Fmt.t
  val encoder : t -> encoder
  val decoder : t -> decoder
end

module type VALUE = sig
  type 'a send
  type 'a recv
  type error
  type encoder
  type decoder

  val encode : encoder -> 'a send -> 'a -> (unit, error) t
  val decode : decoder -> 'a recv -> ('a, error) t
end

module Context : sig
  type t
  type encoder = Pkt_line.Encoder.encoder
  type decoder = Pkt_line.Decoder.decoder

  type capabilities = {
    client_caps : Capability.t list;
    server_caps : Capability.t list;
  }

  val pp : Capability.t Fmt.t -> t Fmt.t
  val make : client_caps:Capability.t list -> t
  val encoder : t -> encoder
  val decoder : t -> decoder
  val capabilities : t -> capabilities
  val replace_server_caps : t -> Capability.t list -> unit
  val is_cap_shared : t -> Capability.t -> bool
end

module Scheduler
    (Value : VALUE
               with type encoder = Context.encoder
                and type decoder = Context.decoder) : sig
  type error = Value.error

  val return : 'v -> ('v, 'err) t
  val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val map : ('a, 'err) t -> f:('a -> 'b) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
  val ( let+ ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
  val fail : 'err -> ('v, 'err) t
  val reword_error : ('err0 -> 'err1) -> ('v, 'err0) t -> ('v, 'err1) t

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

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a

  module Infix : sig
    val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
    val ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
    val return : 'v -> ('v, 'err) t
    val fail : 'err -> ('v, 'err) t
  end

  module Syntax : sig
    val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
    val ( let+ ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
    val return : 'v -> ('v, 'err) t
    val fail : 'err -> ('v, 'err) t
  end
end
