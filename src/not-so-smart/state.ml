let ( <.> ) f g x = f (g x)

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

module Context = struct
  open Pkt_line

  type t = {
    encoder : Encoder.encoder;
    decoder : Decoder.decoder;
    mutable capabilities : Capability.t list * Capability.t list;
  }

  type encoder = Encoder.encoder
  type decoder = Decoder.decoder

  let pp _ppf _t = ()

  let make capabilities =
    {
      encoder = Encoder.create ();
      decoder = Decoder.create ();
      capabilities = capabilities, [];
    }

  let encoder { encoder; _ } = encoder
  let decoder { decoder; _ } = decoder
  let capabilities { capabilities; _ } = capabilities

  let update ({ capabilities = client_side, _; _ } as t) server_side =
    t.capabilities <- client_side, server_side

  let shared capability t =
    let client_side, server_side = t.capabilities in
    let a = List.exists (Capability.equal capability) client_side in
    a && List.exists (Capability.equal capability) server_side
end

module Scheduler
    (Context : CONTEXT)
    (Value : S
               with type encoder = Context.encoder
                and type decoder = Context.decoder) =
struct
  type error = Value.error

  let bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t =
    let rec aux ~f m =
      match m with
      | Return v -> f v
      | Read { k; off; len; buffer; eof } ->
          Read { k = aux ~f <.> k; off; len; buffer; eof = aux ~f <.> eof }
      | Write { k; off; len; buffer } ->
          Write { k = aux ~f <.> k; off; len; buffer }
      | Error _ as err -> err
    in
    fun m ~f ->
      match m with
      | Return v -> f v
      | Error _ as err -> err
      | Read _ -> aux ~f m
      | Write _ -> aux ~f m

  let ( let* ) m f = bind m ~f
  let ( >>= ) m f = bind m ~f
  let return v = Return v
  let fail error = Error error

  let reword_error f x =
    let rec go = function
      | Read { k; buffer; off; len; eof } ->
          Read { k = go <.> k; buffer; off; len; eof = go <.> eof }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v
      | Error err -> Error (f err)
    in
    go x

  let encode :
      type a.
      Context.t ->
      a Value.send ->
      a ->
      (Context.t -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w v k ->
    let rec go = function
      | Return () -> k ctx
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Read { k; buffer; off; len; eof } ->
          Read { k = go <.> k; buffer; off; len; eof = go <.> eof }
      | Error err -> Error (`Protocol err)
    in
    go (Value.encode (Context.encoder ctx) w v)

  let send :
      type a.
      Context.t -> a Value.send -> a -> (unit, [> `Protocol of error ]) t =
   fun ctx w x -> encode ctx w x (fun _ctx -> Return ())

  let decode :
      type a.
      Context.t ->
      a Value.recv ->
      (Context.t -> a -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w k ->
    let rec go : (a, 'err) t -> ('b, [> `Protocol of error ]) t = function
      | Read { k; buffer; off; len; eof } ->
          Read { k = go <.> k; buffer; off; len; eof = go <.> eof }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> k ctx v
      | Error err -> Error (`Protocol err)
    in
    go (Value.decode (Context.decoder ctx) w)

  let recv : type a. Context.t -> a Value.recv -> (a, [> `Protocol of error ]) t
      =
   fun ctx w -> decode ctx w (fun _ctx v -> Return v)

  let error_msgf fmt = Fmt.kstr (fun err -> Error (`Msg err)) fmt
end
