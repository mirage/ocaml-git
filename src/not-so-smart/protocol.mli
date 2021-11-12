module Advertised_refs : sig
  type ('uid, 'reference) t

  val pp : (string, string) t Fmt.t
  val head : ('a, string) t -> 'a option
  val capabilities : ('uid, 'reference) t -> Capability.t list

  val refs :
    ('uid, 'reference) t -> ('uid * 'reference * (* peeled *) bool) list

  val reference :
    equal:('ref -> 'ref -> bool) ->
    ?peeled:bool ->
    'ref ->
    ('uid, 'ref) t ->
    'uid option

  val references :
    equal:('ref -> 'ref -> bool) ->
    ?peeled:bool ->
    'ref list ->
    ('uid, 'ref) t ->
    'uid list

  val map :
    fuid:('uid0 -> 'uid1) ->
    fref:('ref0 -> 'ref1) ->
    ('uid0, 'ref0) t ->
    ('uid1, 'ref1) t

  val equal :
    uid:('uid -> 'uid -> bool) ->
    reference:('ref -> 'ref -> bool) ->
    ('uid, 'ref) t ->
    ('uid, 'ref) t ->
    bool

  val v1 :
    ?shallows:'uid list ->
    ?capabilities:Capability.t list ->
    ('uid * 'ref * bool) list ->
    ('uid, 'ref) t
end

module Proto_request : sig
  type t

  val pp : t Fmt.t
  val upload_pack : host:string -> ?port:int -> ?version:int -> string -> t
  val receive_pack : host:string -> ?port:int -> ?version:int -> string -> t
end

module Want : sig
  type ('uid, 'reference) t

  val want :
    capabilities:Capability.t list ->
    ?deepen:[ `Depth of int | `Timestamp of int64 | `Not of 'reference ] ->
    ?filter:Filter.t ->
    ?shallows:'uid list ->
    ?others:'uid list ->
    'uid ->
    ('uid, 'reference) t
end

module Result : sig
  type 'uid t = private NAK | ACK of 'uid

  val pp : string t Fmt.t
end

module Negotiation : sig
  type 'uid t = private
    | ACK of 'uid
    | ACK_continue of 'uid
    | ACK_ready of 'uid
    | ACK_common of 'uid
    | NAK

  val is_common : 'uid t -> bool
  val is_ready : 'uid t -> bool
  val is_nak : 'uid t -> bool
  val pp : string t Fmt.t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Commands : sig
  type ('uid, 'ref) command = private
    | Create of 'uid * 'ref
    | Delete of 'uid * 'ref
    | Update of 'uid * 'uid * 'ref

  type ('uid, 'ref) t

  val create : 'uid -> 'ref -> ('uid, 'ref) command
  val delete : 'uid -> 'ref -> ('uid, 'ref) command
  val update : 'uid -> 'uid -> 'ref -> ('uid, 'ref) command
  val capabilities : ('uid, 'ref) t -> Capability.t list

  val v :
    capabilities:Capability.t list ->
    ?others:('uid, 'ref) command list ->
    ('uid, 'ref) command ->
    ('uid, 'ref) t

  val commands : ('uid, 'ref) t -> ('uid, 'ref) command list
  val pp : 'uid Fmt.t -> 'ref Fmt.t -> ('uid, 'ref) t Fmt.t

  val map :
    fuid:('uid0 -> 'uid1) ->
    fref:('ref0 -> 'ref1) ->
    ('uid0, 'ref0) t ->
    ('uid1, 'ref1) t
end

module Shallow : sig
  type 'uid t = private Shallow of 'uid | Unshallow of 'uid

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Status : sig
  type 'ref t = private {
    result : (unit, string) result;
    commands : [ `FF of 'ref | `OK of 'ref | `ER of 'ref * string ] list;
  }

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val pp : string t Fmt.t
  val to_result : 'string t -> (unit, string) result

  val v :
    ?err:string ->
    ( ('uid, 'ref) Commands.command,
      ('uid, 'ref) Commands.command * string )
    result
    list ->
    'ref t
end

module Decoder : sig
  open Pkt_line.Decoder

  type nonrec error =
    [ error
    | `Invalid_advertised_ref of string
    | `Invalid_shallow of string
    | `Invalid_negotiation_result of string
    | `Invalid_side_band of string
    | `Invalid_ack of string
    | `Invalid_result of string
    | `Invalid_command_result of string
    | `Invalid_command of string
    | `Unexpected_pkt_line of string
    | `Unexpected_flush
    | `Invalid_pkt_line of string ]

  val pp_error : error Fmt.t

  val decode_advertised_refs :
    decoder -> ((string, string) Advertised_refs.t, [> error ]) state

  val decode_result : decoder -> (string Result.t, [> error ]) state

  val decode_pack :
    ?side_band:bool ->
    push_pack:(string * int * int -> unit) ->
    push_stdout:(string -> unit) ->
    push_stderr:(string -> unit) ->
    decoder ->
    (bool, [> error ]) state

  val decode_negotiation : decoder -> (string Negotiation.t, [> error ]) state
  val decode_shallows : decoder -> (string Shallow.t list, [> error ]) state
  val decode_flush : decoder -> (unit, [> error ]) state

  val decode_status :
    ?sideband:bool -> decoder -> (string Status.t, [> error ]) state

  val decode_packet : trim:bool -> decoder -> (string, [> error ]) state

  val decode_commands :
    decoder -> ((string, string) Commands.t option, [> error ]) state
end

module Encoder : sig
  open Pkt_line.Encoder

  type nonrec error = error

  val pp_error : error Fmt.t
  val encode_proto_request : encoder -> Proto_request.t -> error state
  val encode_want : encoder -> (string, string) Want.t -> error state
  val encode_done : encoder -> error state
  val encode_flush : encoder -> error state
  val encode_commands : encoder -> (string, string) Commands.t -> error state

  val encode_advertised_refs :
    encoder -> (string, string) Advertised_refs.t -> error state

  val encode_pack :
    ?side_band:bool -> ?stateless:bool -> encoder -> string -> error state

  val unsafe_encode_packet : encoder -> packet:string -> unit
end
