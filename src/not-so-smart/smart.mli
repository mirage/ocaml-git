(** Implementation of smart protocol.

    This module does not have any Git logics. It provides a light implementation
    of the Smart protocol to be able to [fetch]/[pull] or [push] with a Git
    {i server}. *)

module Capability = Capability

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

  val v1 :
    ?shallows:'uid list ->
    ?capabilities:Capability.t list ->
    ('uid * 'ref * bool) list ->
    ('uid, 'ref) t

  val equal :
    uid:('uid -> 'uid -> bool) ->
    reference:('ref -> 'ref -> bool) ->
    ('uid, 'ref) t ->
    ('uid, 'ref) t ->
    bool
end

module Proto_request : sig
  type t

  val pp : t Fmt.t

  val upload_pack :
    host:[ `Addr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ] ->
    ?port:int ->
    ?version:int ->
    string ->
    t

  val receive_pack :
    host:[ `Addr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ] ->
    ?port:int ->
    ?version:int ->
    string ->
    t
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

  val v :
    capabilities:Capability.t list ->
    ?others:('uid, 'ref) command list ->
    ('uid, 'ref) command ->
    ('uid, 'ref) t

  val commands : ('uid, 'ref) t -> ('uid, 'ref) command list

  val map :
    fuid:('uid0 -> 'uid1) ->
    fref:('ref0 -> 'ref1) ->
    ('uid0, 'ref0) t ->
    ('uid1, 'ref1) t
end

module Status : sig
  type 'ref t = private {
    result : (unit, string) result;
    commands : ('ref, 'ref * string) result list;
  }

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val pp : string t Fmt.t
  val to_result : 'ref t -> (unit, string) result

  val v :
    ?err:string ->
    ( ('uid, 'ref) Commands.command,
      ('uid, 'ref) Commands.command * string )
    result
    list ->
    'ref t
end

module Shallow : sig
  type 'uid t = private Shallow of 'uid | Unshallow of 'uid

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

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

type error =
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  | `Expected_eol
  | `Expected_eol_or_space
  | `Unexpected_end_of_input
  | `No_enough_space
  | `Assert_predicate of char -> bool
  | `Invalid_advertised_ref of string
  | `Invalid_shallow of string
  | `Invalid_negotiation_result of string
  | `Invalid_side_band of string
  | `Invalid_ack of string
  | `Invalid_result of string
  | `Invalid_command_result of string
  | `No_enough_space
  | `Unexpected_flush
  | `Invalid_pkt_line ]

val pp_error : error Fmt.t

module Context : sig
  type t

  type capabilities = {
    client_caps : Capability.t list;
    server_caps : Capability.t list;
  }

  val make : client_caps:Capability.t list -> t
  val capabilities : t -> capabilities
  val replace_server_caps : t -> Capability.t list -> unit
  val is_cap_shared : t -> Capability.t -> bool
end

type 'a send

val proto_request : Proto_request.t send
val want : (string, string) Want.t send
val negotiation_done : unit send
val flush : unit send
val commands : (string, string) Commands.t send
val send_pack : ?stateless:bool -> bool -> string send

type 'a recv

val advertised_refs : (string, string) Advertised_refs.t recv
val negotiation_result : string Result.t recv

val recv_pack :
  ?side_band:bool ->
  ?push_stdout:(string -> unit) ->
  ?push_stderr:(string -> unit) ->
  (string * int * int -> unit) ->
  bool recv

val ack : string Negotiation.t recv
val shallows : string Shallow.t list recv
val status : string Status.t recv
val packet : trim:bool -> string recv
val send_advertised_refs : (string, string) Advertised_refs.t send
val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t
val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

val encode :
  Context.t ->
  'a send ->
  'a ->
  (Context.t -> ('b, ([> `Protocol of error ] as 'err)) t) ->
  ('b, 'err) t

val decode :
  Context.t ->
  'a recv ->
  (Context.t -> 'a -> ('b, ([> `Protocol of error ] as 'err)) t) ->
  ('b, 'err) t

val send : Context.t -> 'a send -> 'a -> (unit, [> `Protocol of error ]) t
val recv : Context.t -> 'a recv -> ('a, [> `Protocol of error ]) t
val return : 'v -> ('v, 'err) t
val fail : 'err -> ('v, 'err) t
val reword_error : ('err0 -> 'err1) -> ('v, 'err0) t -> ('v, 'err1) t

val error_msgf :
  ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a

(**/**)

module Unsafe : sig
  val write : Context.t -> string -> unit
end
