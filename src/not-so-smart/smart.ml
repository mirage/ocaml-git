let ( <.> ) f g x = f (g x)

module Capability = Capability

include struct
  open Protocol
  module Proto_request = Proto_request
  module Advertised_refs = Advertised_refs
  module Want = Want
  module Result = Result
  module Negotiation = Negotiation
  module Shallow = Shallow
  module Commands = Commands
  module Status = Status
end

module Witness = struct
  type 'a send =
    | Proto_request : Proto_request.t send
    | Want : (string, string) Want.t send
    | Done : unit send
    | Flush : unit send
    | Commands : (string, string) Commands.t send
    | Send_pack : { side_band : bool; stateless : bool } -> string send
    | Advertised_refs : (string, string) Advertised_refs.t send

  type 'a recv =
    | Advertised_refs : (string, string) Advertised_refs.t recv
    | Result : string Result.t recv
    | Status : string Status.t recv
    | Packet : bool -> string recv
    | Recv_pack : {
        side_band : bool;
        push_pack : string * int * int -> unit;
        push_stdout : string -> unit;
        push_stderr : string -> unit;
      }
        -> bool recv
    | Ack : string Negotiation.t recv
    | Shallows : string Shallow.t list recv
end

module Value = struct
  open Pkt_line

  type encoder = Encoder.encoder
  type decoder = Decoder.decoder

  include Witness

  type error = [ Protocol.Encoder.error | Protocol.Decoder.error ]

  let encode :
      type a. encoder -> a send -> a -> (unit, [> Encoder.error ]) State.t =
   fun encoder w v ->
    let encoder_state =
      let open Protocol.Encoder in
      match w with
      | Proto_request -> encode_proto_request encoder v
      | Want -> encode_want encoder v
      | Done -> encode_done encoder
      | Commands -> encode_commands encoder v
      | Send_pack { side_band; stateless } ->
          encode_pack ~side_band ~stateless encoder v
      | Flush -> encode_flush encoder
      | Advertised_refs -> encode_advertised_refs encoder v
    in
    let rec translate_to_state_t = function
      | Encoder.Done -> State.Return ()
      | Write { continue; buffer; off; len } ->
          State.Write
            { k = translate_to_state_t <.> continue; buffer; off; len }
      | Error err -> State.Error (err :> error)
    in
    translate_to_state_t encoder_state

  let decode : type a. decoder -> a recv -> (a, [> Decoder.error ]) State.t =
   fun decoder w ->
    let rec transl :
        (a, [> Protocol.Decoder.error ]) Decoder.state ->
        (a, [> Decoder.error ]) State.t = function
      | Decoder.Done v -> State.Return v
      | Read { buffer; off; len; continue; eof } ->
          State.Read
            { k = transl <.> continue; buffer; off; len; eof = transl <.> eof }
      | Error { error; _ } -> State.Error error
    in
    transl
      (let open Protocol.Decoder in
      match w with
      | Advertised_refs -> decode_advertised_refs decoder
      | Result -> decode_result decoder
      | Recv_pack { side_band; push_pack; push_stdout; push_stderr } ->
          decode_pack ~side_band ~push_pack ~push_stdout ~push_stderr decoder
      | Ack -> decode_negotiation decoder
      | Status -> decode_status decoder
      | Shallows -> decode_shallows decoder
      | Packet trim -> decode_packet ~trim decoder)
end

type ('a, 'err) t = ('a, 'err) State.t =
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

module Context = struct
  type capabilities = {
    client_caps : Capability.t list;
    server_caps : Capability.t list;
  }

  let pp_capabilities _ppf _v = ()

  include State.Context

  type nonrec t = capabilities t

  let make ~client_caps = make { client_caps; server_caps = [] }
  let pp ppf v = pp pp_capabilities ppf v
  let capabilities ctx = context ctx

  let replace_server_caps ctx caps =
    update ~f:(fun ~old_ctx -> { old_ctx with server_caps = caps }) ctx

  let is_cap_shared ctx cap =
    let { client_caps; server_caps } = capabilities ctx in
    let is_cap_in caps = List.exists (fun c -> Capability.equal c cap) caps in
    is_cap_in client_caps && is_cap_in server_caps
end

include Witness

let proto_request = Proto_request
let advertised_refs = Advertised_refs
let want = Want
let negotiation_done = Done
let negotiation_result = Result
let commands = Commands

let recv_pack ?(side_band = false) ?(push_stdout = ignore)
    ?(push_stderr = ignore) ~push_pack =
  Recv_pack { side_band; push_pack; push_stdout; push_stderr }

let status = Status
let flush = Flush
let ack = Ack
let shallows = Shallows

let send_pack ?(stateless = false) side_band =
  Send_pack { side_band; stateless }

let packet ~trim = Packet trim
let send_advertised_refs : _ send = Advertised_refs

include State.Scheduler (Context) (Value)

let pp_error ppf = function
  | #Protocol.Encoder.error as err -> Protocol.Encoder.pp_error ppf err
  | #Protocol.Decoder.error as err -> Protocol.Decoder.pp_error ppf err

module Unsafe = struct
  let write context packet =
    let encoder = Context.encoder context in
    Protocol.Encoder.unsafe_encode_packet encoder ~packet
end
