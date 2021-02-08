module Capability = Capability_v2
module Proto_vals_v2 = Proto_vals_v2

module Witness = struct
  type 'a send =
    | Proto_request : Proto_vals_v2.Proto_request.t send
    | Ls_refs_req
        : ([ `Client_caps of
             Capability.t list
             (* TODO: not really client_caps but not sure whose caps that are; so needs investigation *)
           ]
          * Proto_vals_v2.Ls_refs.request)
          send
    | Flush : unit send

  type 'a recv =
    | Capability_advertisement : Capability.t list recv
    | Ls_refs_res : Proto_vals_v2.Ls_refs.response recv
end

module Context = State.Context

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

module Value = struct
  include Witness

  type error = [ Proto_vals_v2.Encoder.error | Proto_vals_v2.Decoder.error ]
  type encoder = Pkt_line.Encoder.encoder
  type decoder = Pkt_line.Decoder.decoder

  let encode : type a. encoder -> a send -> a -> (unit, error) State.t =
   fun encoder w v ->
    let encoder_state =
      let open Proto_vals_v2.Encoder in
      match w with
      | Proto_request -> encode_proto_request encoder v
      | Ls_refs_req ->
          let `Client_caps capabilities, req = v in
          encode_ls_refs_request capabilities encoder req
      | Flush -> encode_flush encoder
    in
    let rec translate_to_state_t = function
      | Pkt_line.Encoder.Done -> State.Return ()
      | Write { continue; buffer; off; len } ->
          let k i = continue i |> translate_to_state_t in
          State.Write { k; buffer; off; len }
      | Error err -> State.Error (err :> error)
    in
    translate_to_state_t encoder_state

  let decode : type a. decoder -> a recv -> (a, error) State.t =
   fun decoder w ->
    let rec transl :
        (a, [> Proto_vals_v2.Decoder.error ]) Pkt_line.Decoder.state ->
        (a, [> Proto_vals_v2.Decoder.error ]) State.t = function
      | Pkt_line.Decoder.Done v -> State.Return v
      | Read { buffer; off; len; continue; eof } ->
          let k i = continue i |> transl in
          let eof i = eof i |> transl in
          State.Read { k; buffer; off; len; eof }
      | Error { error; _ } -> State.Error error
    in
    transl
      (match w with
      | Capability_advertisement ->
          Proto_vals_v2.Decoder.decode_capability_ads decoder
      | Ls_refs_res -> Proto_vals_v2.Decoder.decode_ls_refs_response decoder)
end

include State.Scheduler (Value)

let pp_error ppf = function
  | #Proto_vals_v2.Encoder.error as err ->
      Proto_vals_v2.Encoder.pp_error ppf err
  | #Proto_vals_v2.Decoder.error as err ->
      Proto_vals_v2.Decoder.pp_error ppf err
