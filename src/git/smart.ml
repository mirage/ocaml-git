(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type DECODER =
sig
  module Hash : S.HASH

  type decoder

  val pp_decoder : decoder Fmt.t

  type error =
    [ `Expected_char of char
    | `Unexpected_char of char
    | `Unexpected_flush_pkt_line
    | `No_assert_predicate of (char -> bool)
    | `Expected_string of string
    | `Unexpected_empty_pkt_line
    | `Malformed_pkt_line
    | `Unexpected_end_of_input ]

  val pp_error : error Fmt.t

  type 'a state =
    | Ok of 'a
    | Read of { buffer   : Cstruct.t
              ; off      : int
              ; len      : int
              ; continue : int -> 'a state }
    | Error of { err       : error
               ; buf       : Cstruct.t
               ; committed : int }

  type advertised_refs =
    { shallow      : Hash.t list
    ; refs         : (Hash.t * string * bool) list
    ; capabilities : Capability.t list }

  val pp_advertised_refs : advertised_refs Fmt.t

  type shallow_update =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list }

  val pp_shallow_update : shallow_update Fmt.t

  type acks =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list
    ; acks      : (Hash.t * [ `Common | `Ready | `Continue | `ACK ]) list }

  val pp_acks : acks Fmt.t

  type negociation_result =
    | NAK
    | ACK of Hash.t
    | ERR of string

  val pp_negociation_result : negociation_result Fmt.t

  type pack =
    [ `Raw of Cstruct.t
    | `Out of Cstruct.t
    | `Err of Cstruct.t ]

  type report_status =
    { unpack   : (unit, string) result
    ; commands : (string, string * string) result list }

  val pp_report_status : report_status Fmt.t

  type _ transaction =
    | HttpReferenceDiscovery : string -> advertised_refs transaction
    | ReferenceDiscovery     : advertised_refs transaction
    | ShallowUpdate          : shallow_update transaction
    | Negociation            : Hash.t list * ack_mode -> acks transaction
    | NegociationResult      : negociation_result transaction
    | PACK                   : side_band -> flow transaction
    | ReportStatus           : side_band -> report_status transaction
    | HttpReportStatus       : string list * side_band -> report_status transaction
  and ack_mode =
    [ `Ack
    | `Multi_ack
    | `Multi_ack_detailed ]
  and flow =
    [ `Raw of Cstruct.t
    | `End
    | `Err of Cstruct.t
    | `Out of Cstruct.t ]
  and side_band =
    [ `Side_band
    | `Side_band_64k
    | `No_multiplexe ]

  val decode : decoder -> 'result transaction -> 'result state
  val decoder : unit -> decoder
end

module type ENCODER =
sig
  module Hash : S.HASH

  type encoder

  val set_pos : encoder -> int -> unit
  val free : encoder -> Cstruct.t

  type 'a state =
    | Write of { buffer    : Cstruct.t
               ; off       : int
               ; len       : int
               ; continue  : int -> 'a state }
    | Ok of 'a

  type upload_request =
    { want         : Hash.t * Hash.t list
    ; capabilities : Capability.t list
    ; shallow      : Hash.t list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of string ] option }

  type request_command =
    [ `UploadPack
    | `ReceivePack
    | `UploadArchive ]

  type git_proto_request =
    { pathname        : string
    ; host            : (string * int option) option
    ; request_command : request_command }

  type command =
    | Create of Hash.t * string
    | Delete of Hash.t * string
    | Update of Hash.t * Hash.t * string

  type push_certificate =
    { pusher   : string
    ; pushee   : string
    ; nonce    : string
    ; options  : string list
    ; commands : command list
    ; gpg      : string list }

  type update_request =
    { shallow      : Hash.t list
    ; requests     : [`Raw of command * command list | `Cert of push_certificate]
    ; capabilities : Capability.t list }

  type http_upload_request =
    { want         : Hash.t * Hash.t list
    ; capabilities : Capability.t list
    ; shallow      : Hash.t list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of string ] option
    ; has          : Hash.t list }

  type action =
    [ `GitProtoRequest   of git_proto_request
    | `UploadRequest     of upload_request
    | `HttpUploadRequest of [ `Done | `Flush ] * http_upload_request
    | `UpdateRequest     of update_request
    | `HttpUpdateRequest of update_request
    | `Has               of Hash.t list
    | `Done
    | `Flush
    | `Shallow           of Hash.t list
    | `PACK              of int ]

  val encode : encoder -> action -> unit state
  val encoder : unit -> encoder
end

module type CLIENT =
sig
  module Hash : S.HASH

  module Decoder : DECODER with module Hash = Hash
  module Encoder : ENCODER with module Hash = Hash

  type context

  type result =
    [ `Refs of Decoder.advertised_refs
    | `ShallowUpdate of Decoder.shallow_update
    | `Negociation of Decoder.acks
    | `NegociationResult of Decoder.negociation_result
    | `PACK of Decoder.flow
    | `Flush
    | `Nothing
    | `ReadyPACK of Cstruct.t
    | `ReportStatus of Decoder.report_status ]
  type process =
    [ `Read of (Cstruct.t * int * int * (int -> process))
    | `Write of (Cstruct.t * int * int * (int -> process))
    | `Error of (Decoder.error * Cstruct.t * int)
    | result ]
  type action =
    [ `GitProtoRequest of Encoder.git_proto_request
    | `Shallow of Hash.t list
    | `UploadRequest of Encoder.upload_request
    | `UpdateRequest of Encoder.update_request
    | `Has of Hash.t list
    | `Done
    | `Flush
    | `ReceivePACK
    | `SendPACK of int
    | `FinishPACK ]

  val capabilities : context -> Capability.t list
  val set_capabilities : context -> Capability.t list -> unit
  val encode : Encoder.action -> (context -> process) -> context -> process
  val decode : 'a Decoder.transaction -> ('a -> context -> process) -> context -> process
  val pp_result : result Fmt.t
  val run : context -> action -> process
  val context : Encoder.git_proto_request -> context * process
end

module Decoder (H : S.HASH with type hex = string)
  : DECODER with module Hash = H =
struct
  module Hash = H

  (* XXX(dinosaure): Why this decoder? We can use Angstrom instead or another
     library. It's not my first library about the parsing (see Mr. MIME) and I
     like a lot Angstrom. But I know the limitation about Angstrom and the best
     case to use it. I already saw other libraries like ocaml-imap specifically
     to find the best way to parse an email.

     You need all the time to handle the performance, the re-usability, the
     scalability and others constraints like the memory.

     So, about the smart Git protocol, I have the choice between Angstrom,
     something similar than the PACK decoder or this decoder.

     - Angstrom is good to describe the smart Git protocol. The expressivity is
       good and the performance is another good point. A part of Angstrom is
       about the alteration when you have some possibilities about the input. We
       have some examples when we compute the format of the Git object.

       And the best point is to avoid any headache to handle the input buffer
       about any alteration. I explained this specific point in the [Helper]
       module (which one provide a common non-blocking interface to decode
       something described by Angstrom).

       For all of this, it's a good way to use Angstrom in this case. But it's
       not the best. Indeed, the smart Git protocol is think in all state about
       the length of the input by the /pkt-line/ format. Which one describes all
       the time the length of the payload and limit this payload to 65520 bytes.

       So the big constraint about the alteration and when we need to keep some
       bytes in the current input buffer to retry the next alteration if the
       first one fails (and have a headache to handle the input) never happens.
       And if it's happen, the input is wrong.

     - like the PACK Decoder. If you look the PACK Decoder, it's another way to
       decode something in the non-blocking world. The good point is to handle
       all aspect of your decoder and, sometimes, describe a weird semantic
       about your decoder which is not available in Angstrom. You can do
       something hacky and wrap all in a good interface « à la Daniel Bünzli ».

       So if you want to do something fast and hacky in some contexts (like
       switch between a common functional way and a imperative way easily)
       because you know the constraint about your protocol/format, it's a good
       way. But you need a long time to do this and it is not easily composable
       like Angstrom because it's closely specific to your protocol/format.

     - like ocaml-imap. The IMAP protocol is very close to the smart Git
       protocol in some way and the interface seems to be good to have an
       user-friendly interface to communicate with a Git server without a big
       overhead because the decoder is funded on some assertions about the
       protocol (like the PKT line for the smart Git protocol or the end of line
       for the IMAP protocol).

       Then, the decoder is very hacky because we don't use the continuation all
       the time (like Angstrom) to keep a complex state but just fuck all up by
       an exception.

       And the composition between some conveniences primitives is easy (more
       easy than the second way).

     So for all of this, I decide to use this way to decode the smart Git
     protocol and provide a clear interface to the user (and keep a non-blocking
     land about all). So enjoy it!
  *)

  module Log =
  struct
    let src = Logs.Src.create "git.smart.decoder" ~doc:"logs git's smart decoder event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type decoder =
    { mutable buffer : Cstruct.t
    ; mutable pos    : int
    ; mutable eop    : int option (* end of packet *)
    ; mutable max    : int }

  let pp_decoder ppf { buffer; pos; eop; max; } =
    let pp = Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len in

    match eop with
    | Some eop ->
      Fmt.pf ppf "{ @[<hov>current = %a;@ \
                           next = %a;@] }"
        (Fmt.hvbox pp) (Cstruct.sub buffer pos eop)
        (Fmt.hvbox pp) (Cstruct.sub buffer eop max)
    | None -> Fmt.pf ppf "#raw"

  type error =
    [ `Expected_char of char
    | `Unexpected_char of char
    | `Unexpected_flush_pkt_line
    | `No_assert_predicate of (char -> bool)
    | `Expected_string of string
    | `Unexpected_empty_pkt_line
    | `Malformed_pkt_line
    | `Unexpected_end_of_input ]

  let err_unexpected_end_of_input    decoder = (`Unexpected_end_of_input, decoder.buffer, decoder.pos)
  let err_expected               chr decoder = (`Expected_char chr, decoder.buffer, decoder.pos)
  let err_unexpected_char        chr decoder = (`Unexpected_char chr, decoder.buffer, decoder.pos)
  let err_assert_predicate predicate decoder = (`No_assert_predicate predicate, decoder.buffer, decoder.pos)
  let err_expected_string          s decoder = (`Expected_string s, decoder.buffer, decoder.pos)
  let err_unexpected_empty_pkt_line  decoder = (`Unexpected_empty_pkt_line, decoder.buffer, decoder.pos)
  let err_malformed_pkt_line         decoder = (`Malformed_pkt_line, decoder.buffer, decoder.pos)
  let err_unexpected_flush_pkt_line  decoder = (`Unexpected_flush_pkt_line, decoder.buffer, decoder.pos)

  let pp_error ppf = function
    | `Expected_char chr         -> Fmt.pf ppf "(`Expected_char %c)" chr
    | `Unexpected_char chr       -> Fmt.pf ppf "(`Unexpected_char %c)" chr
    | `No_assert_predicate _     -> Fmt.pf ppf "(`No_assert_predicate #predicate)"
    | `Expected_string s         -> Fmt.pf ppf "(`Expected_string %s)" s
    | `Unexpected_empty_pkt_line -> Fmt.pf ppf "`Unexpected_empty_pkt_line"
    | `Malformed_pkt_line        -> Fmt.pf ppf "`Malformed_pkt_line"
    | `Unexpected_end_of_input   -> Fmt.pf ppf "`Unexpected_end_of_input"
    | `Unexpected_flush_pkt_line -> Fmt.pf ppf "`Unexpected_flush_pkt_line"

  type 'a state =
    | Ok of 'a
    | Read of { buffer     : Cstruct.t
              ; off        : int
              ; len        : int
              ; continue   : int -> 'a state }
    | Error of { err       : error
               ; buf       : Cstruct.t
               ; committed : int }

  exception Leave of (error * Cstruct.t * int)

  let p_return (type a) (x : a) _ : a state = Ok x

  let p_safe k decoder : 'a state =
    try k decoder
    with Leave (err, buf, pos) ->
      Error { err
            ; buf
            ; committed = pos }

  let p_end_of_input decoder = match decoder.eop with
    | Some eop -> eop
    | None -> decoder.max

  let p_peek_char decoder =
    if decoder.pos < (p_end_of_input decoder)
    then Some (Cstruct.get_char decoder.buffer decoder.pos)
    else None

  let p_current decoder =
    if decoder.pos < (p_end_of_input decoder)
    then Cstruct.get_char decoder.buffer decoder.pos
    else raise (Leave (err_unexpected_end_of_input decoder))

  let p_junk_char decoder =
    if decoder.pos < (p_end_of_input decoder)
    then decoder.pos <- decoder.pos + 1
    else raise (Leave (err_unexpected_end_of_input decoder))

  let p_char chr decoder =
    match p_peek_char decoder with
    | Some chr' when chr' = chr ->
      p_junk_char decoder
    | Some _ ->
      raise (Leave (err_expected chr decoder))
    | None ->
      raise (Leave (err_unexpected_end_of_input decoder))

  let p_satisfy predicate decoder =
    match p_peek_char decoder with
    | Some chr when predicate chr ->
      p_junk_char decoder; chr
    | Some _ ->
      raise (Leave (err_assert_predicate predicate decoder))
    | None ->
      raise (Leave (err_unexpected_end_of_input decoder))

  let p_space decoder = p_char ' ' decoder
  let p_null  decoder = p_char '\000' decoder

  let p_while1 predicate decoder =
    let i0 = decoder.pos in

    while decoder.pos < (p_end_of_input decoder)
          && predicate (Cstruct.get_char decoder.buffer decoder.pos)
    do decoder.pos <- decoder.pos + 1 done;

    if i0 < decoder.pos
    then Cstruct.sub decoder.buffer i0 (decoder.pos - i0)
    else raise (Leave (err_unexpected_char (p_current decoder) decoder))

  let p_while0 predicate decoder =
    let i0 = decoder.pos in

    while decoder.pos < (p_end_of_input decoder)
          && predicate (Cstruct.get_char decoder.buffer decoder.pos)
    do decoder.pos <- decoder.pos + 1 done;

    Cstruct.sub decoder.buffer i0 (decoder.pos - i0)

  let p_string s decoder =
    let i0 = decoder.pos in
    let ln = String.length s in

    while decoder.pos < (p_end_of_input decoder)
          && (decoder.pos - i0) < ln
          && String.get s (decoder.pos - i0) = Cstruct.get_char decoder.buffer decoder.pos
    do decoder.pos <- decoder.pos + 1 done;

    if decoder.pos - i0 = ln
    then Cstruct.sub decoder.buffer i0 ln
    else raise (Leave (err_expected_string s decoder))

  let p_hexdigit decoder =
    match p_satisfy (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false) decoder with
    | '0' .. '9' as chr -> Char.code chr - 48
    | 'a' .. 'f' as chr -> Char.code chr - 87
    | 'A' .. 'F' as chr -> Char.code chr - 55
    | _ -> assert false

  let p_pkt_payload ?(strict = false) k decoder expect =
    let pkt = if expect < 0 then `Malformed else if expect = 0 then `Empty else `Line expect in

    if expect <= 0
    then begin
      decoder.eop <- Some decoder.pos;
      k ~pkt decoder
    end else begin
      (* compress *)
      if decoder.pos > 0
      then begin
        Cstruct.blit decoder.buffer decoder.pos decoder.buffer 0 (decoder.max - decoder.pos);
        decoder.max <- decoder.max - decoder.pos;
        decoder.pos <- 0;
      end;

      let rec loop rest off =
        if rest <= 0
        then begin
          let off, pkt =
            if Cstruct.get_char decoder.buffer (off + rest - 1) = '\n' && not strict
            then begin
              if rest < 0
              then Cstruct.blit decoder.buffer (off + rest) decoder.buffer (off + rest - 1) (off - (off + rest));

              off - 1, `Line (expect - 1)
            end else off, `Line expect
          in

          decoder.max <- off;
          decoder.eop <- Some (off + rest);
          p_safe (k ~pkt) decoder
        end else begin
          if off >= Cstruct.len decoder.buffer
          then raise (Invalid_argument "PKT Format: payload upper than 65520 bytes")
          else Read { buffer = decoder.buffer
                    ; off = off
                    ; len = Cstruct.len decoder.buffer - off
                    ; continue = fun n -> loop (rest - n) (off + n) }
        end
      in

      loop (expect - (decoder.max - decoder.pos)) decoder.max
    end

  let p_pkt_len_safe ?(strict = false) k decoder =
    let a = p_hexdigit decoder in
    let b = p_hexdigit decoder in
    let c = p_hexdigit decoder in
    let d = p_hexdigit decoder in

    let expect = (a * (16 * 16 * 16)) + (b * (16 * 16)) + (c * 16) + d in

    if expect = 0
    then begin
      decoder.eop <- Some decoder.pos;
      k ~pkt:`Flush decoder
    end else
      p_pkt_payload ~strict k decoder (expect - 4)

  let p_pkt_line ?(strict = false) k decoder =
    decoder.eop <- None;

    if decoder.max - decoder.pos >= 4
    then p_pkt_len_safe ~strict k decoder
    else begin
      (* compress *)
      if decoder.pos > 0
      then begin
        Cstruct.blit decoder.buffer decoder.pos decoder.buffer 0 (decoder.max - decoder.pos);
        decoder.max <- decoder.max - decoder.pos;
        decoder.pos <- 0;
      end;

      let rec loop off =
        if off - decoder.pos >= 4
        then begin
          decoder.max <- off;
          p_safe (p_pkt_len_safe ~strict k) decoder
        end else begin
          if off >= Cstruct.len decoder.buffer
          then raise (Invalid_argument "PKT Format: payload upper than 65520 bytes")
          else Read { buffer = decoder.buffer
                    ; off = off
                    ; len = Cstruct.len decoder.buffer - off
                    ; continue = fun n -> loop (off + n) }
        end
      in

      loop decoder.max
    end

  let zero_id = String.make Hash.Digest.length '\000' |> Hash.of_string

  let p_hash decoder =
    p_while1 (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) decoder
    |> Cstruct.to_string
    |> Hash.of_hex

  let not_null = (<>) '\000'

  let p_capability decoder =
    let capability =
      p_while1
        (function '\x61' .. '\x7a' | '0' .. '9' | '-' | '_' -> true | _ -> false)
        decoder
      |> Cstruct.to_string
    in match p_peek_char decoder with
    | Some '=' ->
      p_junk_char decoder;
      let value =
        p_while1 (function '\033' .. '\126' -> true | _ -> false) decoder
        |> Cstruct.to_string in
      Capability.of_string ~value capability
    | _ ->
      Capability.of_string capability

  let p_capabilities1 decoder =
    let acc = [ p_capability decoder ] in

    let rec loop acc = match p_peek_char decoder with
      | Some ' ' ->
        p_junk_char decoder;
        let capability = p_capability decoder in
        loop (capability :: acc)
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> List.rev acc
    in

    loop acc

  let p_first_ref decoder =
    let obj_id = p_hash decoder in
    p_space decoder;
    let refname = Cstruct.to_string (p_while0 not_null decoder) in
    p_null decoder;
    let capabilities =match p_peek_char decoder with
      | Some ' ' ->
        p_junk_char decoder;
        p_capabilities1 decoder
      | Some _ ->
        p_capabilities1 decoder
      | None -> raise (Leave (err_unexpected_end_of_input decoder))
    in

    if Hash.equal obj_id zero_id
    && refname = "capabilities^{}"
    then `NoRef capabilities
    else `Ref ((obj_id, refname, false), capabilities)

  let shallow decoder =
    let _ = p_string "shallow" decoder in
    p_space decoder;
    let obj_id = p_hash decoder in
    obj_id

  let unshallow decoder =
    let _ = p_string "unshallow" decoder in
    p_space decoder;
    let obj_id = p_hash decoder in
    obj_id

  let other_ref decoder =
    let obj_id = p_hash decoder in
    p_space decoder;
    let refname = Cstruct.to_string (p_while0 (function '^' -> false | _ -> true) decoder) in

    let peeled = match p_peek_char decoder with
      | Some '^' ->
        p_char '^' decoder;
        p_char '{' decoder;
        p_char '}' decoder;
        true
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> false
    in

    (obj_id, refname, peeled)

  type advertised_refs =
    { shallow      : Hash.t list
    ; refs         : (Hash.t * string * bool) list
    ; capabilities : Capability.t list }

  let pp_advertised_refs ppf { shallow; refs; capabilities; } =
    let sep = Fmt.unit ";@ " in
    let pp_ref ppf (hash, refname, peeled) =
      match peeled with
      | true -> Fmt.pf ppf "%a %s^{}" Hash.pp hash refname
      | false -> Fmt.pf ppf "%a %s" Hash.pp hash refname
    in

    Fmt.pf ppf "{ @[<hov>shallow = [ %a ];@ \
                         refs = [ %a ];@ \
                         capabilites = [ %a ];@] }"
      (Fmt.hvbox (Fmt.list ~sep Hash.pp)) shallow
      (Fmt.hvbox (Fmt.list ~sep pp_ref)) refs
      (Fmt.hvbox (Fmt.list ~sep Capability.pp)) capabilities

  let rec p_advertised_refs ~pkt ~first ~shallow_state refs decoder =
    match pkt with
    | `Flush ->
      p_return refs decoder
    | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
    | `Line _ ->
      match p_peek_char decoder with
      | Some 's' ->
        let rest = shallow decoder in
        p_pkt_line (p_advertised_refs
                      ~first:true
                      ~shallow_state:true
                      { refs with shallow = rest :: refs.shallow })
          decoder
      | Some _ when shallow_state = false ->
        if first = false
        then match p_first_ref decoder with
          | `NoRef capabilities ->
            p_pkt_line (p_advertised_refs
                          ~first:true
                          ~shallow_state:false
                          { refs with capabilities })
              decoder
          | `Ref (first, capabilities) ->
            p_pkt_line (p_advertised_refs
                          ~first:true
                          ~shallow_state:false
                          { refs with capabilities
                                    ; refs = [ first ] })
              decoder
        else
          let rest = other_ref decoder in
          p_pkt_line (p_advertised_refs
                        ~first:true
                        ~shallow_state:false
                        { refs with refs = rest :: refs.refs })
            decoder
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> raise (Leave (err_unexpected_end_of_input decoder))

  let p_http_advertised_refs ~service ~pkt decoder =
    match pkt with
    | `Flush -> raise (Leave (err_unexpected_flush_pkt_line decoder))
    | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
    | `Line _ ->
      ignore @@ p_string "# service=" decoder;
      ignore @@ p_string service decoder;
      p_pkt_line (fun ~pkt decoder -> match pkt with
          | `Flush ->
            p_pkt_line
              (p_advertised_refs
                 ~first:false
                 ~shallow_state:false
                 { shallow = []
                 ; refs = []
                 ; capabilities = [] })
              decoder
          | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
          | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
          | `Line _ as pkt -> p_advertised_refs ~pkt ~first:false ~shallow_state:false
                                { shallow = []
                                ; refs = []
                                ; capabilities = [] }
                                decoder)
        decoder

  let p_advertised_refs decoder =
    p_pkt_line
      (p_advertised_refs
         ~first:false
         ~shallow_state:false
         { shallow = []
         ; refs = []
         ; capabilities = [] })
      decoder

  let p_http_advertised_refs ~service decoder =
    p_pkt_line
      (p_http_advertised_refs ~service)
      decoder

  type shallow_update =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list }

  let pp_shallow_update ppf { shallow; unshallow; } =
    let sep = Fmt.unit ";@ " in

    Fmt.pf ppf "{ @[<hov>shallow = [ %a ];@ \
                         unshallow = [ %a ];@] }"
      (Fmt.hvbox (Fmt.list ~sep Hash.pp)) shallow
      (Fmt.hvbox (Fmt.list ~sep Hash.pp)) unshallow

  let rec p_shallow_update ~pkt lst decoder = match pkt with
    | `Flush -> p_return lst decoder
    | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
    | `Line _ -> match p_peek_char decoder with
      | Some 's' ->
        let x = shallow decoder in
        p_pkt_line (p_shallow_update { lst with shallow = x :: lst.shallow }) decoder
      | Some 'u' ->
        let x = unshallow decoder in
        p_pkt_line (p_shallow_update { lst with unshallow = x :: lst.unshallow }) decoder
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> raise (Leave (err_unexpected_end_of_input decoder))

  let p_shallow_update decoder =
    p_pkt_line (p_shallow_update { shallow = []
                                 ; unshallow = [] })
      decoder

  let multi_ack_detailed decoder =
    ignore @@ p_string "ACK" decoder;
    p_space decoder;
    let hash = p_hash decoder in

    let detail = match p_peek_char decoder with
      | None -> raise (Leave (err_unexpected_end_of_input decoder))
      | Some ' ' ->
        p_junk_char decoder;
        (match p_peek_char decoder with
         | Some 'r' ->
           ignore @@ p_string "ready" decoder;
           `Ready
         | Some 'c' ->
           ignore @@ p_string "common" decoder;
           `Common
         | Some chr -> raise (Leave (err_unexpected_char chr decoder))
         | None -> raise (Leave (err_unexpected_end_of_input decoder)))
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
    in

    hash, detail

  let multi_ack decoder =
    ignore @@ p_string "ACK" decoder;
    p_space decoder;
    let hash = p_hash decoder in
    p_space decoder;
    ignore @@ p_string "continue" decoder;

    hash

  let ack decoder =
    ignore @@ p_string "ACK" decoder;
    p_space decoder;
    let hash = p_hash decoder in

    hash

  type acks =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list
    ; acks      : (Hash.t * [ `Common | `Ready | `Continue | `ACK ]) list }

  let pp_ack ppf (hash, detail) =
    let pp_detail ppf = function
      | `Common -> Fmt.string ppf "`Common"
      | `Ready -> Fmt.string ppf "`Ready"
      | `Continue -> Fmt.string ppf "`Continue"
      | `ACK -> Fmt.string ppf "`ACK"
    in

    (Fmt.pair Hash.pp pp_detail) ppf (hash, detail)

  let pp_acks ppf { shallow; unshallow; acks; } =
    let sep = Fmt.unit ";@ " in

    Fmt.pf ppf "{ @[<hov>shallow = [ %a ];@ \
                         unshallow = [ %a ];@ \
                         acks = [ %a ];@] }"
      (Fmt.hvbox (Fmt.list ~sep Hash.pp)) shallow
      (Fmt.hvbox (Fmt.list ~sep Hash.pp)) unshallow
      (Fmt.hvbox (Fmt.list ~sep pp_ack)) acks

  type negociation_result =
    | NAK
    | ACK of Hash.t
    | ERR of string

  let pp_negociation_result ppf = function
    | NAK -> Fmt.string ppf "NAK"
    | ACK hash -> Fmt.pf ppf "(ACK %a)" Hash.pp hash
    | ERR err -> Fmt.pf ppf "(ERR %s)" err

  let p_negociation_result ~pkt k decoder = match pkt with
    | `Flush -> raise (Leave (err_unexpected_flush_pkt_line decoder))
    | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
    | `Line _ ->
      match p_peek_char decoder with
      | Some 'N' ->
        ignore @@ p_string "NAK" decoder;
        k NAK decoder
      | Some 'A' ->
        ignore @@ p_string "ACK" decoder;
        p_space decoder;
        let hash = p_hash decoder in
        k (ACK hash) decoder
      | Some 'E' ->
        ignore @@ p_string "ERR" decoder;
        p_space decoder;
        let msg = Cstruct.to_string @@ p_while1 (fun _ -> true) decoder in
        k (ERR msg) decoder
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> raise (Leave (err_unexpected_end_of_input decoder))

  let rec p_negociation ~pkt ~mode k rest acks decoder =
    match pkt with
    | `Flush -> raise (Leave (err_unexpected_flush_pkt_line decoder))
    | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
    | `Line _ ->
      match p_peek_char decoder, mode with
      | Some 's', _ ->
        let x = shallow decoder in
        p_pkt_line (p_negociation ~mode k rest { acks with shallow = x :: acks.shallow }) decoder
      | Some 'u', _ ->
        let x = unshallow decoder in
        p_pkt_line (p_negociation ~mode k rest { acks with unshallow = x :: acks.unshallow }) decoder
      | Some 'A', `Multi_ack_detailed ->
        let (hash, detail) = multi_ack_detailed decoder in
        let rest = List.filter (fun hash' -> not (Hash.equal hash hash')) rest in
        let next =
          if List.length rest > 0
          then p_pkt_line (p_negociation ~mode k rest { acks with acks = (hash, detail) :: acks.acks })
          else k { acks with acks = List.rev acks.acks } ~pkt:`Empty in
        next decoder
      | Some 'A', `Multi_ack ->
        let hash = multi_ack decoder in
        let rest = List.filter (fun hash' -> not (Hash.equal hash hash')) rest in
        let next =
          if List.length rest > 0
          then p_pkt_line (p_negociation ~mode k rest { acks with acks = (hash, `Continue) :: acks.acks })
          else k { acks with acks = List.rev acks.acks } ~pkt:`Empty in
        next decoder
      | Some 'A', `Ack ->
        let hash = ack decoder in
        k { acks with acks = [ (hash, `ACK) ] } ~pkt:`Empty decoder
      | Some 'N', (`Multi_ack | `Multi_ack_detailed) ->
        ignore @@ p_string "NAK" decoder;
        k { acks with acks = List.rev acks.acks } ~pkt:`Empty decoder
      | Some 'N', `Ack ->
        ignore @@ p_string "NAK" decoder;
        k { acks with acks = List.rev acks.acks } ~pkt:`Empty decoder
      | Some chr, _ -> raise (Leave (err_unexpected_char chr decoder))
      | None, _ -> raise (Leave (err_unexpected_end_of_input decoder))

  let p_negociation ~mode has decoder =
    p_pkt_line (p_negociation ~mode
                  (fun value ~pkt:_ decoder -> p_return value decoder)
                  has
                  { shallow = []
                  ; unshallow = []
                  ; acks = [] })
      decoder

  type pack =
    [ `Raw of Cstruct.t
    | `Out of Cstruct.t
    | `Err of Cstruct.t ]

  let p_pack ~pkt ~mode decoder = match pkt, mode with
    | `Malformed, _ -> raise (Leave (err_malformed_pkt_line decoder))
    | `Empty, _ -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Line n, `No_multiplexe ->
      let raw = Cstruct.sub decoder.buffer decoder.pos n in
      decoder.pos <- decoder.pos + n;
      p_return (`Raw raw) decoder
    | `Flush, _ -> p_return `End decoder
    | `Line n, (`Side_band_64k | `Side_band) ->
      let raw = Cstruct.sub decoder.buffer (decoder.pos + 1) (n - 1) in

      match p_peek_char decoder with
      | Some '\001' -> decoder.pos <- decoder.pos + n; p_return (`Raw raw) decoder
      | Some '\002' -> decoder.pos <- decoder.pos + n; p_return (`Out raw) decoder
      | Some '\003' -> decoder.pos <- decoder.pos + n; p_return (`Err raw) decoder
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> raise (Leave (err_unexpected_end_of_input decoder))

  let p_negociation_result decoder =
    p_pkt_line (p_negociation_result p_return) decoder

  let p_pack ~mode decoder =
    p_pkt_line ~strict:true (p_pack ~mode) decoder

  type report_status =
    { unpack   : (unit, string) result
    ; commands : (string, string * string) result list }

  let pp_report_status ppf { unpack; commands; } =
    let sep = Fmt.unit ";@ " in

    let pp_command = Fmt.result ~ok:Fmt.string ~error:(Fmt.pair Fmt.string Fmt.string) in

    Fmt.pf ppf "{ @[<hov>unpack = %a;@ \
                         commands = [ %a ];@] }"
      (Fmt.result ~ok:(Fmt.unit "ok") ~error:Fmt.string) unpack
      (Fmt.hvbox (Fmt.list ~sep pp_command)) commands

  let p_unpack decoder : (unit, string) result =
    ignore @@ p_string "unpack" decoder;
    p_space decoder;
    let msg = p_while1 (fun _ -> true) decoder in

    match Cstruct.to_string msg with
    | "ok" -> Ok ()
    | err  -> Error err

  let p_command_status decoder : (string, string * string) result =
    let status = p_while1 (function ' ' -> false | _ -> true) decoder in

    match Cstruct.to_string status with
    | "ok" ->
      p_space decoder;
      let refname = p_while1 (fun _ -> true) decoder |> Cstruct.to_string in
      Ok refname
    | "ng" ->
      p_space decoder;
      let refname = p_while1 (function ' ' -> false | _ -> true) decoder |> Cstruct.to_string in
      p_space decoder;
      let msg = p_while1 (fun _ -> true) decoder |> Cstruct.to_string in
      Error (refname, msg)
    | _ -> raise (Leave (err_unexpected_char '\000' decoder))

  (* XXX(dinosaure): The Smart protocol is a shit. Le fin mot de
     l'histoire c'est que comme ils ont eu la flemme de faire un vrai
     truc, ils ont decide de mettre un PKT dans un PKT pour le fun si on
     a une sideband. C'est super drole. *)
  let rec p_http_report_status ~pkt ?unpack ?(commands = []) ~sideband ~references decoder =
    let go_unpack ~pkt k decoder =
      match pkt with
      | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
      | `Flush -> raise (Leave (err_unexpected_flush_pkt_line decoder))
      | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
      | `Line _ ->
        match p_peek_char decoder with
        | Some 'u' ->
          let unpack = p_unpack decoder in
          k unpack decoder
        | Some chr -> raise (Leave (err_unexpected_char chr decoder))
        | None -> raise (Leave (err_unexpected_end_of_input decoder))
    in

    let go_command ~pkt kcons kfinal decoder =
      match pkt with
      | `Malformed -> raise (Leave (err_malformed_pkt_line decoder))
      | `Flush -> kfinal decoder
      | `Empty -> raise (Leave (err_unexpected_empty_pkt_line decoder))
      | `Line _ ->
        match p_peek_char decoder with
        | Some ('o' | 'n') ->
          let command = p_command_status decoder in
          kcons command decoder
        | Some chr -> raise (Leave (err_unexpected_char chr decoder))
        | None -> raise (Leave (err_unexpected_end_of_input decoder))
    in

    match pkt, sideband, unpack with
    | `Malformed, _, _ -> raise (Leave (err_malformed_pkt_line decoder))
    | `Flush, _, _ -> raise (Leave (err_unexpected_flush_pkt_line decoder))
    | `Empty, _, _ -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Line _, (`Side_band | `Side_band_64k), unpack ->
      (match p_peek_char decoder with
       | Some '\001' ->
         p_junk_char decoder;
         (match unpack with
          | None ->
            let k unpack decoder =
              p_pkt_line ~strict:true
                (p_http_report_status ~unpack ~commands:[] ~sideband ~references)
                decoder in
            p_pkt_line (go_unpack k) decoder
          | Some unpack ->
            let kcons command decoder =
              p_pkt_line ~strict:true
                (p_http_report_status ~unpack ~sideband ~commands:(command :: commands) ~references)
                decoder in
            let kfinal decoder = p_return { unpack; commands; } decoder in
            p_pkt_line (go_command kcons kfinal) decoder)
       | Some '\002' ->
         ignore @@ p_while0 (fun _ -> true) decoder;
         p_pkt_line (p_http_report_status ?unpack ~commands ~sideband ~references) decoder
       | Some '\003' ->
         ignore @@ p_while0 (fun _ -> true) decoder;
         p_pkt_line (p_http_report_status ?unpack ~commands ~sideband ~references) decoder
       | Some chr ->
         raise (Leave (err_unexpected_char chr decoder))
       | None ->
         raise (Leave (err_unexpected_end_of_input decoder)))
    | `Line _ as pkt, _, None ->
      let k unpack decoder =
        p_pkt_line ~strict:true (p_http_report_status ~unpack ~sideband ~commands:[] ~references) decoder in
      go_unpack ~pkt k decoder
    | `Line _ as pkt, _, Some unpack ->
      let kcons command decoder =
        p_pkt_line ~strict:true (p_http_report_status ~unpack ~sideband ~commands:(command :: commands) ~references) decoder in
      let kfinal decoder = p_return { unpack; commands; } decoder in
      go_command ~pkt kcons kfinal decoder

  let p_http_report_status references sideband decoder =
    p_pkt_line ~strict:true (p_http_report_status ~references ?unpack:None ~commands:[] ~sideband) decoder

  let rec p_report_status ~pkt ~unpack ~commands ~sideband decoder =
    let go unpack commands sideband decoder =
      match p_peek_char decoder with
      | Some 'u' ->
        let unpack = p_unpack decoder in
        p_pkt_line (p_report_status ~unpack:(Some unpack) ~commands ~sideband) decoder
      | Some ('o' | 'n') ->
        let command = p_command_status decoder in
        let commands = match commands with
          | Some lst -> Some (command :: lst)
          | None -> Some [ command ]
        in

        p_pkt_line (p_report_status ~unpack ~commands ~sideband) decoder
      | Some chr -> raise (Leave (err_unexpected_char chr decoder))
      | None -> raise (Leave (err_unexpected_end_of_input decoder))
    in

    match pkt, sideband, unpack, commands with
    | `Malformed, _, _, _ -> raise (Leave (err_malformed_pkt_line decoder))
    | `Flush, _, Some unpack, Some (_ :: _ as commands) -> p_return { unpack; commands; } decoder
    | `Flush, _, _, _ -> raise (Leave (err_unexpected_flush_pkt_line decoder))
    | `Empty, _, _, _ -> raise (Leave (err_unexpected_empty_pkt_line decoder))
    | `Line _, (`Side_band | `Side_band_64k), _, _ ->
      (match p_peek_char decoder with
       | Some '\001' ->
         p_junk_char decoder;
         go unpack commands sideband decoder
       | Some '\002' ->
         ignore @@ p_while0 (fun _ -> true) decoder;
         p_pkt_line (p_report_status ~unpack ~commands ~sideband) decoder
       | Some '\003' ->
         ignore @@ p_while0 (fun _ -> true) decoder;
         p_pkt_line (p_report_status ~unpack ~commands ~sideband) decoder
       | Some chr -> raise (Leave (err_unexpected_char chr decoder))
       | None -> raise (Leave (err_unexpected_empty_pkt_line decoder)))
    | `Line _, `No_multiplexe, _, _ ->
      go unpack commands sideband decoder

  let p_report_status sideband decoder =
    p_pkt_line (p_report_status ~unpack:None ~commands:None ~sideband) decoder

  (* XXX(dinosaure): désolé mais ce GADT, c'est quand même la classe. *)
  type _ transaction =
    | HttpReferenceDiscovery : string -> advertised_refs transaction
    | ReferenceDiscovery : advertised_refs transaction
    | ShallowUpdate      : shallow_update transaction
    | Negociation        : Hash.t list * ack_mode -> acks transaction
    | NegociationResult  : negociation_result transaction
    | PACK               : side_band -> flow transaction
    | ReportStatus       : side_band -> report_status transaction
    | HttpReportStatus   : string list * side_band -> report_status transaction
  and ack_mode =
    [ `Ack | `Multi_ack | `Multi_ack_detailed ]
  and flow =
    [ `Raw of Cstruct.t | `End | `Err of Cstruct.t | `Out of Cstruct.t ]
  and side_band =
    [ `Side_band | `Side_band_64k | `No_multiplexe ]

  let decode
    : type result. decoder -> result transaction -> result state
    = fun decoder -> function
    | HttpReferenceDiscovery service -> p_safe (p_http_advertised_refs ~service) decoder
    | ReferenceDiscovery             -> p_safe p_advertised_refs decoder
    | ShallowUpdate                  -> p_safe p_shallow_update decoder
    | Negociation (has, ackmode)     -> p_safe (p_negociation ~mode:ackmode has) decoder
    | NegociationResult              -> p_safe p_negociation_result decoder
    | PACK sideband                  -> p_safe (p_pack ~mode:sideband) decoder
    | ReportStatus sideband          -> p_safe (p_report_status sideband) decoder
    | HttpReportStatus (refs, sideband) -> p_safe (p_http_report_status refs sideband) decoder

  let decoder () =
    { buffer = Cstruct.create 65535
    ; pos    = 0
    ; max    = 0
    ; eop    = None }
end

module Encoder (H : S.HASH with type hex = string)
  : ENCODER with module Hash = H =
struct
  module Hash = H

  type encoder =
    { mutable payload : Cstruct.t
    ; mutable pos     : int }

  let set_pos encoder pos =
    encoder.pos <- pos

  let free { payload; pos; } =
    Cstruct.sub payload pos (Cstruct.len payload - pos)

  type 'a state =
    | Write of { buffer   : Cstruct.t
               ; off      : int
               ; len      : int
               ; continue : int -> 'a state }
    | Ok of 'a

  let flush k encoder =
    if encoder.pos > 0
    then let rec k1 n =
           if n < encoder.pos
           then Write { buffer = encoder.payload
                      ; off = n
                      ; len = encoder.pos - n
                      ; continue = fun m -> k1 (n + m) }
           else begin
             encoder.pos <- 4;
             k encoder
           end
      in
      k1 0
    else
      k encoder

  let writes s k encoder =
    let _len = Cstruct.len encoder.payload in
    let go j l encoder =
      let rem = _len - encoder.pos in
      let len = if l > rem then rem else l in
      Cstruct.blit_from_string s j encoder.payload encoder.pos len;
      encoder.pos <- encoder.pos + len;
      if len < l
      then raise (Invalid_argument "PKT Format: payload upper than 65520 bytes")
      else k encoder
    in
    go 0 (String.length s) encoder

  let w_lf k e = writes "\n" k e

  let noop k encoder = k encoder

  let pkt_line ?(lf = false) writes k encoder =
    let pkt_len encoder =
      let has = encoder.pos in
      let hdr = Fmt.strf "%04x" has in

      Cstruct.blit_from_string hdr 0 encoder.payload 0 4;
      flush k encoder
    in

    writes ((if lf then w_lf else noop) @@ pkt_len) encoder

  let pkt_flush k encoder =
    Cstruct.blit_from_string "0000" 0 encoder.payload 0 4;
    flush k encoder

  let zero_id = String.make Hash.Digest.length '\000' |> Hash.of_string

  let w_space k encoder =
    writes " " k encoder

  let w_null k encoder =
    writes "\000" k encoder

  let w_capabilities lst k encoder =
    let rec loop lst encoder = match lst with
      | [] -> k encoder
      | [ x ] -> writes (Capability.to_string x) k encoder
      | x :: r ->
        (writes (Capability.to_string x)
         @@ w_space
         @@ loop r)
        encoder
    in

    loop lst encoder

  let w_hash hash k encoder =
    writes (Hash.to_hex hash) k encoder

  let w_first_want obj_id capabilities k encoder =
    (writes "want"
     @@ w_space
     @@ w_hash obj_id
     @@ w_space
     @@ w_capabilities capabilities k)
      encoder

  let w_want obj_id k encoder =
    (writes "want"
     @@ w_space
     @@ w_hash obj_id k)
      encoder

  let w_shallow obj_id k encoder =
    (writes "shallow"
     @@ w_space
     @@ w_hash obj_id k)
      encoder

  let w_deepen depth k encoder =
    (writes "deepen"
     @@ w_space
     @@ writes (Fmt.strf "%d" depth) k)
      encoder

  let w_deepen_since timestamp k encoder =
    (writes "deepen-since"
     @@ w_space
     @@ writes (Fmt.strf "%Ld" timestamp) k)
      encoder

  let w_deepen_not refname k encoder =
    (writes "deepen-not"
     @@ w_space
     @@ writes refname k)
      encoder

  let w_first_want ?lf obj_id capabilities k encoder =
    pkt_line ?lf (w_first_want obj_id capabilities) k encoder
  let w_want ?lf obj_id k encoder =
    pkt_line ?lf (w_want obj_id) k encoder
  let w_shallow ?lf obj_id k encoder =
    pkt_line ?lf (w_shallow obj_id) k encoder
  let w_deepen ?lf depth k encoder =
    pkt_line ?lf (w_deepen depth) k encoder
  let w_deepen_since ?lf timestamp k encoder =
    pkt_line ?lf (w_deepen_since timestamp) k encoder
  let w_deepen_not ?lf refname k encoder =
    pkt_line ?lf (w_deepen_not refname) k encoder
  let w_done_and_lf k encoder =
    pkt_line ~lf:true (writes "done") k encoder

  type upload_request =
    { want         : Hash.t * Hash.t list
    ; capabilities : Capability.t list
    ; shallow      : Hash.t list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of string ] option }

  let w_list w l k encoder =
    let rec aux l encoder = match l with
      | [] -> k encoder
      | x :: r ->
        w x (aux r) encoder
    in
    aux l encoder

  let w_upload_request ?lf upload_request k encoder =
    let first, rest = upload_request.want in

    (w_first_want ?lf first upload_request.capabilities
     @@ (w_list (w_want ?lf) rest)
     @@ (w_list (w_shallow ?lf) upload_request.shallow)
     @@ (match upload_request.deep with
         | Some (`Depth depth)  -> w_deepen ?lf depth
         | Some (`Timestamp t) -> w_deepen_since ?lf t
         | Some (`Ref refname)  -> w_deepen_not ?lf refname
         | None -> noop)
     @@ pkt_flush k)
      encoder

  type http_upload_request =
    { want         : Hash.t * Hash.t list
    ; capabilities : Capability.t list
    ; shallow      : Hash.t list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of string ] option
    ; has          : Hash.t list }

  let w_has hash k encoder =
    (writes "have"
     @@ w_space
     @@ w_hash hash k)
      encoder

  let w_has ?lf hash k encoder = pkt_line ?lf (w_has hash) k encoder

  let w_http_upload_request at_the_end http_upload_request k encoder =
    (w_upload_request ~lf:true { want         = http_upload_request.want
                               ; capabilities = http_upload_request.capabilities
                               ; shallow      = http_upload_request.shallow
                               ; deep         = http_upload_request.deep }
     @@ (w_list (w_has ~lf:true) http_upload_request.has)
     @@ (if at_the_end = `Done
         then w_done_and_lf k
         else pkt_flush k))
      encoder

  let w_flush k encoder =
    pkt_flush k encoder

  type request_command =
    [ `UploadPack
    | `ReceivePack
    | `UploadArchive ]

  type git_proto_request =
    { pathname        : string
    ; host            : (string * int option) option
    ; request_command : request_command }

  let w_request_command request_command k encoder = match request_command with
    | `UploadPack    -> writes "git-upload-pack"    k encoder
    | `ReceivePack   -> writes "git-receive-pack"   k encoder
    | `UploadArchive -> writes "git-upload-archive" k encoder

  let w_git_proto_request git_proto_request k encoder =
    let w_host host k encoder = match host with
      | Some (host, Some port) ->
        (writes "host="
         @@ writes host
         @@ writes ":"
         @@ writes (Fmt.strf "%d" port)
         @@ w_null k)
          encoder
      | Some (host, None) ->
        (writes "host="
         @@ writes host
         @@ w_null k)
          encoder
      | None -> noop k encoder
    in

    (w_request_command git_proto_request.request_command
     @@ w_space
     @@ writes git_proto_request.pathname
     @@ w_null
     @@ w_host git_proto_request.host k)
      encoder

  let w_done k encoder = pkt_line (writes "done") k encoder

  let w_has l k encoder =
    let rec go l encoder = match l with
      | [] -> w_flush k encoder
      | x :: r ->
        (w_has x @@ go r) encoder
    in
    go l encoder

  let w_git_proto_request git_proto_request k encoder =
    pkt_line (w_git_proto_request git_proto_request) k encoder

  let w_shallow l k encoder =
    let rec go l encoder = match l with
      | [] -> k encoder
      | x :: r ->
        pkt_line
          (fun k -> writes "shallow"
            @@ w_space
            @@ w_hash x k)
          (go r)
          encoder
    in

    go l encoder

  type update_request =
    { shallow      : Hash.t list
    ; requests     : [`Raw of command * command list | `Cert of push_certificate]
    ; capabilities : Capability.t list }
  and command =
    | Create of Hash.t * string (* XXX(dinosaure): break the dependence with [Store] and consider the reference name as a string. *)
    | Delete of Hash.t * string
    | Update of Hash.t * Hash.t * string
  and push_certificate =
    { pusher   : string
    ; pushee   : string (* XXX(dinosaure): the repository url anonymized. *)
    ; nonce    : string
    ; options  : string list
    ; commands : command list
    ; gpg      : string list }

  let w_command command k encoder =
    match command with
    | Create (hash, refname) ->
      (w_hash zero_id
       @@ w_space
       @@ w_hash hash
       @@ w_space
       @@ writes refname k)
        encoder
    | Delete (hash, refname) ->
      (w_hash hash
       @@ w_space
       @@ w_hash zero_id
       @@ w_space
       @@ writes refname k)
        encoder
    | Update (old_id, new_id, refname) ->
      (w_hash old_id
       @@ w_space
       @@ w_hash new_id
       @@ w_space
       @@ writes refname k)
        encoder

  let w_first_command capabilities first k encoder =
    (w_command first
     @@ w_null
     @@ w_capabilities capabilities k)
      encoder

  let w_first_command capabilities first k encoder =
    pkt_line (w_first_command capabilities first) k encoder

  let w_command command k encoder =
    pkt_line (w_command command) k encoder

  let w_commands capabilities (first, rest) k encoder =
    (w_first_command capabilities first
     @@ w_list w_command rest
     @@ pkt_flush k)
      encoder

  let w_push_certificates capabilities push_cert k encoder =
    (* XXX(dinosaure): clean this code, TODO! *)

    ((fun k e -> pkt_line ~lf:true (fun k -> writes "push-cert" @@ w_null @@ w_capabilities capabilities k) k e)
     @@ (fun k e -> pkt_line ~lf:true (writes "certificate version 0.1") k e)
     @@ (fun k e -> pkt_line ~lf:true (fun k -> writes "pusher" @@ w_space @@ writes push_cert.pusher k) k e)
     @@ (fun k e -> pkt_line ~lf:true (fun k -> writes "pushee" @@ w_space @@ writes push_cert.pushee k) k e)
     @@ (fun k e -> pkt_line ~lf:true (fun k -> writes "nonce" @@ w_space @@ writes push_cert.nonce k) k e)
     @@ (fun k e -> w_list (fun x k e -> pkt_line ~lf:true (fun k -> writes "push-option" @@ w_space @@ writes x k) k e) push_cert.options k e)
     @@ (fun k e -> pkt_line ~lf:true noop k e)
     @@ (fun k e -> w_list (fun x k e -> pkt_line ~lf:true (w_command x) k e) push_cert.commands k e)
     @@ (fun k e -> w_list (fun x k e -> pkt_line ~lf:true (writes x) k e) push_cert.gpg k e)
     @@ (fun k e -> pkt_line ~lf:true (writes "push-cert-end") k e)
     @@ pkt_flush
     @@ k)
    encoder

  let w_update_request update_request k encoder =
    (w_shallow update_request.shallow
     @@ (match update_request.requests with
         | `Raw commands   -> w_commands update_request.capabilities commands
         | `Cert push_cert -> w_push_certificates update_request.capabilities push_cert)
     @@ k)
      encoder

  let flush_pack k encoder =
    if encoder.pos > 0
    then let rec k1 n =
           if n < encoder.pos
           then Write { buffer = encoder.payload
                      ; off = n
                      ; len = encoder.pos - n
                      ; continue = fun m -> k1 (n + m) }
           else begin
             encoder.pos <- 0;
             k encoder
           end
      in
      k1 0
    else
      k encoder

  let w_pack n k encoder =
    encoder.pos <- encoder.pos + n;
    flush_pack k encoder

  let w_http_update_request i k encoder =
    w_update_request i k encoder

  type action =
    [ `GitProtoRequest   of git_proto_request
    | `UploadRequest     of upload_request
    | `HttpUploadRequest of [ `Done | `Flush ] * http_upload_request
    | `UpdateRequest     of update_request
    | `HttpUpdateRequest of update_request
    | `Has               of Hash.t list
    | `Done
    | `Flush
    | `PACK              of int
    | `Shallow           of Hash.t list ]

  let encode encoder = function
    | `GitProtoRequest c        -> w_git_proto_request c     (fun _ -> Ok ()) encoder
    | `UploadRequest i          -> w_upload_request i        (fun _ -> Ok ()) encoder
    | `HttpUploadRequest (v, i) -> w_http_upload_request v i (fun _ -> Ok ()) encoder
    | `UpdateRequest i          -> w_update_request i        (fun _ -> Ok ()) encoder
    | `HttpUpdateRequest i      -> w_http_update_request i   (fun _ -> Ok ()) encoder
    | `Has l                    -> w_has l                   (fun _ -> Ok ()) encoder
    | `Done                     -> w_done                    (fun _ -> Ok ()) encoder
    | `Flush                    -> w_flush                   (fun _ -> Ok ()) encoder
    | `Shallow l                -> w_shallow l               (fun _ -> Ok ()) encoder
    | `PACK n                   -> w_pack n                  (fun _ -> Ok ()) encoder

  let encoder () =
    { payload = Cstruct.create 65535
    ; pos     = 4 }
end

module Client (H : S.HASH with type hex = string)
  : CLIENT with module Hash = H =
struct
  module Hash = H
  module Decoder = Decoder(H)
  module Encoder = Encoder(H)

  type context =
    { decoder      : Decoder.decoder
    ; encoder      : Encoder.encoder
    ; mutable capabilities : Capability.t list }

  let capabilities { capabilities; _ } = capabilities
  let set_capabilities context capabilities =
    context.capabilities <- capabilities

  let encode x k ctx =
    let rec loop = function
      | Encoder.Write { buffer; off; len; continue; } ->
        `Write (buffer, off, len, fun n -> loop (continue n))
      | Encoder.Ok () -> k ctx
    in
    loop (Encoder.encode ctx.encoder x)

  let decode phase k ctx =
    let rec loop = function
      | Decoder.Ok v -> k v ctx
      | Decoder.Read { buffer; off; len; continue; } ->
        `Read (buffer, off, len, fun n -> loop (continue n))
      | Decoder.Error { err; buf; committed; } ->
        `Error (err, buf, committed)
    in
    loop (Decoder.decode ctx.decoder phase)

  type result =
    [ `Refs of Decoder.advertised_refs
    | `ShallowUpdate of Decoder.shallow_update
    | `Negociation of Decoder.acks
    | `NegociationResult of Decoder.negociation_result
    | `PACK of Decoder.flow
    | `Flush
    | `Nothing
    | `ReadyPACK of Cstruct.t
    | `ReportStatus of Decoder.report_status ]

  type process =
    [ `Read  of (Cstruct.t * int * int * (int -> process ))
    | `Write of (Cstruct.t * int * int * (int -> process))
    | `Error of (Decoder.error * Cstruct.t * int)
    | result ]

  let pp_result ppf = function
    | `Refs refs ->
      Fmt.pf ppf "(`Refs %a)" (Fmt.hvbox Decoder.pp_advertised_refs) refs
    | `ShallowUpdate shallow_update ->
      Fmt.pf ppf "(`ShallowUpdate %a)" (Fmt.hvbox Decoder.pp_shallow_update) shallow_update
    | `Negociation acks ->
      Fmt.pf ppf "(`Negociation %a)"
        (Fmt.hvbox Decoder.pp_acks) acks
    | `NegociationResult result ->
      Fmt.pf ppf "(`NegociationResult %a)" (Fmt.hvbox Decoder.pp_negociation_result) result
    | `PACK (`Err _) ->
      Fmt.pf ppf "(`Pack stderr)"
    | `PACK (`Out _) ->
      Fmt.pf ppf "(`Pack stdout)"
    | `PACK (`Raw _) ->
      Fmt.pf ppf "(`Pack pack)"
    | `PACK `End ->
      Fmt.pf ppf "(`Pack `End)"
    | `Flush ->
      Fmt.pf ppf "`Flush"
    | `Nothing ->
      Fmt.pf ppf "`Nothing"
    | `ReadyPACK _ ->
      Fmt.pf ppf "(`ReadyPACK #raw)"
    | `ReportStatus status ->
      Fmt.pf ppf "(`ReportStatus %a)" (Fmt.hvbox Decoder.pp_report_status) status

  type action =
    [ `GitProtoRequest of Encoder.git_proto_request
    | `Shallow of Hash.t list
    | `UploadRequest of Encoder.upload_request
    | `UpdateRequest of Encoder.update_request
    | `Has of Hash.t list
    | `Done
    | `Flush
    | `ReceivePACK
    | `SendPACK of int
    | `FinishPACK ]

  let run context = function
    | `GitProtoRequest c ->
      encode
        (`GitProtoRequest c)
        (decode Decoder.ReferenceDiscovery
           (fun refs ctx ->
              ctx.capabilities <- refs.Decoder.capabilities;
              `Refs refs))
        context
    | `Flush ->
      encode `Flush (fun _ -> `Flush) context
    | `UploadRequest (descr : Encoder.upload_request) ->
      let common = List.filter (fun x -> List.exists ((=) x) context.capabilities) descr.Encoder.capabilities in
      (* XXX(dinosaure): we update with the shared capabilities between the
         client and the server. *)

      context.capabilities <- common;

      let next = match descr.Encoder.deep with
        | Some (`Depth n) ->
          if n > 0
          then decode Decoder.ShallowUpdate (fun shallow_update _ -> `ShallowUpdate shallow_update)
          else (fun _ -> `ShallowUpdate { Decoder.shallow = []; unshallow = []; })
        | _ -> (fun _ -> `ShallowUpdate { Decoder.shallow = []; unshallow = []; })
      in
      encode (`UploadRequest descr) next context
    | `UpdateRequest (descr : Encoder.update_request) ->
      let common = List.filter (fun x -> List.exists ((=) x) context.capabilities) descr.Encoder.capabilities in

      (* XXX(dinosaure): same as below. *)

      context.capabilities <- common;

      encode (`UpdateRequest descr) (fun { encoder; _ } ->
          Encoder.set_pos encoder 0;
          let raw = Encoder.free encoder in

          `ReadyPACK raw) context
    | `Has has ->
      let ackmode =
        if List.exists ((=) `Multi_ack_detailed) context.capabilities
        then `Multi_ack_detailed
        else if List.exists ((=) `Multi_ack) context.capabilities
        then `Multi_ack
        else `Ack
      in

      encode (`Has has) (decode (Decoder.Negociation (has, ackmode)) (fun status _ -> `Negociation status)) context
    | `Done ->
      encode `Done (decode Decoder.NegociationResult (fun result _ -> `NegociationResult result)) context
    | `ReceivePACK ->
      let sideband =
        if List.exists ((=) `Side_band_64k) context.capabilities
        then `Side_band_64k
        else if List.exists ((=) `Side_band) context.capabilities
        then `Side_band
        else `No_multiplexe
      in
      (decode (Decoder.PACK sideband) (fun flow _ -> `PACK flow)) context
    | `SendPACK w ->
      encode (`PACK w)
        (fun { encoder; _ } ->
          Encoder.set_pos encoder 0;
          let raw = Encoder.free encoder in

          `ReadyPACK raw)
        context
    | `FinishPACK ->
      let sideband =
        if List.exists ((=) `Side_band_64k) context.capabilities
        then `Side_band_64k
        else if List.exists ((=) `Side_band) context.capabilities
        then `Side_band
        else `No_multiplexe
      in

      if List.exists ((=) `Report_status) context.capabilities
      then decode (Decoder.ReportStatus sideband) (fun result _ -> `ReportStatus result) context
      else `Nothing
    (* XXX(dinosaure): the specification does not explain what the server send
       when we don't have the capability [report-status]. *)
    | `Shallow l ->
      encode (`Shallow l) (fun _ -> `Nothing) context

  let context c =
    let context =
      { decoder = Decoder.decoder ()
      ; encoder = Encoder.encoder ()
      ; capabilities = [] }
    in

    context, encode (`GitProtoRequest c)
      (decode Decoder.ReferenceDiscovery
         (fun refs ctx ->
            ctx.capabilities <- refs.Decoder.capabilities;
            `Refs refs))
      context
end
