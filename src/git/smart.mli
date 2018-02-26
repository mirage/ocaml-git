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

module type COMMON =
sig
  type hash
  type reference

  type advertised_refs =
    { shallow      : Hash.t list
    ; refs         : (hash * reference * bool) list
    ; capabilities : Capability.t list }
  (** When the client initially connects the server will immediately respond
     with a listing of each reference it has (all branches and tags) along with
     the object name that each reference currently points to.

      This type represents the first sentence of the server. [refs] contains all
     branches and tags. The [bool] value informs than the reference is peeled
     ([true]) or not ([false]). [capabilities] contains all informed
     capabilities by the server. [shallow] contains all informed shallowed
     hashes in the server. *)

  type shallow_update =
    { shallow   : hash list
    ; unshallow : hash list }
  (** Only when the client sent a positive depth request, the server will
     determine which commits will and will not be shallow and send this
     information to the client.

      The server writes ["shallow"] lines for each commit whose parents will not
     be sent as a result. The server writes an ["unshallow"] line for each
     commit which the client has indicated is {i shallow}, but is no longer {i
     shallow} at the currently requested depth (that is, its parents will now be
     sent). The server MUST NOT marks as ["unshallow"] anything which the client
     has not indicated was ["shallow"].

      This type represents this information. *)

  type acks =
    { shallow   : hash list
    ; unshallow : hash list
    ; acks      : (hash * [ `Common | `Ready | `Continue | `ACK ]) list }
  (** In the negociation phase, the server will ACK obj-ids differently
     depending on which ack mode is chosen by the client:

      {ul
      {- [`Multi_ack] mode:
      {ul
      {- the server will respond with [`Continue] for any common commits.}
      {- once the server has found an acceptable common base commit and is ready
     to make a packfile, it will blindly ACK all ["have"] obj-ids back to the
     client.}
      {- the server will then send a ["NAK"] and then wait for another response
     from the client - either a ["done"] or another list of ["have"] lines.}}}
      {- [`Multi_ack_detailed] mode:
      {ul
      {- the server will differentiate the ACKs where it is signaling that it is
     ready to send data with [`Ready] lines, and signals the identified common
     commits with [`Common] lines.}}}
      {- [None] mode:
      {ul {- upload-pack sends [`ACK] on the first common object it finds. After
     that it says nothing until the client gives it a ["done"].} {- upload-pack
     sends ["NAK"] on a [`Flush] if no common object has been found yet. If one
     has been found, and thus an ACK was already sent, it's silent on the
     [`Flush].}}}}

      This type represents this information. *)

  (** When the client wants to finish the negociation by [`Done], the server
     will either send a final [ACK obj_id] or it will send a [NAK]. [obj_id] is
     the object name of the last commit determined to be common. The server only
     sends this information after [`Done] if there is at least one common base
     and [`Multi_ack] or [`Multi_ack_detailed] is enabled. The server always
     sends [NAK] after [`Done] if there is no common base found.

      Instead [ACK _] or [NAK], the server may send an error message (for
     example, if it does not recognize an object in a ["want"] line received
     from the client).

      This type represents this information. *)
  type negociation_result =
    | NAK
    | ACK of hash
    | ERR of string

  (** If [`Side_band] or [`Side_band_64k] capabilities have been specified by
     the client, the server will send the packfile data multiplexed.

      Each packet starting with the {i packet-line} length of the amount of data
     that follows, followed by a single byte specifying the sideband the
     following data is comming in on.

      In [`Side_band] mode, it will send up to 999 data bytes plus 1 control
     code, for a total of up to 1000 bytes in a {i packet line}. In
     [`Side_band_64k] mode it will send up to 65519 data bytes plus 1 control
     code, for a total of up to 65520 bytes in a {i packed-line}.

      The sideband byte will be a ["1"] ([`Raw]), ["2"] ([`Out]) or a ["3"]
     ([`Err]). Sideband ["1"] will contain packfile data, sideband [`Out] will
     be used for progress information that the client will generally print to
     [stderr] and sideband [`Err] is used for error information.

      In any case, th server will stream the entire packfile in [`Raw]. *)
  type pack =
    [ `Raw of Cstruct.t
    | `Out of Cstruct.t
    | `Err of Cstruct.t ]

  (** Ther receiving the pack data from the sender, the receiver sends a report
     if [`Report_status] capability is in effect. It is a short listing of what
     happened in that update. It will first list the status of the packfile
     unpacking as either [Ok ()] or [Error msg]. Then it will list the status
     for each of the references that it tried to update. each line is either [Ok
     refname] if the update was successful, or [Errorr (refname, msg)] if the
     update was not.

      This type represents this information. *)
  type report_status =
    { unpack   : (unit, string) result
    ; commands : (reference, reference * string) result list }

  (** After reference and capabilities discovery, the client can decide to enter
     to the negociation phase, where the client and server determine what the
     minimal packfile necessary for transport is, by telling the server what
     objects it wants, its shallow objects (if any), and the maximum commit
     depth it wants (if any). The client will also send a list of the
     capabilities it wants to be in effect, out of what the server said.

      This type represents this information. *)
  type upload_request =
    { want         : hash * hash list
    ; capabilities : Capability.t list
    ; shallow      : hash list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of reference ] option }

  type request_command =
    [ `Upload_pack (** When the client wants to fetch/clone. *)
    | `Receive_pack (** When the client wants to push. *)
    | `Upload_archive (** When the client wants an archive of the remote git repository. *)
    ] (** The Git command. *)

  type git_proto_request =
    { pathname        : string
    ; host            : (string * int option) option
    ; request_command : request_command }
  (** The Git transport starts off by sending the command and the repository on
     the wire using the {i packet-line} format, followed by a NUL byte and a
     hostname parameter, terminated by a NUL byte.deep

      This type represents this information. *)

  type command =
    | Create of hash * reference (** When the client wants to create a new reference. *)
    | Delete of hash * reference (** When the client wants to delete an existing reference in the server side. *)
    | Update of hash * hash * reference (** When the client wants to update an existing reference in the server-side. *)

  type push_certificate =
    { pusher   : string
    ; pushee   : string
    ; nonce    : string
    ; options  : string list
    ; commands : command list
    ; gpg      : string list }

  type update_request =
    { shallow      : hash list
    ; requests     : [`Raw of command * command list | `Cert of push_certificate]
    ; capabilities : Capability.t list }
  (** Once the client knows what the references the server is at, it can send a
     list of reference update requests. For each reference on the server that it
     wants to update, it sends a line listing the obj-id currently on the
     server, the obj-id the client would like to update it to and the name of
     the reference.

      This type represents this information. *)

  type http_upload_request =
    { want         : hash * hash list
    ; capabilities : Capability.t list
    ; shallow      : hash list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of reference ] option
    ; has          : hash list }

  val pp_advertised_refs: advertised_refs Fmt.t
  val pp_shallow_update: shallow_update Fmt.t
  val pp_acks: acks Fmt.t
  val pp_negociation_result: negociation_result Fmt.t
  val pp_pack: pack Fmt.t
  val pp_report_status: report_status Fmt.t
  val pp_upload_request: upload_request Fmt.t
  val pp_request_command: request_command Fmt.t
  val pp_git_proto_request: git_proto_request Fmt.t
  val pp_command: command Fmt.t
  val pp_push_certificate: push_certificate Fmt.t
  val pp_update_request: update_request Fmt.t
  val pp_http_upload_request: http_upload_request Fmt.t

  type 'a equal = 'a -> 'a -> bool

  val equal_advertised_refs: advertised_refs equal
  val equal_shallow_update: shallow_update equal
  val equal_acks: acks equal
  val equal_negociation_result: negociation_result equal
  val equal_pack: pack equal
  val equal_report_status: report_status equal
  val equal_upload_request: upload_request equal
  val equal_request_command: request_command equal
  val equal_git_proto_request: git_proto_request equal
  val equal_command: command equal
  val equal_push_certificate: push_certificate equal
  val equal_update_request: update_request equal
  val equal_http_upload_request: http_upload_request equal
end

module Common (Hash: S.HASH) (Reference: Reference.S)
  : COMMON
    with type hash = Hash.t
     and type reference = Reference.t

(** The Smart protocol including the encoder and the decoder. *)

module type DECODER =
sig
  module Hash: S.HASH
  module Reference: Reference.S
  module Common: COMMON with type hash := Hash.t and type reference := Reference.t

  type decoder
  (** The type decoder. *)

  val pp_decoder : decoder Fmt.t
  (** Pretty-printer of {!decoder}. *)

  (** The type error. *)
  type error =
    [ `Expected_char of char
    (** Appears when we encountered an other character than what we
        expected stricly. *)
    | `Unexpected_char of char
    (** Appears when we encountered a character and we don't know what
        we can do with this. *)
    | `Unexpected_flush_pkt_line
    (** Appears when we encountered a flush packet and we don't expect
        this. *)
    | `No_assert_predicate of (char -> bool)
    (** Appears when one character does not respect the predicate. *)
    | `Expected_string of string
    (** Appears when we don't have strictly the string expected. *)
    | `Unexpected_empty_pkt_line
    (** Appears when we encountered an empty packet and we don't
        expect this. *)
    | `Malformed_pkt_line
    (** Appears when we encountered a flow which does not respect the
        packet format. *)
    | `Unexpected_end_of_input
    | `Unexpected_pkt_line
    | `Unexpected_hashes of (Hash.t * Hash.t)
    ]

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  type 'a state =
    | Ok of 'a
    (** The end value of the decoding. *)
    | Read of { buffer   : Cstruct.t
              ; off      : int
              ; len      : int
              ; continue : int -> 'a state }
    (** Means that we expect an input. We provide an {!Cstruct.t} with
        an offset and a length. The client is able to {!Cstruct.blit}
        the input in this range. Then, he can call [continue] with how
        many byte(s) he read. *)
    | Error of { err       : error
               ; buf       : Cstruct.t
               ; committed : int }
    (** When we retrieve an error, we return this value with how many
        byte(s) we processed and the current input. *)

  (** The type transaction to describe what is expected to
      decode/receive. *)
  type _ transaction =
    | HttpReferenceDiscovery : string -> Common.advertised_refs transaction
    | ReferenceDiscovery     : Common.advertised_refs transaction
    | ShallowUpdate          : Common.shallow_update transaction
    | Negociation            : Hash.Set.t * ack_mode -> Common.acks transaction
    | NegociationResult      : Common.negociation_result transaction
    | PACK                   : side_band -> flow transaction
    | ReportStatus           : side_band -> Common.report_status transaction
    | HttpReportStatus       : string list * side_band -> Common.report_status transaction
    | Upload_request         : Common.upload_request transaction
    | Git_proto_request      : Common.git_proto_request transaction
    | Update_request         : Common.update_request transaction

  (** The ACK mode type to describe which mode is shared by the client
      and the server. *)
  and ack_mode =
    [ `Ack
    | `Multi_ack
    | `Multi_ack_detailed
    ]

  (** The representation of the output side-band. *)
  and flow =
    [ `Raw of Cstruct.t
    | `End
    | `Err of Cstruct.t
    | `Out of Cstruct.t
    ]

  (** The side-band mode type to describe which mode is shared by the
      client and the server. *)
  and side_band =
    [ `Side_band
    | `Side_band_64k
    | `No_multiplexe
    ]

  val decode : decoder -> 'result transaction -> 'result state
  (** [decode decoder transaction] decodes the input represented by
      [decoder] in the way of the [transaction] and returns the value
      expected and described by [transaction] or an error. *)

  val decoder : unit -> decoder
  (** [decoder ()] makes a new decoder. *)

  val of_string: string -> 'v transaction -> ('v, error * Cstruct.t * int) result
end

module Decoder
    (Hash: S.HASH)
    (Reference: Reference.S)
    (Common: COMMON with type hash := Hash.t and type reference := Reference.t)
  : DECODER with module Hash = Hash
             and module Reference = Reference
             and module Common = Common
(** The {i functor} to make the Decoder by a specific hash
    implementation. *)

module type ENCODER = sig
  module Hash: S.HASH
  module Reference: Reference.S
  module Common: COMMON with type hash := Hash.t and type reference := Reference.t

  type encoder
  (** The type encoder. *)

  val set_pos : encoder -> int -> unit
  (** [set_pos encoder pos] is unsafe and change the internal position
      of the encoder. Don't use it. *)

  val free : encoder -> Cstruct.t
  (** [free encoder] returns the free internal buffer of the
      [encoder]. Don't use it. *)

  type 'a state =
    | Write of { buffer    : Cstruct.t
               ; off       : int
               ; len       : int
               ; continue  : int -> 'a state }
    (** Means that we want to flush the internal buffer of the
        encoder. We provide and {!Cstruct.t} with an offset and a
        length. The client is able to {!Cstruct.blit} this buffer to e
        output in this range. Then, he can call [continue] with how
        many byte(s) he wrote. *)
    | Ok of 'a
    (** The end value of the encoding. *)

  type action =
    [ `GitProtoRequest    of Common.git_proto_request
    | `UploadRequest      of Common.upload_request
    | `HttpUploadRequest  of [ `Done | `Flush ] * Common.http_upload_request
    | `Advertised_refs    of Common.advertised_refs
    | `Shallow_update     of Common.shallow_update
    | `Negociation        of Common.acks
    | `Negociation_result of Common.negociation_result
    | `Report_status      of [ `No_multiplexe | `Side_band | `Side_band_64k ] * Common.report_status
    | `UpdateRequest      of Common.update_request
    | `HttpUpdateRequest  of Common.update_request
    | `Has                of Hash.Set.t
    | `Done
    | `Flush
    | `Shallow            of Hash.t list
    | `PACK               of int ]
  (** The type action to describe what is expected to encode/send. *)

  val encode : encoder -> action -> unit state
  (** [encode encoder action] encodes to an output represented by
      [encoder] in the way of the [action] and returns an unit
      value. *)

  val encoder : unit -> encoder
  (** [encoder ()] makes a new decoder. *)

  val to_string: action -> string
end

module Encoder
    (Hash: S.HASH)
    (Reference: Reference.S)
    (Common: COMMON with type hash := Hash.t and type reference := Reference.t)
  : ENCODER with module Hash = Hash
             and module Reference = Reference
             and module Common = Common
(** The {i functor} to make the Encoder by a specific hash
    implementation. *)

module type CLIENT =
sig
  module Hash: S.HASH
  module Reference: Reference.S

  module Common: COMMON
    with type hash := Hash.t
     and type reference := Reference.t

  module Decoder: DECODER
    with module Hash = Hash
     and module Reference = Reference
     and module Common := Common
  (** The {!Decoder} module constrained by the same [Hash] module. *)

  module Encoder: ENCODER
    with module Hash = Hash
     and module Reference = Reference
     and module Common := Common
  (** The {!Encoder} module constrained by the same [Hash] module. *)

  type context
  (** The type context. *)

  val capabilities : context -> Capability.t list
  (** [capabilities context] returns the current shared capabilities
      between the client and the server. *)

  val set_capabilities : context -> Capability.t list -> unit
  (** [set_capabilities context cs] sets the current capabilities of
      the [context]. *)

  type result =
    [ `Refs of Common.advertised_refs
    | `ShallowUpdate of Common.shallow_update
    | `Negociation of Common.acks
    | `NegociationResult of Common.negociation_result
    | `PACK of Decoder.flow
    | `Flush
    | `Nothing
    | `ReadyPACK of Cstruct.t
    | `ReportStatus of Common.report_status
    ] (** The possible repsonse of the server. *)

  type process =
    [ `Read of (Cstruct.t * int * int * (int -> process))
    | `Write of (Cstruct.t * int * int * (int -> process))
    | `Error of (Decoder.error * Cstruct.t * int)
    | result
    ] (** The expected actions by the context. *)

  val encode : Encoder.action -> (context -> process) -> context -> process
  (** [encode action k ctx] starts to encode an [action] in the
      context [ctx] and call [k] at the end of the encoding. *)

  val decode : 'a Decoder.transaction -> ('a -> context -> process) -> context -> process
  (** [decode t k ctx] starts to decode an expected
      {!Decoder.transaction} [t] and call [k] at the end of the
      decoding. *)

  val pp_result : result Fmt.t
  (** Pretty-print of {!result}. *)

  type action =
    [ `GitProtoRequest of Common.git_proto_request
    | `Shallow of Hash.t list
    | `UploadRequest of Common.upload_request
    | `UpdateRequest of Common.update_request
    | `Has of Hash.Set.t
    | `Done
    | `Flush
    | `ReceivePACK
    | `SendPACK of int
    | `FinishPACK ]
  (** Close to {!Encoder.action} but more exhaustive and
      context-dependant. *)

  val run : context -> action -> process
  (** [run ctx action] sends an action to the server and schedule a
      specific {!Decoder.transaction} then. *)

  val context : Common.git_proto_request -> context * process
  (** [context request] makes a new context and the continuation of
      the transport. *)
end

module Client
    (Hash: S.HASH)
    (Reference: Reference.S)
  : CLIENT with module Hash = Hash
            and module Reference = Reference
(** The {i functor} to make the Client by a specific hash
    implementation. *)
