type capability =
  [ `Multi_ack
  (** The ["multi-ack"] capability allows the server to return ["ACK obj-id
      continue"] as soon as it finds a commit that it can use as a common base,
      between the client's wants and the client's have set.

      By sending this early, the server can potentially head off the client from
      walking any further down that particular branch of the client's repository
      history. The client may still need to walk down other branches, sending have
      lines for those, until the server has a complete cut across the DAG, or the
      client has said ["done"]. *)
  | `Multi_ack_detailed
  (** This is an extension of [`Multi_ack] that permits client to better
      understand ther server's in-memory state. *)
  | `No_done
  (** This capability should only be used with the smart HTTP protocol. If
      [`Multi_ack_detailed] and [`No_done] are both present, then the sender is free
      to immediately send a pack following its first ["ACK obj-id ready"]
      message.

      Without [`No_done] in the smart HTTP protocol, the server session would
      end and the client has to make another trip to send ["done"] before the
      server can send the pack. [`No_done] removes the last round and thus
      slightly reduces latency. *)
  | `Thin_pack
  (** A thin pack is one with deltas which reference base objects not contained
      within the pack (but are known to exist at the receiving end). This can reduce
      the network traffic significantly, but it requires the receiving end to know
      how to "thicken" these packs by adding the missing bases to the pack.

      The [`UploadPack] server advertises [`Thin_pack] when it can generate and
      send a thin pack. A client requests the [`Thin_pack] capability when it
      understands how to ["thicken"] it, notifying the server that it can
      receive such a pack. A client MUST NOT request the [`Thin_pack] capability
      if it cannot turn a thin pack into a self-contained pack.

      [`ReceivePack], on the other hand, is assumed by default to be able to
      handle thin packs, but can ask the client not to use the feature by
      advertising the [`No_thin] capability. A client MUST NOT send a thin pack
      if the server advertises the [`No_thin] capability. *)
  | `Side_band
  (** See [`Side_band_64k]. *)
  | `Side_band_64k
  (** This capability means that server can send, and client understand
      multiplexed progress reports and error into interleaved with the packfile
      itself.

      These two options are mutually exclusive. A modern client always favors
      [`Side_band_64k].

      Either mode indicates that the packfile data will be streamed broken up
      into packets of up to either 1000 bytes in the case of [`Side_band], or
      65520 bytes in the case of [`Side_band_64k]. Each packet is made up of a
      leading 4-byte {i pkt-line} length of how much data is in the packet,
      followed by a 1-byte stream code, followed by the actual data.

      Further, with [`Side_band] and its up to 1000-byte messages, it's actually
      999 bytes of payload and 1 byte for the stream code. With
      [`Side_band_64k], same deal, you have up to 65519 bytes of data and 1 byte
      for the stream code.

      The client MUST send only maximum of one of [`Side_band] and
      [`Side_band_64k]. Server MUST diagnose it as an error if client requests
      both. *)
  | `Ofs_delta
  (** Server can send, and client understand PACKv2 with delta referring to its
      base by position in path rather than by an obj-id. That is, they can send/read
      OBJ_OFS_DETLA (aka type 6) in a packfile. *)
  | `Agent of string
  (** The server may optionnaly send a capability of the form ["agent=X"] to
      notify the client that the server is running version ["X"]. The client may
      optionnaly return its own agent string by responding with an ["agent=Y"]
      capability (but it MUST NOT do so if the server did not mention the agent
      capability). the ["X"] and ["Y"] strings may contain any printable ASCII
      characters except space (i.e. the byte range [32 < x < 127]), and are
      typically of the form ["package/version"] (e.g., ["git/1.8.3.1"]). The agent
      strings are purely informative for statistics and debugging purposes, and MUST
      NOT be used to programmatically assume the presence or absence of particular
      features. *)
  | `Shallow
  (**i This capability adds ["deepen"], ["shallow"] and ["unshallow"] commands
     to the fetch-pack/upload-pack protocol so clients can request shallow
     clones. *)
  | `Deepen_since
  (** This capability adds ["deepen-since"] command to fetch-pack/upload-pack
      protocol so the client can request shallow clones that are cut at a specific
      time, instead of depth. Internally it's equivalent of doing ["git rev-list
      --max-age=<timestamp>"] on the server side. [`Deepen_since] cannot be used
      with [`Deepen]. *)
  | `Deepen_not
  (** This capability adds [`Deepen_not] command to fetch-pacj/upload-pack
      protocol so the client can request shallow clones that are cut at a specific
      revision, instead of depth. Internanlly it's quivalent of doing ["git
      rev-list --not <rev>"] on the server side. [`Deepen_not] cannot be used with
      [`Deepen], but can be used with [`Deepen_since]. *)
  | `No_progress
  (** The client was started with ["git clone -q"] or something, and does not
      want that side band 2. Basically the client just says ["I do not wish to
      receive stream 2 on sideband, so do not send it to me, and if you did, I will
      drop it on the floor anyway"]. However, the sideband channel 3 is still used
      for error responses. *)
  | `Include_tag
  (** The [`Include_tag] capability is about sending annotated tags if we are
      sending objects they point to. If we pack an object to the client, and a tag
      object points exactly at that object, we pack the tag object too. In general
      this allows a client to get all new annotated tags when it fetches a branch,
      in a single network connection.

      Clients MAY always send [`Include_tags], hardcoding it into a request when
      the server advertises this capability. The decision for a client to
      request [`Include_tag] only has to do with the client's desires for tag
      ["refs/tags/*"] namespace.

      Servers MUST pack the tags if their referrant is packed and the client has
      requested [`Include_tag].

      Clients MUST be prepared for the case where a server has ignored
      [`Include_tag] and has not actually sent tags in the pack. In such cases
      the client SHOULD issue a subsequent fetch to acquire the tags that
      [`Include_tag] would have otherwise given the client.

      The server SHOULD send [`Include_tag], if it supports it, regardless of
      whether or not there are tags available. *)
  | `Report_status
  (** The [`ReceivePack] process can receive a [`Report_status] capability,
      which tells it that the client wants a report of what happened after a
      packfile upload and reference update. If the pushing client requests this
      capability, after unpacking and updating references the server will respond
      with whether the packfile unpacked successfully and if each reference was
      updated successfully. If any of those were not successful, it will send back
      an error message. *)
  | `Delete_refs
  (** If the server sends back the [`Delete_refs] capability, it means that it
      is capable of accepting a zero-id value as the target value of a reference
      update. It is not sent back by the client, it simply informs the client that
      it can be sent zero-id values to delete references. *)
  | `Quiet
  (** If the [`ReceivePack] server advertises the [`Quiet] capability, it is
      capable of silencing human-readable progress output which otherwise may be
      shown when processing the receiving pack. A send-pack client should respond
      with the [`Quiet] capability to suppress server-side progress reporting if the
      local progress reporting is also being suppressed (e.g., via ["git push -q"],
      or if [stderr] does not go to a tty). *)
  | `Atomic
  (** If the server sends the [`Atomic] capability it is capable of acceping
      atomic pushes. If the pushing client requests this capability, the server will
      update the refs in one atomic transaction. Either all refs are updated or
      none. *)
  | `Push_options
  (** If the server sends the [`Push_options] capability it is able to accept
      push options after the update commands have been sent, but before the packfile
      is streamed. If the pushing client requests this capability, the server will
      pass the options to the pre- and post- receive hooks that process this push
      request. *)
  | `Allow_tip_sha1_in_want
  (** If the upload-pack server advertises this capability, fetch-pack may send
      ["want"] lines with hashes that exists at the server but are not advertised by
      upload-pack. *)
  | `Allow_reachable_sha1_in_want
  (** If the upload-pack server advertises this capability, fetch-pack may send
      ["want"] lines with hashes that exists at the server but are not advertised by
      upload-pack. *)
  | `Push_cert of string
  (** The receive-pack server that advertises this capability is willing to
      accept a signed push certificate, and asks the <nonce> to be included in the
      push certificate. A send-pack client MUST NOT send a push-cert packet unless
      the receive-pack server advertises this capability. *)
  | `Other of string
  (** Unrecognized capability. *)
  | `Parameter of string * string
  (** Unrecognized capability with a value. *)
  ]

val string_of_capability : capability -> string
(** [string_of_capability c] returns a string representaiton of the capability
    [c]. *)

exception Capability_expect_value of string
(** Exception to inform than the capability expects a value. *)

val capability_of_string : ?value:string -> string -> capability
(** [capability_of_capability s] tries to decode [s] to a capability. If the
    capability excepts a value, we raise [Capability_expect_value]. *)

val pp_capability : capability Fmt.t
(** Pretty-printer of {!capability}. *)

module type DECODER =
sig
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  type decoder
  (** The type decoder. *)

  type error =
    [ `Expected_char of char
    (** Appears when we encountered an other character than what we expected stricly. *)
    | `Unexpected_char of char
    (** Appears when we encountered a character and we don't know what we can do with this. *)
    | `Unexpected_flush_pkt_line
    (** Appears when we encountered a flush packet and we don't expect this. *)
    | `No_assert_predicate of (char -> bool)
    (** Appears when one character does not respect the predicate. *)
    | `Expected_string of string
    (** Appears when we don't have strictly the string expected. *)
    | `Unexpected_empty_pkt_line
    (** Appears when we encountered an empty packet and we don't expect this. *)
    | `Malformed_pkt_line
    (** Appears when we encountered a flow which does not respect the packet format. *)
    | `Unexpected_end_of_input
      (** Appears when we encountered the end of the input and we don't expect this. *)
    ] (** The type error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  type 'a state =
    | Ok of 'a
    (** The end value of the decoding. *)
    | Read of { buffer   : Cstruct.t
              ; off      : int
              ; len      : int
              ; continue : int -> 'a state }
    (** Means that we expect an input. We provide an {!Cstruct.t} with an
        offset and a length. The client is able to {!Cstruct.blit} the input in
        this range. Then, he can call [continue] with how many byte(s) he
        read. *)
    | Error of { err       : error
               ; buf       : Cstruct.t
               ; committed : int }
    (** When we retrieve an error, we return this value with how many byte(s) we
        processed and the current input. *)

  type advertised_refs =
    { shallow      : Hash.t list
    ; refs         : (Hash.t * string * bool) list
    ; capabilities : capability list }
  (** When the client initially connects the server will immediately respond
      with a listing of each reference it has (all branches and tags) along with the
      object name that each refeence currently points to.

      This type represents the first sentence of the server. [refs] contains all
      branches and tags. The [bool] value informs than the reference is peeled
      ([true]) or not ([false]). [capabilities] contains all informed
      capabilities by the server. [shallow] contains all informed shallowed
      hashes in the server. *)

  val pp_advertised_refs : advertised_refs Fmt.t
  (** Pretty-printer of {!advertised_refs}. *)

  type shallow_update =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list }
  (** Only when the client sent a positive depth request, the server will
      determine which commits will and will not be shallow and send this information
      to the client.

      The server writes ["shallow"] lines for each commit whose parents will not
      be sentas a result. The server writes an ["unshallow"] line for each
      commit which the client has indicated is {i shallow}, but is no longer {i
      shallow} at the currently requested depth (that is, its parents will now
      be sent). The server MUST NOT mark as ["unshallow"] anything which the
      client has not indicated was ["shallow"].

      This type represents this information. *)

  val pp_shallow_update : shallow_update Fmt.t
  (** Pretty-printer of {!shallow_update}. *)

  type acks =
    { shallow   : Hash.t list
    ; unshallow : Hash.t list
    ; acks      : (Hash.t * [ `Common | `Ready | `Continue | `ACK ]) list }
  (** In the negociation phase, the server will ACK obj-ids differently
      depending on which ack mode is chosen by the client:

      {ul

      {- [`Multi_ack] mode:
      {ul
      {- the server will respond with [`Continue] for any common commits.}
      {- once the serverhas found an acceptable common base commit and is ready
      to make a packfile, it will blindly ACK all ["have"] obj-ids back to the
      client.}
      {- the server will then send a ["NAK"] and then wait for another response
      from the client - either a ["done"] or another list of ["have"] lines.}}}

      {- [`Multi_acj_detailed] mode:

      {ul
      {- the server will differentiate the ACKs where it is signaling that it is
      ready to send data with [`Ready] lines, and signals the identified common
      commits with [`Common] lines.}}}

      {- [None] mode:
      {- upload-pack sends [`ACK] on the first common object it finds. After
      that it says nothing until the client gives it a ["done"].}
      {- upload-pack sends ["NAK"] on a [`Flush] if no common object has been
      found yet. If one has been found, and thus an ACK was already sent, it's
      silent on the [`Flush].}}}

      This type represents this information.
  *)

  val pp_acks : acks Fmt.t
  (** Pretty-printer of {!acks}. *)

  type negociation_result =
    | NAK
    | ACK of Hash.t
    | ERR of string
    (** When the client wants to finish the negociation by [`Done], the server
        will eithersend a final [ACK obj_id] or it will send a [NAK]. [obj_id] is the
        object name of the last commit determined to be common. The server only sends
        this information after [`Done] if there is at least one common base and
        [`Multi_ack] or [`Multi_ack_detailed] is enabled. The server always sends
        [NAK] after [`Done] if there is no common base found.

        Instead [ACK _] or [NAK], the server may send an error message (for
        example, if it does not recognize an object in a ["want"] line received
        from the client).

        This type represents this information. *)

  val pp_negociation_result : negociation_result Fmt.t
  (** Pretty-printer of {!negociation_result}. *)

  type pack =
    [ `Raw of Cstruct.t
    | `Out of Cstruct.t
    | `Err of Cstruct.t ]
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

  type report_status =
    { unpack   : (unit, string) result
    ; commands : (string, string * string) result list }
  (** Ther receiving the pack data from the sender, the receiver sends a report
      if [`Report_status] capability is in effect. It is a short listing of what
      happened in that update. It will first list the status of the packfile
      unpacking as either [Ok ()] or [Error msg]. Then it will list the status for
      each of the references that it tried to update. each line is either [Ok
      refname] if the update was successful, or [Errorr (refname, msg)] if the
      update was not.

      This type represents this information. *)

  val pp_report_status : report_status Fmt.t
  (** Pretty-printer of {!report_status}. *)

  type _ transaction =
    | ReferenceDiscovery : advertised_refs transaction
    | ShallowUpdate      : shallow_update transaction
    | Negociation        : ack_mode -> acks transaction
    | NegociationResult  : negociation_result transaction
    | PACK               : side_band -> flow transaction
    | ReportStatus       : side_band -> report_status transaction
    (** The type transaction to describe what is expected to decode/receive. *)
  and ack_mode =
    [ `Ack
    | `Multi_ack
    | `Multi_ack_detailed
    ] (** The ACK mode type to describe which mode is shared by the client and the server. *)
  and flow =
    [ `Raw of Cstruct.t
    | `End
    | `Err of Cstruct.t
    | `Out of Cstruct.t
    ] (** The representation of the output side-band. *)
  and side_band =
    [ `Side_band
    | `Side_band_64k
    | `No_multiplexe
    ] (** The side-band mode type to describe which mode is shared by the client and the server. *)

  val decode : decoder -> 'result transaction -> 'result state
  (** [decode decoder transaction] decodes the input represented by [decoder] in
      the way of the [transaction] and returns the value expected and described by
      [transaction] or an error. *)

  val decoder : unit -> decoder
  (** [decoder ()] makes a new decoder. *)
end

module Decoder
    (Digest : Ihash.IDIGEST with type t = Bytes.t)
  : DECODER with type Hash.t = Digest.t
             and module Digest = Digest
(** The {i functor} to make the Decoder by a specific hash implementation. We
    constraint the {!IDGEST} module to generate a {!Bytes.t}. *)

module type ENCODER =
sig
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  type encoder
  (** The type encoder. *)

  val set_pos : encoder -> int -> unit
  (** [set_pos encoder pos] is unsafe and change the internal position of the
      encoder. Don't use it. *)

  val free : encoder -> Cstruct.t
  (** [free encoder] returns the free internal buffer of the [encoder]. Don't
      use it. *)

  type 'a state =
    | Write of { buffer    : Cstruct.t
               ; off       : int
               ; len       : int
               ; continue  : int -> 'a state }
    (** Means that we want to flush the internal buffer of the encoder. We
        provide and {!Cstruct.t} with an offset and a length. The client is able to
        {!Cstruct.blit} this buffer to e output in this range. Then, he can call
        [continue] with how many byte(s) he wrote. *)
    | Ok of 'a
    (** The end value of the encoding. *)

  type upload_request =
    { want         : Hash.t * Hash.t list
    ; capabilities : capability list
    ; shallow      : Hash.t list
    ; deep         : [ `Depth of int | `Timestamp of int64 | `Ref of string ] option }
  (** After reference and capabilities discovery, the client can decide to enter
      to the negociation phase, where the client and server determine what the
      minimal packfile necessary for transport is, by telling the server what
      objects it wants, its shallow objects (if any), and the maximum commit depth
      it wants (if any). The client will also send a list of the capabilities it
      wants to be in effect, out of what the server said.

      This type represents this information. *)

  type request_command =
    [ `UploadPack (** When the client wants to fetch/clone. *)
    | `ReceivePack (** When the client wants to push. *)
    | `UploadArchive (** When the client wants an archive of the remote git repository. *)
    ] (** The Git command. *)

  type git_proto_request =
    { pathname        : string
    ; host            : (string * int option) option
    ; request_command : request_command }
  (** The Git transport starts off by sending the command and the repository
      on the wire using the {i packet-line} format, followed by a NUL byte and a
      hostname parameter, terminated by a NUL byte.deep

      This type represents this information. *)

  type ('a, 'b) either =
    | L of 'a
    | R of 'b
  and update_request =
    { shallow      : Hash.t list
    ; requests     : (command * command list, push_certificate) either
    ; capabilities : capability list }
  (** Once the client knows what the references the server is at, it can send
      a list of reference update requests. For each reference on the server that
      it wants to update, it sends a line listing the obj-id currently on the
      server, the obj-id the client would like to update it to and the name of the
      reference.

      This type represents this information. *)
  and command =
    | Create of Hash.t * string (** When the client wants to create a new reference. *)
    | Delete of Hash.t * string (** When the client wants to delete an existing reference in the server side. *)
    | Update of Hash.t * Hash.t * string (** When the client wants to update an existing reference in the server-side. *)
  and push_certificate =
    { pusher   : string
    ; pushee   : string
    ; nonce    : string
    ; options  : string list
    ; commands : command list
    ; gpg      : string list }

  type action =
    [ `GitProtoRequest of git_proto_request
    | `UploadRequest of upload_request
    | `UpdateRequest of update_request
    | `Has of Hash.t list
    | `Done
    | `Flush
    | `Shallow of Hash.t list
    | `PACK of int ]
  (** The type action to describe what is expected to encode/send. *)

  val encode : encoder -> action -> unit state
  (** [encode encoder action] encodes to an output represented by [encoder] in
      the way of the [action] and returns an unit value. *)

  val encoder : unit -> encoder
  (** [encoder ()] makes a new decoder. *)
end

module Encoder
    (Digest : Ihash.IDIGEST with type t = Bytes.t)
  : ENCODER with type Hash.t = Digest.t
             and module Digest = Digest
(** The {i functor} to make the Encoder by a specific hash implementation. We
    constraint the {!IDIGEST} module to generate a {!Bytes.t}. *)

module type CLIENT =
sig
  module Digest
    : Ihash.IDIGEST with type t = Bytes.t
  (** The [Digest] module used to make the module. *)

  module Decoder
    : DECODER with type Hash.t = Digest.t and module Digest = Digest
  (** The {!Decoder} module constrained by the same [Digest] module. *)

  module Encoder
    : ENCODER with type Hash.t = Digest.t and module Digest = Digest
  (** The {!Encoder} module constrained by the same [Digest] module. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  type context
  (** The type context. *)

  val capabilities : context -> capability list
  (** [capabilities context] returns the current shared capabilities between the client and the server. *)

  val set_capabilities : context -> capability list -> unit
  (** [set_capabilities context cs] sets the current capabilities of the [context]. *)

  type result =
    [ `Refs of Decoder.advertised_refs
    | `ShallowUpdate of Decoder.shallow_update
    | `Negociation of Decoder.acks
    | `NegociationResult of Decoder.negociation_result
    | `PACK of Decoder.flow
    | `Flush
    | `Nothing
    | `ReadyPACK of Cstruct.t
    | `ReportStatus of Decoder.report_status
    ] (** The possible repsonse of the server. *)

  type process =
    [ `Read of (Cstruct.t * int * int * (int -> process))
    | `Write of (Cstruct.t * int * int * (int -> process))
    | `Error of (Decoder.error * Cstruct.t * int)
    | result
    ] (** The expected actions by the context. *)

  val encode : Encoder.action -> (context -> process) -> context -> process
  (** [encode action k ctx] starts to encode an [action] in the context [ctx]
      and call [k] at the end of the encoding. *)

  val decode : 'a Decoder.transaction -> ('a -> context -> process) -> context -> process
  (** [decode t k ctx] starts to decode an expected {!Decoder.transaction} [t]
      and call [k] at the end of the decoding. *)

  val pp_result : result Fmt.t
  (** Pretty-print of {!result}. *)

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
  (** Close to {!Encoder.action} but more exhaustive and context-dependant. *)

  val run : context -> action -> process
  (** [run ctx action] sends an action to the server and schedule a specific
      {!Decoder.transaction} then. *)

  val context : Encoder.git_proto_request -> context * process
  (** [context request] makes a new context and the continuation of the
      transport. *)
end

module Client
    (Digest : Ihash.IDIGEST with type t = Bytes.t)
  : CLIENT with type Hash.t = Digest.t
            and module Digest = Digest
(** The {i functor} to make the Client by a specific hash
    implementation. we constraint the {!IDIGEST} module to
    generate a {Bytes.t}. *)


