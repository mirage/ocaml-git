(** Capabilities module.

    When the client talks with the server, it needs to inform capabilities (what
    it can handle). This is the exhaustive list of capabilities on the current
    Smart protocol. Then, the server responds too with capabilities.

    The common part between the client and the server of capabilities should
    diverge how we handle the Smart protocol. For example, if the client does
    not allow [`Shallow] objects, we permit to define shallow objects on the API
    of the fetch command but we don't use them to notice to the server. *)

type t =
  [ `Multi_ack
    (** The [`Multi-ack] capability allows the server to return
          ["ACK obj-id continue"] as soon as it finds a commit that it can use as
          a common base, between the client's wants and the client's have set.

          By sending this early, the server can potentially head off the client
          from walking any further down that particular branch of the client's
          repository history. The client may still need to walk down other
          branches, sending have lines for those, until the server has a complete
          cut across the DAG, or the client has said ["done"]. *)
  | `Multi_ack_detailed
    (** This is an extension of [`Multi_ack] that permits client to better
          understand ther server's in-memory state. *)
  | `No_done
    (** This capability should only be used with the smart HTTP protocol. If
          [`Multi_ack_detailed] and [`No_done] are both present, then the sender
          is free to immediately send a pack following its first
          ["ACK obj-id ready"] message.

          Without [`No_done] in the smart HTTP protocol, the server session would
          end and the client has to make another trip to send ["done"] before the
          server can send the pack. [`No_done] removes the last round and thus
          slightly reduces latency. *)
  | `Thin_pack
    (** A thin pack is one with deltas which reference base objects not
          contained within the pack (but are known to exist at the receiving end).
          This can reduce the network traffic significantly, but it requires the
          receiving end to know how to "thicken" these packs by adding the missing
          bases to the pack.

          The [`UploadPack] server advertises [`Thin_pack] when it can generate
          and send a thin pack. A client requests the [`Thin_pack] capability when
          it understands how to ["thicken"] it, notifying the server that it can
          receive such a pack. A client MUST NOT request the [`Thin_pack]
          capability if it cannot turn a thin pack into a self-contained pack.

          [`ReceivePack], on the other hand, is assumed by default to be able to
          handle thin packs, but can ask the client not to use the feature by
          advertising the [`No_thin] capability. A client MUST NOT send a thin
          pack if the server advertises the [`No_thin] capability. *)
  | `Side_band  (** See {!`Side_band_64k}. *)
  | `Side_band_64k
    (** This capability means that server can send, and client understand
          multiplexed progress reports and error into interleaved with the
          packfile itself.

          These two options are mutually exclusive. A modern client always favors
          [`Side_band_64k].

          Either mode indicates that the packfile data will be streamed broken up
          into packets of up to either 1000 bytes in the case of [`Side_band], or
          65520 bytes in the case of [`Side_band_64k]. Each packet is made up of a
          leading 4-byte {i pkt-line} length of how much data is in the packet,
          followed by a 1-byte stream code, followed by the actual data.

          Further, with [`Side_band] and its up to 1000-byte messages, it's
          actually 999 bytes of payload and 1 byte for the stream code. With
          [`Side_band_64k], same deal, you have up to 65519 bytes of data and 1
          byte for the stream code.

          The client MUST send only maximum of one of [`Side_band] and
          [`Side_band_64k]. Server MUST diagnose it as an error if client requests
          both. *)
  | `Ofs_delta
    (** Server can send, and client understand PACKv2 with delta referring to
          its base by position in path rather than by an obj-id. That is, they can
          send/read OBJ_OFS_DETLA (aka type 6) in a packfile. *)
  | `Agent of string
    (** The server may optionnaly send a capability of the form ["agent=X"] to
          notify the client that the server is running version ["X"]. The client
          may optionnaly return its own agent string by responding with an
          ["agent=Y"] capability (but it MUST NOT do so if the server did not
          mention the agent capability). the ["X"] and ["Y"] strings may contain
          any printable ASCII characters except space (i.e. the byte range
          [32 < x < 127]), and are typically of the form ["package/version"]
          (e.g., ["git/1.8.3.1"]). The agent strings are purely informative for
          statistics and debugging purposes, and MUST NOT be used to
          programmatically assume the presence or absence of particular features. *)
  | `Shallow
    (** This capability adds ["deepen"], ["shallow"] and ["unshallow"] commands
          to the fetch-pack/upload-pack protocol so clients can request shallow
          clones. *)
  | `Deepen_since
    (** This capability adds ["deepen-since"] command to fetch-pack/upload-pack
          protocol so the client can request shallow clones that are cut at a
          specific time, instead of depth. Internally it's equivalent of doing
          ["git rev-list --max-age=<timestamp>"] on the server side.
          [`Deepen_since] cannot be used with [`Deepen]. *)
  | `Deepen_not
    (** This capability adds [`Deepen_not] command to fetch-pacj/upload-pack
          protocol so the client can request shallow clones that are cut at a
          specific revision, instead of depth. Internanlly it's equivalent of
          doing ["git rev-list --not <rev>"] on the server side. [`Deepen_not]
          cannot be used with [`Deepen], but can be used with [`Deepen_since]. *)
  | `No_progress
    (** The client was started with ["git clone -q"] or something, and does not
          want that side band 2. Basically the client just says
          ["I do not wish to receive stream 2 on sideband, so do not send it to
          me, and if you did, I will drop it on the floor anyway"]. However, the
          sideband channel 3 is still used for error responses. *)
  | `Include_tag
    (** The [`Include_tag] capability is about sending annotated tags if we are
          sending objects they point to. If we pack an object to the client, and a
          tag object points exactly at that object, we pack the tag object too. In
          general this allows a client to get all new annotated tags when it
          fetches a branch, in a single network connection.

          Clients MAY always send [`Include_tags], hardcoding it into a request
          when the server advertises this capability. The decision for a client to
          request [`Include_tag] only has to do with the client's desires for tag
          ["refs/tags/*"] namespace.

          Servers MUST pack the tags if their referrant is packed and the client
          has requested [`Include_tag].

          Clients MUST be prepared for the case where a server has ignored
          [`Include_tag] and has not actually sent tags in the pack. In such cases
          the client SHOULD issue a subsequent fetch to acquire the tags that
          [`Include_tag] would have otherwise given the client.

          The server SHOULD send [`Include_tag], if it supports it, regardless of
          whether or not there are tags available. *)
  | `Report_status
    (** The [`ReceivePack] process can receive a [`Report_status] capability,
          which tells it that the client wants a report of what happened after a
          packfile upload and reference update. If the pushing client requests
          this capability, after unpacking and updating references the server will
          respond with whether the packfile unpacked successfully and if each
          reference was updated successfully. If any of those were not successful,
          it will send back an error message. *)
  | `Delete_refs
    (** If the server sends back the [`Delete_refs] capability, it means that it
          is capable of accepting a zero-id value as the target value of a
          reference update. It is not sent back by the client, it simply informs
          the client that it can be sent zero-id values to delete references. *)
  | `Quiet
    (** If the [`ReceivePack] server advertises the [`Quiet] capability, it is
          capable of silencing human-readable progress output which otherwise may
          be shown when processing the receiving pack. A send-pack client should
          respond with the [`Quiet] capability to suppress server-side progress
          reporting if the local progress reporting is also being suppressed
          (e.g., via ["git push -q"], or if [stderr] does not go to a tty). *)
  | `Atomic
    (** If the server sends the [`Atomic] capability it is capable of acceping
          atomic pushes. If the pushing client requests this capability, the
          server will update the refs in one atomic transaction. Either all refs
          are updated or none. *)
  | `Push_options
    (** If the server sends the [`Push_options] capability it is able to accept
          push options after the update commands have been sent, but before the
          packfile is streamed. If the pushing client requests this capability,
          the server will pass the options to the pre- and post- receive hooks
          that process this push request. *)
  | `Allow_tip_sha1_in_want
    (** If the upload-pack server advertises this capability, fetch-pack may
          send ["want"] lines with hashes that exists at the server but are not
          advertised by upload-pack. *)
  | `Allow_reachable_sha1_in_want
    (** If the upload-pack server advertises this capability, fetch-pack may
          send ["want"] lines with hashes that exists at the server but are not
          advertised by upload-pack. *)
  | `Push_cert of string
    (** The receive-pack server that advertises this capability is willing to
          accept a signed push certificate, and asks the <nonce> to be included in
          the push certificate. A send-pack client MUST NOT send a push-cert
          packet unless the receive-pack server advertises this capability. *)
  | `Symref of string * string
  | `Other of string  (** Unrecognized capability. *)
  | `Parameter of string * string  (** Unrecognized capability with a value. *)
  ]

val to_string : t -> string
(** [to_string c] returns a string representaiton of the capability [c]. *)

exception Capability_expect_value of string
(** Exception to inform than the capability expects a value. *)

val of_string : ?value:string -> string -> t
(** [of_capability s] tries to decode [s] to a capability. If the capability
      excepts a value, we raise [Capability_expect_value]. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val compare : t -> t -> int
(** Comparison function of {!t}. *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)
