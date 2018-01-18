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

(** Decoder of the PACK file. *)

(** A Window. It consists to keep an 1 megabyte area of a PACK
    file. *)
module Window :
sig
  type t =
    { raw : Cstruct.t
    ; off : int64
    ; len : int }
  (** The type of the window. *)

  val inside: int64 -> t -> bool
  (** [inside off t] checks if the offset [off] is available inside in
      [ŧ]. *)

  val pp: t Fmt.t
  (** Pretty-printer of {!t}. *)
end

(** The non-blocking decoder of the Hunks stream. *)
module type H =
sig
  module Hash: S.HASH

  (** The type error. *)
  type error =
    | Reserved_opcode of int
    (** Appear when we catch a reserved opcode. *)
    | Wrong_copy_hunk of int * int * int
    (** Appear when the [Copy] hunk refers to a wrong area (because is
        bigger than it's possible to store into the target or because
        it does not correspond to a valid area from the source). *)

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** The type of the Hunk decoder. *)
  type t =
    { i_off          : int
    ; i_pos          : int
    ; i_len          : int
    ; read           : int
    ; _length        : int
    ; _reference     : reference
    ; _source_length : int
    ; _target_length : int
    ; _hunk          : hunk option
    ; _tmp           : Cstruct.t
    ; state          : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header    of k
    | Stop
    | List      of k
    | Is_insert of (Cstruct.t * int * int)
    | Is_copy   of k
    | End
    | Exception of error
  and res =
    | Wait   of t
    | Error  of t * error
    | Cont   of t
    | Ok     of t * hunks
  (** The type of compressed/delta-ified object. *)
  and hunk =
    | Insert of Cstruct.t
      (** Raw buffer. *)
    | Copy of int * int
      (** absolute offset ⨯ length *)

  (** The type of the reference. It's a negative offset or the hash of
      the source object. *)
  and reference =
    | Offset of int64
    | Hash of Hash.t
  (** The type of the Hunk. *)
  and hunks =
    { reference     : reference
    (** Reference to the source. *)
    ; length        : int
    (** Inflated length of the serialized Hunk. *)
    ; source_length : int
    (** Length of the source object. *)
    ; target_length : int
    (** Expected length of the target object. *) }

  val partial_hunks: t -> hunks
  (** [partial_hunks t] returns a partial hunks to get some available
      information. {!hunks.hunks} is not available. *)

  val pp_reference: reference Fmt.t
  (** A pretty-printer of {!reference}. *)

  val pp_hunks: hunks Fmt.t
  (** A pretty-printer of {!hunks}. *)

  val pp: t Fmt.t
  (** Pretty-printer of the decoder {!t}. *)

  val eval:
       Cstruct.t
    -> t
    -> [ `Hunk of t * hunk
       | `Await of t
       | `Error of t * error
       | `Ok of t * hunks ]
  (** [eval src t] is:

      {ul
      {- [`Await t] iff [t] needs more input storage. The client must
      use {!refill} to provide a new buffer and then call {!eval} with
      [`Await] until other value returned.}
      {- [`Hunk t] when [t] can return a new {!hunk}. The client can
      call {!continue} to move to the next step of the process,
      otherwise the decode sticks on this situation. The value will be
      consumed then. If the {!hunk} is [Insert], the internal
      [Cstruct.t] need to be copied because {!eval} will erase the
      content then.}
      {- [`Ok (t, hunks)] when [t] is done. We returns the complete
      value {!hunks}. Then, [t] sticks on this situation, the client
      can remove it.}
      {- [`Error (t, exn)] iff the decoder [t] meet an {!error} [exn].
      The decoder can't continue and sticks in this situation.}} *)

  val default: int -> reference -> t
  (** Make a new decoder state {!t} from a {!reference}. We need to
      notice the length of the inflated stream to know when the
      decoder {!t} is done. *)

  val refill: int -> int -> t -> t
  (** [refill off len t] provides a new [t] with [len] bytes to read,
      starting at [off]. This byte range is read by calls to {!eval}
      with [t] until [`Await] is returned. *)

  val continue: t -> t
  (** [continue t] provides a new [t] to move to the next step of the
      process and consumes the current {!hunk} returned by {!eval}. *)

  val current: t -> hunk
  (** [current t] provides the current {!hunk} from the decoder [t].
      This hunk is equivalent to: [eval _ t], which returns [`Hunk]
      with the same (physically) {!hunk}.

      You only can call this function when you ensure than [eval _ t]
      returns [`Hunk]. Otherwise, we raise an exception
      [Invalid_argument]. *)

  val used_in: t -> int
  (** [used_in ŧ] returns how many byte [t] consumed in the current
      buffer noticed to the previous call of {!eval}. *)

  val available_in: t -> int
  (** [available_in t] returns how many byte is available in the
      current buffer noticed to the previous call of {!eval}. *)

  val read: t -> int
  (** [read t] returns how many byte [t] read at the beginning. The
      client can assert than [read t] is equal to [length] noticed
      when he {!make} the new decoder. *)
end

module MakeHunkDecoder (Hash : S.HASH) : H with module Hash = Hash

(** The non-blocking decoder of the PACK stream. *)
module type P =
sig
  module Hash: S.HASH
  module Inflate: S.INFLATE
  module HunkDecoder: H with module Hash := Hash

  (** The type error. *)
  type error =
    | Invalid_byte of int
    (** Appears when the header of the PACK file is wrong. *)
    | Reserved_kind of int
    (** Appears when the kind of the current PACK object is [0]. *)
    | Invalid_kind of int
    (** Appears when the kind of the current PACK object is undefined.
        TODO: this case does not appear, lol ... *)
    | Inflate_error of Inflate.error
    (** Appears when the {!Inflate} module returns an error when it
        tries to inflate a PACK object. *)
    | Hunk_error of HunkDecoder.error
    (** Appears when the {!H} module returns an error. *)
    | Hunk_input of int * int
    (** The Hunk object is length-defined. So, when we try to decode
        this specific PACK object, if the decoder {!H.t} expects some
        new bytes and we already get all input, this error appears
        (with respectively how many byte(s) we expected and how many
        byte(s) the decoder {!H.t} computed). *)
    | Invalid_length of int * int
    (** Appears when the length of the inflated object does not
        correspond by what was it noticed by the PACK file. *)

  val pp_error : error Fmt.t
  (** Pretty-printer for an {!error}. *)

  type t
  (** The type of the decoder. *)

  val pp : t Fmt.t
  (** Pretty-printer for {!t}. *)

  type kind =
    | Commit
    | Tree
    | Blob
    | Tag
    | Hunk of HunkDecoder.hunks
  (** The kind of the PACK object. It can be:

      {ul
      {- A [Commit]}
      {- A [Tree]}
      {- A [Blob]}
      {- A [Tag]}
      {- A [Hunk]: this is not a git object but a specific PCK object.
      The {!H.hunks} refers to another PACK object by {!H.reference}
      and it's just a result of a diff between the reference and the
      current object.} } *)

  val default: Cstruct.t -> Inflate.window -> t
  (** [default tmp window] makes a new decoder of a PACK file.

      [tmp] is a [Cstruct.t] used as an internal output of the
      inflated stream. We let the user to allocate the optimized
      length for this temporary buffer and we take the ownership (so,
      you don't use it to another compute).

      [window] is the {Inflate.window} needed to inflate. We keep this
      window as long as we use have objects in the PACK stream and
      reset it for each object (and avoid any needed allocation).
      Then, we take the ownership so you are not able to use it for
      another process.

      This state does not allocate any large caml value. *)

  val many: t -> int32
  (** [many t] returns how many objects will/did compute for the
      current stream. *)

  val from_window: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  (** [from_window pack_window off_in_window tmp window] makes a new
      decoder of a PACK file specialized from a {!Window.t}. This
      decoder compute only one object in a precise offset (noticed by
      [pack_window] and the relative offset [off_in_window]). Then
      [tmp] and [window] have the same purpose than {!default}. For
      this state, {!eval} has a different behaviour. Indeed, after the
      specified object de-serialized, {!eval} returns directly [`End]
      even if it's not the last object in the PACK stream. Then, the
      hash produced is empty. *)

  val process_length: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  (** [process_length pack_window off_in_window tmp window] makes a
      new specific decoder only to get the real length of the PACK
      object specified by [pack_window] and the relative offset
      [off_in_window]. This decoder is only able to be used with
      {!eval_length} (and not {!eval}). For all git object, we can
      catch the real inflated length of the expected object. For the
      [Hunk _] object, we can get the real inflated length of the PACK
      object and call {!H.partial_hunks} with the internal Hunk
      decoder state. This compute appear only when {!eval_length}
      returns [`Length].

      This decoder is focused to only get the length of the PACK
      object. So we inflate for example only what it needed to get
      this information. So, obviously, it is more faster than {!eval}
      and compute all of the object to get the information then. *)

  val process_metadata: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  (** [process_metadata] same purpose than {!process_length} but stop
      the decoder to a step to get some meta-data about the expected
      PACK object. *)

  val refill: int -> int -> t -> t
  (** [refill off len t] provides a new [t] with [len] bytes to read,
      starting at [off]. This byte range is read by calls to {!eval}
      with [t] until [`Await] is returned. *)

  val flush: int -> int -> t -> t
  (** [flush off len t] provides [t] with [len] bytes to write, starting at
      [off]. This byte range is written by calls to {!eval} with [t] until
      [`Flush] is returned. Use {!output} to know how many byte [t] wrote. *)

  val next_object: t -> t
  (** [next_object t] provides a new [t] which continue the
      de-serialization of the PACK stream. Indeed, when the client
      catches a new [`Object], the decoder sticks on the this
      situation. So to move to the next step, the client have to call
      {!next_object} to continue the process. *)

  val continue: t -> t
  (** [continue t] defers {!H.continue} to the internal state {!H.t}.
      The client is able to use it only when
      {!eval}/{!eval_length}/{!eval_metadata} returns [`Hunk]. *)

  (** Meta-data information. *)

  val kind: t -> kind
  (** [kind t] returns the {!kind} of the current object. The client
      is able to use this function only when {!eval} returns [`Object]
      or [`Hunk]. Otherwise, we raise an [Invalid_argument]. This
      function is available when the client process the meta-data of
      the expected object and can call it when {!eval_metadata}
      returns [`Metadata]. *)

  val length: t -> int
  (** [length t] returns the real inflated length of the current
      object in the PACK stream. The client is able to use this
      function only when {!eval} returns [`Object] or [`Hunk].
      Otherwise, we raise an [Invalid_argument]. This function is
      available when the client process the length of the expected
      object and can call it when {!eval_length} returns [`Length]. *)

  val offset: t -> int64
  (** [offset t] returns the absolute offset of the current object in
      the PACK file. The client is able to use this function only when
      {!eval} returns [`Object] or [`Hunk]. Otherwise, we raise an
      [Invalid_argument]. This function is available when the client
      process the meta-data of the expected object and can call it
      when {!eval_metadata} returns [`Metadata]. *)

  val consumed: t -> int
  (** [consumed t] returns how many byte(s) the decoder consumed to
      de-serialize the current/expected object in the PACK stream. The
      client is able to use it only when {!eval} returns [`Object].
      Otherwise, we raise an [Invalid_argument]. *)

  val crc: t -> Crc32.t
  (** [crc t] returns the CRC-32 checksum computed when the decoder
      consumed all byte(s) needed to de-serialize the current/expected
      object. The client is able to use this function only when
      {!eval} returns [`Object]. Otherwise, we raise an
      [Invalid_argument]. *)

  val output: t -> Cstruct.t * int
  (** [output t] returns the output produced by the decoder [t] when
      it inflates a git object. The [Cstruct.t] is physically the same
      than [tmp] noticed when the client makes the decoder [t]. Then,
      [output] returns how many byte(s) the decoder wrote inside
      [tmp]. The client is able to use this function only when the
      kind of the current pack object processed is different to
      [Hunk]. Otherwise, we raise an error. *)

  val eval:
       Cstruct.t
    -> t
    -> [ `Object of t
       | `Hunk of t * HunkDecoder.hunk
       | `Await of t
       | `Flush of t
       | `End of t * Hash.t
       | `Error of t * error ]
  (** [eval src t] is:

      {ul
      {- [`Await t] iff [t] more input storage. The client muse use
      {!refill} to provide a new buffer and then call {!eval} with
      [`Await] until other value returned.}
      {- [`Object t] iff [t] finishes to compute one object. The
      client is able to use the meta-data functions (like {!kind} or
      {!crc}) and should use {!next_object} to move the decoder to the
      next step. Otherwise, the decoder [t] sticks in this situation.}
      {- [Hunk (t, hunk)] defers the value returned by the internal
      {!H.t} decoder. The client should use {!continue} to move the
      decoder to the next step. In this situation, we ensure than the
      future {!kind} of the current/expected object is {!Hunk _}.}
      {- [`Flush t] iff [t] needs more output storage. The client must
      use {!flush} to provide a new buffer and then call {!eval} with
      [`Flush] until [`Object] is returned. In this situation, we
      ensure than the future {!kind} of the current/expected object is
      different than the {!Hunk _}.}
      {- [`End (t, hash)] when the decoder is done. [t] sticks to this
      situation. The client can remove it. We return the hash produced
      by the PACK stream.}
      {- [`Error (t, exn)] iff the decoder meet an {!error} [exn]. The
      decoder can't continue and sticks in this situation.}} *)

  val eval_length:
       Cstruct.t
    -> t
    -> [ `Length of t
       | `Await of t
       | `Flush of t
       | `End of (t * Hash.t)
       | `Error of (t * error) ]
  (** [eval_length src t] has the same purpose than {!eval} plus:

      {ul
      {- [`Length t] iff [t] can return the real inflated length of the
      current/expected object. The client should use {!length} to get this
      information.}} *)

  val eval_metadata:
       Cstruct.t
    -> t
    -> [ `Metadata of t
       | `Await of t
       | `Flush of t
       | `End of (t * Hash.t)
       | `Error of (t * error) ]
  (** [eval_length src t] has the same purpose than {!eval} plus:

      {ul
      {- [`Metadata t] iff [t] can be used with meta-data functions
      (like {!kind} or {!length}).}} *)
end

module MakePackDecoder
    (Hash: S.HASH)
    (Inflate: S.INFLATE)
    (HunkDecoder: H with module Hash := Hash)
  : P with module Hash = Hash
       and module Inflate = Inflate
       and module HunkDecoder := HunkDecoder

(** The toolbox about the PACK file. *)
module type D =
sig
  module Hash: S.HASH
  module Mapper: S.MAPPER
  module Inflate: S.INFLATE
  module HunkDecoder: H with module Hash := Hash
  module PackDecoder: P
    with module Hash := Hash
     and module Inflate := Inflate
     and module HunkDecoder := HunkDecoder

  (** The type error. *)
  type error =
    | Invalid_hash of Hash.t
    (** Appears when the user requested a wrong hash. *)
    | Invalid_offset of int64
    (** Appears when the given offset is not available inside the PACK
        file. *)
    | Invalid_target of (int * int)
    (** Appears when the result of the application of a {!P.H.hunks}
        returns a bad raw. *)
    | Unpack_error of PackDecoder.t * Window.t * PackDecoder.error
    (** Appears when we have an {!P.error}. *)
    | Mapper_error of Mapper.error

  val pp_error : error Fmt.t
  (** Pretty-printer for {!error}. *)

  type t
  (** The type of the decoder. *)

  type kind = [ `Commit | `Blob | `Tree | `Tag ]
  (** The type of the kind of the git object. *)

  module Object:
  sig
    type from =
      | Offset of { length   : int
                  (** Real inflated length of the object. *)
                  ; consumed : int
                  (** How many byte(s) consumed to de-serialize the
                      object. *)
                  ; offset   : int64
                  (** The absolute offset in the PACK file of the
                      object. *)
                  ; crc      : Crc32.t
                  (** The CRC-32 checksum of the object. *)
                  ; base : from
                  (** The source object. *)
                  ; }
      (** When the source is external of the PACK file. *)
      | External of Hash.t
      (** When the object is not come from a source but directly
          serialized (no delta-ification) in the PACK file. *)
      | Direct of { consumed : int
                  (** How many byte(s) consumed to de-serialize the
                      current object. *)
                  ; offset   : int64
                  (** The absolute offset in the PACK file of the
                      current object. *)
                  ; crc      : Crc32.t
                  (** The CRC-32 checksum of the source object. *)
                  ; }
      (** The type to describe where the object come from (directly
          from the PACK file or from a delta-ification with an
          [External _] object or a {i in-PACK} object. *)
    and t =
      { kind   : kind
      (** The kind of the object. *)
      ; raw    : Cstruct.t
      (** The de-serialized raw of the object. *)
      ; length : int64
      (** The real inflated/undelta-ified length of the object. *)
      ; from   : from
      (** If the object is come from a delta-ification or it stored directly in
         the PACK file. *)
      ; }
    (** The type of the git object (after we processed all necessary
        undelta-ification). *)

    val pp: t Fmt.t
    (** Pretty-printer for {!t}. *)

    val first_crc_exn: t -> Crc32.t
    (** [first_crc_exn t] gets the CRC-32 checksum of [obj] {!t}. This
        function can't raise an exception for any object created from
        this API. However, an object created by the hand could not
        respect the assertion. *)
  end

  val find_window: t -> int64 -> (Window.t * int, Mapper.error) result Lwt.t
  (** [find t absolute_offset] returns a couple of a {!Window.t} which
      contains the absolute offset requested and the relative offset in
      the window. We allocate a new {!Window.t} only when we don't find
      a previous valid {Window.t}. If the absolute offset is bad, we
      defer the exception from [Mapper.map]. *)

  val make: ?bucket:int -> Mapper.fd
    -> (Hash.t -> Object.t option)
    -> (Hash.t -> (Crc32.t * int64) option)
    -> (int64 -> Hash.t option)
    -> (Hash.t -> (kind * Cstruct.t) option Lwt.t)
    -> (t, Mapper.error) result Lwt.t
  (** [make ?bucket fd cache idx revidx extern] makes a new decoder
      when:

      {ul
      {- [cache] is a function to return if it is available an object
      expected by the decoder.}
      {- [idx] is a function to return the absolute offset of the
      requested object if it is available in the PACK file.}
      {- [revidx] is a function to return if it is available the hash
      associated to the absolute offset.}
      {- [extern] is a function to must return the git object
      expected. Otherwise, we return an {!error} {!Invalid_hash}.}}

      Then, the client must to provide a valid file descriptor of the
      PACK file and can specify how many window(s) the decoder can
      make (must be upper than 1). *)

  val idx: t -> (Hash.t -> (Crc32.t * int64) option)
  (** [idx t] provides the [idx] function noticed by the client when
      he {!make} [t]. *)

  val cache: t -> (Hash.t -> Object.t option)
  (** [cache t] provides the [cache] function noticed by the client
      when he {!make} [t]. *)

  val revidx: t -> (int64 -> Hash.t option)
  (** [revidx t] provides the [revidx] function noticed by the client
      when he {!make} [t]. *)

  val extern: t -> (Hash.t -> (kind * Cstruct.t) option Lwt.t)
  (** [extern t] provides the [extern] function noticed by the client
      when he {!make} [t]. *)

  val update_idx: (Hash.t -> (Crc32.t * int64) option) -> t -> t
  val update_cache: (Hash.t -> Object.t option) -> t -> t
  val update_revidx: (int64 -> Hash.t option) -> t -> t
  val update_extern: (Hash.t -> (kind * Cstruct.t) option Lwt.t) -> t -> t

  val length:
       ?chunk:int
    -> t
    -> Hash.t
    -> Cstruct.t
    -> Inflate.window
    -> (int, error) result Lwt.t
  (** [length ?chunk t hash tmp window] returns the length of the git
      object. To be care, it's not the length needed to get the git
      object - sometimes the length needed is more than the length of
      the object requested (for this request, see {!needed}).

      [?chunk] is how many byte(s) the client wants to fill to the
      internal decoder {!P.t} when it returns [`Await].

      Then, the client need to specify the [tmp] buffer and the
      [window] used by the internal decoder {!P.t} (see
      {!P.default}). *)

  val needed_from_hash:
       ?chunk:int
    -> ?cache:(Hash.t -> int option)
    -> t
    -> Hash.t
    -> Cstruct.t
    -> Inflate.window
    -> (int, error) result Lwt.t
  (** [needed ?chunk ?cache t hash tmp window] returns the maximum
      size needed to store the object identified by [hash].

      [?chunk] is how many byte(s) the client wants to fill to the
      internal decoder {!P.t} when it returns [`Await].

      [?cache] is used to get directly (without de-serialization) the
      maximum size needed for the requested object (consider than is
      not the same as [hash]).

      Then, the client need to specify the [tmp] buffer and the
      [window] used by the internal decoder {!P.t} (see
      {!P.default}). *)

  val get_from_offset:
       ?chunk:int
    -> ?limit:bool
    -> ?htmp:Cstruct.t array
    -> t
    -> int64
    -> (Cstruct.t * Cstruct.t * int)
    -> Cstruct.t
    -> Inflate.window
    -> (Object.t, error) result Lwt.t
  (** [optimized_get' ?chunk t absolute_offset (raw0, raw1, length)
      tmp window] returns the object located to the [absolute_offset]
      in the PACK file. This function does not allocate any buffer. It
      uses [raw0] and [raw1] to undelta-ify the git object requested -
      so [raw0] and [raw1] must need to be well-sized to store all PACK
      object needed to de-serialize the requested object. Then, we
      returns an {!Object.t} which contains a [Cstruct.sub] in the
      field [Object.raw] physically equal to [raw0] or [raw1].

      The client need to notice the [tmp] and the [window] needed by
      the internal decoder {!P.t}.

      And [?chunk] corresponds to how many byte(s) the client wants to
      fill to the internal decoder {!P.t} when it returns [`Await]. *)

  val get_from_hash:
       ?chunk:int
    -> ?limit:bool
    -> ?htmp:Cstruct.t array
    -> t
    -> Hash.t
    -> (Cstruct.t * Cstruct.t * int)
    -> Cstruct.t
    -> Inflate.window
    -> (Object.t, error) result Lwt.t
  (** [optimized_get] has the same purpose than {!optimized_get'} but
      instead to notice the absolute offset of the requested object,
      the client notify the unique hash identifier. Internally, we use
      the [idx] function needed by {!make} to get the absolute offset.
      Otherwise, we returns an [Invalid_hash]. *)

  val get_with_hunks_allocation_from_offset:
       ?chunk:int
    -> t
    -> int64
    -> Cstruct.t
    -> Inflate.window
    -> (Cstruct.t * Cstruct.t)
    -> (Object.t, error) result Lwt.t
  (** [get'] has the same purpose than {!optimized_get'} but it checks
      if [raw0] and [raw1] are well-sized to contain all object needed
      to undelta-ify the requested object and to contain the requested
      object. *)

  val get_with_hunks_allocation_from_hash:
       ?chunk:int
    -> t
    -> Hash.t
    -> Cstruct.t
    -> Inflate.window
    -> (Cstruct.t * Cstruct.t)
    -> (Object.t, error) result Lwt.t
  (** [get] has the same purpose than {!optimized_get} but it checks
      if [raw0] and [raw1] are well-sized to contain all object needed
      to undelta-ify the requested object and to contain the requested
      object. *)

  val get_with_result_allocation_from_hash:
       ?chunk:int
    -> ?htmp:Cstruct.t array
    -> t
    -> Hash.t
    -> Cstruct.t
    -> Inflate.window
    -> (Object.t, error) result Lwt.t
  (** [get_with_allocation ?chunk t hash tmp window] has the same
      purpose than {!optimized_get} but it allocates what it needed to
      store the requested object. *)

  val get_with_result_allocation_from_offset :
       ?chunk:int
    -> ?htmp:Cstruct.t array
    -> t
    -> int64
    -> Cstruct.t
    -> Inflate.window
    -> (Object.t, error) result Lwt.t
end

module MakeDecoder
    (Hash: S.HASH)
    (Mapper: S.MAPPER)
    (Inflate: S.INFLATE)
    (HunkDecoder: H with module Hash := Hash)
    (PackDecoder: P with module Hash := Hash
                     and module Inflate := Inflate
                     and module HunkDecoder := HunkDecoder)
  : D with module Hash = Hash
       and module Mapper = Mapper
       and module Inflate = Inflate
       and module HunkDecoder := HunkDecoder
       and module PackDecoder := PackDecoder
