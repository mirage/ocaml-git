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

(** PACK serializer implementation. *)

(** This module represents the kind of a Git object - but only the
    kind, {!Kind.t} does not have the Git value. *)
module Kind: sig

  type t =
    | Commit
    | Tag
    | Tree
    | Blob
    (** The type of kind. *)

  val to_int: t -> int
  (** [to_int t] returns an unique [int] value of the kind [t]. This
      value can be used to sort a list of kinds. *)

  val to_bin: t -> int
  (** [to_bin t] returns a binary code to serialize the kind [t]. *)

  val pp: t Fmt.t
  (** Pretty-printer of {!t}. *)
end

(** This module is the serialiser of the list of hunks in the PACK
   entry when we retrieve a delta-ified Git object. This encoder is a
   non-blocking encoder used in the same time than the {!Deflater} -
   that means the content produced is always deflated. *)
module type H = sig

  module Hash: S.HASH

  type error
  (** The type of error. *)

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  type t
  (** The type of the encoder. *)

  (** The type of the reference. It's a negative offset or the hash
      of the source object. *)
  type reference =
    | Offset of int64
    | Hash of Hash.t

  val pp: t Fmt.t
  (** Pretty-printer of {!t}. *)

  val default: reference -> int -> int -> Rabin.t list -> t
  (** Make a new encoder state {!t} from a {!reference}. We need to
      notice the length of the inflated source and the length of the
      inflated target then, the compression list. *)

  val refill: int -> int -> t -> t
  (** [refill off len t] provides a new [t] with [len] bytes to
      read, starting at [off]. This byte range is read by calls to
      {!eval} with [t] until [`Await] is returned. The encoder
      expects the target raw (not the source) to write [Insert]
      opcodes. The client must send the target raw continuously
      because internally, we assert than a continuous stream of the
      target raw and pick only the needed byte range. *)

  val flush: int -> int -> t -> t
  (** [flush off len t] provides [t] with [len] bytes to write,
      starting at [off]. This byte range is written by calls to
      {!eval} with [t] until [`Flush] is returned. *)

  val finish: t -> t
  (** [finish t] provides a new [t] which does not expect any input.
      An {!eval} of the new [t] will never return an [`Await] value
      then. *)

  val eval: Cstruct.t -> Cstruct.t -> t ->
    [ `Await of t
    | `Flush of t
    | `End of t
    | `Error of (t * error) ]
  (** [eval src t] is:

      {ul
      {- [`Await t] iff [t] needs more input storage. The client
      must use {!refill} to provide a new buffer and then call
      {!eval} with [`Await] until other value returned.}
      {- [`Flush t] iff [t] needs more output storage. The client
      must use {!flush} to provide a new buffer and then call
      {!eval} with [`Flush] until [`End] is returned.}
      {- [`End t] when [t] is done. Then, [t] sticks on this
      situation, the client can remove it.}
      {- [`Error (t, exn)] iff the encoder [t] meet an {!error}
      [exn]. The encoder can't continue and sticks in this
      situation.}} *)

  val used_in: t -> int
  (** [used_in ŧ] returns how many byte [t] consumed in the current
      buffer noticed to the previous call of {!eval}. *)

  val used_out: t -> int
  (** [used_out ŧ] returns how many byte [t] wrote in the current
      buffer noticed to the previous call of {!eval}. *)
end

module MakeHunkEncoder (Hash: S.HASH): H with module Hash = Hash

(** The entry module. It used to able to manipulate the meta-data only
   needed by the delta-ification of the Git object (and avoid to
   de-serialize all of the Git object to compute the delta-ification).
   *)
module type ENTRY = sig

  module Hash: S.HASH

  type t
  (** The type of an entry. *)

  (** The type of a source. *)
  type source =
    | From of Hash.t
    (** To notice than an user-defined source exists for the
        delta-ification. *)
    | None
    (** To notice than no user-defined source exists. *)

  val pp: t Fmt.t
  (** Pretty-printer for {!t}. *)

  val pp_source: source Fmt.t
  (** Pretty-printer for {!source}. *)

  val hash: string -> int
  (** [hash path] produces a integer to correspond with [path]. *)

  val make:
    Hash.t
    -> ?name:string
    -> ?preferred:bool
    -> ?delta:source
    -> Kind.t
    -> int64
    -> t
  (** [make hash ?name ?preferred ?delta kind length] returns a new
      entry when:

      {ul
      {- [hash] corresponds to the unique ID of the git object.}
      {- [name] corresponds to the optional name of the git object.
      For a [Tree] or a [Blob], the client should notice the given
      name (to optimize the delta-ification).}
      {- [preferred] notices to the delta-ification algorithm to
      prefer to use this entry as a source.}
      {- [delta] notices an existing user specified source for the
      entry (and the delta-ification algorithm will use it).}
      {- [kind] corresponds to the kind of the object.}
      {- [length] corresponds to the real inflated length of the
      object.}} *)

  val kind: t -> Kind.t
  val preferred: t -> bool
  val delta: t -> source
  val length: t -> int64

  val with_delta: t -> source -> t
  val with_preferred: t -> bool -> t

  val id: t -> Hash.t
  (** [id t] returns the unique identifier of the entry [t]. *)

  val name: t -> string -> t
  (** [name t name] provides a new [t] with the specified [name]. *)

  val compare: t -> t -> int
  (** The comparison function for the entry {!t}. It returns [0]
      when [a] equal [b]. It returns a negative integer if [a] is {i
      less} than [b] and a positive integer when [a] is {i greater}
      than [b]. This function is used to sort a list of entries with
      the Git heuristic and produce an optimal delta-ification.

      According to Git, it's a lexicographic sort by the {!Kind.t},
      the optional name of the entry (hashed by {!hash}) and sorted
      by size (larger to smaller). *)

  val topological_sort: t list -> t list
  (** [topological_sort lst] orders [lst] so that no entry precedes
      an entry it depends upon. So you will find any entries
      considered as a source before any entries when you apply the
      delta-ification. *)
end

module MakeEntry (Hash: S.HASH): ENTRY with module Hash = Hash

(** This module is the engine to delta-ify Git objects together. The
   current implementation is a stop the world process which can not
   compute iteratively the delta-ification for some Git objects.

      This process is the biggest process about memory consumption.
   Indeed, for each computation, we need to keep some Git objects to
   get the best diff between them. As Git, this process keeps 10
   objects while we do the delta-ification for all - these objects are
   represented as inflated raw data in a {!Cstruct.t}.

      However, if we try to delta-ify big objects (like your
   repository has the season 1 of Narcos), we should explose your
   memory (because we will load 10 movies in your memory). So the
   client can restrict the delta-ification by the weigth of the window
   (which contains your objects) instead by the number of objects
   inside.

      Then, the final result does not keep Git objects loaded - the
   OCaml GC will delete them - but a lighter representation of the
   diff for each entry. And it's why, for the serialization of the
   PACK file, we re-ask (and re-load) your objects (however, in this
   last case, we don't need the ownership - see {!expect}). *)
module type DELTA = sig

  module Hash: S.HASH
  module Entry: ENTRY with module Hash := Hash

  (** The type of the delta-ification. *)
  type t =
    { mutable delta: delta }
  and delta =
    | Z (** Means no delta-ification. *)
    | S of { length    : int
           (** Length of the PACK object. *)
           ; depth     : int
           (** Depth of the PACK object. *)
           ; hunks     : Rabin.t list
           (** Compression list. *)
           ; src       : t
           (** Source. *)
           ; src_length: int64
           (** Length of the source object. *)
           ; src_hash  : Hash.t
           (** Hash of the source object. *)
           ; }
    (** Delta-ification with a description of the source. *)

  (** The type of error. *)
  type error = Invalid_hash of Hash.t
  (** Appears when we have an invalid hash. *)

  val pp_error: error Fmt.t
  (** Pretty-printer for {!error}. *)

  val deltas:
    ?memory:bool
    -> Entry.t list
    -> (Hash.t -> Cstruct.t option Lwt.t)
    -> (Entry.t -> bool)
    -> int
    -> int
    -> ((Entry.t * t) list, error) result Lwt.t
    (** [deltas ?memory lst getter tagger window depth].

        This is the main algorithm about the serialization of the git
        object in a PACK file. The purpose is to order and delta-ify
        entries.

        [getter] is a function to access to the real inflated raw of
        the git object requested by the hash. This function must
        allocate the raw (or let the ownership to this function). The
        algorithm takes the ownership anyway.

        [tagger] is a function to annotate an entry as preferred to
        serialize firstly.

        [window] corresponds to the number of how many object(s) we
        keep for the delta-ification or, if [memory] is [true], how
        many byte(s) we keep for the delta-ification. The client can
        control the memory consumption of this algorithm precisely if
        he wants.

        [depth] is the maximum of the delta-ification allowed.

        If you want to understand the algorithm, look the source
        code. *)
end

module MakeDelta
    (Hash: S.HASH)
    (Entry: ENTRY with module Hash := Hash)
  : DELTA with module Hash = Hash
           and module Entry := Entry

(** Interface which describes the encoder of the PACK file. *)
module type P = sig

  module Hash: S.HASH
  module Deflate: S.DEFLATE

  module Entry: ENTRY with module Hash := Hash

  module Delta: DELTA
    with module Hash := Hash
     and module Entry := Entry

  module HunkEncoder: H with module Hash := Hash
  module Radix: Radix.S with type key = Hash.t
  (** The Radix tree zhich zill represent the IDX file of the PACK
      stream. *)

  (** The type error. *)
  type error =
    | Deflate_error of Deflate.error
    (** Appears when the deflate algorithm raises an error. *)
    | Invalid_hash of Hash.t
    (** Appears when the hash requested does not exist. *)

  val pp_error: error Fmt.t
  (** Pretty-printer for {!error}. *)

  type t
  (** The type of the encoder. *)

  val used_out: t -> int
  (** [used_out ŧ] returns how many byte [t] wrote in the current
      buffer noticed to the previous call of {!eval}. *)

  val used_in: t -> int
  (** [used_in ŧ] returns how many byte [t] consumed in the current
      buffer noticed to the previous call of {!eval}. *)

  val flush: int -> int -> t -> t
  (** [flush off len t] provides [t] with [len] bytes to write,
      starting at [off]. This byte range is written by calls to
      {!eval} with [t] until [`Flush] is returned. *)

  val refill: int -> int -> t -> t
  (** [refill off len t] provides a new [t] with [len] bytes to read,
      starting at [off]. This byte range is read by calls to {!eval}
      with [t] until [`Await] is returned. The client should fill by
      the {!expect} git object. *)

  val finish: t -> t
  (** [finish t] provides a new [t] which terminate to serialize the
      current Git object. The next call of {!eval} should not return
      an [`Await] value unless the output space is enough to start to
      serialize the next entry. *)

  val expect: t -> Hash.t
  (** [expect t] returns the object expected to the serialization. At
      this time, the serializer requests the inflated raw of this git
      object. The client is able to use it only when {!eval} return
      [`Await]. The encoder does not wqnt the ownership of the
      inflated raw, it access on it only as a read-only flow (that
      means, the src {!Cstruct.t} could be physicaly the same. *)

  val idx: t -> (Crc32.t * int64) Radix.t
  (** [idx t] returns a {!Radix} tree which contains the CRC-32
      checksum and the absolute offset for each Git object serialized.
      The client is able to use it only when {!eval} returns
      [`End]. *)

  val default: Cstruct.t -> (Entry.t * Delta.t) list -> t
  (** Make a new encoder {!t} of the PACK stream.

      [tmp] is a [Cstruct.t] used as an internal output of the Hunk
      Encoder {!H}. We let the user to allocate the optimized length
      for this temporary buffer and we take the ownership (so, you
      don't use it to another computer).

      Then, the client need to notice the ordered list of what he
      wants to serialize. *)

  val eval: Cstruct.t -> Cstruct.t -> t ->
    [ `Flush of t
    | `Await of t
    | `End of (t * Hash.t)
    | `Error of (t * error) ]
    (** [eval src t] is:

        {ul
        {- [`Await t] iff [t] needs more input storage. The client
        must use {!refill} to provide a new buffer and then call
        {!eval} with [`Await] until other value returned.}
        {- [`Flush t] iff [t] needs more output storage. The client
        must use {!flush} to provide a new buffer and then call
        {!eval} with [`Flush] until other value returned.}
        {- [`End (t, hash)] when [t] is done. Then, [t] sticks on this
        situation, the client can remove it. [hash] is the hash
        calculated of the PACK stream.}
        {- [`Error (t, exn)] iff the encoder [t] meet an {!error}
        [exn]. The encoder can't continue and sticks in this
        situation.}} *)
end

module MakePackEncoder
    (Hash: S.HASH)
    (Deflate: S.DEFLATE)
    (Entry: ENTRY with module Hash := Hash)
    (Delta: DELTA with module Hash := Hash
                   and module Entry := Entry)
    (HunkEncoder: H with module Hash := Hash)
  : P with module Hash = Hash
       and module Deflate = Deflate
       and module Entry := Entry
       and module Delta := Delta
       and module HunkEncoder := HunkEncoder
(** The {i functor} to make the PACK encoder by a specific hash
    implementation and a specific deflate algorithm. *)

module MakeStreamEncoder
    (Hash: S.HASH)
    (Deflate: S.DEFLATE) : sig
  include P
end with module Hash = Hash
     and module Deflate = Deflate
