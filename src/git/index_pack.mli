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

(** Index pack implementation (serialization/unserialization). *)

(** Interface which describes the lazy implementation of the decoder of an IDX
    file. *)
module type LAZY = sig
  (** The [Hash] module used to make the implementation. *)
  module Hash : S.HASH

  (** The type error. *)
  type error =
    | Invalid_header of string
        (** Appear when the header of the IDX file is incorrect. *)
    | Invalid_version of int32
        (** Appear when the version of the IDX file is wrong. *)
    | Invalid_index
        (** Appear when we try to read an area outside the IDX file. *)
    | Expected_bigoffset_table
        (** Appear when we try to read a big offset table and we can't catch
            it. *)
    | Invalid_bigoffset_index of int
        (** Appear when we try to read a big offset value and we can't catch
            it. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** State of the IDX lzy decoder. *)
  type t

  val make : ?cache:int -> Cstruct.t -> (t, error) result
  (** Make a new state from a [Cstruct.t] buffer. You can specify how many
      elements we can store to the cache. This function returns the state [t]
      or an {!error}.

      Indeed, in this function we check if the IDX file stored entirely on the
      {!Cstruct.t} is well-formed. Otherwise, we return an explicit error. *)

  val find : t -> Hash.t -> (Checkseum.Crc32.t * int64) option
  (** [find t hash] get the CRC-32 checksum and the absolute offset binded on
      [hash] in the IDX file represented by [t] only if [hash] exists.
      Otherwise, it returns [None]. *)

  val mem : t -> Hash.t -> bool
  (** [mem t hash] returns [true] if [hash] exists in the IDX file represented
      by [t]. Otherwise, it returns [false]. *)

  val iter : t -> (Hash.t -> Checkseum.Crc32.t * int64 -> unit) -> unit
  (** Iteration in the IDX file. *)

  val fold : t -> (Hash.t -> Checkseum.Crc32.t * int64 -> 'a -> 'a) -> 'a -> 'a
  (** Fold in the IDX file. *)

  val cardinal : t -> int
end

(** The {i functor} to make the {i lazy} decoder of the IDX file. Internally,
    we use a [Cstruct.t] representation of the IDX file notified to the [make]
    function. This [Cstruct.t] should never change by the client. All processes
    available in this module read only the content.

    By {i lazy}, we mean that we don't try to make an OCaml value which
    contains all binded values available in the IDX file - and, by this way, we
    don't process entirely the file. We read values only when the client ask to
    get these values - {i call by need}. Use this decoder with the {i syscall}
    [mmap] to get the expected {!Cstruct.t} could be useful when [mmap] do a
    lazy read. *)
module Lazy (Hash : S.HASH) : LAZY with module Hash := Hash

(** Interface which describes the implementation of the decoder of an IDX file. *)
module type DECODER = sig
  (** The [Hash] module used to make the implementation. *)
  module Hash : S.HASH

  (** The error type. *)
  type error =
    | Invalid_byte of int
        (** Appear when we expect a specific byte and we catch another one. *)
    | Invalid_version of Int32.t
        (** Appear when the version of the IDX file is wrong. *)
    | Invalid_index_of_bigoffset of int
        (** Appear when we try to read a big offset value and we can't catch
            it. *)
    | Expected_bigoffset_table
        (** Appear when we don't have a big offset table but expect one. *)
    | Invalid_hash of Hash.t * Hash.t
        (** Appear when the hash produced when we un-serialize the IDX file
            does not correspond with the hash provided. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** The decoder state. *)
  type t

  val pp : t Fmt.t
  (** Pretty-printer of the decoder {!t}. *)

  val make : unit -> t
  (** Make a new decoder state {!t}. *)

  val refill : int -> int -> t -> t
  (** [refill off len t] provides a new [t] with [len] bytes to read, starting
      at [off]. This byte range is read by calls to {!eval} with [t] until
      [`Await] is returned. *)

  val eval :
       Cstruct.t
    -> t
    -> [ `Await of t
       | `End of t * Hash.t
       | `Hash of t * (Hash.t * Checkseum.Crc32.t * int64)
       | `Error of t * error ]
  (** [eval src t] is:

      {ul

      {- [`Await t] iff [t] needs more input storage. The client must use
      {!refill} to provide a new buffer and then call {!eval} with [`Await]
      until other value returned.} {- [`End (t, hash)] when [t] is done. We
      returns the hash of the IDX file.} {- [`Hash (t, (hash, crc, offset))]
      when [t] can returns a new value [(hash, crc, offset)]. The client can
      call {!eval} to continue the process. The value will be consumed then.}
      {- [`Error (t, exn)] iff the decoder meet an {!error} [exn]. The decoder
      can't continue and sticks in this situation.}} *)
end

(** The {i functor} to make the decoder module by a specific hash
    implementation. We constraint the {!Hash.S} module to compute a {Cstruct.t}
    flow. This module is a {i non-blocking} decoder with a pure state of the
    IDX file. It's better to use this module instead {!Lazy} if the client
    wants an OCaml representation of the IDX file - he can construct this
    specific OCaml value step by step with this decoder like a Radix tree.

    In the result, if the client construct an efficient data-structure (like a
    Radix tree) when he decodes the IDX file, the [find] operation should be
    more fast than the {!Lazy.find}. However, the {!Lazy.make} operation could
    be more fast than to decode and to construct an OCaml value. *)
module Decoder (Hash : S.HASH) : DECODER with module Hash := Hash

(** Interface which describes the implementation of the encoder of an IDX file. *)
module type ENCODER = sig
  (** The [Hash] module used to make the implementation. *)
  module Hash : S.HASH

  (** The type error. We can't have an error to serialize an IDX file - it's
      just to homogenize interfaces each others. *)
  type error

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** The encoder state. *)
  type t

  val pp : t Fmt.t
  (** Pretty-printer of the encoder {!t}. *)

  (** An abstract representation of an iterative container. *)
  type 'a sequence = ('a -> unit) -> unit

  val default : (Hash.t * (Checkseum.Crc32.t * int64)) sequence -> Hash.t -> t
  (** [default seq pack_hash] makes a new {!encoder} to serialize [seq] and
      associates the IDX stream produced with the [pack_hash] PACK file. This
      function takes care about the order of [seq], so the client does not need
      to sort the iterative container. *)

  val flush : int -> int -> t -> t
  (** [flush off len t] provides [t] with [len] bytes to write, starting at
      [off]. This byte range is written by calls to {!eval} with [t] until
      [`Flush] is returned. Use {!used_out} to know how many byte [t] wrote. *)

  val used_out : t -> int
  (** [used_out t] returns how many byte [t] wrote in the current buffer
      noticed to the previous call of {!eval}. *)

  val eval : Cstruct.t -> t -> [`Flush of t | `End of t | `Error of t * error]
  (** [eval dst t] is:

      {ul {- [`Flush t] iff [t] needs more output storage. The client must use
      {!flush} to provide a new buffer and then call {!eval} with [`Flush]
      until [`End] is returned.} {- [`End t] when the encoder is done. [t]
      sticks to this situation. The client can remove it.} {- [`Error (t, exn)]
      iff the encoder meet an {!error} [exn]. The encoder can't continue and
      sticks in this situation.}} *)
end

(** The {i functor} to make the encoder module by a specific hash
    implementation. *)
module Encoder (Hash : S.HASH) : ENCODER with module Hash := Hash
