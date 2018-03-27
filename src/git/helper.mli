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

(** The Helper module should mnot be used by the client but only by
    maintainers of ocaml-git. It provides useful functions to interact
    with any I/O operations. *)

(** [ppe ~name pp] make a new pretty-printer which shows your [value]
    like ["(name %a)" pp value]. *)
val ppe : name:string -> 'a Fmt.t -> 'a Fmt.t

module Pair:
sig
  val flip: ('a * 'b) -> ('b * 'a)
  val fst: ('a * 'b) -> 'a
  val snd: ('a * 'b) -> 'b
end

module Option:
sig
  val map: ('a -> 'b) -> 'a option -> 'b option
  val ( >>= ): 'a option -> ('a -> 'b) -> 'b option
end

(** Helper for some convenience bijection elements. *)
module BaseIso:
sig
  val flip: ('a * 'b) -> ('b * 'a)

  val int64: (string, int64) Encore.Bijection.texn
  val cstruct: (Encore.Lole.bigstring, Cstruct.t) Encore.Bijection.texn
  val char_elt: char -> (char, unit) Encore.Bijection.texn
  val string_elt: string -> (string, unit) Encore.Bijection.texn
end

(** [MakeDecoder] makes a module which respects the interface
    {!S.DECODER} from an angstrom decoder.

    This decoder is implemented on top of some assertions (see comment
    in the [helper.ml] file for more informations), don't use it
    outside the scope of ocaml-git. *)
module MakeDecoder (A: S.DESC with type 'a t = 'a Angstrom.t): S.DECODER
  with type t = A.e
   and type init = Cstruct.t
   and type error = Error.Decoder.t

(** [MakeInflater] makes a module which respects the interface
    {!S.DECODER} from an angstrom decoder and an inflate implementation.

    This module use internallya {!MakeDecoder}, so we have the same
    assertions - it's an extension of the previous decoder with an
    inflator. Then, this decoder decodes both the inflated flow and
    the serialized flow with fixed-size buffers. *)
module MakeInflater (Z: S.INFLATE) (A: S.DESC with type 'a t = 'a Angstrom.t): S.DECODER
  with type t = A.e
   and type init = Z.window * Cstruct.t * Cstruct.t
   and type error = [ Error.Decoder.t | `Inflate of Z.error ]

(** [MakeEncoder] makes a module which respects the interface
    {!S.ENCODER} from a {!S.MINIENC.encoder}. This module (instead
    {!MakeEncoder} allocates one buffer from a size specified by the user.
    However this size must be a power of two.

    Then, this encoder [`Never] fails but we define a concrete error
    type to allow the client to match on it - then, he can use [asset
    false] without fear. *)
module MakeEncoder (M: S.DESC with type 'a t = 'a Encore.Encoder.t): S.ENCODER
  with type t = M.e
   and type init = int * M.e
   and type error = Error.never

(** [MakeDeflater] makes a module which respects the interface
    {!S.ENCODER} from a {!S.MINIENC.encoder}. As {!MakeEncoder}, this
    module allocates one buffer from a size specified by the user.
    However this size must be a power of two.

    This encoder encodes both the OCaml value and deflates the
    stream. *)
module MakeDeflater (Z: S.DEFLATE) (M: S.DESC with type 'a t = 'a Encore.Encoder.t): S.ENCODER
  with type t = M.e
   and type init = int * M.e * int * Cstruct.t
   and type error = [ `Deflate of Z.error ]

(** [digest (module Hash) (module Faraday) ~kind value] digests
    [value] with the [Hash] implementation and use the [Faraday]
    encoder to {i stream} on the [Hash.digest] function. [kind] is the
    kind of the [value] (Commit, Blob, etc.) to make the Git header in
    top of the serialized [value]. *)
val digest:
  (module S.IDIGEST with type t = 'hash) ->
  (module S.FARADAY with type t = 't) -> kind:string -> 't -> 'hash

(** [fdigest (module Hash) (module Encoder) ?capacity ~tmp ~kind
    ~length value] digests [value] with the [Hash] implementation and use
    the [Encoder] to stream on the [Hash.digest] function.

    [?capacity] is the size of the internal buffer used to serialize
    your [value] (must be a power of two).

    [kind] is the kind of the [value] (Commit, Blob, etc.) to make the
    Git header in top of the serialized [value].

    [length] is the function which calculate the weight of the [value]
    when it is serialized.

    [tmp] is aan internal buffer used to store the stream of the
    encoder and used by [Hash.digest]. *)
val fdigest:
  (module S.IDIGEST with type t = 'hash) ->
  (module S.ENCODER
    with type t = 't
     and type init = int * 't
     and type error = Error.never) ->
  ?capacity:int -> tmp:Cstruct.t -> kind:string -> length:('t -> int64) -> 't -> 'hash

module type ENCODER =
sig
  type state
  type result
  type error

  val eval: Cstruct.t -> state ->
    [ `Flush of state
    | `End of (state * result)
    | `Error of (state * error) ] Lwt.t

  val used: state -> int
  val flush: int -> int -> state -> state
end

module Encoder (E: ENCODER) (FS: S.FS): sig

  type error =
    [ `Encoder of E.error
    | FS.error Error.FS.t ]

  (** [to_file t f tmp state] encodes [state] in the file [f] using
      [tmp] as an intermediary buffer.

      The result is [Error `Stack] if at least [limit] writes have
      returned 0.

      If [atomic] is set, the operation is guaranteed to be atomic. *)
  val to_file: ?limit:int -> ?atomic:bool -> FS.t -> Fpath.t -> Cstruct.t ->
    E.state -> (E.result, error) result Lwt.t

end

module FS (FS: S.FS): sig

  include (module type of struct include FS end)

  val with_open_r: FS.t -> Fpath.t ->
    ([ `Read ] FS.File.fd ->
     ('a, [> `Open of Fpath.t * FS.error
          | `Move of Fpath.t * Fpath.t * FS.error ] as 'e) result Lwt.t) ->
    ('a, 'e) result Lwt.t
  (** [with_open_r t p f] opens the file [p] in the file-system
      [t] with read-only mode and calls [f] on the resulting
      file-descriptor. When [f] completes, the file-descriptor is
      closed. Failure to close that file-descriptor is ignored. *)

  val with_open_w: ?atomic:bool -> FS.t -> Fpath.t ->
    ([ `Write ] FS.File.fd ->
     ('a, [> `Open of Fpath.t * FS.error
          | `Move of Fpath.t * Fpath.t * FS.error ] as 'e) result Lwt.t) ->
    ('a, 'e) result Lwt.t
    (** Same as {!with_open_r} but the file [p] is opened in
        read-write mode. If [atomic] is set (it is by default), the
        application of [f] is guaranteed to be atomic. This is done by
        creating a temporary file and renaming it when all the work is
        done. *)

end
