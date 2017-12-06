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

module BaseBytes:
sig
  include S.BASE with type t = Bytes.t

  val to_hex : t -> string
  val of_hex : string -> t
end

(** [MakeDecoder] makes a module which respects the interface
    {!S.DECODER} from an angstrom decoder.

    This decoder is implemented on top of some assertions (see comment
    in the [helper.ml] file for more informations), don't use it
    outside the scope of ocaml-git. *)
module MakeDecoder (A :S.ANGSTROM): S.DECODER
  with type t = A.t
   and type init = Cstruct.t
   and type error = [ `Decoder of string ]

(** [MakeInflater] makes a module which respects the interface
    {!S.DECODER} from an angstrom decoder and an inflate implementation.

    This module use internallya {!MakeDecoder}, so we have the same
    assertions - it's an extension of the previous decoder with an
    inflator. Then, this decoder decodes both the inflated flow and
    the serialized flow with fixed-size buffers. *)
module MakeInflater (Z: S.INFLATE) (A: S.ANGSTROM): S.DECODER
  with type t = A.t
   and type init = Z.window * Cstruct.t * Cstruct.t
   and type error = [ `Decoder of string | `Inflate of Z.error ]

(** [MakeEncoder] makes a module which respects the interface
    {!S.ENCODER} from a {!S.MINIENC.encoder}. This module (instead
    {!MakeEncoder} allocates one buffer from a size specified by the user.
    However this size must be a power of two.

    Then, this encoder [`Never] fails but we define a concrete error
    type to allow the client to match on it - then, he can use [asset
    false] without fear. *)
module MakeEncoder (M: S.MINIENC): S.ENCODER
  with type t = M.t
   and type init = int * M.t
   and type error = [ `Never ]

(** [MakeDeflater] makes a module which respects the interface
    {!S.ENCODER} from a {!S.MINIENC.encoder}. As {!MakeEncoder}, this
    module allocates one buffer from a size specified by the user.
    However this size must be a power of two.

    This encoder encodes both the OCaml value and deflates the
    stream. *)
module MakeDeflater (Z: S.DEFLATE) (M: S.MINIENC): S.ENCODER
  with type t = M.t
   and type init = int * M.t * int * Cstruct.t
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
     and type error = [ `Never ]) ->
  ?capacity:int -> tmp:Cstruct.t -> kind:string -> length:('t -> int64) -> 't -> 'hash

module type ENCODER =
sig
  type state
  type raw
  type result
  type error

  val raw_length: raw -> int
  val raw_blit: raw -> int -> raw -> int -> int -> unit

  val eval: string -> raw -> state ->
    [ `Flush of state
    | `End of (state * result)
    | `Error of (state * error) ] Lwt.t

  val used: state -> int
  val flush: int -> int -> state -> state
end

type ('state, 'raw, 'result, 'error) encoder =
  (module ENCODER
    with type state = 'state
     and type raw = 'raw
     and type result = 'result
     and type error = 'error)
and ('fd, 'raw, 'error) writer = 'raw -> ?off:int -> ?len:int -> 'fd -> (int, 'error) result Lwt.t

(** [safe_encoder_to_file ~limit encoder writer fd tmp state] encodes
    a value contained in [state] to a file represented by [fd] with
    the output operation [writer]. This function takes care about
    shift/compress the buffer [tmp] from the value returned by
    [writer]. Then, if [writer] returns [0], we continue to try to
    write [limit] times - if [writer] continue to fail, we stop and
    return an error [`Stack].

    If we retrieve an encoder error while we serialize the value, we
    stop and return [`Encoder]. If the writer returns an error, we
    stop and retrun [`Writer].

    In any case, we {b don't close} the file-descriptor. Then, if we
    retrieve an error, that means we wrote {b partially} the value in
    your file-system - and you need to take care about that.
*)
val safe_encoder_to_file:
  string ->
  limit:int ->
  ('state, 'raw, 'res, 'err_encoder) encoder ->
  ('fd, 'raw, 'err_writer) writer ->
  'fd -> 'raw -> 'state ->
  ('res, [ `Stack | `Encoder of 'err_encoder | `Writer of 'err_writer ]) result Lwt.t
