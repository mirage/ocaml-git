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

module type S =
sig
  module Hash
    : Ihash.S
  (** The [Hash] module used to make the module. *)

  module Inflate
    : S.INFLATE
  (** The [Inflate] module used to make this interface. *)

  module Deflate
    : S.DEFLATE
  (** The [Deflate] module used to make this interface. *)

  module Blob
    : Blob.S with type Hash.t = Hash.t
  (** The {!Blob} module. *)

  module Commit
    : Commit.S with type Hash.t = Hash.t
  (** The {!Commit} module. *)

  module Tree
    : Tree.S with type Hash.t = Hash.t
  (** The {!Tree} module. *)

  module Tag
    : Tag.S with type Hash.t = Hash.t
  (** The {!Tag} module. *)

  type t =
    | Blob   of Blob.t   (** The {!Blob.t} OCaml value. *)
    | Commit of Commit.t (** The {!Commit.t} OCaml value. *)
    | Tree   of Tree.t   (** The {!Tree.t} OCaml value. *)
    | Tag    of Tag.t    (** The {!Tag.t} OCaml value. *)
  (** OCaml value which represents a Git object. *)

  module A : sig
    include S.ANGSTROM with type t = t

    val kind : [ `Commit | `Tree | `Tag | `Blob ] Angstrom.t
    (** A convenience parser to recognize the kind of the Git object. It's used
        in the {i loose} implementation and it's better to provide in the
        interface this parser. *)
  end
  (** The Angstrom decoder of the Git object. *)

  module F
    : S.FARADAY  with type t = t
  (** The Faraday encoder of the Git object. *)

  module D
    : S.DECODER  with type t = t
                  and type raw = Cstruct.t
                  and type init = Inflate.window * Cstruct.t * Cstruct.t
                  and type error = [ `Decoder of string | `Inflate of Inflate.error ]
  (** The decoder of the Git object. We constraint the input to be an
      {!Inflate.window} and a {Cstruct.t} which used by the {Inflate} module and an
      other {Cstruct.t} as an internal buffer.

      All error from the {!Inflate} module is relayed to the [`Inflate] error
      value. *)

  module M
    : S.MINIENC  with type t = t
  (** The {!Minienc} encoder of the Git object. *)

  module E
    : S.ENCODER  with type t = t
                  and type raw = Cstruct.t
                  and type init = int * t * int * Cstruct.t
                  and type error = [ `Deflate of Deflate.error ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the level of the
      compression, the value {!t}, the memory consumption of the encoder (in bytes)
      and an internal buffer between the compression and the encoder.

      All error from the {!Deflate} module is relayed to the [`Deflate] error
      value. *)

  include Ihash.DIGEST with type t := t
                        and type hash := Hash.t
  include S.BASE with type t := t
end

module type BUFFER =
sig
  type t
  (** The abstract type of buffers. *)
  type raw
  (** The type of result. *)
  type fixe
  (** The type of input. *)

  val create : int -> t
  (** [create n] returns a fresh buffer, initially empty. The [n] parameter is
      the initial size of the internal byte sequence that holds the buffer contents.
      That byte sequence is automatically reallocated when more than [n] bytes are
      stored in the buffer, but shrinks back to [n] bytes when [reset] is called.

      For best performance, [n] should be of the same order of magnitude as the
      number of characters that are expected to be stored in the buffer. Nothing
      bad will happen if the buffer grows beyond that limit, however. *)

  val contents : t -> raw
  (** Return a copy of the current contents of the buffer. The buffer itself is
      unchanged. *)

  val add : t -> fixe -> unit
  (** [add fixe buffer] appends the fixed-size buffer [fixe] at the end of the
      buffer [buffer]. *)

  val clear : t -> unit
  (** Empty the buffer. *)

  val reset : t -> unit
  (** Empty the buffer and {i de-allocate} the internal byte sequence holding the
      buffer contents, replacing it with the initial internal byte sequence of
      length [n] that was allocated by {!Buffer.create} [n]. For long-lived buffers
      that may have grown a lot, [reset] allows faster reclamation of the space used
      by the buffer. *)
end

module type RAW =
sig
  module Buffer
    : BUFFER
  (** The [Buffer] module used to make this module. *)

  include S

  module EE
    : S.ENCODER
      with type t = t
       and type raw = Cstruct.t
       and type init = int * t
       and type error = [ `Never ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the value
      {!t} and the memory consumption of the encoder (in bytes). The encoder can
      not fail.

      This encoder does not {i deflate} the content (instead {!E}).

      NOTE: we can not unspecified the error type (it needs to be concrete) but,
      because the encoder can not fail, we define the error as [`Never]. *)

  module DD
    : S.DECODER
      with type t = t
       and type raw = Cstruct.t
       and type init = Cstruct.t
       and type error = [ `Decoder of string ]

  val to_deflated_raw : ?capacity:int -> ?level:int -> ztmp:Cstruct.t -> t -> (Buffer.raw, E.error) result
  (** [to_deflated_raw ?capacity ?level ~ztmp value] serializes and deflates the
      value [value]. [capacity] is the memory consumption of the encoder in bytes
      (default to [0x100]), [level] is the level of the compression (default to [4])
      and [ztmp] is an internal buffer used to store the serialized value before the
      {i deflation}.

      All error from the {!Deflate} module is relayed to the [`Deflate] error
      value. *)

  val to_raw : ?capacity:int -> t -> (Buffer.raw, EE.error) result
  (** [to_raw ?capacity value] serializes the value [value]. [capacity] is the
      memory consumption of the encoder in bytes (default to [0x100]).

      This function can not returns an {!EE.error} (see {!EE}). *)

  val of_raw : kind:[ `Commit | `Blob | `Tree | `Tag ] -> Cstruct.t -> (t, [ `Decoder of string ]) result
  (** [of_raw ~kind inflated] makes a Git object as an OCaml value
      {!t}. This decoder does not expect an {i header} to recognize
      which kind of Git object is it. That means the [inflated] raw
      should not contain [kind size\000] at the beginning (in this
      case, you should use {!of_raw_with_header}. *)

  val of_raw_with_header : Cstruct.t -> (t, DD.error) result
  (** [of_raw_with_header inflated] makes a Git object as an OCaml
      value {!t}. This decoder expects an {i header} to choose which Git
      object it is. *)
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (I : S.INFLATE)
    (D : S.DEFLATE)
  : S with module Hash = H
       and module Inflate = I
       and module Deflate = D
(** The {i functor} to make the OCaml representation of the Git object by a
    specific hash implementation, an {!S.INFLATE} implementation for the
    decompression and a {!S.DEFLATE} implementation for the compression. We
    constraint the {!Ihash.S} module to compute a {Cstruct.t} flow and generate
    a [string] as the hexadecimal representation of the hash. *)

module Raw
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    (B : BUFFER with type raw = string
                 and type fixe = Cstruct.t)
  : RAW with module Hash = H
         and module Inflate = I
         and module Deflate = D
         and module Buffer = B
(** The {i functor} to make the OCaml representation of the Git object by a
    specific hash implementation, and {!S.INFLATE} implementation for the
    decompression and a {!S.DEFLATE} implementation for the compression We
    constraint the {!Ihash.S} module to compute a {Cstruct.t} flow and generate a
    [string] as the hexadecimal representation of the hash.

    This module provide a {i blocking} implementation of the serialization and
    the de-serialization of the Git object in a {!BUFFER}. We constraint the
    {!BUFFER} to save a fixed-size buffer {Cstruct.t} in the {!BUFFER.t} and
    return a {Cstruct.t} as a result. *)
