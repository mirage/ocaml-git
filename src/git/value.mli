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

  module A
    : S.ANGSTROM with type t = t
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

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (I : S.INFLATE)
    (D : S.DEFLATE)
  : S with module Hash = H
       and module Inflate = I
       and module Deflate = D
(** The {i functor} to make the OCaml representation of the Git object by a
    specific hash, an {!Inflate} implementation for the compression and a
    {!Deflate} implementation for the decompression. We constraint the
    {!IDIGEST} module to compute a {Cstruct.t} flow and generate a [string] as
    the hexadecimal representation of the hash. *)
