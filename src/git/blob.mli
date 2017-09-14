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
  type t = private Cstruct.t
  (** A Git Blob object. *)

  module Hash
    : S.HASH
  (** The [Hash] module used to make this interface. *)

  module D
    : S.DECODER with type t = t
                 and type raw = Cstruct.t
                 and type init = Cstruct.t
                 and type error = [ `Decoder of string ]
  (** The decoder of the Git Blob object. We constraint the input to be a
      {Cstruct.t}. This decoder needs a {Cstruct.t} as an internal buffer. *)

  module E
    : S.ENCODER with type t = t
                 and type raw = Cstruct.t
  (** The encoder of the Git Blob object. *)

  module A
    : S.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git Blob object. *)

  module F
    : S.FARADAY  with type t = t
  (** The Faraday encoder of the Git Blob object. *)

  include S.DIGEST with type t := t
                    and type hash := Hash.t

  include S.BASE with type t := t
end

module Make
    (H : S.HASH with type Digest.buffer = Cstruct.t)
  : S with module Hash = H
(** The {i functor} to make the OCaml representation of the Git Blob object by a
    specific hash implementation. We constraint the {!Hash.S} module to
    compute a {Cstruct.t} flow. *)
