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

(** A Git Blob object. *)

type t = private Cstruct.t
(** A Git Blob object which store entirely your files of your Git
    repository. *)

module type S = sig

  type nonrec t = t
  (** The Git Blob is structurally a {!Cstruct.t} independantly of the
      hash implementation. *)

  module Hash: S.HASH
  (** The [Hash] module used to make the implementation. *)

  module D: S.DECODER
      with type t = t
       and type init = Cstruct.t
       and type error = Error.never
  (** The decoder of the Git Blob object. We constraint the input to
      be a {Cstruct.t}. This decoder needs a {Cstruct.t} as an
      internal buffer. *)

  module E: S.ENCODER
    with type t = t
     and type error = Error.never
  (** The encoder of the Git Blob object. *)

  module A: sig type nonrec t = t val decoder : int -> t Angstrom.t end
  (** The Angstrom decoder of the Git Blob object. *)

  module F: S.FARADAY with type t = t
  (** The Faraday encoder of the Git Blob object. *)

  include S.DIGEST
    with type t := t
     and type hash := Hash.t

  include S.BASE with type t := t

  val of_cstruct: Cstruct.t -> t
  (** [of_cstruct cs] returns the blob value of a [Cstruct.t]. This
      function does not take the ownership on [cs]. So, consider at
      this time to not change [cs] and consider it as a constant. *)

  val to_cstruct: t -> Cstruct.t
  (** [to_cstruct blob] returns the [Cstruct.t] of the Blob [blob].
      This function does not create a fresh [Cstruct.t], so consider
      it as a constant [Cstruct.t] ([set] functions not allowed). *)

  val of_string: string -> t
  (** [of_string s] returns the blob value of a [string]. *)

  val to_string: t -> string
  (** [to_string blob] returns a [string] which contains the [blob]
      value - in other words, the content of your file. *)
end

module Make (H: S.HASH): S with module Hash = H
(** The {i functor} to make the OCaml representation of the Git Blob
    object by a specific hash implementation. *)
