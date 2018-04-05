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
(** A Git Blob object which store entirely your file of your Git repository. *)

module type S = sig

  type nonrec t = t
  (** The Git Blob is structurally a {!Cstruct.t} independantly of the
      hash implementation. *)

  module Hash: S.HASH
  module MakeMeta: functor (Meta: Encore.Meta.S) -> sig val p: t Meta.t end

  module A: S.DESC    with type 'a t = 'a Angstrom.t and type e = t
  module M: S.DESC    with type 'a t = 'a Encore.Encoder.t and type e = t
  module D: S.DECODER with type t = t and type init = Cstruct.t and type error = Error.Decoder.t
  module E: S.ENCODER with type t = t and type error = Error.never
  include S.DIGEST    with type t := t and type hash := Hash.t
  include S.BASE      with type t := t

  val length: t -> int64
  (** [length t] returns the length of the blob object [t]. Note that we use
     {!Cstruct.len} and cast result to [int64]. *)

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

module Make (Hash: S.HASH): S with module Hash = Hash
(** The {i functor} to make the OCaml representation of the Git Blob
    object by a specific hash implementation. *)
