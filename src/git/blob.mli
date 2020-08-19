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








module type S = sig
  type hash

  type nonrec t = t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] returns the length of the blob object [t]. Note that we use
      {!Cstruct.len} and cast result to [int64]. *)

  val of_cstruct : Cstruct.t -> t
  (** [of_cstruct cs] returns the blob value of a [Cstruct.t]. This function
      does not take the ownership on [cs]. So, consider at this time to not
      change [cs] and consider it as a constant. *)

  val to_cstruct : t -> Cstruct.t
  (** [to_cstruct blob] returns the [Cstruct.t] of the Blob [blob]. This
      function does not create a fresh [Cstruct.t], so consider it as a constant
      [Cstruct.t] ([set] functions not allowed). *)

  val of_string : string -> t
  (** [of_string s] returns the blob value of a [string]. *)

  val to_string : t -> string
  (** [to_string blob] returns a [string] which contains the [blob] value - in
      other words, the content of your file. *)
end

(** The {i functor} to make the OCaml representation of the Git Blob object by a
    specific hash implementation. *)
module Make (Hash : S.HASH) : S with module Hash := Hash
