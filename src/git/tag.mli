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

(** A Git Tag object. *)

module type S = sig
  module Hash : S.HASH
  (** The [Hash] module used to make the implementation. *)

  type t
  (** A Git Tag object. The tag object is very much like a {!Commit.t} object -
      it contains a {i tagger}, a date, a message, and a pointer. Generally, the
      tag points to a commit rather than a tree. It's like a branch reference,
      but it never moves - it always points to the same commit but gives it a
      friendlier name. *)

  type kind = Blob | Commit | Tag | Tree

  val make : Hash.t -> kind -> ?tagger:User.t -> tag:string -> string -> t
  (** [make hash kind ?tagger ~tag descr] makes a new tag to a Git object with
      the kind [kind] by the [tagger] with the name [tag] and the description
      [descr].

      This function does not check if the Git object pointed by the hash has
      really the kind [kind] - and obviously if the [hash] is valid. *)

  module MakeMeta (Meta : Encore.Meta.S) : sig
    val p : t Meta.t
  end

  module A : S.DESC with type 'a t = 'a Angstrom.t and type e = t

  module M : S.DESC with type 'a t = 'a Encore.Encoder.t and type e = t

  module D :
    S.DECODER
      with type t = t
       and type init = Cstruct.t
       and type error = Error.Decoder.t

  module E :
    S.ENCODER
      with type t = t
       and type init = Cstruct.t * t
       and type error = Error.never

  include S.DIGEST with type t := t and type hash := Hash.t

  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] returns the length of the tag object [t]. *)

  val obj : t -> Hash.t
  (** [obj t] returns the pointed hash of the Tag [t]. *)

  val tag : t -> string
  (** [tag t] returns the tag information of [t]. *)

  val message : t -> string

  val kind : t -> kind

  val tagger : t -> User.t option
end

(** The {i functor} to make the OCaml representation of the Git Tag object by a
    specific hash implementation. *)
module Make (Hash : S.HASH) : S with module Hash := Hash
