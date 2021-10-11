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

(** A Git Tag object.

    A tag containing a reference pointing to another object, which can contain a
    message just like a {!module:Commit}. It can also contain a (PGP) signature, in
    which case it is called a "signed tag object". *)

type kind = Blob | Commit | Tag | Tree

type 'hash t
(** A Git Tag object. The tag object is very much like a {!Commit.t} object - it
    contains a {i tagger}, a date, a message, and a pointer. Generally, the tag
    points to a commit rather than a tree. It's like a branch reference, but it
    never moves - it always points to the same commit but gives it a friendlier
    name. *)

module type S = sig
  type hash
  type nonrec t = hash t

  val make : hash -> kind -> ?tagger:User.t -> tag:string -> string option -> t
  (** [make hash kind ?tagger ~tag descr] makes a new tag with the kind [kind]
      by the [tagger] with the name [tag] and the description [descr].

      This function does not check if the reference [hash] points to an existing
      [kind] Git object. *)

  val format : t Encore.t
  (** [format] is a description of how to encode/decode of {!t} object. *)

  include S.DIGEST with type t := t and type hash := hash
  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] is the length of the given tag object. *)

  val obj : t -> hash
  (** [obj t] returns the reference of the given tag. *)

  val tag : t -> string
  (** [tag t] returns the information of the given tag. *)

  val message : t -> string option
  val kind : t -> kind
  val tagger : t -> User.t option
end

(** {i Functor} building an implementation of the tag structure. The {i functor}
    returns a structure containing a type [hash] of digests and a type [t] of
    tags (structurally equal to {!t}). *)
module Make (Hash : S.HASH) : S with type hash = Hash.t
