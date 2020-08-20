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


type kind = Blob | Commit | Tag | Tree

type 'hash t

module type S = sig
  type hash

  type nonrec t = hash t

  val make : hash -> kind -> ?tagger:User.t -> tag:string -> string -> t


  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] returns the length of the tag object [t]. *)

  val obj : t -> hash

  val tag : t -> string
  (** [tag t] returns the tag information of [t]. *)

  val message : t -> string

  val kind : t -> kind

  val tagger : t -> User.t option
end

(** The {i functor} to make the OCaml representation of the Git Tag object by a
    specific hash implementation. *)
module Make (Hash : S.HASH) : S with type hash = Hash.t
