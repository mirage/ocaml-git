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

(** A Git Tree object. *)


type perm =
  [ `Normal  (** A {!Blob.t}. *)
  | `Everybody
  | `Exec  (** An executable. *)
  | `Link  (** A {!Blob.t} that specifies the path of a {i symlink}. *)
  | `Dir  (** A sub-{!Tree.t}. *)
  | `Commit  (** A sub-module ({!Commit.t}). *) ]

val equal_perm : perm -> perm -> bool

type 'hash entry = { perm : perm; name : string; node : 'hash }

val entry : name:string -> perm -> 'hash -> 'hash entry

val pp_entry : pp:'hash Fmt.t -> 'hash entry Fmt.t

val equal_entry :
  equal:('hash -> 'hash -> bool) -> 'hash entry -> 'hash entry -> bool

type 'hash t = private 'hash entry list

val v : 'hash entry list -> 'hash t


val pp : pp:'hash Fmt.t -> 'hash t Fmt.t
val equal : equal:('hash -> 'hash -> bool) -> 'hash t -> 'hash t -> bool
val add : 'hash entry -> 'hash t -> 'hash t
val remove : name:string -> 'hash t -> 'hash t
val is_empty : 'hash t -> bool
val hashes : 'hash t -> 'hash list
val to_list : 'hash t -> 'hash entry list
val of_list : 'hash entry list -> 'hash t
val iter : ('hash entry -> unit) -> 'hash t -> unit

module type S = sig
  type hash

  type nonrec entry = hash entry

  type nonrec t = hash t

  val entry : name:string -> perm -> hash -> entry

  val v : entry list -> t

  val add : entry -> t -> t

  val remove : name:string -> t -> t

  val is_empty : t -> bool

  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] returns the length of the tree object [t]. *)

  val hashes : t -> hash list

  val to_list : t -> entry list

  val of_list : entry list -> t

  val iter : (entry -> unit) -> t -> unit
end

(** The {i functor} to make the OCaml representation of the Git Tree object by a
    specific hash implementation. *)
module Make (Hash : S.HASH) : S with module Hash := Hash
