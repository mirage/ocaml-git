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

(** A Git Commit object. *)

module type S = sig


  val make :
    tree:Hash.t ->
    author:User.t ->
    committer:User.t ->
    ?parents:Hash.t list ->
    ?extra:(string * string list) list ->
    string ->
    t
  (** [make ~author ~committer ?parents ~tree msg] makes an OCaml value {!t}.
      [?parents] should be a non-empty list and corresponds to a list of hashes
      of commits. [tree] should be a hash of a {Tree.t} object.

      This function does not write a new commit on the store and does not check
      the validity of [parents] and [tree]. By this way, this function never
      fails. *)




  include S.BASE with type t := t

  val length : t -> int64
  (** [length t] returns the length of the commit object [t]. *)

  (** [parents c] returns all parents of the Git Commit object [c]. *)

  (** [tree c] returns the hash of top-level {!Tree.t} of the Git Commit object
      [c]. *)

  val committer : t -> User.t
  (** [committer c] returns the committer of the commit [c]. *)

  val author : t -> User.t
  (** [author c] returns the author of the commit [c]. *)

  val message : t -> string
  (** [message c] returns the message of the commit [c]. *)

  val extra : t -> (string * string list) list

  val compare_by_date : t -> t -> int
  (** [compare_by_date a b] compares the Git Commit object [a] and [b] by the
      date of the author. The {!compare} function as the same behaviour. *)
end

(** The {i functor} to make the OCaml representation of the Git Commit object by
    a specific hash implementation. *)
module Make (Hash : S.HASH) : S with module Hash := Hash
