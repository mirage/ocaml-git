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

(** A Git Tree object.

    A Tree object ties on or more {!Blob.t} objects into a directory structure.
    In addition, a tree object can refer to other tree objects, thus creating a
    directory hierarchy. *)

type perm =
  [ `Normal  (** A {!Blob.t}. *)
  | `Everybody
  | `Exec  (** An executable. *)
  | `Link  (** A {!Blob.t} that specifies the path of a {i symlink}. *)
  | `Dir  (** A sub-{!Tree.t}. *)
  | `Commit  (** A sub-module ({!Commit.t}). *) ]
(** Type of type node. *)

val equal_perm : perm -> perm -> bool
(** The equal function for {!perm}. *)

type 'hash entry = { perm : perm; name : string; node : 'hash }
(** Type of entry. It describes the name [name], the type [perm] and the
    reference to the node [node]. *)

val entry : name:string -> perm -> 'hash -> 'hash entry
(** [entry ~name perm node] is the entry to [node] with the name [name] and the
    {i mode} [perm]. *)

val pp_entry : pp:'hash Fmt.t -> 'hash entry Fmt.t
(** Pretty-printer of {!entry}. *)

val equal_entry :
  equal:('hash -> 'hash -> bool) -> 'hash entry -> 'hash entry -> bool
(** The equal function for {!entry}. *)

type 'hash t = private 'hash entry list
(** Type of tree is a list of file names and modes along with refs to the
    associated blob and/or tree objects. *)

val v : 'hash entry list -> 'hash t
(** [v entries] ties all [entries] into one tree object. It does not remove
    duplicate but re-order the given list such as:

    {[
      Tree.digest (Tree.v [ { name= a; _ }; { name= b; _ }] ) ;;
      - : hash = 8d14531846b95bfa3564b58ccfb7913a034323b8
      Tree.digest (Tree.v [ { name= b; _ }; { name= a; _ }] ) ;;
      - : hash = 8d14531846b95bfa3564b58ccfb7913a034323b8
    ]} *)

val pp : pp:'hash Fmt.t -> 'hash t Fmt.t
(** Pretty-printer of {!t}. *)

val equal : equal:('hash -> 'hash -> bool) -> 'hash t -> 'hash t -> bool
(** The equal function for {!t}. *)

val add : 'hash entry -> 'hash t -> 'hash t
(** [add entry tree] returns a tree containing all elements of [tree], plus
    [entry]. If [entry] was already in [tree], [tree] is unchanged. *)

val remove : name:string -> 'hash t -> 'hash t
(** [remove ~name tree] returns a [tree] containing all elements of [tree],
    except one with the name [name]. If any entries of the given [tree] don't
    have the name [name], [tree] is returned unchanged. *)

val is_empty : 'hash t -> bool
(** [is_empty tree] tests whether the given [tree] is empty or not. *)

val hashes : 'hash t -> 'hash list
(** [hashes tree] returns the list of all node references of the given [tree]. *)

val to_list : 'hash t -> 'hash entry list
(** [to_list tree] returns the list of all entries of the given [tree]. *)

val of_list : 'hash entry list -> 'hash t
(** Same as {!v}. *)

val iter : ('hash entry -> unit) -> 'hash t -> unit
(** [iter f tree] applies [f] in turn to all elements of [s]. *)

module type S = sig
  type hash

  type nonrec entry = hash entry

  type nonrec t = hash t

  val entry : name:string -> perm -> hash -> entry
  (** [entry ~name perm node] is the entry to [node] with the name [name] and
      the {i mode} [perm]. *)

  val v : entry list -> t
  (** [v entries] ties all [entries] into one tree object. It does not remove
      duplicate but re-order the given list such as:

      {[
        Tree.digest (Tree.v [ { name= a; _ }; { name= b; _ }] ) ;;
        - : hash = 8d14531846b95bfa3564b58ccfb7913a034323b8
        Tree.digest (Tree.v [ { name= b; _ }; { name= a; _ }] ) ;;
        - : hash = 8d14531846b95bfa3564b58ccfb7913a034323b8
      ]} *)

  val add : entry -> t -> t
  (** [add entry tree] returns a tree containing all elements of [tree], plus
      [entry]. If [entry] was already in [tree], [tree] is unchanged. *)

  val remove : name:string -> t -> t
  (** [remove ~name tree] returns a [tree] containing all elements of [tree],
      except one with the name [name]. If any entries of the given [tree] don't
      have the name [name], [tree] is returned unchanged. *)

  val is_empty : t -> bool
  (** [is_empty tree] tests whether the given [tree] is empty or not. *)

  val format : t Encore.t
  (** [format] is a description of how to encode/decode a {!t} object. *)

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64
  (** [length tree] is the length of the given tree object. *)

  val hashes : t -> hash list
  (** [hashes tree] returns the list of all node references of the given [tree]. *)

  val to_list : t -> entry list
  (** [to_list tree] returns the list of all entries of the given [tree]. *)

  val of_list : entry list -> t
  (** Same as {!v}. *)

  val iter : (entry -> unit) -> t -> unit
  (** [iter f tree] applies [f] in turn to all elements of [s]. *)
end

(** {i Functor} building an implementation of the tree structure. The {i
    functor} returns a structure containing a type [hash] of digests and a type
    [t] of trees (structurally equal to {!t}). *)
module Make (Hash : S.HASH) : S with type hash = Hash.t
