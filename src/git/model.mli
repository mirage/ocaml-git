(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Git data-model *)

open Lib

(** Abstrat type for SHA1 strings. *)
module SHA1:  Abstract.SIG

type sha1 = SHA1.t

(** Abstract type for hexadecimal strings. *)
module type HEXSIG = sig
  include Abstract.SIG
  val of_sha1: sha1 -> t
  val to_sha1: t -> sha1
end

(** We have an abstract type by underlying object type. *)
module Hex: sig
  include HEXSIG
  module Commit: HEXSIG
  module Tree  : HEXSIG
end

(** Git user. *)
type user = {
  name : string;
  email: string;
  date : string;
}

(** Git blob. This is just a raw string. *)
module Blob: Abstract.SIG

type blob = Blob.t

(** Git commit. *)
type commit = {
  tree     : Hex.Tree.t;
  parents  : Hex.Commit.t list;
  author   : user;
  committer: user;
  message  : string;
}

(** Git tree entry. *)
type entry = {
  perm: [`normal|`exec|`link|`dir];
  file: string;
  sha1: sha1;
  }

(** A tree is simply a list of entries. *)
type tree = entry list

(** Git tags. *)
type tag = {
  commit     : Hex.Commit.t;
  tag        : string;
  tagger     : user;
  tag_message: string;
}

(** Git objects. *)
type obj =
  | Blob   of blob
  | Commit of commit
  | Tag    of tag
  | Tree   of tree

(** Git repository. *)
type t = {
  blobs  : (sha1 *  blob ) list;
  commits: (sha1 * commit) list;
  trees  : (sha1 *   tree) list;
  tags   : (sha1 *    tag) list;
}

(** The empty repository. *)
val empty: t

(** Find an object, given its sha1. *)
val find: sha1 -> t -> obj
