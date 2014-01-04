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

open Core_kernel.Std

module SHA1 = struct

  include (String: Identifiable.S)

  module Commit: Identifiable.S = String
  module Tree: Identifiable.S = String
  module Blob: Identifiable.S = String

  let commit c = of_string (Commit.to_string c)
  let to_commit n = Commit.of_string (to_string n)
  let tree t = of_string (Tree.to_string t)
  let blob b = of_string (Blob.to_string b)

  let sha1 str =
    let hash = Cryptokit.Hash.sha1 () in
    hash#add_string str;
    of_string hash#result
end

type sha1 = SHA1.t

type user = {
  name : string;
  email: string;
  date : string;
}

module Blob: Identifiable.S = String

type blob = Blob.t

type commit = {
  tree     : SHA1.Tree.t;
  parents  : SHA1.Commit.t list;
  author   : user;
  committer: user;
  message  : string;
}

type entry = {
  perm: [`normal|`exec|`link|`dir];
  file: string;
  node: sha1;
  }

type tree = entry list

type tag = {
  commit     : SHA1.Commit.t;
  tag        : string;
  tagger     : user;
  tag_message: string;
}

type hunk =
  | Insert of string
  | Copy of int * int

type 'a delta = {
  source: 'a;
  source_length: int;
  result_length: int;
  hunks: hunk list;
}

type value =
  | Blob   of blob
  | Commit of commit
  | Tag    of tag
  | Tree   of tree

type packed_value =
  | Value     of value
  | Ref_delta of sha1 delta
  | Off_delta of int delta

type pack_index = {
  offsets: int SHA1.Map.t;
  lengths: int option SHA1.Map.t;
}

type pack = sha1 -> packed_value

let commit c = Commit c
let blob b = Blob b
let tree t = Tree t
let tag t = Tag t

let value v = Value v
let ref_delta d = Ref_delta d
let off_delta d = Off_delta d

type t = {
  root   : string;
  buffers: (sha1, Mstruct.t) Hashtbl.t;
  packs  : (sha1, pack) Hashtbl.t;
  indexes: (sha1, pack_index) Hashtbl.t;
}
