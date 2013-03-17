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

open Lib

module SHA1 = Abstract.String

module HexString (U: sig end): sig
  include Abstract.SIG
  val of_sha1: SHA1.t -> t
  val to_sha1: t -> SHA1.t
end = struct

  include Abstract.String

  let of_sha1 sha1 =
    let hex = Misc.hex_encode (SHA1.to_string sha1) in
    of_string hex

  let to_sha1 hex =
    let sha1 = Misc.hex_decode (to_string hex) in
    SHA1.of_string sha1

end

module Hex = struct
  include HexString(struct end)
  module Commit = HexString(struct end)
  module Tree   = HexString(struct end)
end

type user = {
  name : string;
  email: string;
  date : string;
}

module Blob = Abstract.String

type blob = Blob.t

type commit = {
  tree     : Hex.Tree.t;
  parents  : Hex.Commit.t list;
  author   : user;
  committer: user;
  message  : string;
}

type entry = {
  perm: [`normal|`exec|`link|`dir];
  file: string;
  sha1: SHA1.t;
  }

type tree = entry list

type tag = {
  commit     : Hex.Commit.t;
  tag        : string;
  tagger     : user;
  tag_message: string;
}

type git_object =
  | Blob   of blob
  | Commit of commit
  | Tag    of tag
  | Tree   of tree

type t = {
  blobs  : (SHA1.t *  blob ) list;
  commits: (SHA1.t * commit) list;
  trees  : (SHA1.t *   tree) list;
  tags   : (SHA1.t *    tag) list;
}

let empty = {
  blobs   = [];
  commits = [];
  trees   = [];
  tags    = [];
}

let find sha1 t =
  try Blob (List.assoc sha1 t.blobs)
  with Not_found ->
    try Commit (List.assoc sha1 t.commits)
    with Not_found ->
      try Tree (List.assoc sha1 t.trees)
      with Not_found ->
        Tag (List.assoc sha1 t.tags)





