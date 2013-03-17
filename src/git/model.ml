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

module Hex = struct
  module Object = Abstract.String
  module Commit = Abstract.String
  module Tree = Abstract.String
end

type user = {
  name : string;
  email: string;
  date : string;
}

module Blob = Abstract.String

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
  | Blob   of Blob.t
  | Commit of commit
  | Tag    of tag
  | Tree   of tree







