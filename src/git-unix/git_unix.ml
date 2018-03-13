(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Fs    = Fs
module Net   = Net
module Index = Index
module SHA1  = Git.Hash.Make(Digestif.SHA1)
module Sync  = Git.Sync.Make(Net)
module HTTP  = Http.Make

module FS = struct
  include Git.Store.FS(SHA1)(Fs)(Git.Inflate)(Git.Deflate)

  let with_fs ?root ?dotgit ?compression ?buffer fs =
    create ?root ?dotgit ?compression ?buffer fs

  let create ?temp_dir ?root ?dotgit ?compression ?buffer () =
    let fs = Fs.v ?temp_dir () in
    create ?root ?dotgit ?compression ?buffer fs
end
