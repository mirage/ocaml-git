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
open Model

type handle = string

let create t = t

let (/) = Filename.concat

let file_of_sha1 root sha1 =
  let hex = Misc.hex_encode (SHA1.to_string sha1) in
  let prefix = String.sub hex 0 2 in
  let suffix = String.sub hex 2 (String.length hex - 2) in
  File.Name.of_string (root / "objects" / prefix / suffix)

let read root t sha1 =
  let file = file_of_sha1 root sha1 in
  let contents = Backend.Object.of_string file (File.read file) in
  match contents with
  | Blob x   -> { t with blobs   = (sha1, x) :: t.blobs   }
  | Commit x -> { t with commits = (sha1, x) :: t.commits }
  | Tree x   -> { t with trees   = (sha1, x) :: t.trees   }
  | Tag x    -> { t with tags    = (sha1, x) :: t.tags    }

let read dir =
  let objects = Filename.concat dir "objects" in
  let subdirs = Array.to_list (Sys.readdir objects) in
  let subdirs = List.filter (fun s -> s <> "info" && s <> "pack") subdirs in
  Printf.printf "subdirs=%s\n" (String.concat ", " subdirs);
  let subdirs =
    List.map
      (fun prefix -> (prefix, Filename.concat objects prefix))
      subdirs in
  let objects =
    List.map (fun (prefix, dir) ->
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      let hex = List.map (fun suffix -> prefix ^ suffix) files in
      List.map (fun h -> SHA1.of_string (Misc.hex_decode h)) hex
    ) subdirs in
  let objects = List.flatten objects in
  List.fold_left (read dir) empty objects
