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

type t = string

let create t = t

let objects dir =
  let objects = Filename.concat dir "objects" in
  let subdirs = Sys.readdir objects in
  let subdirs = List.map
      (fun prefix -> (prefix, Filename.concat objects prefix))
      (Array.to_list subdirs) in
  let objects =
    List.map (fun (prefix, dir) ->
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      List.map
        (fun suffix -> (prefix^suffix), Filename.concat dir suffix)
        files
    ) subdirs in
  let objects = List.flatten objects in
  List.map (fun (h,f) ->
    Hex.Object.of_string h,
    File.Name.of_string f
  ) objects

