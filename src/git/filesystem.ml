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
open File.OP

type t = (string, Model.blob) Trie.t

let init t commit =
  let rec aux node =
    match Store.succ t node with
    | []       ->
      begin match Store.read t node with
        | Some (Blob b) -> Some (Trie.create ~value:b ())
        | _             -> None
      end
    | children ->
      let children = lazy (
        List.fold_left (fun acc (l, n) ->
          match l with
          | `parent
          | `tag _  -> acc
          | `file l ->
            match aux n with
            | None   -> acc
            | Some t -> (l, t) :: acc
        ) [] children
      ) in
      Some (Trie.create ~children ())
  in
  aux (Node.commit commit)

let write t trie =
  Trie.iter (fun path blob ->
    let file = String.concat "/" (File.Dirname.to_string t.root :: path) in
    File.mkdir (File.dirname (File.Name.of_string file));
    let oc = open_out file in
    output_string oc (Blob.to_string blob);
    close_out oc
  ) trie
