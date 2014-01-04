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

open GitTypes

type filesystem = (string, blob) Lazy_trie.t

let filesystem_of_local t commit =
  let rec aux node =
    match GitLocal.succ t node with
    | []       ->
      begin match GitLocal.read t node with
        | Some (Blob b) -> Some (Lazy_trie.create ~value:b ())
        | _             -> None
      end
    | children ->
      let children = lazy (
        List.fold_left ~f:(fun acc (l, n) ->
          match l with
          | `parent
          | `tag _  -> acc
          | `file l ->
            match aux n with
            | None   -> acc
            | Some t -> (l, t) :: acc
        ) ~init:[] children
      ) in
      Some (Lazy_trie.create ~children ())
  in
  aux (SHA1.commit commit)

let write_filesystem t trie =
  Lazy_trie.iter (fun path blob ->
    let file = String.concat ~sep:"/" (t.root :: path) in
    GitMisc.mkdir (Filename.dirname file);
    Out_channel.write_all file ~data: (Blob.to_string blob)
  ) trie
