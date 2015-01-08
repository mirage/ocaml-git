(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf

module Log = Log.Make(struct let section = "tree" end)

type perm = [
    `Normal
  | `Exec
  | `Link
  | `Dir
  | `Commit
]

type entry = {
  perm: perm;
  name: string;
  node: SHA.t;
}

type t = entry list

let hash = Hashtbl.hash
let compare = compare
let equal = (=)

let pretty_perm = function
  | `Normal -> "normal"
  | `Exec   -> "exec  "
  | `Link   -> "link  "
  | `Dir    -> "dir   "
  | `Commit -> "commit"

let pretty_entry e =
  sprintf "%s %s    %S\n"
    (pretty_perm e.perm)
    (SHA.to_hex e.node)
    e.name

let pretty t =
  let b = Buffer.create 1024 in
  List.iter (fun e -> Buffer.add_string b (pretty_entry e)) t;
  Buffer.contents b

let perm_of_string buf = function
  | "44"
  | "100644" -> `Normal
  | "100755" -> `Exec
  | "120000" -> `Link
  | "40000"  -> `Dir
  | "160000" -> `Commit
  | x        -> Mstruct.parse_error_buf buf "%S is not a valid permission." x

let string_of_perm = function
  | `Normal -> "100644"
  | `Exec   -> "100755"
  | `Link   -> "120000"
  | `Dir    -> "40000"
  | `Commit -> "160000"

let escape = Char.chr 42

let escaped_chars =
  escape :: List.map Char.chr [ 0x00; 0x2f ]

let needs_escape x = List.mem x escaped_chars

let encode path =
  if not (Misc.string_exists needs_escape path) then
    path
  else
    let n = String.length path in
    let b = Buffer.create n in
    let last = ref 0 in
    for i = 0 to n - 1 do
      if needs_escape path.[i] then (
        let c = Char.chr (Char.code path.[i] + 1) in
        if i - !last > 0 then Buffer.add_substring b path !last (i - !last);
        Buffer.add_char b escape;
        Buffer.add_char b c;
        last := i + 1;
      )
    done;
    if n - !last > 0 then
      Buffer.add_substring b path !last (n - !last);
    Buffer.contents b

let add_entry buf e =
  Buffer.add_string buf (string_of_perm e.perm);
  Buffer.add_char buf Misc.sp;
  Buffer.add_string buf (encode e.name);
  Buffer.add_char buf Misc.nul;
  SHA.add buf e.node

let decode path =
  if not (Misc.string_mem escape path) then path
  else
    let n = String.length path in
    let b = Buffer.create n in
    let last = ref 0 in
    for i = 0 to n - 1 do
      if path.[i] = escape then (
        if i - !last > 0 then Buffer.add_substring b path !last (i - !last);
        if i + 1 < n then (
          let c = Char.chr (Char.code path.[i+1] - 1) in
          Buffer.add_char b c;
        );
        last := i + 2;
      );
    done;
    if n - !last > 0 then
      Buffer.add_substring b path !last (n - !last);
    Buffer.contents b

let input_entry buf =
  let perm = match Mstruct.get_string_delim buf Misc.sp with
    | None      -> Mstruct.parse_error_buf buf "invalid perm"
    | Some perm -> perm in
  let name = match Mstruct.get_string_delim buf Misc.nul with
    | None      -> Mstruct.parse_error_buf buf "invalid filename"
    | Some name -> name in
  let name = decode name in
  let node = SHA.input buf in
  let entry = {
    perm = perm_of_string buf perm;
    name; node
  } in
  Some entry

let add buf ?level:_ t =
  List.iter (add_entry buf) t

let input buf =
  let rec aux entries =
    if Mstruct.length buf <= 0 then
      List.rev entries
    else
      match input_entry buf with
      | None   -> List.rev entries
      | Some e -> aux (e :: entries) in
  aux []
