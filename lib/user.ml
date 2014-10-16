(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Sexplib.Std
open Printf

module Log = Log.Make(struct let section = "user" end)

type t = {
  name : string;
  email: string;
  date : string;
} with sexp

let hash = Hashtbl.hash
let equal = (=)
let compare = compare

let pretty t =
  sprintf "[name: %s | email: %s | date: %s]"
    t.name t.email t.date

(* XXX needs to escape name/email/date *)
let add buf t =
  Buffer.add_string buf t.name ;
  Buffer.add_string buf " <"   ;
  Buffer.add_string buf t.email;
  Buffer.add_string buf "> "   ;
  Buffer.add_string buf t.date

let input buf =
  let i = match Mstruct.index buf Misc.lt with
    | Some i -> i-1
    | None   -> Mstruct.parse_error_buf buf "invalid user name" in
  let name = Mstruct.get_string buf i in
  Mstruct.shift buf 2;
  let j = match Mstruct.index buf Misc.gt with
    | Some j -> j
    | None   -> Mstruct.parse_error_buf buf "invalid user email" in
  let email = Mstruct.get_string buf j in
  (* skip 2 bytes *)
  Mstruct.shift buf 2;
  let date = Mstruct.get_string buf (Mstruct.length buf) in
  { name; email; date }
