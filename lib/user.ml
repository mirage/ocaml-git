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

module Log = Log.Make(struct let section = "user" end)

type tz_offset = {
  sign: [`Plus | `Minus];
  hours: int;
  min: int;
}

let default_tz_offset =
  { sign = `Plus; hours=0; min= 0}

let pp_tz_offset ppf t =
  let pp_sign ppf = function
    | `Plus -> Format.fprintf ppf "`Plus"
    | `Minus -> Format.fprintf ppf "`Minus"
  in
  Format.fprintf ppf "{@[<hov 2>sign = '%a';@ hours = %d;@ min = %d@ @]}"
    pp_sign t.sign t.hours t.min

type t = {
  name : string;
  email: string;
  date : int64 * tz_offset option;
}

let pp_date ppf (date, tz) =
  let pp_tz ppf = function
    | None   -> Format.fprintf ppf "None"
    | Some o -> Format.fprintf ppf "Some %a" pp_tz_offset o
  in
  Format.fprintf ppf "@[<hov 2>(%LdL,@ %a)@]" date pp_tz tz

let add_date buf (date, tz) =
  Buffer.add_string buf (Int64.to_string date);
  Buffer.add_char buf ' ';
  match tz with
  | None    -> Buffer.add_string buf "+0000"
  | Some tz ->
    let sign = match tz.sign with `Plus -> "+" | `Minus -> "-" in
    Printf.bprintf buf "%s%02d%02d" sign tz.hours tz.min

let hash = Hashtbl.hash
let equal = (=)
let compare = compare

let pp ppf t =
  Format.fprintf ppf "{@[<hov>name=\"%s\";@ email=\"%s\";@ date=%a@]}"
    t.name t.email pp_date t.date

let pretty = Misc.pretty pp

(* XXX needs to escape name/email/date *)
let add buf ?level:_ t =
  Buffer.add_string buf t.name ;
  Buffer.add_string buf " <"   ;
  Buffer.add_string buf t.email;
  Buffer.add_string buf "> "   ;
  add_date buf t.date

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
  let seconds = match Mstruct.get_string_delim buf ' ' with
    | None   -> Mstruct.parse_error_buf buf "Invalid Git date"
    | Some s -> Int64.of_string s
  in
  let sign = match Mstruct.get_char buf with
    | '+' -> `Plus
    | '-' -> `Minus
    | c   -> Mstruct.parse_error_buf buf "wrong sign: %d" (Char.code c)
  in
  let hours =
    let str = Mstruct.get_string buf 2 in
    try int_of_string str
    with Failure "int_of_string" ->
      Mstruct.parse_error_buf buf "%S is not a valid hour" str
  in
  let min =
    let str = Mstruct.get_string buf 2 in
    try int_of_string str
    with Failure "int_of_string" ->
      Mstruct.parse_error_buf buf "%S is not a valid hour" str
  in
  let tz = { sign; hours; min } in
  let tz = if tz = default_tz_offset then None else Some tz in
  { name; email; date = (seconds, tz) }
