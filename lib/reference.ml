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

module Log = Misc.Log_make(struct let section = "reference" end)

type t = string

let compare = String.compare
let equal = (=)
let hash = Hashtbl.hash

let add _buf ?level:_ _t = failwith "TODO: Reference.add"
let input _buf = failwith "TODO: Reference.input"

let to_raw x = x
let of_raw x = x
let pretty x = String.escaped x
let pp ppf x = Format.fprintf ppf "%s" (pretty x)

module Map = Misc.Map(Misc.S)

let compare x y =
  match x, y with
  | "HEAD", "HEAD" -> 0
  | "HEAD", _      -> (-1)
  | _     , "HEAD" -> 1
  | _     , _      -> compare x y

let head = "HEAD"

type head_contents =
  | SHA of SHA.Commit.t
  | Ref of string

let pp_head_contents ppf = function
  | SHA x -> Format.fprintf ppf "SHA %a" SHA.Commit.pp x
  | Ref x -> Format.pp_print_string ppf x

let equal_head_contents x y = match x, y with
  | SHA x, SHA y -> SHA.Commit.equal x y
  | Ref x, Ref y -> String.compare x y = 0
  | _ -> false

let err_head_contents str =
  let err = Printf.sprintf "%S is not a valid HEAD contents" str in
  failwith err

let is_head x =
  String.compare head x = 0

let head_contents_of_commit refs sha1 =
  let refs = Map.remove "HEAD" refs in
  let alist = Misc.inverse_assoc (Map.to_alist refs) in
  match Misc.try_assoc sha1 alist with
  | None   -> SHA sha1
  | Some r -> Ref r

let master = "refs/heads/master"

let is_valid r =
  Misc.string_forall (function
      | '{'
      | '}'
      | '^' -> false
      | _   -> true
    ) r

let head_contents_of_string ~of_hex str =
  match Stringext.split ~on:' ' (String.trim str) with
  | [sha1]  -> SHA (of_hex sha1 |> SHA.to_raw |> SHA.Commit.of_raw)
  | [_;ref] -> Ref (of_raw ref)
  | _       -> err_head_contents str
