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

module Log = Log.Make(struct let section = "reference" end)

type t = string

let compare = String.compare
let equal = (=)
let hash = Hashtbl.hash

let add _buf ?level:_ _t =
  failwith "TODO: Reference.add"

let input _buf =
  failwith "TODO: Reference.input"

let to_raw x = x
let of_raw x = x
let pretty x = String.escaped x

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

let is_head x =
  String.compare head x = 0

let head_contents refs sha1 =
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
