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

(* FIXME: might want to have an option type with string|cstruct *)
include Git_misc.S
let hash = Hashtbl.hash
let equal x y = String.compare x y = 0

let pretty t =
  if String.length t < 70 then Printf.sprintf "%S" t
  else Printf.sprintf "\"%s..%d\""
      (String.escaped (String.sub t 0 70))
      (String.length t)

let pp ppf t = Format.fprintf ppf "%s" (pretty t)
let to_raw x = x
let of_raw x = x
let input buf = Mstruct.to_string buf
let add buf ?level:_ t = Buffer.add_string buf t
