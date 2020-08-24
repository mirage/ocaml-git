(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

open Lwt.Infix

let src = Logs.Src.create "git.refs" ~doc:"Git references"

module Log = (val Logs.src_log src : Logs.LOG)

type t = string

let of_string x = x

let to_string x = x

let sep = "/"


let head = "HEAD"

let is_head = String.equal head

let master = "refs/heads/master"

let to_path = function "HEAD" -> Path.v "HEAD" | refs -> Path.v refs

let of_path path =
  match Path.segs path with
  | [] -> Fmt.invalid_arg "Reference.of_path: empty path"
  | [ "HEAD" ] -> head
  | "HEAD" :: _ ->
      Fmt.invalid_arg "Reference.of_path: HEAD can not be followed by values"
  | "refs" :: _ as refs -> String.concat sep refs
  | _ :: _ ->
      invalid_arg "Reference.of_path: bad path (need to be prefixed by refs)"

let pp ppf x = Fmt.pf ppf "%s" (String.escaped x)

  type nonrec t = t

end

end



end

