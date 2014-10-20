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

type t = Uri.t

let to_uri x = x
let of_uri x = x

let of_string str =
  let uri = Uri.of_string str in
  match Uri.host uri with
  | Some _ -> uri
  | None   ->
    match Misc.string_lsplit2 str ~on:':' with
    | None       -> uri
    | Some (host, path) ->
      let userinfo, host =
        match Misc.string_lsplit2 host ~on:'@' with
        | None       -> None  , host
        | Some (a,b) -> Some a, b in
      let scheme = "git+ssh" in
      Uri.make ~scheme ?userinfo ~host ~path ()

let to_string uri =
  match Uri.scheme uri with
  | Some "git+ssh" ->
    let userinfo = match Uri.userinfo uri with
      | None   -> ""
      | Some u -> u ^ "@" in
    let host = match Uri.host uri with
      | None   -> ""
      | Some h -> h in
    Printf.sprintf "%s%s:%s" userinfo host (Uri.path uri)
  | _ -> Uri.to_string uri

type mode = [ `Git | `SSH | `HTTP ]

let mode t =
  match Uri.scheme t with
  | Some "git"     -> `Git
  | Some "git+ssh" -> `SSH
  | _              -> `HTTP
