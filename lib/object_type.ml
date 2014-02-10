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

open Core_kernel.Std
module Log = Log.Make(struct let section = "object-type" end)

module T = struct
  type t =
    | Blob
    | Commit
    | Tag
    | Tree
  with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Tag"
end
include T
include Identifiable.Make (T)

let to_string = function
  | Blob   -> "blob"
  | Commit -> "commit"
  | Tag    -> "tag"
  | Tree   -> "tree"

let pretty = to_string

let add buf t =
  Bigbuffer.add_string buf (to_string t)

let of_string = function
  | "blob"   -> Some Blob
  | "commit" -> Some Commit
  | "tag"    -> Some Tag
  | "tree"   -> Some Tree
  | _        -> None

let input buf =
  match Mstruct.get_string_delim buf Misc.sp with
  | None     -> Mstruct.parse_error_buf buf "no object type"
  | Some str ->
    match of_string str with
    | None   -> Mstruct.parse_error_buf buf "%s: invalid object type" str
    | Some k -> k
