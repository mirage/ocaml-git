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
open Lwt
module Log = Log.Make(struct let section = "value" end)

module T = struct
  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tag    of Tag.t
    | Tree   of Tree.t
  with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Value"
end
include T
include Identifiable.Make (T)

let pretty = function
  | Blob b   -> sprintf "== Blob ==\n%s" (Blob.pretty b)
  | Commit c -> sprintf "== Commit ==\n%s" (Commit.pretty c)
  | Tag t    -> sprintf "== Tag ==\n%s" (Tag.pretty t)
  | Tree t   -> sprintf "== Tree ==\n%s" (Tree.pretty t)

let commit c = Commit c
let blob b = Blob b
let tree t = Tree t
let tag t = Tag t


type successor =
  [ `Commit of SHA1.t
  | `Tag of string * SHA1.t
  | `Tree of string * SHA1.t ]

let succ = function
  | `Commit s
  | `Tag (_, s)
  | `Tree (_, s) -> s

(* XXX: not tail-rec *)
let rec find ~succ sha1 path =
  match path with
  | []   -> return (Some sha1)
  | h::t ->
    succ sha1 >>= fun succs ->
    Lwt_list.fold_left_s (fun acc s ->
        match (acc, s) with
        | Some _, _            -> return acc
        | _     , `Commit _    -> return acc
        | _     , `Tag (l, s)
        | _     , `Tree (l, s) ->
          if String.(l=h) then
            find ~succ s t >>= function
            | None   -> return_none
            | Some f -> return (Some f)
          else
            return acc
      ) None succs

let find_exn ~succ sha1 path =
  find succ sha1 path >>= function
  | None   -> fail Not_found
  | Some x -> return x

let mem ~succ sha1 path =
  find succ sha1 path >>= function
  | None   -> return false
  | Some _ -> return true


let type_of = function
  | Blob _   -> Object_type.Blob
  | Commit _ -> Object_type.Commit
  | Tag _    -> Object_type.Tag
  | Tree _   -> Object_type.Tree

let add_contents buf = function
  | Blob b   -> Blob.add buf b
  | Commit c -> Commit.add buf c
  | Tag t    -> Tag.add buf t
  | Tree t   -> Tree.add buf t

let add_header buf typ size =
  Bigbuffer.add_string buf (Object_type.to_string typ);
  Bigbuffer.add_char   buf Misc.sp;
  Bigbuffer.add_string buf (string_of_int size);
  Bigbuffer.add_char   buf Misc.nul

let add_inflated buf t =
  Log.debugf "add_inflated %s" (to_string t);
  let tmp = Bigbuffer.create 1024 in
  add_contents tmp t;
  let size = Bigbuffer.length tmp in
  add_header buf (type_of t) size;
  Bigbuffer.add_buffer buf tmp

let sha1 t =
  let buf = Misc.with_buffer (fun buf -> add_inflated buf t) in
  SHA1.create buf

let add buf t =
  Log.debugf "add %s" (to_string t);
  let inflated = Misc.with_buffer (fun buf -> add_inflated buf t) in
  let deflated = Misc.deflate_bigstring inflated in
  Bigbuffer.add_string buf (Bigstring.to_string deflated)

let type_of_inflated buf =
  let obj_type =
    match Mstruct.get_string_delim buf Misc.sp with
    | None   -> Mstruct.parse_error_buf buf "value: type"
    | Some t -> t in
  match Object_type.of_string obj_type with
  | Some t -> t
  | None   -> Mstruct.parse_error_buf buf "%S is not a valid object type." obj_type

let input_inflated buf =
  let obj_type = type_of_inflated buf in
  let size =
    match Mstruct.get_string_delim buf Misc.nul with
    | None   -> Mstruct.parse_error_buf buf "value: size"
    | Some s ->
      try int_of_string s
      with _ -> Mstruct.parse_error_buf buf "%S is not a valid integer." s in
  if Int.(size <> Mstruct.length buf) then
    Mstruct.parse_error_buf buf
      "[expected-size: %d; actual-size: %d]\n"
      size (Mstruct.length buf);
  let buf = Mstruct.sub buf 0 size in
  match obj_type with
  | Object_type.Blob   -> Blob.input buf   |> blob
  | Object_type.Commit -> Commit.input buf |> commit
  | Object_type.Tag    -> Tag.input buf    |> tag
  | Object_type.Tree   -> Tree.input buf   |> tree

let input buf =
  input_inflated (Misc.inflate_mstruct buf)
