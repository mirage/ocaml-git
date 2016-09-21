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

module Log = (val Git_misc.src_log "value" : Logs.LOG)

module T = struct

  type t =
    | Blob   of Git_blob.t
    | Commit of Git_commit.t
    | Tag    of Git_tag.t
    | Tree   of Git_tree.t

  let equal = (=)
  let hash = Hashtbl.hash
  let compare = compare

  let pp ppf = function
    | Blob b   -> Format.fprintf ppf "@[<hov 2>Blob@ %a@]" Git_blob.pp b
    | Commit c -> Format.fprintf ppf "@[<hov 2>Commit@ %a@]" Git_commit.pp c
    | Tag t    -> Format.fprintf ppf "@[<hov 2>Tag@ %a@]" Git_tag.pp t
    | Tree t   -> Format.fprintf ppf "@[<hov 2>Git_tree@ %a@]" Git_tree.pp t

end

include T

let commit c = Commit c
let blob b = Blob b
let tree t = Tree t
let tag t = Tag t

let type_of = function
  | Blob _   -> Git_object_type.Blob
  | Commit _ -> Git_object_type.Commit
  | Tag _    -> Git_object_type.Tag
  | Tree _   -> Git_object_type.Tree

let type_of_inflated buf =
  let obj_type =
    match Mstruct.get_string_delim buf Git_misc.sp with
    | None   -> Mstruct.parse_error_buf buf "value: type"
    | Some t -> t in
  match Git_object_type.of_string obj_type with
  | Some t -> t
  | None   ->
    Mstruct.parse_error_buf buf "%S is not a valid object type." obj_type

module IO (D: Git_hash.DIGEST) (I: Git_inflate.S) = struct

  include T
  module Hash_IO = Git_hash.IO(D)
  module Git_commit_IO = Git_commit.IO(D)
  module Git_tag_IO = Git_tag.IO(D)
  module Git_tree_IO = Git_tree.IO(D)

  let add_header buf typ size =
    Buffer.add_string buf (Git_object_type.to_string typ);
    Buffer.add_char   buf Git_misc.sp;
    Buffer.add_string buf (string_of_int size);
    Buffer.add_char   buf Git_misc.nul

  let add_contents buf = function
    | Blob b   -> Git_blob.add buf b
    | Commit c -> Git_commit_IO.add buf c
    | Tag t    -> Git_tag_IO.add buf t
    | Tree t   -> Git_tree_IO.add buf t

  let add_inflated buf t =
    Log.debug (fun l -> l "add_inflated");
    let tmp = Buffer.create 1024 in
    add_contents tmp t;
    let size = Buffer.length tmp in
    add_header buf (type_of t) size;
    Buffer.add_buffer buf tmp

  let name t =
    let buf = Git_misc.with_buffer (fun buf -> add_inflated buf t) in
    D.string buf

  let input_inflated buf =
    let obj_type = type_of_inflated buf in
    let size =
      match Mstruct.get_string_delim buf Git_misc.nul with
      | None   -> Mstruct.parse_error_buf buf "value: size"
      | Some s ->
        try int_of_string s
        with Failure _ ->
          Mstruct.parse_error_buf buf
            "Value.input_inflated: %S is not a valid integer." s
    in
    if size <> Mstruct.length buf then
      Mstruct.parse_error_buf buf
        "[expected-size: %d; actual-size: %d]\n"
        size (Mstruct.length buf);
    let buf = Mstruct.sub buf 0 size in
    match obj_type with
    | Git_object_type.Blob   -> Git_blob.input buf      |> blob
    | Git_object_type.Commit -> Git_commit_IO.input buf |> commit
    | Git_object_type.Tag    -> Git_tag_IO.input buf    |> tag
    | Git_object_type.Tree   -> Git_tree_IO.input buf   |> tree

  let add buf ?level t =
    Log.debug (fun log -> log "add %a" pp t);
    let inflated = Git_misc.with_buffer' (fun buf -> add_inflated buf t) in
    let deflated = I.deflate ?level inflated in
    Buffer.add_string buf (Cstruct.to_string deflated)

  let fail fmt = Printf.ksprintf failwith ("Value: " ^^ fmt)
  let err_inflate () = fail "not a valid compressed object"

  let input buf = match I.inflate buf with
    | None   -> err_inflate ()
    | Some s -> input_inflated s

end

module Cache = struct

  let len = 12
  let cache: t Git_lru.t ref = ref (Git_lru.make len)
  let cache_inflated: string Git_lru.t ref = ref (Git_lru.make len)

  let clear () =
    Git_lru.clear !cache;
    Git_lru.clear !cache_inflated

  let set_size len =
    cache := Git_lru.make len;
    cache_inflated := Git_lru.make len

  let find h = Git_lru.find !cache h
  let find_inflated h = Git_lru.find !cache_inflated h
  let add h t = Git_lru.add !cache h t
  let add_inflated h str = Git_lru.add !cache_inflated h str

end

type read = Git_hash.t -> t option Lwt.t
type read_inflated = Git_hash.t -> string option Lwt.t
type write = t -> Git_hash.t Lwt.t
type write_inflated = string -> Git_hash.t Lwt.t

module type IO = sig
  include Git_s.IO with type t = t
  val name: t -> Git_hash.t
  val add_header: Buffer.t -> Git_object_type.t -> int -> unit
  val add_inflated: Buffer.t -> t -> unit
  val input_inflated: Mstruct.t -> t
end
