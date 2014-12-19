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

open Printf

module Log = Log.Make(struct let section = "value" end)

type t =
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tag    of Tag.t
  | Tree   of Tree.t

let equal = (=)
let hash = Hashtbl.hash
let compare = compare

let pretty = function
  | Blob b   -> sprintf "== Blob ==\n%s\n" (Blob.pretty b)
  | Commit c -> sprintf "== Commit ==\n%s" (Commit.pretty c)
  | Tag t    -> sprintf "== Tag ==\n%s" (Tag.pretty t)
  | Tree t   -> sprintf "== Tree ==\n%s" (Tree.pretty t)

let commit c = Commit c
let blob b = Blob b
let tree t = Tree t
let tag t = Tag t

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
  Buffer.add_string buf (Object_type.to_string typ);
  Buffer.add_char   buf Misc.sp;
  Buffer.add_string buf (string_of_int size);
  Buffer.add_char   buf Misc.nul

let add_inflated buf t =
  Log.debug "add_inflated";
  let tmp = Buffer.create 1024 in
  add_contents tmp t;
  let size = Buffer.length tmp in
  add_header buf (type_of t) size;
  Buffer.add_buffer buf tmp

let sha1 t =
  let buf = Misc.with_buffer (fun buf -> add_inflated buf t) in
  SHA.of_string buf

let add buf t =
  Log.debug "add %s" (pretty t);
  let inflated = Misc.with_buffer' (fun buf -> add_inflated buf t) in
  let deflated = Misc.deflate_cstruct inflated in
  Buffer.add_string buf (Cstruct.to_string deflated)

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
  if size <> Mstruct.length buf then
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


module LRU = struct

  (* Copyright (c) 2013, Simon Cruanes
     All rights reserved.

     Redistribution and use in source and binary forms, with or
     without modification, are permitted provided that the following
     conditions are met:

     Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
     Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.

     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
     AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
     ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
     POSSIBILITY OF SUCH DAMAGE.  *)

  (* From
     https://github.com/c-cube/ocaml-containers/blob/8c0670155d62511c540d125f106b9ec6bfab5931/src/data/CCCache.ml *)

  type key = SHA.t

  module H = Hashtbl.Make(SHA)

  type 'a t = {
    table : 'a node H.t;  (* hashtable key -> node *)
    mutable first : 'a node option;
    size : int;           (* max size *)
  }
  and 'a node = {
    mutable key : key;
    mutable value : 'a;
    mutable next : 'a node;
    mutable prev : 'a node;
  } (** Meta data for the value, making a chained list *)

  let make size =
    assert (size > 0);
    { table = H.create size;
      size;
      first=None;
    }

  let clear c =
    H.clear c.table;
    c.first <- None;
    ()

  (* take first from queue *)
  let take_ c =
    match c.first with
    | Some n when n.next == n ->
      (* last element *)
      c.first <- None;
      n
    | Some n ->
      c.first <- Some n.next;
      n.prev.next <- n.next;
      n.next.prev <- n.prev;
      n
    | None ->
      failwith "LRU: empty queue"

  (* push at back of queue *)
  let push_ c n =
    match c.first with
    | None ->
      n.next <- n;
      n.prev <- n;
      c.first <- Some n
    | Some n1 when n1==n -> ()
    | Some n1 ->
      n.prev <- n1.prev;
      n.next <- n1;
      n1.prev.next <- n;
      n1.prev <- n

  (* remove from queue *)
  let remove_ n =
    n.prev.next <- n.next;
    n.next.prev <- n.prev

  (* Replace least recently used element of [c] by x->y *)
  let replace_ c x y =
    (* remove old *)
    let n = take_ c in
    H.remove c.table n.key;
    (* add x->y, at the back of the queue *)
    n.key <- x;
    n.value <- y;
    H.add c.table x n;
    push_ c n;
    ()

  (* Insert x->y in the cache, increasing its entry count *)
  let insert_ c x y =
    let rec n = {
      key = x;
      value = y;
      next = n;
      prev = n;
    } in
    H.add c.table x n;
    push_ c n;
    ()

  let get c x =
    let n = H.find c.table x in
    (* put n at the back of the queue *)
    remove_ n;
    push_ c n;
    n.value

  let set c x y =
    let len = H.length c.table in
    assert (len <= c.size);
    if len = c.size
    then replace_ c x y
    else insert_ c x y

  let _size c () = H.length c.table

  let _iter c f =
    H.iter (fun x node -> f x node.value) c.table
end

module Cache = struct
  let cache = LRU.make 1024
  let clear () = LRU.clear cache
  let find sha1 = try Some (LRU.get cache sha1) with Not_found -> None
  let find_exn sha1 = LRU.get cache sha1
  let add sha1 str = LRU.set cache sha1 str
end
