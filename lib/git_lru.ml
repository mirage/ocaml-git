(* Copyright (c) 2013, Simon Cruanes All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
   Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
   STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
   OF THE POSSIBILITY OF SUCH DAMAGE.  *)

(* From
   https://github.com/c-cube/ocaml-containers/blob/8c0670155d62511c540d125f106b9ec6bfab5931/src/data/CCCache.ml *)

type key = Git_hash.t

module H = Hashtbl.Make(Git_hash)

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

(* new functions *)

let find c x =
  try Some (get c x)
  with Not_found -> None

let add c x y =
  match find c x with
  | None   -> set c x y
  | Some _ -> ()
