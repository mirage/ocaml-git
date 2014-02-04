(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open GitMisc.OP
open GitTypes

module Log = Log.Make(struct let section = "memory" end)

type t = {
  root   : string;
  buffers: (sha1, Bigstring.t) Hashtbl.t;
  values : (sha1, value) Hashtbl.t;
  refs   : (reference, SHA1.Commit.t) Hashtbl.t;
  mutable packs: (pack_index * pack) list;
}

let root t =
  t.root

let create ?root () =
  let root = match root with
    | None   -> "root"
    | Some r -> r in
  let t = {
    root;
    buffers = SHA1.Table.create ();
    values  = SHA1.Table.create ();
    refs    = Reference.Table.create ();
    packs   = [];

  } in
  return t

let read_inflated t sha1 =
  if Hashtbl.mem t.buffers sha1 then
    let buf = Hashtbl.find_exn t.buffers sha1 in
    return (Some buf)
  else
    return_none

let write_inflated t inflated =
  let sha1 = SHA1.create inflated in
  if Hashtbl.mem t.buffers sha1 then
    return sha1
  else (
    Log.debugf "write_inflated %s:%S" (SHA1.to_hex sha1) (Bigstring.to_string inflated);
    Log.infof "Writing %s" (SHA1.to_hex sha1);
    Hashtbl.add_exn t.buffers sha1 inflated;
    return sha1
  )

let write_and_check_inflated t sha1 inflated =
  if Hashtbl.mem t.buffers sha1 then
    return_unit
  else (
    let new_sha1 = SHA1.create inflated in
    if sha1 = new_sha1 then
      write_inflated t inflated >>= fun _ -> return_unit
    else (
      Printf.eprintf "%S\n" (Bigstring.to_string inflated);
      Printf.eprintf
        "Marshaling error: expected:%s actual:%s\n" (SHA1.to_hex sha1) (SHA1.to_hex new_sha1);
      fail (Failure "build_packed_object")
    )
  )

let inflated value =
  Git.output_inflated value

let write t value =
  write_inflated t (inflated value) >>= fun sha1 ->
  match Hashtbl.find t.values sha1 with
  | Some _ -> return sha1
  | None   ->
    Hashtbl.add_exn t.values sha1 value;
    return sha1

let write_pack t pack =
  let index = Git.Pack_index.of_pack pack in
  t.packs <- (index, pack) :: t.packs;
  return index

let write_and_check t sha1 value =
  match Hashtbl.find t.values sha1 with
  | Some _ -> return_unit
  | None   ->
    Hashtbl.add_exn t.values sha1 value;
    write_and_check_inflated t sha1 (inflated value)

let list t =
  return (Hashtbl.keys t.values)

let read t sha1 =
  match Hashtbl.find t.values sha1 with
  | Some _ as v -> return v
  | None        ->
    read_inflated t sha1 >>= function
    | None     -> return_none
    | Some buf ->
      let value = Git.input_inflated (Mstruct.of_bigarray buf) in
      Hashtbl.add_exn t.values sha1 value;
      return (Some value)

let mem t sha1 =
  return (Hashtbl.mem t.values sha1)

let read_exn t sha1 =
  read t sha1 >>= function
  | None   -> fail Not_found
  | Some v -> return v

let type_of t sha1 =
  read_inflated t sha1 >>= function
  | None     -> return_none
  | Some buf ->
    let buf = Mstruct.of_bigarray buf in
    return (Some (Git.type_of_inflated buf))

let string_of_type_opt = function
  | Some `Blob   -> "Blob"
  | Some `Commit -> "Commit"
  | Some `Tag    -> "Tag"
  | Some `Tree   -> "Tree"
  | None         -> "Unknown"

let dump t =
  Log.debugf "dump";
  list t >>= fun sha1s ->
  Lwt_list.iter_s (fun sha1 ->
      type_of t sha1 >>= fun typ ->
      Printf.eprintf "%s %s\n"
        (SHA1.to_hex sha1)
        (string_of_type_opt typ);
      return_unit
    ) sha1s

let references t =
  return (Hashtbl.keys t.refs)

let mem_reference t ref =
  return (Hashtbl.mem t.refs ref)

let read_reference t ref =
  Log.infof "Reading %s" (Reference.to_string ref);
  return (Hashtbl.find t.refs ref)

let remove_reference t ref =
  Hashtbl.remove t.refs ref;
  return_unit

let read_reference_exn t ref =
  read_reference t ref >>= function
  | None   -> fail Not_found
  | Some s -> return s

let succ root sha1 =
  let commit c = `Commit (SHA1.of_commit c) in
  let tree l s = `Tree (l, SHA1.of_tree s) in
  let tag t = `Tag (t.Tag.tag, t.Tag.sha1) in
  read root sha1 >>= function
  | None                  -> return_nil
  | Some (Value.Blob _)   -> return_nil
  | Some (Value.Commit c) -> return (tree "" c.Commit.tree :: List.map ~f:commit c.Commit.parents)
  | Some (Value.Tag t)    -> return [tag t]
  | Some (Value.Tree t)   ->
    let t = Tree.entries t in
    return (List.map ~f:(fun e -> `Tree (e.Tree.name, e.Tree.node)) t)

let write_reference t ref sha1 =
  Log.infof "Writing %s" (Reference.to_string ref);
  Hashtbl.replace t.refs ref sha1;
  return_unit

(* XXX: do not load the blobs *)
let load_filesystem t commit =
  Log.debugf "load_filesystem %s" (SHA1.Commit.to_hex commit);
  let rec aux (mode, sha1): ('a, perm * blob) Lazy_trie.t option Lwt.t =
    Log.debugf "aux %s" (SHA1.to_hex sha1);
    read t sha1 >>= function
    | Some (Value.Blob b) ->
      return (Some (Lazy_trie.create ~value:(mode, b) ()))
    | Some (Value.Tree t) ->
      Lwt_list.fold_left_s (fun acc e ->
          aux (e.Tree.perm, e.Tree.node) >>= function
          | None   -> return acc
          | Some t -> return ((e.Tree.name, t) :: acc)
        ) [] (Tree.entries t)
      >>= fun children ->
      let children = lazy children in
      return (Some (Lazy_trie.create ~children ()))
    | Some (Value.Commit c) -> aux (`Dir, SHA1.of_tree c.Commit.tree)
    | _ -> return_none
  in
  aux (`Dir, SHA1.of_commit commit) >>= function
  | None    -> fail (Failure "create")
  | Some fs -> return fs

let iter_blobs t ~f ~init =
  load_filesystem t init >>= fun trie ->
  Lazy_trie.fold (fun acc path (mode, blob) ->
      acc >>= fun () ->
      f (t.root :: path) mode blob
  ) trie return_unit

let read_cache t =
  failwith "Memory.read_cache: TODO"

let write_cache t head =
  failwith "Memory.update_cache: TODO"
