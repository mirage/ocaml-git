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

module Log = Log.Make(struct let section = "local" end)

type t = {
  root   : string;
  buffers: (sha1, Mstruct.t) Hashtbl.t;
  packs  : (sha1, pack) Hashtbl.t;
  indexes: (sha1, pack_index) Hashtbl.t;
}

let create ?root () =
  let root = match root with
    | None   -> Sys.getcwd ()
    | Some r -> if Filename.is_relative r then Sys.getcwd () / r else r in
  let t = {
    root;
    buffers = SHA1.Table.create ();
    packs   = SHA1.Table.create ();
    indexes = SHA1.Table.create ();
  } in
  return t

let auto_flush = ref true

let set_auto_flush b =
  auto_flush := b

let get_auto_flush () =
  !auto_flush

(* Loose objects *)
module Loose = struct

  let file root sha1 =
    let hex = SHA1.to_hex sha1 in
    let prefix = String.sub hex 0 2 in
    let suffix = String.sub hex 2 (String.length hex - 2) in
    root / ".git" / "objects" / prefix / suffix

  let read_inflated t sha1 =
    if Hashtbl.mem t.buffers sha1 then
      let buf = Hashtbl.find_exn t.buffers sha1 in
      return (Some (Mstruct.clone buf))
    else
      let file = file t.root sha1 in
      if Sys.file_exists file then
        GitMisc.mstruct_of_file file >>= fun buf ->
        let buf = GitMisc.inflate_mstruct buf in
        Hashtbl.add_exn t.buffers sha1 buf;
        return (Some (Mstruct.clone buf))
      else
        return_none

  let write_inflated t inflated =
    let sha1 = SHA1.create inflated in
    Log.debugf "write_inflated %s:%S" (SHA1.to_hex sha1) inflated;
    let file = file t.root sha1 in
    GitMisc.mkdir (Filename.dirname file) >>= fun () ->
    let deflated = GitMisc.deflate_string inflated in
    begin
      if !auto_flush then (
        Log.infof "Writing %s" file;
        Lwt_io.(with_file ~mode:Output file
                  (fun oc -> write oc deflated))
      ) else
        return_unit
    end >>= fun () ->
    return sha1

  let write_and_check_inflated t sha1 inflated =
    let file = file t.root sha1 in
    if Sys.file_exists file then return_unit
    else (
      let new_sha1 = SHA1.create inflated in
      if sha1 = new_sha1 then
        write_inflated t inflated >>= fun _ -> return_unit
      else (
        Printf.eprintf "%S\n" inflated;
        Printf.eprintf
          "Marshaling error: expected:%s actual:%s\n" (SHA1.to_hex sha1) (SHA1.to_hex new_sha1);
        fail (Failure "build_packed_object")
      )
    )

  let inflated value =
    let buf = Buffer.create 1024 in
    Git.output_inflated buf value;
    Buffer.contents buf

  let write t value =
    write_inflated t (inflated value)

  let write_and_check t sha1 value =
    write_and_check_inflated t sha1 (inflated value)

  let list root =
    Log.debugf "Loose.list %s" root;
    let objects = root / ".git" / "objects" in
    GitMisc.directories objects >>= fun objects ->
    let objects = List.map ~f:Filename.basename objects in
    let objects = List.filter ~f:(fun s -> (s <> "info") && (s <> "pack")) objects in
    Lwt_list.fold_left_s (fun acc prefix ->
      let dir = root / ".git" / "objects" / prefix in
      GitMisc.files dir >>= fun suffixes ->
      let suffixes = List.map ~f:Filename.basename suffixes in
      let objects = List.map ~f:(fun suffix ->
          SHA1.of_hex (prefix ^ suffix)
        ) suffixes in
      return (objects @ acc)
    ) [] objects

end

module Packed = struct

  let file root sha1 =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let pack_file = "pack-" ^ (SHA1.to_hex sha1) ^ ".pack" in
    pack_dir / pack_file

  let list root =
    Log.debugf "Packed.list %s" root;
    let packs = root / ".git" / "objects" / "pack" in
    GitMisc.files packs >>= fun packs ->
    let packs = List.map ~f:Filename.basename packs in
    let packs = List.filter ~f:(fun f -> Filename.check_suffix f ".idx") packs in
    let packs = List.map ~f:(fun f ->
        let p = Filename.chop_suffix f ".idx" in
        let p = String.sub p 5 (String.length p - 5) in
        SHA1.of_hex p
      ) packs in
    return packs

  let index root sha1 =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let idx_file = "pack-" ^ (SHA1.to_hex sha1) ^ ".idx" in
    pack_dir / idx_file

  let read_index t sha1 =
    if Hashtbl.mem t.indexes sha1 then
      return (Hashtbl.find_exn t.indexes sha1)
    else
      let file = index t.root sha1 in
      if Sys.file_exists file then
        GitMisc.mstruct_of_file file >>= fun buf ->
        let idx = Git.Idx.input buf in
        Hashtbl.add_exn t.indexes sha1 idx;
        return idx
      else (
        Printf.eprintf "%s does not exist." file;
        fail (Failure "read_index")
      )

  let read t sha1 =
    if Hashtbl.mem t.packs sha1 then
      return (Hashtbl.find_exn t.packs sha1)
    else (
      let file = file t.root sha1 in
      read_index t sha1 >>= fun index ->
      if Sys.file_exists file then (
        GitMisc.mstruct_of_file file >>= fun buf ->
        let fn = Git.Pack.input index buf in
        Hashtbl.add_exn t.packs sha1 fn;
        return fn
      ) else (
        Printf.eprintf "No file associated with the pack object %s.\n" (SHA1.to_hex sha1);
        fail (Failure "read_file")
      )
    )

  let rec read_inflated t (pack, idx) sha1 =
    let error offset =
      let idx = Map.data idx.offsets in
      let idx = String.concat ~sep:"," (List.map ~f:string_of_int idx) in
      Printf.eprintf "%d: invalid offset.\nValid offsets are: {%s}\n" offset idx;
      failwith "invalid offset" in
    if Map.mem idx.offsets sha1 then (
      let offset = Map.find_exn idx.offsets sha1 in
      read t pack >>= fun fn ->
      let packed_value = fn sha1 in
      let read_inflated sha1 =
        Loose.read_inflated t sha1 >>= function
        | Some buf -> return buf
        | None     -> read_inflated t (pack,idx) sha1 in
      let write value =
        Loose.write_and_check t sha1 value >>= fun () ->
        return sha1 in
      Git.Pack.unpack ~read_inflated ~write ~idx:idx.offsets ~offset packed_value >>= fun u ->
      assert (u = sha1);
      read_inflated sha1
    ) else
      error (-1)

  let read_inflated t sha1 =
    let rec find = function
      | []    -> return_none
      | n::ns ->
        read_index t n >>= fun idx ->
        if Map.mem idx.offsets sha1 then
          read_inflated t (n,idx) sha1 >>= fun v ->
          return (Some v)
        else
          find ns in
    list t.root >>=
    find

end

let list t =
  Log.debugf "list";
  Loose.list t.root  >>= fun objects ->
  Packed.list t.root >>= fun packs   ->
  (* Add cached objects *)
  let objects = List.dedup (Hashtbl.keys t.buffers @ objects) in
  Lwt_list.fold_left_s (fun acc p ->
      Packed.read_index t p >>= fun idx ->
      return ((Map.keys idx.offsets) @ acc)
    ) objects packs

let read_inflated t sha1 =
  let file = Loose.file t.root sha1 in
  if Hashtbl.mem t.buffers sha1 || Sys.file_exists file
  then Loose.read_inflated t sha1
  else Packed.read_inflated t sha1

let read t sha1 =
  read_inflated t sha1 >>= function
  | None     -> return_none
  | Some buf -> return (Some (Git.input_inflated buf))

let read_exn t sha1 =
  read_inflated t sha1 >>= function
  | None     -> fail Not_found
  | Some buf -> return (Git.input_inflated buf)

let type_of t sha1 =
  read_inflated t sha1 >>= function
  | None     -> return_none
  | Some buf -> return (Some (Git.type_of_inflated buf))

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
  let refs = t.root / ".git" / "refs" in
  GitMisc.rec_files refs >>= fun files ->
  let n = String.length t.root in
  Lwt_list.map_p (fun file ->
      let r = String.sub file n (String.length file - n) in
      Log.infof "Reading %s" file;
      Lwt_io.(with_file ~mode:Input file read) >>= fun hex ->
      return (r, SHA1.of_hex hex)
    ) files

let succ root sha1 =
  let commit c = `Commit (SHA1.of_commit c) in
  let tree l s = `Tree (l, SHA1.of_tree s) in
  let tag t = `Tag (t.Tag.tag, SHA1.of_commit t.Tag.commit) in
  read root sha1 >>= function
  | None                  -> return_nil
  | Some (Value.Blob _)   -> return_nil
  | Some (Value.Commit c) -> return (tree "" c.Commit.tree :: List.map ~f:commit c.Commit.parents)
  | Some (Value.Tag t)    -> return [tag t]
  | Some (Value.Tree t)   ->
    let t = Tree.entries t in
    return (List.map ~f:(fun e -> `Tree (e.Tree.name, e.Tree.node)) t)

let write t value =
  Log.debugf "write %s" (Value.to_string value);
  Loose.write t value

let write_and_check t sha1 value =
  Loose.write_and_check t sha1 value

let write_inflated t inflated =
  Loose.write_inflated t inflated

let write_and_check_inflated t sha1 inflated =
  Loose.write_and_check_inflated t sha1 inflated

let write_reference t name sha1 =
  let file = t.root / ".git" / name in
  GitMisc.mkdir (Filename.dirname file) >>= fun () ->
  Log.infof "Writing %s" file;
  Lwt_io.(with_file ~mode:Output file (fun oc -> write oc (SHA1.to_hex sha1)))

(* XXX: do not load the blobs *)
let load_filesystem t commit =
  let rec aux sha1 =
    succ t sha1 >>= function
    | [] ->
      begin read t sha1 >>= function
        | Some (Value.Blob b) -> return (Some (Lazy_trie.create ~value:b ()))
        | _                   -> return_none
      end
    | children ->
      Lwt_list.fold_left_s (fun acc -> function
          | `Commit _
          | `Tag _       -> return acc
          | `Tree (l, n) ->
            aux n >>= function
            | None   -> return acc
            | Some t -> return ((l, t) :: acc)
        ) [] children
      >>= fun children ->
      let children = lazy children in
      return (Some (Lazy_trie.create ~children ()))
  in
  aux (SHA1.of_commit commit) >>= function
  | None    -> fail (Failure "create")
  | Some fs -> return fs

let expand_filesystem t commit =
  load_filesystem t commit >>= fun trie ->
  Lazy_trie.fold (fun acc path blob ->
      acc >>= fun () ->
      let file = String.concat ~sep:"/" (t.root :: path) in
      GitMisc.mkdir (Filename.dirname file) >>= fun () ->
      Log.infof "Writing %s" file;
      Lwt_io.(with_file ~mode:Output file (fun oc -> write oc (Blob.to_string blob)))
  ) trie return_unit

let flush t =
  Hashtbl.fold ~f:(fun ~key ~data acc ->
      acc >>= fun () ->
      let data = Mstruct.to_string data in
      write_and_check_inflated t key data
    ) ~init:return_unit t.buffers
