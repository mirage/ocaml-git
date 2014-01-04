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

open GitMisc.OP
open GitTypes


let create root =
  {
    root;
    buffers = SHA1.Table.create ();
    packs   = SHA1.Table.create ();
    indexes = SHA1.Table.create ();
  }

let current () =
  create (Sys.getcwd ())

let to_hex node =
  GitMisc.hex_encode (SHA1.to_string node)

let of_hex hex =
  SHA1.of_string (GitMisc.hex_decode hex)

module Loose = struct

  let file root node =
    let hex = to_hex node in
    let prefix = String.sub hex 0 2 in
    let suffix = String.sub hex 2 (String.length hex - 2) in
    root / ".git" / "objects" / prefix / suffix

  let read_inflated t node =
    if Hashtbl.mem t.buffers node then
      let buf = Hashtbl.find_exn t.buffers node in
      Some (Mstruct.clone buf)
    else
      let file = file t.root node in
      if Sys.file_exists file then
        let buf = GitMisc.mstruct_of_file file in
        let buf = GitMisc.inflate_mstruct buf in
        Hashtbl.replace t.buffers node buf;
        Some (Mstruct.clone buf)
      else
        None

  let write_inflated t inflated =
    let node = SHA1.sha1 inflated in
    Printf.eprintf "Expanding %s\n" (to_hex node);
    let file = file t.root node in
    GitMisc.mkdir (Filename.dirname file);
    let deflated = GitMisc.deflate_string inflated in
    Out_channel.write_all file ~data:deflated;
    node

  let write_and_check_inflated t node inflated =
    let file = file t.root node in
    if not (Sys.file_exists file)
    then (
      let new_node = SHA1.sha1 inflated in
      if node = new_node then
        ignore (write_inflated t inflated)
      else (
        Printf.eprintf "%S\n" inflated;
        Printf.eprintf
          "Marshaling error: expected:%s actual:%s\n" (to_hex node) (to_hex new_node);
        failwith "build_packed_object"
      )
    )

  let write t value =
    let inflated = GitMarshal.Value.output_inflated value in
    write_inflated t inflated

  let write_and_check t node value =
    let inflated = GitMarshal.Value.output_inflated value in
    write_and_check_inflated t node inflated

  let list root =
    let objects = root / ".git" / "objects" in
    let objects = GitMisc.directories objects in
    let objects = List.map ~f:Filename.basename objects in
    let objects = List.filter ~f:(fun s -> (s <> "info") && (s <> "pack")) objects in
    List.fold_left ~f:(fun acc prefix ->
      let dir = root / ".git" / "objects" / prefix in
      let suffixes = GitMisc.files dir in
      let suffixes = List.map ~f:Filename.basename suffixes in
      let objects = List.map ~f:(fun suffix ->
          of_hex (prefix ^ suffix)
        ) suffixes in
      objects @ acc
    ) ~init:[] objects

end

module Packed = struct

  let file root node =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let pack_file = "pack-" ^ (to_hex node) ^ ".pack" in
    pack_dir / pack_file

  let list root =
    let packs = root / ".git" / "objects" / "pack" in
    let packs = GitMisc.files packs in
    let packs = List.map ~f:Filename.basename packs in
    let packs = List.filter ~f:(fun f -> Filename.check_suffix f ".idx") packs in
    List.map ~f:(fun f ->
      let p = Filename.chop_suffix f ".idx" in
      let p = String.sub p 5 (String.length p - 5) in
      of_hex p
    ) packs

  let index root node =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let idx_file = "pack-" ^ (to_hex node) ^ ".idx" in
    pack_dir / idx_file

  let read_index t node: pack_index =
    if Hashtbl.mem t.indexes node then
      Hashtbl.find_exn t.indexes node
    else
      let file = index t.root node in
      if Sys.file_exists file then
        let idx = GitMarshal.Idx.input (GitMisc.mstruct_of_file file) in
        Hashtbl.replace t.indexes node idx;
        idx
      else (
        Printf.eprintf "%s does not exist." file;
        failwith "read_index"
      )

  let read t node =
    if Hashtbl.mem t.packs node then
      Hashtbl.find_exn t.packs node
    else (
      let file = file t.root node in
      let index = read_index t node in
      if Sys.file_exists file then (
        let buf = GitMisc.mstruct_of_file file in
        let fn = GitMarshal.Pack.input index buf in
        Hashtbl.replace t.packs node fn;
        fn
      ) else (
        Printf.eprintf "No file associated with the pack object %s.\n" (to_hex node);
        failwith "read_file"
      )
    )

  let rec read_inflated t (pack,idx) node =
    let error offset =
      let idx = Map.data idx.offsets in
      let idx = String.concat ~sep:"," (List.map ~f:string_of_int idx) in
      Printf.eprintf "%d: invalid offset.\nValid offsets are: {%s}\n" offset idx;
      failwith "invalid offset" in
    if Map.mem idx.offsets node then (
      let offset = Map.find_exn idx.offsets node in
      let packed_value = read t pack node in
      let read node =
        match Loose.read_inflated t node with
        | Some buf -> buf
        | None     -> read_inflated t (pack,idx) node in
      let write value =
        Loose.write_and_check t node value;
        node in
      let n = GitMarshal.Pack.unpack ~read ~write idx.offsets offset packed_value in
      assert (n = node);
      read node
    ) else
      error (-1)

  let read_inflated t node =
    let rec find = function
      | []    -> None
      | n::ns ->
        let idx = read_index t n in
        if Map.mem idx.offsets node then
          Some (read_inflated t (n,idx) node)
        else
          find ns in
    find (list t.root)

end

let list t =
  let objects = Loose.list t.root in
  let packs = Packed.list t.root in
  List.fold_left ~f:(fun acc p ->
    let idx = Packed.read_index t p in
    (Map.keys idx.offsets) @ acc
  ) ~init:objects packs

let read t node =
  let file = Loose.file t.root node in
  let buf =
    if Sys.file_exists file then Loose.read_inflated t node
    else Packed.read_inflated t node in
  match buf with
  | None     -> None
  | Some buf -> Some (GitMarshal.Value.input_inflated buf)

let dump root =
  let nodes = list root in
  List.iter ~f:(fun n ->
    Printf.eprintf "%s\n" (to_hex n)
  ) nodes

let references t =
  let refs = t.root / ".git" / "refs" in
  let files = GitMisc.rec_files refs in
  let n = String.length t.root in
  List.map ~f:(fun file ->
      let r = String.sub file n (String.length file - n) in
      r, of_hex (In_channel.read_all file)
    ) files

let succ root node =
  let parent c = (`parent, SHA1.commit c) in
  let tag t = (`tag t.tag, SHA1.commit t.commit) in
  match read root node with
  | None            -> []
  | Some (Blob _)   -> []
  | Some (Commit c) -> (`file "", SHA1.tree c.tree) :: List.map ~f:parent c.parents
  | Some (Tag t)    -> [tag t]
  | Some (Tree t)   -> List.map ~f:(fun e -> (`file e.file, e.node)) t

let write t value =
  Loose.write t value

let write_and_check t node value =
  Loose.write_and_check t node value

let write_inflated t inflated =
  Loose.write_inflated t inflated

let write_and_check_inflated t node inflated =
  Loose.write_and_check_inflated t node inflated

let write_reference t name node =
  let file = t.root / ".git" / name in
  GitMisc.mkdir (Filename.dirname file);
  Out_channel.write_all file (to_hex node)
