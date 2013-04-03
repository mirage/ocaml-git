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

open Lib
open Model
open File.OP

let create root =
  {
    root;
    buffers = Hashtbl.create 1024;
    packs   = Hashtbl.create 1024;
    indexes = Hashtbl.create 1024;
  }

let current () =
  create (File.cwd ())

let to_hex node =
  Misc.hex_encode (Node.to_string node)

let of_hex hex =
  Node.of_string (Misc.hex_decode hex)

module Loose = struct

  let file root node =
    let hex = to_hex node in
    let prefix = String.sub hex 0 2 in
    let suffix = String.sub hex 2 (String.length hex - 2) in
    root / ".git" / "objects" / prefix // suffix

  let read_inflated t node =
    if Hashtbl.mem t.buffers node then
      let buf = Hashtbl.find t.buffers node in
      Some (IO.clone buf)
    else
      let file = file t.root node in
      if File.exists file then
        let buf = IO.of_file file in
        let buf = IO.inflate buf in
        Hashtbl.add t.buffers node buf;
        Some (IO.clone buf)
      else
        None

  let write_inflated t inflated =
    let node = Model.Node.sha1 inflated in
    Printf.eprintf "Expanding %s\n" (to_hex node);
    let file = file t.root node in
    File.mkdir (File.dirname file);
    let oc = open_out_bin (File.Name.to_string file) in
    let deflated = Misc.deflate_string inflated in
    output_string oc deflated;
    close_out oc;
    node

  let write_and_check_inflated t node inflated =
    let file = file t.root node in
    if not (File.exists file)
    then (
      let new_node = Model.Node.sha1 inflated in
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
    let inflated = Backend.Value.output_inflated value in
    write_inflated t inflated

  let write_and_check t node value =
    let inflated = Backend.Value.output_inflated value in
    write_and_check_inflated t node inflated

  let list root =
    let objects = root / ".git" / "objects" in
    let objects = File.directories objects in
    let objects = List.map File.Dirname.basename objects in
    let objects = List.filter (fun s -> (s <> "info") && (s <> "pack")) objects in
    List.fold_left (fun acc prefix ->
      let dir = root / ".git" / "objects" / prefix in
      let suffixes = File.files dir in
      let suffixes = List.map File.basename suffixes in
      let objects = List.map (fun suffix ->
          of_hex (prefix ^ suffix)
        ) suffixes in
      objects @ acc
    ) [] objects

end

module Packed = struct

  let file root node =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let pack_file = "pack-" ^ (to_hex node) ^ ".pack" in
    pack_dir // pack_file

  let list root =
    let packs = root / ".git" / "objects" / "pack" in
    let packs = File.files packs in
    let packs = List.map File.basename packs in
    let packs = List.filter (fun f -> Filename.check_suffix f ".idx") packs in
    List.map (fun f ->
      let p = Filename.chop_suffix f ".idx" in
      let p = String.sub p 5 (String.length p - 5) in
      of_hex p
    ) packs

  let index root node =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let idx_file = "pack-" ^ (to_hex node) ^ ".idx" in
    pack_dir // idx_file

  let read_index t node: pack_index =
    if Hashtbl.mem t.indexes node then
      Hashtbl.find t.indexes node
    else
      let file = index t.root node in
      if File.exists file then
        let idx = Backend.Idx.input (IO.of_file file) in
        Hashtbl.add t.indexes node idx;
        idx
      else (
        Printf.eprintf "%s does not exist." (File.Name.to_string file);
        failwith "read_index"
      )

  let read t node =
    if Hashtbl.mem t.packs node then
      Hashtbl.find t.packs node
    else (
      let file = file t.root node in
      let index = read_index t node in
      if File.exists file then (
        let buf = IO.of_file file in
        let fn = Backend.Pack.input index buf in
        Hashtbl.add t.packs node fn;
        fn
      ) else (
        Printf.eprintf "No file associated with the pack object %s.\n" (to_hex node);
        failwith "read_file"
      )
    )

  let rec read_inflated t (pack,idx) node =
    let error offset =
      let idx = List.map snd idx.offsets in
      let idx = String.concat "," (List.map string_of_int idx) in
      Printf.eprintf "%d: invalid offset.\nValid offsets are: {%s}\n" offset idx;
      failwith "invalid offset" in
    if List.mem_assoc node idx.offsets then (
      let offset = List.assoc node idx.offsets in
      let packed_value = read t pack node in
      let read node =
        match Loose.read_inflated t node with
        | Some buf -> buf
        | None     -> read_inflated t (pack,idx) node in
      let write value =
        Loose.write_and_check t node value;
        node in
      let n = Backend.Pack.unpack ~read ~write idx.offsets offset packed_value in
      assert (n = node);
      read node
    ) else
      error (-1)

  let read_inflated t node =
    let rec find = function
      | []    -> None
      | n::ns ->
        let idx = read_index t n in
        if List.mem_assoc node idx.offsets then
          Some (read_inflated t (n,idx) node)
        else
          find ns in
    find (list t.root)

end

let list t =
  let objects = Loose.list t.root in
  let packs = Packed.list t.root in
  List.fold_left (fun acc p ->
    let idx = Packed.read_index t p in
    (List.map fst idx.offsets) @ acc
  ) objects packs

let read t node =
  let file = Loose.file t.root node in
  let buf =
    if File.exists file then Loose.read_inflated t node
    else Packed.read_inflated t node in
  match buf with
  | None     -> None
  | Some buf -> Some (Backend.Value.input_inflated buf)

let dump root =
  let nodes = list root in
  List.iter (fun n ->
    Printf.eprintf "%s\n" (to_hex n)
  ) nodes

let refs t =
  let refs = t.root / ".git" / "refs" in
  let files = File.rec_files refs in
  let n = String.length (File.Dirname.to_string t.root) in
  List.map (fun file ->
    let file = File.Name.to_string file in
    let r = String.sub file n (String.length file - n) in
    r, of_hex (File.read (File.Name.of_string file))
  ) files

let succ root node =
  let parent c = (`parent, Node.commit c) in
  let tag t = (`tag t.tag, Node.commit t.commit) in
  match read root node with
  | None            -> []
  | Some (Blob _)   -> []
  | Some (Commit c) -> (`file "", Node.tree c.tree) :: List.map parent c.parents
  | Some (Tag t)    -> [tag t]
  | Some (Tree t)   -> List.map (fun e -> (`file e.file, e.node)) t

let write t value =
  Loose.write t value

let write_and_check t node value =
  Loose.write_and_check t node value

let write_inflated t inflated =
  Loose.write_inflated t inflated

let write_and_check_inflated t node inflated =
  Loose.write_and_check_inflated t node inflated

let write_reference t name node =
  let file = t.root / ".git" // name in
  File.mkdir (File.dirname file);
  let oc = open_out_bin (File.Name.to_string file) in
  output_string oc (to_hex node);
  close_out oc
