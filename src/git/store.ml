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

let create root = {
  root;
  buffers = Hashtbl.create 1024;
  indexes = Hashtbl.create 1024;
}

let to_hex node =
  Misc.hex_encode (Node.to_string node)

let of_hex hex =
  Node.of_string (Misc.hex_decode hex)

let loose_file root node =
  let hex = to_hex node in
  let prefix = String.sub hex 0 2 in
  let suffix = String.sub hex 2 (String.length hex - 2) in
  root / "objects" / prefix // suffix

let loose_objects root =
  let objects = root / "objects" in
  let objects = File.directories objects in
  let objects = List.map File.Dirname.basename objects in
  let objects = List.filter (fun s -> (s <> "info") && (s <> "pack")) objects in
  List.fold_left (fun acc prefix ->
    let dir = root / "objects" / prefix in
    let suffixes = File.files dir in
    let suffixes = List.map File.basename suffixes in
    let objects = List.map (fun suffix ->
        of_hex (prefix ^ suffix)
      ) suffixes in
    objects @ acc
  ) [] objects

let get_loose_buffer t node =
  if Hashtbl.mem t.buffers node then
    let buf = Hashtbl.find t.buffers node in
    IO.clone buf
  else
    let file = loose_file t.root node in
    if File.exists file then
      let buf = IO.of_file file in
      let buf = IO.inflate buf in
      Hashtbl.add t.buffers node buf;
      IO.clone buf
    else (
      Printf.eprintf "No file associated with the loose object %s.\n" (to_hex node);
      failwith "read_file"
    )

let packed_file root node =
  let pack_dir = root / "objects" / "pack" in
  let pack_file = "pack-" ^ (to_hex node) ^ ".pack" in
  pack_dir // pack_file


let packed_objects root =
  let packs = root / "objects" / "pack" in
  let packs = File.files packs in
  let packs = List.map File.basename packs in
  let packs = List.filter (fun f -> Filename.check_suffix f ".idx") packs in
  List.map (fun f ->
    let p = Filename.chop_suffix f ".idx" in
    let p = String.sub p 5 (String.length p - 5) in
    of_hex p
  ) packs

let get_packed_buffer t node =
  let file = packed_file t.root node in
  if File.exists file then
    let buf = IO.of_file file in
    Hashtbl.add t.buffers node buf;
    IO.clone buf
  else (
    Printf.eprintf "No file associated with the pack object %s.\n" (to_hex node);
    failwith "read_file"
  )

let index_file root node =
  let pack_dir = root / "objects" / "pack" in
  let idx_file = "pack-" ^ (to_hex node) ^ ".idx" in
  pack_dir // idx_file

let read_index t node =
  if Hashtbl.mem t.indexes node then
    Hashtbl.find t.indexes node
  else
    let file = index_file t.root node in
    if File.exists file then
      let idx = Backend.Idx.input (IO.of_file file) in
      Hashtbl.add t.indexes node idx;
      idx
    else (
      Printf.eprintf "%s does not exist." (File.Name.to_string file);
      failwith "read_index"
    )

let nodes t =
  let objects = loose_objects t.root in
  let packs = packed_objects t.root in
  List.fold_left (fun acc p ->
    let idx = read_index t p in
    (List.map fst idx.offsets) @ acc
  ) objects packs

let expand t node value =
  let file = loose_file t.root node in
  if not (File.exists file) then (
    let inflated = Backend.Value.output_inflated value in
    let hash = Cryptokit.Hash.sha1 () in
    hash#add_string inflated;
    let actual_node = Node.of_string hash#result in
    if node <> actual_node then (
      Backend.Value.dump value;
      Printf.eprintf "Marshaling error: expected:%s actual:%s\n" (to_hex node) (to_hex actual_node);
      failwith "build_packed_object"
    );
    Printf.eprintf "XXX write %s\n" (to_hex node);
    File.mkdir (File.dirname file);
    let oc = open_out_bin (File.Name.to_string file) in
    let deflated = Misc.deflate_string inflated in
    output_string oc deflated;
    close_out oc;
  )

let rec build_packed_object t (pack,idx) node =
  Printf.eprintf "build_packed_file pack:%s node:%s\n" (to_hex pack) (to_hex node);
  let error offset =
    let idx = List.map snd idx.offsets in
    let idx = String.concat "," (List.map string_of_int idx) in
    Printf.eprintf "%d: invalid offset.\nValid offsets are: {%s}\n" offset idx;
    failwith "invalid offset" in
  if List.mem_assoc node idx.offsets then (
    let offset = List.assoc node idx.offsets in
    let pbuf = get_packed_buffer t pack in
    let version = Backend.Pack.version pbuf in
    let input =
      if version = 2 then Backend.PackedValue2.input
      else Backend.PackedValue3.input in
    let pbuf = match List.assoc node idx.lengths with
      | None   -> IO.shift pbuf offset; pbuf
      | Some l -> IO.sub pbuf offset l in
    let value =
      match input pbuf with
      | Value v     -> Printf.eprintf "XXX value\n"; v
      | Ref_delta d ->
        let source = get_loose_buffer t d.source in
        Printf.eprintf "XXX ref-delta (%d)\n" (IO.length source);
        Backend.Pack.apply_delta { d with source }
      | Off_delta d ->
        let offset = offset - d.source in
        let base, _ =
          try List.find (fun (_,o) -> o=offset) idx.offsets
          with Not_found -> error offset in
        let node = build_packed_object t (pack,idx) base in
        let source = get_loose_buffer t node in
        Printf.eprintf "XXX off-detla (%d)\n" (IO.length source);
        Backend.Pack.apply_delta { d with source } in
    expand t node value;
    Printf.eprintf "XXX OK\n";
    node
  ) else
    error (-1)

let scan_packs t node =
  let rec find = function
    | []    -> None
    | n::ns ->
      let idx = read_index t n in
      if List.mem_assoc node idx.offsets then
        let node = build_packed_object t (n,idx) node in
        Some (get_loose_buffer t node)
      else
        find ns in
  find (packed_objects t.root)

let read t node =
  let file = loose_file t.root node in
  let buf =
    if File.exists file then Some (get_loose_buffer t node)
    else scan_packs t node in
  match buf with
  | None     -> None
  | Some buf -> Some (Backend.Value.input_inflated buf)

let dump root =
  let nodes = nodes root in
  List.iter (fun n ->
    Printf.eprintf "%s\n" (to_hex n)
  ) nodes

let refs t =
  let refs = t.root / "refs" in
  let files = File.rec_files refs in
  let n = String.length (File.Dirname.to_string t.root) in
  List.map (fun file ->
    let file = File.Name.to_string file in
    let r = String.sub file n (String.length file - n) in
    r, of_hex (File.read (File.Name.of_string file))
  ) files

let succ root node =
  match read root node with
  | None            -> []
  | Some (Blob _)   -> []
  | Some (Commit c) -> Node.tree c.tree :: List.map Node.commit c.parents
  | Some (Tag t)    ->
    [Node.commit t.commit]
  | Some (Tree t)   ->
    List.map (fun e -> e.node) t
