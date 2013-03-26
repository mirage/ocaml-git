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
open IO
open Model

let (|>) x f = f x

let (++) f g x = f (g x)

let todo feature =
  failwith (Printf.sprintf "TODO(%s)" feature)

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'
let lt  = '<'
let gt  = '>'

let input_hex buf =
  let buf = IO.push buf "hex-tree" in
  IO.input_string buf None
  |> Misc.hex_decode

let input_hex_tree buf =
  Node.Tree.of_string (input_hex buf)

let input_hex_commit buf =
  Node.Commit.of_string (input_hex buf)

let output_hex buf hex =
  Buffer.add_string buf (Misc.hex_encode hex)

let output_hex_commit buf hex =
  output_hex buf (Node.Commit.to_string hex)

let output_hex_tree buf hex =
  output_hex buf (Node.Tree.to_string hex)

let input_node buf =
  let buf = IO.push buf "node" in
  IO.input_string buf (Some 20)
  |> Node.of_string

let output_node buf t =
  Buffer.add_string buf (Node.to_string t)

module type VALUE = sig
  type t
  val dump: t -> unit
  val output: Buffer.t -> t -> unit
  val input: IO.buffer -> t
end

module User: sig
  include VALUE with type t := user
  val pretty: user -> string
end = struct

  (* XXX needs to escape name/email/date *)
  let output buf t =
    Buffer.add_string buf t.name ;
    Buffer.add_string buf " <"   ;
    Buffer.add_string buf t.email;
    Buffer.add_string buf "> "   ;
    Buffer.add_string buf t.date

  let input buf =
    let buf = IO.push buf "user" in
    let i = match IO.index buf lt with
      | Some i -> i-1
      | None   -> parse_error buf "invalid user name" in
    let name = IO.input_string buf (Some i) in
    IO.shift buf 2;
    let j = match IO.index buf gt with
      | Some j -> j
      | None   -> parse_error buf "invalid user email" in
    let email = IO.input_string buf (Some j) in
    (* skip 2 bytes *)
    IO.shift buf 2;
    let date = IO.input_string buf None in
    { name; email; date }

  let pretty t =
    Printf.sprintf
      "[name: %S] [email: %S] [date: %S]"
      t.name t.email t.date

  let dump t =
    Printf.eprintf "%s\n" (pretty t)

end

module Blob: VALUE with type t := Blob.t = struct

  let dump t =
    Printf.eprintf "%s\n" (Blob.to_string t)

  let input buf =
    let buf = IO.push buf "blob" in
    IO.input_string buf None
    |> Blob.of_string

  let output buf t =
    Buffer.add_string buf (Blob.to_string t)

end

module Commit: VALUE with type t := commit = struct

  let dump t =
    Printf.eprintf
      "  tree     : %s\n\
      \  parents  : %s\n\
      \  author   : %s\n\
      \  committer: %s\n\
      \  message  :\n%s\n"
      (Misc.hex_encode (Node.Tree.to_string t.tree))
      (String.concat ", " (List.map (Misc.hex_encode ++ Node.Commit.to_string) t.parents))
      (User.pretty t.author)
      (User.pretty t.committer)
      t.message

  let output_parent buf parent =
    Buffer.add_string buf "parent ";
    output_hex_commit buf parent;
    Buffer.add_char   buf lf

  let output buf t =
    Buffer.add_string buf "tree ";
    output_hex_tree   buf t.tree;
    Buffer.add_char   buf lf;
    List.iter (output_parent buf) t.parents;
    Buffer.add_string buf "author ";
    User.output        buf t.author;
    Buffer.add_char   buf lf;
    Buffer.add_string buf "committer ";
    User.output        buf t.committer;
    Buffer.add_char   buf lf;
    Buffer.add_char   buf lf;
    Buffer.add_string buf t.message

  let input_parents buf =
    let buf = IO.push buf "parents" in
    let rec aux parents =
      match IO.input_string_delim buf sp with
      | None   -> List.rev parents
      | Some p ->
        if p = "parent" then
          match IO.input_delim buf lf input_hex_commit with
          | None   -> parse_error buf "input_parents"
          | Some h -> aux (h :: parents)
        else (
          (* we cancel the shift we've done to input the key *)
          let n = String.length p in
          IO.shift buf (-n-1);
          List.rev parents
        ) in
    aux []

  let input_key_value buf expected input_value =
    let error actual =
      parse_error buf "keys: [actual: %s] [expected: %s]" actual expected in
    let key =
      let buf = IO.push buf "key" in
      match IO.input_string_delim buf sp with
      | None   -> error "<none>"
      | Some k -> k in
    if key <> expected then
      error key
    else
      match IO.input_delim buf lf input_value with
      | None   -> parse_error buf "no value to input"
      | Some v -> v

  let input buf =
    let buf       = IO.push         buf "commit" in
    let tree      = input_key_value buf "tree" input_hex_tree in
    let parents   = input_parents   buf in
    let author    = input_key_value buf "author" User.input in
    let committer = input_key_value buf "committer" User.input in
    IO.shift buf 1;
    let message   = IO.input_string buf None in
    { parents; message; tree; author; committer }

end

module Tree: VALUE with type t := tree = struct

  let pretty_perm = function
    | `normal -> "normal"
    | `exec   -> "exec"
    | `link   -> "link"
    | `dir    -> "dir"

  let perm_of_string buf = function
    | "100644" -> `normal
    | "100755" -> `exec
    | "120000" -> `normal
    | "40000"  -> `dir
    | x -> parse_error buf "%S is not a valid permission." x

  let string_of_perm = function
    | `normal -> "100644"
    | `exec   -> "100755"
    | `link   -> "120000"
    | `dir    -> "40000"

  let dump_entry e =
    Printf.eprintf
      "perm: %s\n\
       file: %s\n\
       node: %S\n"
      (pretty_perm e.perm)
      e.file
      (Node.to_string e.node)

  let dump t =
    List.iter dump_entry t

  let output_entry buf e =
    Buffer.add_string buf (string_of_perm e.perm);
    Buffer.add_char   buf sp;
    Buffer.add_string buf e.file;
    Buffer.add_char   buf nul;
    output_node       buf e.node

  let input_entry buf =
    let buf  = IO.push buf "entry" in
    let perm = match IO.input_string_delim buf sp with
      | None      -> parse_error buf "invalid perm"
      | Some perm -> perm in
    let file = match IO.input_string_delim buf nul with
      | None      -> parse_error buf "invalid filename"
      | Some file -> file in
    let node = input_node buf in
    let entry = {
      perm = perm_of_string buf perm;
      file; node
    } in
    Some entry

  let output buf t =
    List.iter (output_entry buf) t

  let input buf =
    let buf = IO.push buf "entries" in
    let rec aux entries =
      if IO.length buf <= 0 then
        List.rev entries
      else
        match input_entry buf with
        | None   -> List.rev entries
        | Some e -> aux (e :: entries) in
    aux []

end

module Tag: VALUE with type t := tag = struct

  type t = tag
  let dump x = todo "tree"
  let output buf t = todo "tree"
  let input buf = todo "tree"

end

module Value: sig
  include VALUE with type t := Model.value
  val input: IO.buffer -> Model.value
  val input_inflated: IO.buffer -> Model.value
  val output_inflated: Model.value -> string
  val output: Model.value -> string
end = struct

  let dump = function
    | Blob b ->
      Printf.eprintf "BLOB:\n";
      Blob.dump b
    | Commit c ->
      Printf.eprintf "COMMIT:\n";
      Commit.dump c
    | Tag t ->
      Printf.eprintf "TAG:\n";
      Tag.dump t
    | Tree t ->
      Printf.eprintf "TREE:\n";
      Tree.dump t

  let obj_type = function
    | Blob _   -> "blob"
    | Commit _ -> "commit"
    | Tag _    -> "tag"
    | Tree _   -> "tree"

  let output_contents buf = function
    | Blob b   -> Blob.output buf b
    | Commit c -> Commit.output buf c
    | Tag t    -> Tag.output buf t
    | Tree t   -> Tree.output buf t

  let output_inflated t =
    let size, contents =
      let buf = Buffer.create 1024 in
      output_contents buf t;
      let size = Buffer.length buf in
      size, buf in
    let buf = Buffer.create size in
    Buffer.add_string buf (obj_type t);
    Buffer.add_char   buf sp;
    Buffer.add_string buf (string_of_int size);
    Buffer.add_char   buf nul;
    Buffer.add_buffer buf contents;
    Buffer.contents buf

  let output t =
    let inflated = output_inflated t in
    let deflated = Misc.deflate_string inflated in
    deflated

  let input_inflated buf =
    let buf = IO.push buf "value" in
    (* XXX call zlib directly on the stream interface *)
    let obj_type =
      let buf = IO.push buf "type" in
      match IO.input_string_delim buf sp with
      | None   -> parse_error buf "value: type"
      | Some t -> t in
    let size =
      let buf = IO.push buf "size" in
      match IO.input_string_delim buf nul with
      | None   -> parse_error buf "value: size"
      | Some s ->
        try int_of_string s
        with _ -> parse_error buf "%S is not a valid integer." s in
    if size <> IO.length buf then
      parse_error buf
        "[expected-size: %d; actual-size: %d]\n"
        size (IO.length buf);
    let buf = IO.sub buf 0 size in
    match obj_type with
    | "blob"   -> Blob.input buf   |> blob
    | "commit" -> Commit.input buf |> commit
    | "tag"    -> Tag.input buf    |> tag
    | "tree"   -> Tree.input buf   |> tree
    | x        -> parse_error buf "%S is not a valid object type." x

  let input buf =
    input_inflated (IO.inflate buf)

end

module PackedValue (M: sig val version: int end): sig
  include VALUE with type t := packed_value
end = struct

  let isset i bit =
    (i lsr bit) land 1 <> 0

  let input_hunk source_length buf =
    let opcode = IO.input_byte buf in
    if opcode = 0 then
      parse_error buf "0 as value of the first byte of a hunk is reserved.";

    match opcode land 0x80 with

    | 0 ->
      let contents = IO.input_string buf (Some opcode) in
      Insert contents

    | _ ->
      let read bit shift =
        if not (isset opcode bit) then 0
        else IO.input_byte buf lsl shift in
      let offset =
        let o0 = read 0 0 in
        let o1 = read 1 8 in
        let o2 = read 2 16 in
        let o3 = read 3 24 in
        o0 lor o1 lor o2 lor o3 in
      let length =
        let l0 = read 4 0 in
        let l1 = read 5 8 in
        let l2 = read 6 16 in
        if M.version = 2 && l2 <> 0 then
          parse_error buf "result fied set in delta hunk";
        l0 lor l1 lor l2 in
      let length =
        if length = 0 then 0x10000 else length in
      if offset+length > source_length then
        parse_error buf
          "wrong insert hunk (offset:%d length:%d source:%d)"
          offset length source_length;
      Copy (offset, length)

  let input_le_base_128 buf =
    let buf = IO.push buf "le-128" in
    let rec aux int shift =
      let byte = IO.input_byte buf in
      let more = (byte land 0x80) <> 0 in
      let base = byte land 0x7f in
      let int  = (base lsl shift) lor int in
      if more then aux int (shift+7)
      else int in
    aux 0 0

  let input_hunks size source buf =
    let buf = IO.push buf (Printf.sprintf "hunks[%d]" size) in
    let source_length = input_le_base_128 buf in
    let result_length = input_le_base_128 buf in
    let rec aux acc =
      if IO.length buf = 0 then List.rev acc
      else aux (input_hunk source_length buf :: acc) in
    let hunks = aux [] in
    { source; hunks; source_length; result_length }

  let input_be_modified_base_128 buf =
    let buf = IO.push buf "be-128" in
    let rec aux i n =
      let byte = IO.input_byte buf in
      let more = (byte land 0x80) <> 0 in
      let i    = if n >= 2 then i+1 else i in
      let i    = (i lsl 7) lor (byte land 0x7f) in
      if more then aux i (n+1)
      else i in
    aux 0 1

  let input buf =
    let buf  = IO.push buf "pack-value" in
    let byte = IO.input_byte buf in
    let more = (byte land 0x80) <> 0 in
    let kind = (byte land 0x70) lsr 4 in
    let size =
      let low = (byte land 0x0f) in
      if more then
        let ss = input_le_base_128 buf in
        low lor (ss lsl 4)
      else low in

    let with_inflated buf fn =
      (* XXX: the 2 following lines are very expensive *)
      let buf = IO.push buf "inflated"in
      fn (IO.inflate buf) in

    match kind with
    | 0b000 -> parse_error   buf "invalid: Reserved"
    | 0b001 -> with_inflated buf Commit.input |> commit |> value
    | 0b010 -> with_inflated buf Tree.input   |> tree   |> value
    | 0b011 -> with_inflated buf Blob.input   |> blob   |> value
    | 0b100 -> with_inflated buf Tag.input    |> tag    |> value
    | 0b101 -> parse_error   buf "invalid: Reserved"
    | 0b110 ->
      let base  = input_be_modified_base_128 buf in
      let buf   = IO.push buf (Printf.sprintf "delta-off[%d]" base) in
      let hunks = with_inflated buf (input_hunks size base)in
      off_delta hunks
    | 0b111 ->
      let base  = input_node buf in
      let buf   = IO.push buf "delta-ref" in
      let hunks = with_inflated buf (input_hunks size base) in
      ref_delta hunks
    | _     -> assert false

  let output buf t =
    todo "delta"

  let dump t =
    todo "delta"

end

module PackedValue2 = PackedValue(struct let version = 2 end)
module PackedValue3 = PackedValue(struct let version = 3 end)


module Pack: sig
  include VALUE with type t := pack
  val input: pack_index -> IO.buffer -> pack
  val apply_delta: IO.buffer delta -> value
end = struct

  let input idx orig_buf =

    let buf = IO.clone orig_buf in
    let buf = IO.push buf "version" in

    let header = IO.input_string buf (Some 4) in
    if header <> "PACK" then
      parse_error buf "wrong header (%s)" header;

    let version = Int32.to_int (IO.input_be_int32 buf) in
    if version <> 2 && version <> 3 then
      parse_error buf "wrong pack version (%d)" version;

    let size = Int32.to_int (IO.input_be_int32 buf) in

    let packs = Hashtbl.create size in
    fun node ->
      if Hashtbl.mem packs node then
        Hashtbl.find packs node
      else (
        let offset = List.assoc node idx.offsets in
        let buf = IO.clone orig_buf in
        let buf = match List.assoc node idx.lengths with
          | None   -> IO.shift buf offset; buf
          | Some l -> IO.sub buf offset l in
        let value =
          if version = 2 then
            PackedValue2.input buf
          else
            PackedValue3.input buf in
        Hashtbl.add packs node value;
        value
      )

  let apply_hunk source buf = function
    | Insert str     -> Buffer.add_string buf str
    | Copy (off,len) ->
      let view = IO.sub (IO.clone source) off len in
      let str = IO.input_string view None in
      Buffer.add_string buf str

  let dump_delta d =
    Printf.eprintf
      "source-length: %d\n\
       result-length: %d\n"
      d.source_length
      d.result_length;
    List.iter (function
      | Insert str     -> Printf.eprintf "INSERT %S\n" str
      | Copy (off,len) -> Printf.eprintf "COPY (%d,%d)\n" off len
    ) d.hunks

  let apply_delta delta =
    let source = delta.source in
    let kind = match IO.input_string_delim source sp with
      | None   -> parse_error source "missing kind"
      | Some k -> k in
    let size = match IO.input_string_delim source nul with
      | None   -> parse_error source "missing size"
      | Some s -> try int_of_string s with _ -> -1 in
    if size <> delta.source_length then
      parse_error source
        "size differs: delta:%d source:%d\n"
        delta.source_length size;

    let buf = Buffer.create (20 + delta.result_length) in
    Buffer.add_string buf kind;
    Buffer.add_char   buf sp;
    Buffer.add_string buf (string_of_int delta.result_length);
    Buffer.add_char   buf nul;
    List.iter (apply_hunk delta.source buf) delta.hunks;

    let str = Buffer.contents buf in
    let buf = IO.of_string (File.Name.of_string "<hunk>") str in
    Value.input_inflated buf

  let output _ =
    todo "pack"

  let dump _ =
    todo "pack"

end

module Idx: VALUE with type t := pack_index = struct

  let lengths offsets =
    let rec aux acc = function
      | []    -> List.rev acc
      | [h,_] -> aux ((h,None)::acc) []
      | (h1,l1)::((h2,l2)::_ as t) -> aux ((h1,Some (l2-l1))::acc) t in
    (* Compare the list in increasing offests. *)
    let l = List.sort (fun (_,x) (_,y) -> compare x y) offsets in
    aux [] l

  let input buf =
    let buf = IO.push buf "pack-index" in

    let magic = IO.input_string buf (Some 4) in
    if magic <> "\255tOc" then
      parse_error buf "wrong magic index (%S)" magic;
    let version = IO.input_be_int32 buf in
    if version <> 2l then
      parse_error buf "wrong index version (%ld)" version;

    (* Read the fanout *)
    let fanout =
      let a = Array.create 256 0l in
      for i=0 to 255 do
        a.(i) <- IO.input_be_int32 buf;
      done;
      a in

    let nb_objects = Int32.to_int fanout.(255) in

    (* Read the names *)
    let names =
      let a = Array.create nb_objects (Node.of_string "") in
      for i=0 to nb_objects-1 do
        a.(i) <- input_node buf;
      done;
      a in

    (* Read the CRCs *)
    let _crcs =
      let a = Array.create nb_objects 0l in
      for i=0 to nb_objects-1 do
        a.(i) <- IO.input_be_int32 buf;
      done;
      a in

    (* Read the offsets *)
    let offsets, conts =
      let a = Array.create nb_objects 0l in
      let b = Array.create nb_objects false in
      for i=0 to nb_objects-1 do
        b.(i) <- (IO.input_byte buf) land 128 <> 0;
        IO.shift buf (-1);
        a.(i) <- IO.input_be_int32 buf
      done;
      a, b in

    let idx = Array.mapi (fun i name ->
        let offset = offsets.(i) in
        let cont = conts.(i) in
        if cont then (
          let offset = IO.input_be_int64 buf in
          (name, Int64.to_int offset)
        ) else
          (name, Int32.to_int offset)
      ) names in

    let _node = input_node buf in
    let _checksum = input_node buf in

    let offsets = Array.to_list idx in
    let lengths = lengths offsets in
    { offsets; lengths }

  let output buf =
    todo "pack-index"

  let dump buf =
    todo "pack-index"

end
