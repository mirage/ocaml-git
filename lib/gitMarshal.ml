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

open GitTypes

let (++) f g x = f (g x)

let todo feature =
  failwith (Printf.sprintf "TODO(%s)" feature)

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'
let lt  = '<'
let gt  = '>'

let input_hex buf =
  Mstruct.get_string buf (Mstruct.length buf)
  |> GitMisc.hex_decode

let input_hex_tree buf =
  Node.Tree.of_string (input_hex buf)

let input_hex_commit buf =
  Node.Commit.of_string (input_hex buf)

let output_hex buf hex =
  Buffer.add_string buf (GitMisc.hex_encode hex)

let output_hex_commit buf hex =
  output_hex buf (Node.Commit.to_string hex)

let output_hex_tree buf hex =
  output_hex buf (Node.Tree.to_string hex)

let input_node buf =
  Mstruct.get_string buf 20
  |> Node.of_string

let output_node buf t =
  Buffer.add_string buf (Node.to_string t)

module type VALUE = sig
  type t
  val dump: t -> unit
  val output: Buffer.t -> t -> unit
  val input: Mstruct.t -> t
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
    let i = match Mstruct.index buf lt with
      | Some i -> i-1
      | None   -> Mstruct.parse_error_buf buf "invalid user name" in
    let name = Mstruct.get_string buf i in
    Mstruct.shift buf 2;
    let j = match Mstruct.index buf gt with
      | Some j -> j
      | None   -> Mstruct.parse_error_buf buf "invalid user email" in
    let email = Mstruct.get_string buf j in
    (* skip 2 bytes *)
    Mstruct.shift buf 2;
    let date = Mstruct.get_string buf (Mstruct.length buf) in
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
    Mstruct.get_string buf (Mstruct.length buf)
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
      (GitMisc.hex_encode (Node.Tree.to_string t.tree))
      (String.concat ~sep:", "
         (List.map ~f:(GitMisc.hex_encode ++ Node.Commit.to_string) t.parents))
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
    List.iter ~f:(output_parent buf) t.parents;
    Buffer.add_string buf "author ";
    User.output        buf t.author;
    Buffer.add_char   buf lf;
    Buffer.add_string buf "committer ";
    User.output       buf t.committer;
    Buffer.add_char   buf lf;
    Buffer.add_char   buf lf;
    Buffer.add_string buf t.message

  let input_parents buf =
    let rec aux parents =
      match Mstruct.get_string_delim buf sp with
      | None   -> List.rev parents
      | Some p ->
        if p = "parent" then
          match Mstruct.get_delim buf lf input_hex_commit with
          | None   -> Mstruct.parse_error_buf buf "input_parents"
          | Some h -> aux (h :: parents)
        else (
          (* we cancel the shift we've done to input the key *)
          let n = String.length p in
          Mstruct.shift buf (-n-1);
          List.rev parents
        ) in
    aux []

  let input_key_value buf expected input_value =
    let error actual =
      Mstruct.parse_error_buf buf "keys: [actual: %s] [expected: %s]" actual expected in
    let key =
      match Mstruct.get_string_delim buf sp with
      | None   -> error "<none>"
      | Some k -> k in
    if key <> expected then
      error key
    else
      match Mstruct.get_delim buf lf input_value with
      | None   -> Mstruct.parse_error_buf buf "no value to input"
      | Some v -> v

  let input buf =
    let tree      = input_key_value buf "tree" input_hex_tree in
    let parents   = input_parents   buf in
    let author    = input_key_value buf "author" User.input in
    let committer = input_key_value buf "committer" User.input in
    Mstruct.shift buf 1;
    let message   = Mstruct.to_string buf in
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
    | x -> Mstruct.parse_error_buf buf "%S is not a valid permission." x

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
    List.iter ~f:dump_entry t

  let output_entry buf e =
    Buffer.add_string buf (string_of_perm e.perm);
    Buffer.add_char   buf sp;
    Buffer.add_string buf e.file;
    Buffer.add_char   buf nul;
    output_node       buf e.node

  let input_entry buf =
    let perm = match Mstruct.get_string_delim buf sp with
      | None      -> Mstruct.parse_error_buf buf "invalid perm"
      | Some perm -> perm in
    let file = match Mstruct.get_string_delim buf nul with
      | None      -> Mstruct.parse_error_buf buf "invalid filename"
      | Some file -> file in
    let node = input_node buf in
    let entry = {
      perm = perm_of_string buf perm;
      file; node
    } in
    Some entry

  let output buf t =
    List.iter ~f:(output_entry buf) t

  let input buf =
    let rec aux entries =
      if Mstruct.length buf <= 0 then
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
  include VALUE with type t := value
  val input: Mstruct.t -> value
  val input_inflated: Mstruct.t -> value
  val output_inflated: value -> string
  val output: value -> string
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
    let deflated = GitMisc.deflate_string inflated in
    deflated

  let input_inflated buf =
    (* XXX call zlib directly on the stream interface *)
    let obj_type =
      match Mstruct.get_string_delim buf sp with
      | None   -> Mstruct.parse_error_buf buf "value: type"
      | Some t -> t in
    let size =
      match Mstruct.get_string_delim buf nul with
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
    | "blob"   -> Blob.input buf   |> blob
    | "commit" -> Commit.input buf |> commit
    | "tag"    -> Tag.input buf    |> tag
    | "tree"   -> Tree.input buf   |> tree
    | x        -> Mstruct.parse_error_buf buf "%S is not a valid object type." x

  let input buf =
    input_inflated (GitMisc.inflate_mstruct buf)

end

module PackedValue (M: sig val version: int end): sig
  include VALUE with type t := packed_value
end = struct

  let isset i bit =
    (i lsr bit) land 1 <> 0

  let input_hunk source_length buf =
    let opcode = Mstruct.get_uint8 buf in
    if opcode = 0 then
      Mstruct.parse_error_buf buf "0 as value of the first byte of a hunk is reserved.";

    match opcode land 0x80 with

    | 0 ->
      let contents = Mstruct.get_string buf opcode in
      Insert contents

    | _ ->
      let read bit shift =
        if not (isset opcode bit) then 0
        else Mstruct.get_uint8 buf lsl shift in
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
          Mstruct.parse_error_buf buf "result fied set in delta hunk";
        l0 lor l1 lor l2 in
      let length =
        if length = 0 then 0x10000 else length in
      if offset+length > source_length then
        Mstruct.parse_error_buf buf
          "wrong insert hunk (offset:%d length:%d source:%d)"
          offset length source_length;
      Copy (offset, length)

  let input_le_base_128 buf =
    let rec aux int shift =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let base = byte land 0x7f in
      let int  = (base lsl shift) lor int in
      if more then aux int (shift+7)
      else int in
    aux 0 0

  let input_hunks size source buf =
    let source_length = input_le_base_128 buf in
    let result_length = input_le_base_128 buf in
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else aux (input_hunk source_length buf :: acc) in
    let hunks = aux [] in
    { source; hunks; source_length; result_length }

  let input_be_modified_base_128 buf =
    let rec aux i n =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let i    = if n >= 2 then i+1 else i in
      let i    = (i lsl 7) lor (byte land 0x7f) in
      if more then aux i (n+1)
      else i in
    aux 0 1

  let input buf =
    let byte = Mstruct.get_uint8 buf in
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
      fn (GitMisc.inflate_mstruct buf) in

    match kind with
    | 0b000 -> Mstruct.parse_error_buf buf "invalid: Reserved"
    | 0b001 -> with_inflated buf Commit.input |> commit |> value
    | 0b010 -> with_inflated buf Tree.input   |> tree   |> value
    | 0b011 -> with_inflated buf Blob.input   |> blob   |> value
    | 0b100 -> with_inflated buf Tag.input    |> tag    |> value
    | 0b101 -> Mstruct.parse_error_buf buf "invalid: Reserved"
    | 0b110 ->
      let base  = input_be_modified_base_128 buf in
      let hunks = with_inflated buf (input_hunks size base)in
      off_delta hunks
    | 0b111 ->
      let base  = input_node buf in
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

let lengths offsets =
  let rec aux acc = function
    | []    -> List.rev acc
    | [h,_] -> aux ((h,None)::acc) []
    | (h1,l1)::((h2,l2)::_ as t) -> aux ((h1,Some (l2-l1))::acc) t in
  (* Compare the list in increasing offests. *)
  let l = List.sort ~cmp:(fun (_,x) (_,y) -> compare x y) offsets in
  aux [] l

module Pack: sig
  include VALUE with type t := pack
  val input: pack_index -> Mstruct.t -> pack
  val unpack:
    read:(node -> Mstruct.t) -> write:(value -> node) ->
    int Node.Map.t -> int -> packed_value -> node
  val unpack_all:
    read:(node -> Mstruct.t) -> write:(value -> node) ->
    Mstruct.t -> node list
end = struct

  let input_header buf =
    let header = Mstruct.get_string buf 4 in
    if header <> "PACK" then
      Mstruct.parse_error_buf buf "wrong header (%s)" header;
    let version = Int32.to_int_exn (Mstruct.get_be_uint32 buf) in
    if version <> 2 && version <> 3 then
      Mstruct.parse_error_buf buf "wrong pack version (%d)" version;
    version, Int32.to_int_exn (Mstruct.get_be_uint32 buf)

  let input_packed_value version buf =
    if version = 2 then PackedValue2.input buf
    else PackedValue3.input buf

  let input idx buf =
    let version, size = input_header (Mstruct.clone buf) in
    let packs = Node.Table.create () in
    fun node ->
      if Hashtbl.mem packs node then
        Hashtbl.find_exn packs node
      else (
        let offset = Map.find_exn idx.offsets node in
        let buf = Mstruct.clone buf in
        let buf = match Map.find_exn idx.lengths node with
          | None   ->
            Mstruct.shift buf offset;
            Mstruct.sub buf 0 (Mstruct.length buf - 20)
          | Some l -> Mstruct.sub buf offset l in
        let value = input_packed_value version buf in
        Hashtbl.replace packs node value;
        value
      )

  let apply_hunk source buf = function
    | Insert str     -> Buffer.add_string buf str
    | Copy (off,len) ->
      let view = Mstruct.sub (Mstruct.clone source) off len in
      let str = Mstruct.to_string view in
      Buffer.add_string buf str

  let dump_delta d =
    Printf.eprintf
      "source-length: %d\n\
       result-length: %d\n"
      d.source_length
      d.result_length;
    List.iter ~f:(function
      | Insert str     -> Printf.eprintf "INSERT %S\n" str
      | Copy (off,len) -> Printf.eprintf "COPY (%d,%d)\n" off len
    ) d.hunks

  let apply_delta delta =
    let source = delta.source in
    let kind = match Mstruct.get_string_delim source sp with
      | None   -> Mstruct.parse_error_buf source "missing kind"
      | Some k -> k in
    let size = match Mstruct.get_string_delim source nul with
      | None   -> Mstruct.parse_error_buf source "missing size"
      | Some s -> try int_of_string s with _ -> -1 in
    if size <> delta.source_length then
      Mstruct.parse_error_buf source
        "size differs: delta:%d source:%d\n"
        delta.source_length size;

    let buf = Buffer.create (20 + delta.result_length) in
    Buffer.add_string buf kind;
    Buffer.add_char   buf sp;
    Buffer.add_string buf (string_of_int delta.result_length);
    Buffer.add_char   buf nul;
    List.iter ~f:(apply_hunk delta.source buf) delta.hunks;

    let str = Buffer.contents buf in
    let buf = Mstruct.of_string str in
    Value.input_inflated buf

  let unpack ~read ~write idx offset packed_value =
    let value = match packed_value with
      | Value v     -> v
      | Ref_delta d ->
        let source = read d.source in
        apply_delta { d with source }
      | Off_delta d ->
        let offset = offset - d.source in
        let base =
          match Map.fold
                  ~f:(fun ~key ~data acc -> if data=offset then Some key else acc)
                  ~init:None idx
          with None ->
            let msg = Printf.sprintf
                "inflate: cannot find any object starting at offset %d"
                offset in
            failwith msg
             | Some k -> k  in
        let source = read base in
        apply_delta { d with source } in
    write value

  let unpack_all ~read ~write buf =
    Printf.eprintf "UNPACK-ALL\n%!";
    let version, size = input_header buf in
    Printf.eprintf "version=%d size=%d\n%!" version size;

    let next_packed_value () =
      input_packed_value version buf in

    let idx = ref Node.Map.empty in
    for i = 0 to size - 1 do
      Printf.eprintf "UNPACK [%d/%d]\n%!" (i+1) size;
      let offset = Mstruct.offset buf in
      let packed_value = next_packed_value () in
      let node = unpack ~read ~write !idx offset packed_value in
      idx := Map.add !idx ~key:node ~data:offset
    done;
    Map.keys !idx

  let output _ =
    todo "pack"

  let dump _ =
    todo "pack"

end

module Idx: VALUE with type t := pack_index = struct

  let input buf =

    let magic = Mstruct.get_string buf 4 in
    if magic <> "\255tOc" then
      Mstruct.parse_error_buf buf "wrong magic index (%S)" magic;
    let version = Mstruct.get_be_uint32 buf in
    if version <> 2l then
      Mstruct.parse_error_buf buf "wrong index version (%ld)" version;

    (* Read the fanout *)
    let fanout =
      let a = Array.create 256 0l in
      for i=0 to 255 do
        a.(i) <- Mstruct.get_be_uint32 buf;
      done;
      a in

    let nb_objects = Int32.to_int_exn fanout.(255) in

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
        a.(i) <- Mstruct.get_be_uint32 buf;
      done;
      a in

    (* Read the offsets *)
    let offsets, conts =
      let a = Array.create nb_objects 0l in
      let b = Array.create nb_objects false in
      for i=0 to nb_objects-1 do
        b.(i) <- (Mstruct.get_uint8 buf) land 128 <> 0;
        Mstruct.shift buf (-1);
        a.(i) <- Mstruct.get_be_uint32 buf
      done;
      a, b in

    let idx = Array.mapi (fun i name ->
        let offset = offsets.(i) in
        let cont = conts.(i) in
        if cont then (
          let offset = Mstruct.get_be_uint64 buf in
          (name, Int64.to_int_exn offset)
        ) else
          (name, Int32.to_int_exn offset)
      ) names in

    let _node = input_node buf in
    let _checksum = input_node buf in

    let offsets_alist = Array.to_list idx in
    let offsets = Node.Map.of_alist_exn offsets_alist in
    let lengths = Node.Map.of_alist_exn (lengths offsets_alist) in
    { offsets; lengths }

  let output buf =
    todo "pack-index"

  let dump buf =
    todo "pack-index"

end
