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

let input_hex_sha1 buf =
  SHA1.of_hex (input_hex buf)

let input_hex_tree buf =
  SHA1.Tree.of_hex (input_hex buf)

let input_hex_commit buf =
  SHA1.Commit.of_hex (input_hex buf)

let output_hex buf hex =
  Buffer.add_string buf hex

let output_hex_commit buf hex =
  output_hex buf (SHA1.Commit.to_hex hex)

let output_hex_tree buf hex =
  output_hex buf (SHA1.Tree.to_hex hex)

let input_sha1 buf =
  Mstruct.get_string buf 20
  |> SHA1.of_string

let output_sha1 buf t =
  Buffer.add_string buf (SHA1.to_string t)

let output_key_value buf k v =
  Buffer.add_string buf k;
  Buffer.add_char   buf sp;
  Buffer.add_string buf v;
  Buffer.add_char   buf lf

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


module type ISERIALIZABLE = sig
  type t
  val pretty: t -> string
  val dump: t -> unit
  val output_inflated: Buffer.t -> t -> unit
  val input_inflated: Mstruct.t -> t
end

module type SERIALIZABLE = sig
  type t
  val pretty: t -> string
  val dump: t -> unit
  val output: t -> Mstruct.t
  val input: Mstruct.t -> t
end

module User: sig
  include ISERIALIZABLE with type t := User.t
  val pretty: User.t -> string
end = struct

  (* XXX needs to escape name/email/date *)
  let output_inflated buf t =
    let open User in
    Buffer.add_string buf t.name ;
    Buffer.add_string buf " <"   ;
    Buffer.add_string buf t.email;
    Buffer.add_string buf "> "   ;
    Buffer.add_string buf t.date

  let input_inflated buf =
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
    { User.name; email; date }

  let pretty t =
    let open User in
    Printf.sprintf
      "[name: %S] [email: %S] [date: %S]"
      t.name t.email t.date

  let dump t =
    Printf.eprintf "%s\n" (pretty t)

end

module Blob: ISERIALIZABLE with type t := Blob.t = struct

  let pretty t =
    Printf.sprintf "%S" (Blob.to_string t)

  let dump t =
    Printf.eprintf "%s\n" (pretty t)

  let input_inflated buf =
    Mstruct.get_string buf (Mstruct.length buf)
    |> Blob.of_string

  let output_inflated buf t =
    Buffer.add_string buf (Blob.to_string t)

end

module Commit: ISERIALIZABLE with type t := commit = struct

  let pretty t =
    let open Commit in
    Printf.sprintf
      "{\n\
      \  tree     : %s\n\
      \  parents  : %s\n\
      \  author   : %s\n\
      \  committer: %s\n\
      \  message  :\n%s\n\
       }\n"
      (SHA1.Tree.to_hex t.tree)
      (String.concat ~sep:", " (List.map ~f:SHA1.Commit.to_hex t.parents))
      (User.pretty t.author)
      (User.pretty t.committer)
      t.message

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_parent buf parent =
    Buffer.add_string buf "parent ";
    output_hex_commit buf parent;
    Buffer.add_char   buf lf

  let output_inflated buf t =
    let open Commit in
    Buffer.add_string    buf "tree ";
    output_hex_tree      buf t.tree;
    Buffer.add_char      buf lf;
    List.iter ~f:(output_parent buf) t.parents;
    Buffer.add_string    buf "author ";
    User.output_inflated buf t.author;
    Buffer.add_char      buf lf;
    Buffer.add_string    buf "committer ";
    User.output_inflated buf t.committer;
    Buffer.add_char      buf lf;
    Buffer.add_char      buf lf;
    Buffer.add_string    buf t.message

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

  let input_inflated buf =
    let tree      = input_key_value buf "tree" input_hex_tree in
    let parents   = input_parents   buf in
    let author    = input_key_value buf "author" User.input_inflated in
    let committer = input_key_value buf "committer" User.input_inflated in
    Mstruct.shift buf 1;
    let message   = Mstruct.to_string buf in
    { Commit.parents; message; tree; author; committer }

end

module Tree: ISERIALIZABLE with type t := tree = struct

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

  let pretty_entry e =
    let open Tree in
    Printf.sprintf
      "{\n\
      \  perm: %s\n\
      \  name: %s\n\
      \  sha1: %S\n\
       }\n"
      (pretty_perm e.perm)
      e.name
      (SHA1.to_string e.node)

  let pretty t =
    let b = Buffer.create 1024 in
    List.iter ~f:(fun e -> Buffer.add_string b (pretty_entry e)) (Tree.entries t);
    Buffer.contents b

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_entry buf e =
    let open Tree in
    Buffer.add_string buf (string_of_perm e.perm);
    Buffer.add_char   buf sp;
    Buffer.add_string buf e.name;
    Buffer.add_char   buf nul;
    output_sha1       buf e.node

  let input_entry buf =
    let perm = match Mstruct.get_string_delim buf sp with
      | None      -> Mstruct.parse_error_buf buf "invalid perm"
      | Some perm -> perm in
    let name = match Mstruct.get_string_delim buf nul with
      | None      -> Mstruct.parse_error_buf buf "invalid filename"
      | Some name -> name in
    let node = input_sha1 buf in
    let entry = {
      Tree.perm = perm_of_string buf perm;
      name; node
    } in
    Some entry

  let output_inflated buf t =
    List.iter ~f:(output_entry buf) (Tree.entries t)

  let input_inflated buf =
    let rec aux entries =
      if Mstruct.length buf <= 0 then
        Tree.create entries
      else
        match input_entry buf with
        | None   -> Tree.create entries
        | Some e -> aux (e :: entries) in
    aux []

end

module Tag: ISERIALIZABLE with type t := tag = struct

  type t = tag

  let string_of_object_type = function
    | `Blob   -> "blob"
    | `Commit -> "commit"
    | `Tag    -> "tag"
    | `Tree   -> "tree"

  let input_object_type buf =
    match Mstruct.to_string buf with
    | "blob"   -> `Blob
    | "commit" -> `Commit
    | "tag"    -> `Tag
    | "tree"   -> `Tree
    | s        -> Mstruct.parse_error_buf buf "input_object_type: %s" s

  let pretty t =
    let open Tag in
    Printf.sprintf
      "{\n\
      \  object      : %s;\n\
      \  type        : %s;\n\
      \  tag         : %S;\n\
      \  tagger      : %s;\n\
      \  tag_message : %S;\n\
       }"
      (SHA1.to_hex t.sha1)
      (string_of_object_type t.typ)
      t.tag
      (User.pretty t.tagger)
      t.message

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_inflated buf t =
    let open Tag in
    output_key_value     buf "object" (SHA1.to_hex t.sha1);
    output_key_value     buf "type"   (string_of_object_type t.typ);
    output_key_value     buf "tag"    t.tag;
    Buffer.add_string    buf "tagger ";
    User.output_inflated buf t.tagger;
    Buffer.add_char      buf lf;
    Buffer.add_char      buf lf;
    Buffer.add_string    buf t.message

  let input_inflated buf =
    let sha1   = input_key_value buf "object" input_hex_sha1 in
    let typ    = input_key_value buf "type"   input_object_type in
    let tag    = input_key_value buf "tag"    Mstruct.to_string in
    let tagger = input_key_value buf "tagger" User.input_inflated in
    Mstruct.shift buf 1;
    let message = Mstruct.to_string buf in
    { Tag.sha1; typ; tag; tagger; message }

end

module Value = struct

  module V = Value

  let pretty = function
    | V.Blob b   -> Printf.sprintf "BLOB:\n%s" (Blob.pretty b)
    | V.Commit c -> Printf.sprintf "COMMIT:\n%s" (Commit.pretty c)
    | V.Tag t    -> Printf.sprintf "TAG:\n%s" (Tag.pretty t)
    | V.Tree t   -> Printf.sprintf "TREE:\n%s" (Tree.pretty t)

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let obj_type = function
    | V.Blob _   -> "blob"
    | V.Commit _ -> "commit"
    | V.Tag _    -> "tag"
    | V.Tree _   -> "tree"

  let output_contents buf = function
    | V.Blob b   -> Blob.output_inflated buf b
    | V.Commit c -> Commit.output_inflated buf c
    | V.Tag t    -> Tag.output_inflated buf t
    | V.Tree t   -> Tree.output_inflated buf t

  let output_inflated buf t =
    let size, contents =
      let buf = Buffer.create 1024 in
      output_contents buf t;
      let size = Buffer.length buf in
      size, buf in
    Buffer.add_string buf (obj_type t);
    Buffer.add_char   buf sp;
    Buffer.add_string buf (string_of_int size);
    Buffer.add_char   buf nul;
    Buffer.add_buffer buf contents

  let output t =
    let inflated =
      let buf = Buffer.create 1024 in
      output_inflated buf t;
      Buffer.contents buf in
    let deflated = GitMisc.deflate_string inflated in
    Mstruct.of_string deflated

  let type_of_inflated buf =
    let obj_type =
      match Mstruct.get_string_delim buf sp with
      | None   -> Mstruct.parse_error_buf buf "value: type"
      | Some t -> t in
    match obj_type with
    | "blob"   -> `Blob
    | "commit" -> `Commit
    | "tag"    -> `Tag
    | "tree"   -> `Tree
    | x        -> Mstruct.parse_error_buf buf "%S is not a valid object type." x

  let input_inflated buf =
    (* XXX call zlib directly on the stream interface *)
    let obj_type = type_of_inflated buf in
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
    | `Blob   -> Blob.input_inflated buf   |> blob
    | `Commit -> Commit.input_inflated buf |> commit
    | `Tag    -> Tag.input_inflated buf    |> tag
    | `Tree   -> Tree.input_inflated buf   |> tree

  let input buf =
    input_inflated (GitMisc.inflate_mstruct buf)

end

include Value

module PackedValue (M: sig val version: int end): sig
  include ISERIALIZABLE with type t := packed_value
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
      Packed_value.Insert contents

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
      Packed_value.Copy (offset, length)

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
    { Packed_value.source; hunks; source_length; result_length }

  let input_be_modified_base_128 buf =
    let rec aux i n =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let i    = if n >= 2 then i+1 else i in
      let i    = (i lsl 7) lor (byte land 0x7f) in
      if more then aux i (n+1)
      else i in
    aux 0 1

  let input_inflated buf =
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
    | 0b001 -> with_inflated buf Commit.input_inflated |> commit |> value
    | 0b010 -> with_inflated buf Tree.input_inflated   |> tree   |> value
    | 0b011 -> with_inflated buf Blob.input_inflated   |> blob   |> value
    | 0b100 -> with_inflated buf Tag.input_inflated    |> tag    |> value
    | 0b101 -> Mstruct.parse_error_buf buf "invalid: Reserved"
    | 0b110 ->
      let base  = input_be_modified_base_128 buf in
      let hunks = with_inflated buf (input_hunks size base)in
      off_delta hunks
    | 0b111 ->
      let base  = input_sha1 buf in
      let hunks = with_inflated buf (input_hunks size base) in
      ref_delta hunks
    | _     -> assert false

  let output_inflated buf t =
    todo "packed_value"

  let pretty t =
    todo "packed_value"

  let dump t =
    Printf.eprintf (pretty t)

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
  include SERIALIZABLE with type t := pack
  val input: pack_index -> Mstruct.t -> pack
  val unpack:
    read_inflated:(sha1 -> Mstruct.t Lwt.t) ->
    write:(value -> sha1 Lwt.t) ->
    idx:int SHA1.Map.t ->
    offset:int ->
    packed_value -> sha1 Lwt.t
  val unpack_all:
    read_inflated:(sha1 -> Mstruct.t Lwt.t) ->
    write:(value -> sha1 Lwt.t) ->
    Mstruct.t -> sha1 list Lwt.t
end = struct

  open Lwt
  module P = Packed_value

  let input_header buf =
    let header = Mstruct.get_string buf 4 in
    if header <> "PACK" then
      Mstruct.parse_error_buf buf "wrong header (%s)" header;
    let version = Int32.to_int_exn (Mstruct.get_be_uint32 buf) in
    if version <> 2 && version <> 3 then
      Mstruct.parse_error_buf buf "wrong pack version (%d)" version;
    version, Int32.to_int_exn (Mstruct.get_be_uint32 buf)

  let input_packed_value version buf =
    if version = 2 then PackedValue2.input_inflated buf
    else PackedValue3.input_inflated buf

  let input idx buf =
    let version, size = input_header (Mstruct.clone buf) in
    let packs = SHA1.Table.create () in
    fun sha1 ->
      if Hashtbl.mem packs sha1 then
        Hashtbl.find_exn packs sha1
      else (
        let offset = Map.find_exn idx.offsets sha1 in
        let buf = Mstruct.clone buf in
        let buf = match Map.find_exn idx.lengths sha1 with
          | None   ->
            Mstruct.shift buf offset;
            Mstruct.sub buf 0 (Mstruct.length buf - 20)
          | Some l -> Mstruct.sub buf offset l in
        let value = input_packed_value version buf in
        Hashtbl.replace packs sha1 value;
        value
      )

  let apply_hunk source buf = function
    | P.Insert str     -> Buffer.add_string buf str
    | P.Copy (off,len) ->
      let view = Mstruct.sub (Mstruct.clone source) off len in
      let str = Mstruct.to_string view in
      Buffer.add_string buf str

  let dump_delta d =
    let open Packed_value in
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
    let source = delta.P.source in
    let kind = match Mstruct.get_string_delim source sp with
      | None   -> Mstruct.parse_error_buf source "missing kind"
      | Some k -> k in
    let size = match Mstruct.get_string_delim source nul with
      | None   -> Mstruct.parse_error_buf source "missing size"
      | Some s -> try int_of_string s with _ -> -1 in
    if size <> delta.P.source_length then
      Mstruct.parse_error_buf source
        "size differs: delta:%d source:%d\n"
        delta.P.source_length size;

    let buf = Buffer.create (20 + delta.P.result_length) in
    Buffer.add_string buf kind;
    Buffer.add_char   buf sp;
    Buffer.add_string buf (string_of_int delta.P.result_length);
    Buffer.add_char   buf nul;
    List.iter ~f:(apply_hunk delta.P.source buf) delta.P.hunks;

    let str = Buffer.contents buf in
    let buf = Mstruct.of_string str in
    Value.input_inflated buf

  let unpack ~read_inflated ~write ~idx ~offset packed_value =
    begin match packed_value with
      | P.Value v     -> return v
      | P.Ref_delta d ->
        read_inflated d.P.source >>= fun source ->
        let value = apply_delta { d with P.source } in
        return value
      | P.Off_delta d ->
        let offset = offset - d.P.source in
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
        read_inflated base >>= fun source ->
        let value = apply_delta { d with P.source } in
        return value
    end >>=
    write

  let unpack_all ~read_inflated ~write buf =
    let version, size = input_header buf in
    Log.debugf "unpack-all: version=%d size=%d\n%!" version size;

    let next_packed_value () =
      input_packed_value version buf in

    let idx = ref SHA1.Map.empty in
    let rec loop i =
      Printf.printf "\rReceiving objects: %3d%% (%d/%d)%!" (i*100/size) (i+1) size;
      let offset = Mstruct.offset buf in
      let packed_value = next_packed_value () in
      unpack ~read_inflated ~write ~idx:!idx ~offset packed_value >>= fun sha1 ->
      idx := Map.add !idx ~key:sha1 ~data:offset;
      if i >= size - 1 then return_unit
      else loop (i+1) in
    loop 0 >>= fun () ->
    Printf.printf "\rReceiving objects: 100%% (%d/%d), done.\n%!" size size;
    return (Map.keys !idx)

  let output _ =
    todo "pack"

  let pretty _ =
    todo "pack"

  let dump t =
    Printf.eprintf "%s" (pretty t)

end

module Idx: SERIALIZABLE with type t := pack_index = struct

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
      let a = Array.create nb_objects (SHA1.of_string "") in
      for i=0 to nb_objects-1 do
        a.(i) <- input_sha1 buf;
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

    let _sha1 = input_sha1 buf in
    let _checksum = input_sha1 buf in

    let offsets_alist = Array.to_list idx in
    let offsets = SHA1.Map.of_alist_exn offsets_alist in
    let lengths = SHA1.Map.of_alist_exn (lengths offsets_alist) in
    { offsets; lengths }

  let output buf =
    todo "pack-index"

  let pretty _ =
    todo "pack-index"

  let dump t =
    Printf.eprintf "%s" (pretty t)

end

open Lwt

(* XXX: not tail-rec *)
let rec find ~succ sha1 path =
  match path with
  | []   -> return (Some sha1)
  | h::t ->
    succ sha1 >>= fun succs ->
    Lwt_list.fold_left_s (fun acc s ->
        match (acc, s) with
        | Some _, _            -> return acc
        | _     , `Commit _    -> return acc
        | _     , `Tag (l, s)
        | _     , `Tree (l, s) ->
          if l=h then
            find ~succ s t >>= function
            | None   -> return_none
            | Some f -> return (Some f)
          else
            return acc
      ) None succs

let find_exn ~succ sha1 path =
  find succ sha1 path >>= function
  | None   -> fail Not_found
  | Some x -> return x

let mem ~succ sha1 path =
  find succ sha1 path >>= function
  | None   -> return false
  | Some _ -> return true
