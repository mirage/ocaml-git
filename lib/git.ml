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
  Bigbuffer.add_string buf hex

let output_hex_commit buf hex =
  output_hex buf (SHA1.Commit.to_hex hex)

let output_hex_tree buf hex =
  output_hex buf (SHA1.Tree.to_hex hex)

let input_sha1 buf =
  Mstruct.get_string buf 20
  |> SHA1.of_string

let output_sha1 buf t =
  Bigbuffer.add_string buf (SHA1.to_string t)

let output_key_value buf k v =
  Bigbuffer.add_string buf k;
  Bigbuffer.add_char   buf sp;
  Bigbuffer.add_string buf v;
  Bigbuffer.add_char   buf lf

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
  val output_inflated: Bigbuffer.t -> t -> unit
  val input_inflated: Mstruct.t -> t
end

module type SERIALIZABLE = sig
  type t
  val pretty: t -> string
  val dump: t -> unit
  val output: t -> Cstruct.buffer list
  val input: Mstruct.t -> t
end

module User: sig
  include ISERIALIZABLE with type t = User.t
  val pretty: User.t -> string
end = struct

  type t = User.t

  (* XXX needs to escape name/email/date *)
  let output_inflated buf t =
    let open User in
    Bigbuffer.add_string buf t.name ;
    Bigbuffer.add_string buf " <"   ;
    Bigbuffer.add_string buf t.email;
    Bigbuffer.add_string buf "> "   ;
    Bigbuffer.add_string buf t.date

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
      "[name:%S email:%S date:%S]"
      t.name t.email t.date

  let dump t =
    Printf.eprintf "%s\n" (pretty t)

end

module Blob = struct

  type t = Blob.t

  let pretty t =
    Printf.sprintf "%S" (Blob.to_string t)

  let dump t =
    Printf.eprintf "%s\n" (pretty t)

  let input_inflated buf =
    Mstruct.get_string buf (Mstruct.length buf)
    |> Blob.of_string

  let output_inflated buf t =
    Bigbuffer.add_string buf (Blob.to_string t)

end

module Commit = struct

  type t = commit

  let pretty t =
    let open Commit in
    Printf.sprintf
      "tree     : %s\n\
       parents  : %s\n\
       author   : %s\n\
       committer: %s\n\n\
       %s\n"
      (SHA1.Tree.to_hex t.tree)
      (String.concat ~sep:", " (List.map ~f:SHA1.Commit.to_hex t.parents))
      (User.pretty t.author)
      (User.pretty t.committer)
      (String.strip t.message)

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_parent buf parent =
    Bigbuffer.add_string buf "parent ";
    output_hex_commit buf parent;
    Bigbuffer.add_char   buf lf

  let output_inflated buf t =
    let open Commit in
    Bigbuffer.add_string    buf "tree ";
    output_hex_tree      buf t.tree;
    Bigbuffer.add_char      buf lf;
    List.iter ~f:(output_parent buf) t.parents;
    Bigbuffer.add_string    buf "author ";
    User.output_inflated buf t.author;
    Bigbuffer.add_char      buf lf;
    Bigbuffer.add_string    buf "committer ";
    User.output_inflated buf t.committer;
    Bigbuffer.add_char      buf lf;
    Bigbuffer.add_char      buf lf;
    Bigbuffer.add_string    buf t.message

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

module Tree = struct

  type t = tree

  let pretty_perm = function
    | `Normal -> "normal"
    | `Exec   -> "exec"
    | `Link   -> "link"
    | `Dir    -> "dir"

  let perm_of_string buf = function
    | "44"
    | "100644" -> `Normal
    | "100755" -> `Exec
    | "120000" -> `Link
    | "40000"  -> `Dir
    | x        -> Mstruct.parse_error_buf buf "%S is not a valid permission." x

  let string_of_perm = function
    | `Normal -> "100644"
    | `Exec   -> "100755"
    | `Link   -> "120000"
    | `Dir    -> "40000"

  let pretty_entry e =
    let open Tree in
    Printf.sprintf "%s %s    %s\n"
      (pretty_perm e.perm)
      (SHA1.to_hex e.node)
      e.name

  let pretty t =
    let b = Buffer.create 1024 in
    List.iter ~f:(fun e -> Buffer.add_string b (pretty_entry e)) (Tree.entries t);
    Buffer.contents b

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_entry buf e =
    let open Tree in
    Bigbuffer.add_string buf (string_of_perm e.perm);
    Bigbuffer.add_char   buf sp;
    Bigbuffer.add_string buf e.name;
    Bigbuffer.add_char   buf nul;
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
        Tree.create (List.rev entries)
      else
        match input_entry buf with
        | None   -> Tree.create (List.rev entries)
        | Some e -> aux (e :: entries) in
    aux []

end

let string_of_object_type = function
  | `Blob   -> "blob"
  | `Commit -> "commit"
  | `Tag    -> "tag"
  | `Tree   -> "tree"

let object_type_of_string = function
  | "blob"   -> Some `Blob
  | "commit" -> Some `Commit
  | "tag"    -> Some `Tag
  | "tree"   -> Some `Tree
  | _        -> None

module Tag = struct

  type t = tag

  let input_object_type buf =
    let s = Mstruct.to_string buf in
    match object_type_of_string s with
    | Some k -> k
    | None   -> Mstruct.parse_error_buf buf "input_object_type: %s" s

  let pretty t =
    let open Tag in
    Printf.sprintf
      "object: %s\n\
       type  : %s\n\
       tag   : %S\n\
       tagger: %s\n\n\
       %s\n"
      (SHA1.to_hex t.sha1)
      (string_of_object_type t.typ)
      t.tag
      (User.pretty t.tagger)
      (String.strip t.message)

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let output_inflated buf t =
    let open Tag in
    output_key_value     buf "object" (SHA1.to_hex t.sha1);
    output_key_value     buf "type"   (string_of_object_type t.typ);
    output_key_value     buf "tag"    t.tag;
    Bigbuffer.add_string    buf "tagger ";
    User.output_inflated buf t.tagger;
    Bigbuffer.add_char      buf lf;
    Bigbuffer.add_char      buf lf;
    Bigbuffer.add_string    buf t.message

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

  type t = value

  module V = Value

  let pretty = function
    | V.Blob b   -> Printf.sprintf "== Blob ==\n%s" (Blob.pretty b)
    | V.Commit c -> Printf.sprintf "== Commit ==\n%s" (Commit.pretty c)
    | V.Tag t    -> Printf.sprintf "== Tag ==\n%s" (Tag.pretty t)
    | V.Tree t   -> Printf.sprintf "== Tree ==\n%s" (Tree.pretty t)

  let dump t =
    Printf.eprintf "%s" (pretty t)

  let type_of = function
    | V.Blob _   -> `Blob
    | V.Commit _ -> `Commit
    | V.Tag _    -> `Tag
    | V.Tree _   -> `Tree

  let output_contents buf = function
    | V.Blob b   -> Blob.output_inflated buf b
    | V.Commit c -> Commit.output_inflated buf c
    | V.Tag t    -> Tag.output_inflated buf t
    | V.Tree t   -> Tree.output_inflated buf t

  let add_header typ contents =
    let size = Bigstring.length contents in
    let buf = Bigbuffer.create size in
    Bigbuffer.add_string buf (string_of_object_type typ);
    Bigbuffer.add_char   buf sp;
    Bigbuffer.add_string buf (string_of_int size);
    Bigbuffer.add_char   buf nul;
    Bigbuffer.add_string buf (Bigstring.to_string contents);
    GitMisc.buffer_contents buf

  let output_inflated t =
    let contents = Bigbuffer.create 1024 in
    output_contents contents t;
    add_header (type_of t) (GitMisc.buffer_contents contents)

  let sha1 t =
    SHA1.create (output_inflated t)

  let output t =
    let inflated = output_inflated t in
    let deflated = GitMisc.deflate_bigstring inflated in
    [ deflated ]

  let type_of_inflated buf =
    let obj_type =
      match Mstruct.get_string_delim buf sp with
      | None   -> Mstruct.parse_error_buf buf "value: type"
      | Some t -> t in
    match object_type_of_string obj_type with
    | Some t -> t
    | None   -> Mstruct.parse_error_buf buf "%S is not a valid object type." obj_type

  let input_inflated buf =
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

module PackedValue (M: sig val version: int end) = struct

  type t = packed_value

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

  let output_hunk buf = function
    | Packed_value.Insert contents ->
      let len = String.length contents in
      if len > 64 then
        failwith "invalid hunk: insert too large";
      Bigbuffer.add_char buf (Char.of_int_exn (String.length contents));
      Bigbuffer.add_string buf contents
    | Packed_value.Copy (offset, length) ->
      let length = if length = 0x10000 then 0 else length in
      let bit n shift =
        if (n lsr shift) land 0xFF = 0 then 0
        else (
          Bigbuffer.add_char buf (Char.of_int_exn n);
          1
        ) in
      let o0 = bit offset 0 in
      let o1 = bit offset 8 in
      let o2 = bit offset 16 in
      let o3 = bit offset 24 in
      let l0 = bit length 0 in
      let l1 = bit length 8 in
      let l2 = bit length 16 in
      let n =
        o0 + (o1 lsl 1) + (o2 lsl 2) + (o3 lsl 3)
        + (l0 lsl 4) + (l1 lsl 5) + (l2 lsl 6) in
      Bigbuffer.add_char buf (Char.of_int_exn n)

  let input_le_base_128 buf =
    let rec aux int shift =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let base = byte land 0x7f in
      let int  = (base lsl shift) lor int in
      if more then aux int (shift+7)
      else int in
    aux 0 0

  let output_le_base_128 buf int =
    let rec loop i =
      if i <> 0 then
        let more =
          if i < 0x80 then 0
          else 0x80 in
        let byte = more lor (i land 0x7f) in
        Bigbuffer.add_char buf (Char.of_int_exn byte);
        loop (i lsr 7) in
    loop int

  let input_hunks size source buf =
    let source_length = input_le_base_128 buf in
    let result_length = input_le_base_128 buf in
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else aux (input_hunk source_length buf :: acc) in
    let hunks = aux [] in
    { Packed_value.source; hunks; source_length; result_length }

  let output_hunks buf t =
    let open Packed_value in
    output_le_base_128 buf t.source_length;
    output_le_base_128 buf t.result_length;
    List.iter ~f:(output_hunk buf) t.hunks

  let input_be_modified_base_128 buf =
    let rec aux i first =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let i    = if first then i else i+1 in
      let i    = (i lsl 7) lor (byte land 0x7f) in
      if more then aux i false
      else i in
    aux 0 true

  let output_be_modified_base_128 buf int =
    let rec loop i first =
      if i <> 0 then
        let more =
          if i < 0x80 then 0
          else 0x80 in
        let i = if first then i else i-1 in
        let byte = more lor (i land 0x7f) in
        Bigbuffer.add_char buf (Char.of_int_exn byte);
        loop (i lsr 7) false in
    loop int true

  let with_inflated buf fn =
    fn (GitMisc.inflate_mstruct buf)

  let with_inflated_buf buf fn =
    with_inflated buf (fun buf ->
        let contents = Mstruct.to_bigarray buf in
        fn contents
      )

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

    let mk typ buffer =
      let buffer = add_header typ buffer in
      Packed_value.Raw_value buffer in

    match kind with
    | 0b000 -> Mstruct.parse_error_buf buf "invalid: Reserved"
    | 0b001 -> with_inflated_buf buf (mk `Commit)
    | 0b010 -> with_inflated_buf buf (mk `Tree)
    | 0b011 -> with_inflated_buf buf (mk `Blob)
    | 0b100 -> with_inflated_buf buf (mk `Tag)
    | 0b101 -> Mstruct.parse_error_buf buf "invalid: Reserved"
    | 0b110 ->
      let base  = input_be_modified_base_128 buf in
      let hunks = with_inflated buf (input_hunks size base) in
      Packed_value.Off_delta hunks
    | 0b111 ->
      let base  = input_sha1 buf in
      let hunks = with_inflated buf (input_hunks size base) in
      Packed_value.Ref_delta hunks
    | _     -> assert false

  let output_inflated buf t =
    failwith "TODO"

  let pretty t =
    Packed_value.to_string t

  let dump t =
    Printf.eprintf "%s" (pretty t)

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

module Pack = struct

  module Log = Log.Make(struct let section = "pack" end)

  type t = pack

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

  let input buf =
    let version, count = input_header buf in
    let values = ref [] in
    for i = 0 to count - 1 do
      let v = input_packed_value version buf in
      values := v :: !values
    done;
    !values

  let read_packed_value buf { Pack_index.offsets; lengths } =
    let buf = Mstruct.of_bigarray buf in
    let version, size = input_header (Mstruct.clone buf) in
    let packs = SHA1.Table.create () in
    fun sha1 ->
      if Hashtbl.mem packs sha1 then
        Hashtbl.find_exn packs sha1
      else (
        let offset = Map.find_exn offsets sha1 in
        let buf = Mstruct.clone buf in
        let buf = match Map.find_exn lengths sha1 with
          | None   ->
            Mstruct.shift buf offset;
            Mstruct.sub buf 0 (Mstruct.length buf - 20)
          | Some l -> Mstruct.sub buf offset l in
        let value = input_packed_value version buf in
        Hashtbl.replace packs sha1 value;
        value
      )

  let apply_hunk source buf = function
    | P.Insert str     -> Bigbuffer.add_string buf str
    | P.Copy (off,len) ->
      let view = Mstruct.sub source off len in
      let str = Mstruct.to_string view in
      Bigbuffer.add_string buf str

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

  let apply_delta (delta: Bigstring.t Packed_value.delta) =
    let source = Mstruct.of_bigarray delta.P.source in
    let kind = match Mstruct.get_string_delim source sp with
      | None   -> Mstruct.parse_error_buf source "missing kind"
      | Some k -> match object_type_of_string k with
        | None   -> Mstruct.parse_error_buf source "wrong_kind: %s" k
        | Some k -> k in
    let size = match Mstruct.get_string_delim source nul with
      | None   -> Mstruct.parse_error_buf source "missing size"
      | Some s ->
        try int_of_string s
        with Failure "int_of_string" ->
          eprintf "Pack.apply_delta: %s is not a valid size (source: %S)."
            s (Bigstring.to_string (Mstruct.to_bigarray source));
          failwith "Pack.apply_delta" in
    if size <> delta.P.source_length then
      Mstruct.parse_error_buf source
        "size differs: delta:%d source:%d\n"
        delta.P.source_length size;
    let buf = Bigbuffer.create (20 + delta.P.result_length) in
    Bigbuffer.add_string buf (string_of_object_type kind);
    Bigbuffer.add_char   buf sp;
    Bigbuffer.add_string buf (string_of_int delta.P.result_length);
    Bigbuffer.add_char   buf nul;
    List.iter ~f:(apply_hunk source buf) delta.P.hunks;
    GitMisc.buffer_contents buf

  let rev_assoc map d =
    let r = ref None in
    try
      Map.iter
        ~f:(fun ~key ~data -> if data=d then (r := Some key; raise Exit))
        map;
      None
    with Exit ->
      !r

  let unpack_inflated_aux (return, bind) ~read_inflated ~index ~offset = function
    | P.Raw_value x -> return x
    | P.Ref_delta d ->
      bind
        (read_inflated d.P.source)
        (fun source ->
           return (apply_delta { d with P.source }))
    | P.Off_delta d ->
      let offset = offset - d.P.source in
      let base =
        match rev_assoc index.Pack_index.offsets offset with
        | Some k -> k
        | None   ->
          let msg = Printf.sprintf
              "inflate: cannot find any object starting at offset %d"
              offset in
          failwith msg in
      bind
        (read_inflated base)
        (fun source ->
           return (apply_delta { d with P.source }))

  let lwt_monad = (Lwt.return, Lwt.bind)

  let unpack_inflated = unpack_inflated_aux lwt_monad

  let unpack ~read_inflated ~index ~offset packed_value =
    unpack_inflated ~read_inflated ~index ~offset packed_value >>= fun buf ->
    let buf = Mstruct.of_bigarray buf in
    let value = input_inflated buf in
    return value

  let index_aux (return, bind) ~compute_sha1 buf =
    let pack_checksum = SHA1.create buf in
    let buf = Mstruct.of_bigarray buf in
    let version, size = input_header buf in
    Log.debugf "index: version=%d size=%d" version size;
    let empty = Pack_index.empty pack_checksum in
    if size <= 0 then
      return empty
    else (
      let next_packed_value () =
        input_packed_value version buf in
      let rec loop index i =
        let offset = Mstruct.offset buf in
        let packed_value = next_packed_value () in
        let length = Mstruct.offset buf - offset in
        bind
          (compute_sha1 ~size ~index ~offset packed_value)
          (fun sha1 ->
             let index = Pack_index.({
                 offsets = SHA1.Map.add index.offsets ~key:sha1 ~data:offset;
                 lengths = SHA1.Map.add index.lengths ~key:sha1 ~data:(Some length);
                 pack_checksum;
               }) in
             if i >= size - 1 then return index
             else loop index (i+1))
      in
      loop empty 0
    )

  let index = index_aux lwt_monad

  let unpack_and_write ~read_inflated ~write ~index ~offset packed_value =
    unpack ~read_inflated ~index ~offset packed_value >>=
    write

  let unpack_all ~read_inflated ~write buf =
    let i = ref 0 in
    let compute_sha1 ~size ~index ~offset packed_value =
      Printf.printf "\rUnpacking objects: %3d%% (%d/%d)%!" (!i*100/size) (!i+1) size;
      incr i;
      unpack_and_write ~read_inflated ~write ~index ~offset packed_value in
    index ~compute_sha1 buf >>= fun index ->
    Printf.printf "\rUnpacking objects: 100%% (%d/%d), done.\n%!" !i !i;
    return index


  let output _ =
    todo "pack"

  let pretty _ =
    todo "pack"

  let dump t =
    Printf.eprintf "%s" (pretty t)

end

module Pack_index = struct

  open Lwt

  type t = pack_index

  let id_monad =
    (fun x -> x), (fun x f -> f x)

  let of_raw_pack buf =
    let buffers = SHA1.Table.create () in
    let read_inflated sha1 =
      try Hashtbl.find_exn buffers sha1
      with Not_found ->
        eprintf "Pack_index.of_pack: %s not found" (SHA1.to_hex sha1);
        failwith "Pack_index.of_pack" in
    let write buffer =
      let sha1 = SHA1.create buffer in
      Hashtbl.add_exn buffers sha1 buffer;
      sha1 in
    let compute_sha1 ~size:_ ~index ~offset packed_value =
      let buf = Pack.unpack_inflated_aux id_monad ~read_inflated ~index ~offset packed_value in
      write buf
    in
    Pack.index_aux id_monad ~compute_sha1 buf

  let of_pack pack =
    (* XXX: could do beter *)
    let buf = Pack.output pack in
    of_raw_pack buf

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

    let pack_checksum = input_sha1 buf in
    let _checksum = input_sha1 buf in

    let offsets_alist = Array.to_list idx in
    let offsets = SHA1.Map.of_alist_exn offsets_alist in
    let lengths = SHA1.Map.of_alist_exn (lengths offsets_alist) in
    { Pack_index.offsets; lengths; pack_checksum }

  let str_buffer = String.create 4
  let add_be_uint32 buf i =
    EndianString.BigEndian.set_int32 str_buffer 0 i;
    Bigbuffer.add_string buf str_buffer

  let output t =
    let buf = Bigbuffer.create 1024 in
    Bigbuffer.add_string buf "\255tOc";
    add_be_uint32 buf 2l;

    let offsets = Map.to_alist t.Pack_index.offsets in
    let offsets = List.sort ~cmp:(fun (k1,_) (k2,_) -> SHA1.compare k1 k2) offsets in

    let fanout = Array.create 256 0l in
    List.iter ~f:(fun (key, _) ->
        let str = SHA1.to_string key in
        let n = Char.to_int str.[0] in
        for i = n to 255 do
          fanout.(i) <- Int32.succ fanout.(i)
        done;
      ) offsets;
    Array.iter ~f:(add_be_uint32 buf) fanout;

    List.iter ~f:(fun (key, offset) ->
        add_be_uint32 buf (Int32.of_int_exn offset);
        Bigbuffer.add_string buf (SHA1.to_string key)
      ) offsets;

    output_sha1 buf t.Pack_index.pack_checksum;

    (* XXX: slow *)
    let str = GitMisc.buffer_contents buf in
    let checksum = SHA1.create str in

    Bigbuffer.add_string buf (SHA1.to_string checksum);
    [ GitMisc.buffer_contents buf ]

  let pretty t =
    Sexp.to_string_hum (Pack_index.sexp_of_t t)

  let dump t =
    Printf.eprintf "%s" (pretty t)

end

module Cache = struct

  module Log = Log.Make(struct let section = "cache" end)

  type t = cache

  module Entry = struct

    open Cache.Entry

    let pretty t =
      Printf.sprintf
        "%s\n\
        \  ctime: %ld:%ld\n\
        \  mtime: %ld:%ld\n\
        \  dev: %ld\tino: %ld\n\
        \  uid: %ld\tgid: %ld\n\
        \  size: %ld\tflags: %d\n"
        t.name
        t.stats.ctime.lsb32 t.stats.ctime.nsec
        t.stats.mtime.lsb32 t.stats.mtime.nsec
        t.stats.dev t.stats.inode
        t.stats.uid t.stats.gid
        t.stats.size t.stage

    let dump t =
      Sexp.pp_hum Format.err_formatter (Cache.Entry.sexp_of_t t)

    let input_time buf =
      let lsb32 = Mstruct.get_be_uint32 buf in
      let nsec = Mstruct.get_be_uint32 buf in
      { Cache.Entry.lsb32; nsec }

    let output_time buf t =
      Mstruct.set_be_uint32 buf t.lsb32;
      Mstruct.set_be_uint32 buf t.nsec

    let input_mode buf =
      let _zero = Mstruct.get_be_uint16 buf in
      (* XX: check that _zero is full of 0s *)
      let n = Mstruct.get_be_uint16 buf in
      match Int32.(n lsr 12) with
      | 0b1010 -> `Link
      | 0b1110 -> `Gitlink
      | 0b1000 ->
        begin match Int32.(n land 0x1FF) with
          | 0o755 -> `Exec
          | 0o644 -> `Normal
          | d     -> Mstruct.parse_error_buf buf "mode: invalid permission (%d)" d
        end
      | m -> Mstruct.parse_error_buf buf "mode: invalid (%d)" m

    let output_mode buf t =
      let n = match t with
        | `Exec    -> 0o1000__000__111_101_101
        | `Normal  -> 0b1000__000__110_100_100
        | `Link    -> 0b1010__000__000_000_000
        | `Gitlink -> 0b1110__000__000_000_000 in
    Mstruct.set_be_uint16 buf 0;
    Mstruct.set_be_uint16 buf n


    let input_stat_info buf =
      let ctime = input_time buf in
      let mtime = input_time buf in
      let dev = Mstruct.get_be_uint32 buf in
      let inode = Mstruct.get_be_uint32 buf in
      let mode = input_mode buf in
      let uid = Mstruct.get_be_uint32 buf in
      let gid = Mstruct.get_be_uint32 buf in
      let size = Mstruct.get_be_uint32 buf in
      { mtime; ctime; dev; inode; mode; uid; gid; size }

    let output_stat_info buf t =
      output_time buf t.ctime;
      output_time buf t.mtime;
      let uint32 = Mstruct.set_be_uint32 buf in
      uint32 t.dev;
      uint32 t.inode;
      output_mode buf t.mode;
      uint32 t.uid;
      uint32 t.gid;
      uint32 t.size

    let input buf =
      Log.debugf "Cache.Entry.input";
      let offset0 = Mstruct.offset buf in
      let stats = input_stat_info buf in
      let id = input_sha1 buf in
      let stage, len =
        let i = Mstruct.get_be_uint16 buf in
        (i land 0x3000) lsr 12,
        (i land 0x0FFF) in
      Log.debugf "stage:%d len:%d" stage len;
      let name = Mstruct.get_string buf len in
      Mstruct.shift buf 1;
      let bytes = Mstruct.offset buf - offset0 in
      let padding = match bytes mod 8 with
        | 0 -> 0
        | n -> 8-n in
      Mstruct.shift buf padding;
      Log.debugf "name: %s id: %s bytes:%d padding:%d"
        name (SHA1.to_hex id) bytes padding;
      { stats; id; stage; name }

    let output t =
      let len = 63 + String.length t.name in
      let pad = match len mod 8 with
        | 0 -> 0
        | n -> 8-n in
      let mstr = Mstruct.create (len+pad) in
      output_stat_info mstr t.stats;
      Mstruct.set_string mstr (SHA1.to_string t.id);
      let flags = (t.stage lsl 12 + String.length t.name) land 0x3FFF in
      Mstruct.set_be_uint16 mstr flags;
      Mstruct.set_string mstr t.name;
      Mstruct.set_string mstr (String.make (1+pad) '\x00');
      Mstruct.to_bigarray mstr

  end

  let pretty t =
    let buf = Buffer.create 1024 in
    List.iter ~f:(fun e ->
        Buffer.add_string buf (Entry.pretty e)
      ) t.Cache.entries;
    Buffer.contents buf

  let dump t =
    Sexp.pp_hum Format.err_formatter (Cache.sexp_of_t t)

  let input_extensions buf =
    (* TODO: actually read the extension contents *)
    []

  let input buf =
    let header = Mstruct.get_string buf 4 in
    if header <> "DIRC" then
      Mstruct.parse_error_buf buf "%s: wrong cache header." header;
    let version = Mstruct.get_be_uint32 buf in
    if version <> 2l then
      failwith (Printf.sprintf "Only cache version 2 is supported (%ld)" version);
    let n = Mstruct.get_be_uint32 buf in
    Log.debugf "%ld entries" n;
    let entries =
      let rec loop acc n =
        if Int32.(n = 0l) then List.rev acc
        else
          let entry = Entry.input buf in
          loop (entry :: acc) Int32.(n - 1l) in
      loop [] n in
    let extensions = input_extensions buf in
    (* XXX: verify checksum *)
    let _checksum = input_sha1 buf in

    (* TODO: verify the checksum *)
    { Cache.entries; extensions }

  let output t =
    let header = Mstruct.create 12 in
    Mstruct.set_string header "DIRC";
    Mstruct.set_be_uint32 header 2l;
    let entries = t.Cache.entries in
    Mstruct.set_be_uint32 header (Int32.of_int_exn (List.length entries));
    let header = Mstruct.to_bigarray header in

    let body = List.map ~f:Entry.output entries in
    let hash = Cryptokit.Hash.sha1 () in
    (* THOMAS: the Git doc are wrong here ... *)
    hash#add_string (Bigstring.to_string header);
    List.iter body (fun ba ->
        (* XXX: avoid copy to tmp string *)
        hash#add_string (Bigstring.to_string ba)
      );
    let footer = Bigstring.of_string hash#result in
    header :: body @ [footer]

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
