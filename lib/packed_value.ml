(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf

module Log = Log.Make(struct let section = "packed-value" end)

type copy = {
  offset: int;
  length: int;
}

let pretty_copy t =
  sprintf "off:%d len:%d" t.offset t.length

type hunk =
  | Insert of string
  | Copy of copy

let pretty_hunk buf = function
  | Insert str -> bprintf buf " - INSERT %S\n" str
  | Copy copy  -> bprintf buf " - COPY   [%s]\n" (pretty_copy copy)

type 'a delta = {
  source: 'a;
  source_length: int;
  result_length: int;
  hunks: hunk list;
}

let pretty_delta d =
  let buf = Buffer.create 128 in
  bprintf buf
    "source-length: %d\n\
     result-length: %d\n"
    d.source_length
    d.result_length;
  List.iter (pretty_hunk buf) d.hunks;
  Buffer.contents buf

type t =
  | Raw_value of string
  | Ref_delta of SHA.t delta
  | Off_delta of int delta

let hash = Hashtbl.hash
let equal = (=)
let compare = compare

let pretty = function
  | Raw_value s -> sprintf "%S\n" s
  | Ref_delta d -> sprintf "source:%s\n%s" (SHA.to_hex d.source) (pretty_delta d)
  | Off_delta d -> sprintf "source:%d\n%s" d.source (pretty_delta d)

let result_length = function
  | Ref_delta { result_length; _ }
  | Off_delta { result_length; _ } -> result_length
  | Raw_value str -> String.length str

let source_length = function
  | Ref_delta { source_length; _ }
  | Off_delta { source_length; _ } -> source_length
  | Raw_value str -> String.length str

let add_hunk buf ~source ~pos = function
  | Insert str -> Buffer.add_string buf str
  | Copy copy  -> Buffer.add_substring buf source (pos+copy.offset) copy.length

let add_delta buf delta =
  let source = Mstruct.of_string delta.source in
  let object_type = Object_type.input source in
  let size = match Mstruct.get_string_delim source Misc.nul with
    | None   -> Mstruct.parse_error_buf source "missing size"
    | Some s ->
      try int_of_string s
      with Failure "int_of_string" ->
        eprintf "Packed_value.add_delta: %s is not a valid size.\n" s;
        failwith "Packed_value.add_delta" in
  if size <> delta.source_length then
    Mstruct.parse_error_buf source "size differs: delta:%d source:%d\n"
      delta.source_length size;
  Buffer.add_string buf (Object_type.to_string object_type);
  Buffer.add_char   buf Misc.sp;
  Buffer.add_string buf (string_of_int delta.result_length);
  Buffer.add_char   buf Misc.nul;
  let pos = Mstruct.offset source in
  List.iter (add_hunk buf ~source:delta.source ~pos) delta.hunks

module Make (M: sig val version: int end) = struct

  let compare = compare
  let hash = hash
  let equal = equal

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
        if length = 0 then 0x10_000 else length in
      if offset+length > source_length then
        Mstruct.parse_error_buf buf
          "wrong insert hunk (offset:%d length:%d source:%d)"
          offset length source_length;
      Copy { offset; length }

  let add_hunk buf = function
    | Insert contents ->
      let len = String.length contents in
      if len >= 0x80 then
        failwith ("invalid hunk: insert too large: " ^ string_of_int len);
      Buffer.add_char buf (Char.chr (String.length contents));
      Buffer.add_string buf contents
    | Copy { offset; length } ->
      let length = if length = 0x10_000 then 0 else length in
      let bits = ref [] in
      let bit n shift =
        match (n lsr shift) land 0xFF with
        | 0 -> 0
        | n -> bits := Char.chr n :: !bits; 1
      in
      let o0 = bit offset 0 in
      let o1 = bit offset 8 in
      let o2 = bit offset 16 in
      let o3 = bit offset 24 in
      let l0 = bit length 0 in
      let l1 = bit length 8 in
      let l2 = bit length 16 in
      if l2 <> 0 && M.version = 2 then
        failwith "pack version 2 does not support copy hunks of size greater than 64K.";
      let n =
        0x80
        + o0 + (o1 lsl 1) + (o2 lsl 2) + (o3 lsl 3)
        + (l0 lsl 4) + (l1 lsl 5) + (l2 lsl 6) in
      Buffer.add_char buf (Char.chr n);
      List.iter (Buffer.add_char buf) (List.rev !bits)

  let input_le_base_128 buf =
    let rec aux int shift =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let base = byte land 0x7f in
      let int  = (base lsl shift) lor int in
      if more then aux int (shift+7)
      else int in
    aux 0 0

  let add_le_base_128 buf int =
    let bytes = ref [] in
    let rec aux i =
      let more = if i < 0x80 then 0 else 0x80 in
      let byte = more lor (i land 0x7f) in
      bytes := Char.chr byte :: !bytes;
      if i >= 0x80 then aux (i lsr 7) in
    aux int;
    List.iter (Buffer.add_char buf) (List.rev !bytes)

  let input_hunks source buf =
    let source_length = input_le_base_128 buf in
    let result_length = input_le_base_128 buf in
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else aux (input_hunk source_length buf :: acc) in
    let hunks = aux [] in
    { source; hunks; source_length; result_length }

  let add_hunks buf t =
    let { source_length; result_length; hunks; _ } = t in
    add_le_base_128 buf source_length;
    add_le_base_128 buf result_length;
    List.iter (add_hunk buf) hunks

  let input_be_modified_base_128 buf =
    let rec aux i first =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let i    = if first then i else i+1 in
      let i    = (i lsl 7) lor (byte land 0x7f) in
      if more then aux i false
      else i in
    aux 0 true

  let add_be_modified_base_128 buf int =
    let bytes = ref [] in
    let rec aux i first =
      if i = 0 then ()
      else
        let more = if first then 0 else 0x80 in
        let i    = if first then i else i-1 in
        let byte = more lor (i land 0x7f) in
        bytes := (Char.chr byte) :: !bytes;
        if i > 0x80 then aux (i lsr 7) false in
    aux int true;
    List.iter (Buffer.add_char buf) !bytes

  let with_inflated buf size fn =
    let buf = Misc.inflate_mstruct ~output_size:size buf in
    let len = Mstruct.length buf in
    if len <> size then (
      eprintf "Packed_value.with_inflated: inflated size differs. Expecting %d, got %d.\n"
        size len;
      failwith "Packed_value.with_inflated"
    );
    fn buf

  let with_inflated_buf buf size fn =
    with_inflated buf size (fun buf ->
        let contents = Mstruct.to_cstruct buf in
        fn contents
      )

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

    let mk typ str =
      let size = Cstruct.len str in
      let buf = Misc.with_buffer (fun buf ->
          Value.add_header buf typ size;
          Buffer.add_string buf (Cstruct.to_string str)
        ) in
      Raw_value buf in

    match kind with
    | 0b000 -> Mstruct.parse_error "invalid: 0 is reserved"
    | 0b001 -> with_inflated_buf buf size (mk Object_type.Commit)
    | 0b010 -> with_inflated_buf buf size (mk Object_type.Tree)
    | 0b011 -> with_inflated_buf buf size (mk Object_type.Blob)
    | 0b100 -> with_inflated_buf buf size (mk Object_type.Tag)
    | 0b101 -> Mstruct.parse_error "invalid: 5 is reserved"
    | 0b110 ->
      let base  = input_be_modified_base_128 buf in
      let hunks = with_inflated buf size (input_hunks base) in
      Off_delta hunks
    | 0b111 ->
      let base  = SHA.input buf in
      let hunks = with_inflated buf size (input_hunks base) in
      Ref_delta hunks
    | _     -> assert false

  let inflated_buffer = Buffer.create 1024
  let with_deflated buf fn =
    Buffer.reset inflated_buffer;
    fn inflated_buffer;
    let inflated = Buffer.contents inflated_buffer in
    let deflated = Misc.deflate_cstruct (Cstruct.of_string inflated) in
    Buffer.add_string buf (Cstruct.to_string deflated);
    String.length inflated

  let tmp_buffer = Buffer.create 1024

  let add buf t =
    Buffer.reset tmp_buffer;
    let add_deflated_hunks buf hunks =
      with_deflated buf (fun b -> add_hunks b hunks) in
    let size = match t with
      | Raw_value str ->
        begin
          try
            let i = String.index str Misc.nul in
            let s = String.sub str (i+1) (String.length str-i-1) in
            with_deflated tmp_buffer (fun b -> Buffer.add_string b s)
          with Not_found -> failwith (sprintf "Packed_value.add: %S" str)
        end
      | Off_delta hunks ->
        add_be_modified_base_128 tmp_buffer hunks.source;
        add_deflated_hunks tmp_buffer hunks
      | Ref_delta hunks ->
        SHA.add tmp_buffer hunks.source;
        add_deflated_hunks tmp_buffer hunks
    in
    let kind = match t with
      | Off_delta _ -> 0b110
      | Ref_delta _ -> 0b111
      | Raw_value v ->
        match Value.type_of_inflated (Mstruct.of_string v) with
        | Object_type.Commit -> 0b001
        | Object_type.Tree   -> 0b010
        | Object_type.Blob   -> 0b011
        | Object_type.Tag    -> 0b100 in
    let more = if size > 0x0f then 0x80 else 0 in
    Log.debug "add kind:%d size:%d (%b %d)"
      kind size (more=0x80) (size land 0x0f);
    let byte = more lor (kind lsl 4) lor (size land 0x0f) in
    Buffer.add_char buf (Char.chr byte);
    if size > 0x0f then
      add_le_base_128 buf (size lsr 4);
    let str = Buffer.contents tmp_buffer in
    Buffer.add_string buf str

  let crc32 t =
    let buf = Misc.with_buffer (fun buf -> add buf t) in
    Misc.crc32 buf

  let pretty = pretty

end

module V2 = Make(struct let version = 2 end)
module V3 = Make(struct let version = 3 end)
module PIC = struct

  type kind =
    | Raw of string
    | Link of t delta

  and t = {
    kind: kind;
    sha1: SHA.t;
  }

  let pretty_kind = function
    | Raw _  -> "RAW"
    | Link d -> sprintf "link(%s)" (SHA.to_hex d.source.sha1)

  let pretty { kind; sha1 } =
    sprintf "%s: %s" (SHA.to_hex sha1) (pretty_kind kind)

  let rec unpack pic =
    match Value.Cache.find_inflated pic.sha1 with
    | Some x -> x
    | None   ->
      Log.debug "unpack %s" (pretty pic);
      let str =
        match pic.kind with
        | Raw x  -> x
        | Link d ->
          Log.debug "unpack: hop to %s" (SHA.to_hex d.source.sha1);
          let source = unpack d.source in
          Misc.with_buffer (fun buf -> add_delta buf { d with source }) in
      Value.Cache.add_inflated pic.sha1 str;
      str

  let to_value p =
    Log.debug "to_value";
    let buf = unpack p in
    Value.input_inflated (Mstruct.of_string buf)

  let raw sha1 raw =
    { sha1; kind = Raw (Cstruct.to_string raw) }

  module X = struct
    type x = t
    type t = x
    let compare = compare
    let pretty = pretty
  end
  module Map = Misc.Map(X)

end

let of_pic index ~pos t =
  match t.PIC.kind with
  | PIC.Raw x  -> Raw_value x
  | PIC.Link d ->
    try
      let o = PIC.Map.find d.source index in
      Off_delta { d with source = pos - o }
    with Not_found ->
      eprintf "Packed_value.of_pic: cannot fallow the PIC chain.\n";
      failwith "Packed_value.of_pic"

let to_pic offsets sha1s (pos, sha1, t) =
  Log.debug "to_pic(%s): cache miss!" (SHA.to_hex sha1);
  let kind = match t with
    | Raw_value x -> PIC.Raw x
    | Ref_delta d ->
      begin
        try
          let pic = SHA.Map.find d.source sha1s in
          PIC.Link { d with source = pic }
        with Not_found ->
          eprintf
            "Packed_value.to_pic: shallow pack are not supported.\n\
             %s is not in the pack file!\n"
            (SHA.to_hex d.source);
          failwith "Packed_value.to_pic";
      end
    | Off_delta d ->
      let offset = pos - d.source in
      try
        let pic = Misc.IntMap.find offset offsets in
        PIC.Link { d with source = pic }
      with Not_found ->
        eprintf "Cannot find offest %d in the index\n%s"
          d.source (Misc.IntMap.pretty PIC.pretty offsets);
        failwith "Packed_value.to_pic"

  in
  { PIC.sha1; kind }

(* XXX: merge with PIC.unpack *)
let add_inflated_value_aux (return, bind) ~read ~offsets ~pos buf = function
  | Raw_value x ->
    Buffer.add_string buf x;
    return ()
  | Ref_delta d ->
    bind
      (read d.source)
      (fun source ->
         add_delta buf { d with source };
         return ())
  | Off_delta d ->
    let offset = pos - d.source in
    let base =
      try Misc.IntMap.find offset offsets
      with Not_found ->
        eprintf "Packed_value.add_inflated_value: cannot find any object at offset %d\n"
          offset;
        failwith "Packed_inflated_value" in
    bind
      (read base)
      (fun source ->
         add_delta buf { d with source };
         return ())

let lwt_monad = Lwt.return, Lwt.bind
let id_monad = (fun x ->x), (fun x f -> f x)

let add_inflated_value = add_inflated_value_aux lwt_monad
let add_inflated_value_sync = add_inflated_value_aux id_monad
