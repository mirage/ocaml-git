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


open Core_kernel.Std
module Log = Log.Make(struct let section = "packed-value" end)

module Bigstring = struct
  include Bigstring

  let compare t1 t2 =
    match Int.compare (Bigstring.length t1) (Bigstring.length t2) with
    | 0 -> String.compare (Bigstring.to_string t1) (Bigstring.to_string t2)
    | i -> i

  let pretty t =
    if Int.(Bigstring.length t < 70) then sprintf "%S" (Bigstring.to_string t)
    else sprintf "%S[%d]" (Bigstring.To_string.sub t 0 70) (Bigstring.length t)

end

type copy = {
  offset: int;
  length: int;
} with bin_io, compare, sexp

let pretty_copy t =
  sprintf "off:%d len:%d" t.offset t.length

type hunk =
  | Insert of string
  | Copy of copy
with bin_io, compare, sexp

let pretty_hunk = function
  | Insert s -> sprintf "Insert %S" s
  | Copy c   -> sprintf "Copy %s" (pretty_copy c)

type 'a delta = {
  source: 'a;
  source_length: int;
  result_length: int;
  hunks: hunk list;
} with bin_io, compare, sexp

let pretty_delta d =
  let buf = Buffer.create 128 in
  bprintf buf
    "source-length: %d\n\
     result-length: %d\n"
    d.source_length
    d.result_length;
  List.iter ~f:(function
      | Insert str -> bprintf buf "INSERT %S\n" str
      | Copy copy  -> bprintf buf "COPY[%s]\n" (pretty_copy copy)
    ) d.hunks;
  Buffer.contents buf

module T = struct
  module X = struct
    type t =
      | Raw_value of Bigstring.t
      | Ref_delta of SHA1.t delta
      | Off_delta of int delta
    with bin_io, compare, sexp
    let hash (t: t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Value"
  end
  include X
  include Identifiable.Make (X)
end
include T

let pretty = function
  | Raw_value s -> Bigstring.pretty s
  | Ref_delta d -> sprintf "source:%s\n%s" (SHA1.to_hex d.source) (pretty_delta d)
  | Off_delta d -> sprintf "source:%d\n%s" d.source (pretty_delta d)

let result_length = function
  | Ref_delta { result_length }
  | Off_delta { result_length } -> result_length
  | Raw_value str               -> Bigstring.length str

let source_length = function
  | Ref_delta { source_length }
  | Off_delta { source_length } -> source_length
  | Raw_value str               -> Bigstring.length str

let add_hunk ~source buf = function
  | Insert str -> Bigbuffer.add_string buf str
  | Copy copy  ->
    let view = Bigstring.To_string.sub source ~pos:copy.offset ~len:copy.length in
    Bigbuffer.add_string buf view

let add_delta buf delta =
  let source = Mstruct.of_bigarray delta.source in
  let object_type = Object_type.input source in
  let size = match Mstruct.get_string_delim source Misc.nul with
    | None   -> Mstruct.parse_error_buf source "missing size"
    | Some s ->
      try int_of_string s
      with Failure "int_of_string" ->
        eprintf "Packed_value.add_delta: %s is not a valid size.\n" s;
        failwith "Packed_value.add_delta" in
  if Int.(size <> delta.source_length) then
    Mstruct.parse_error_buf source "size differs: delta:%d source:%d\n"
      delta.source_length size;
  Bigbuffer.add_string buf (Object_type.to_string object_type);
  Bigbuffer.add_char   buf Misc.sp;
  Bigbuffer.add_string buf (string_of_int delta.result_length);
  Bigbuffer.add_char   buf Misc.nul;
  List.iter ~f:(add_hunk ~source:delta.source buf) delta.hunks

module Make (M: sig val version: int end) = struct

  open Int

  let isset i bit =
    (i lsr bit) land 1 <> 0

  let input_hunk source_length buf =
    let opcode = Mstruct.get_uint8 buf in
    if Int.(opcode = 0) then
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
      Bigbuffer.add_char buf (Char.of_int_exn (String.length contents));
      Bigbuffer.add_string buf contents
    | Copy { offset; length } ->
      let length = if length = 0x10_000 then 0 else length in
      let bits = ref [] in
      let bit n shift =
        match (n lsr shift) land 0xFF with
        | 0 -> 0
        | n -> bits := Char.of_int_exn n :: !bits; 1
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
      Bigbuffer.add_char buf (Char.of_int_exn n);
      List.iter ~f:(Bigbuffer.add_char buf) (List.rev !bits)

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
      bytes := Char.of_int_exn byte :: !bytes;
      if i >= 0x80 then aux (i lsr 7) in
    aux int;
    let str = String.of_char_list (List.rev !bytes) in
    Bigbuffer.add_string buf str

  let input_hunks source buf =
    let source_length = input_le_base_128 buf in
    let result_length = input_le_base_128 buf in
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else aux (input_hunk source_length buf :: acc) in
    let hunks = aux [] in
    { source; hunks; source_length; result_length }

  let add_hunks buf t =
    let { source_length; result_length; hunks } = t in
    add_le_base_128 buf source_length;
    add_le_base_128 buf result_length;
    List.iter ~f:(add_hunk buf) hunks

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
        bytes := (Char.of_int_exn byte) :: !bytes;
        if i > 0x80 then aux (i lsr 7) false in
    aux int true;
    let bytes = String.of_char_list !bytes in
    Bigbuffer.add_string buf bytes

  let with_inflated buf size fn =
    let buf = Misc.inflate_mstruct buf in
    let len = Mstruct.length buf in
    if len <> size then (
      eprintf "Packed_value.with_inflated: inflated size differs. Expecting %d, got %d.\n"
        size len;
      failwith "Packed_value.with_inflated"
    );
    fn buf

  let with_inflated_buf buf size fn =
    with_inflated buf size (fun buf ->
        let contents = Mstruct.to_bigarray buf in
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
      let size = Bigstring.length str in
      let buf = Misc.with_buffer (fun buf ->
          Value.add_header buf typ size;
          Bigbuffer.add_string buf (Bigstring.to_string str)
        ) in
      Raw_value buf in

    Log.debugf "input kind:%d size:%d (%b)" kind size more;

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
      let base  = SHA1.input buf in
      let hunks = with_inflated buf size (input_hunks base) in
      Ref_delta hunks
    | _     -> assert false

  let inflated_buffer = Bigbuffer.create 1024
  let with_deflated buf fn =
    Bigbuffer.reset inflated_buffer;
    fn inflated_buffer;
    let inflated = Misc.buffer_contents inflated_buffer in
    let deflated = Misc.deflate_bigstring inflated in
    Bigbuffer.add_string buf (Bigstring.to_string deflated);
    Bigstring.length inflated

  let tmp_buffer = Bigbuffer.create 1024
  let add buf t =
    Bigbuffer.reset tmp_buffer;
    let add_deflated_hunks buf hunks =
      with_deflated buf (fun b -> add_hunks b hunks) in
    let size = match t with
      | Raw_value str ->
        begin match Bigstring.find Misc.nul str with
          | None   -> failwith "Packed_value.add"
          | Some i ->
            let s = Bigstring.To_string.subo ~pos:(i+1) str in
            with_deflated tmp_buffer (fun b -> Bigbuffer.add_string b s)
        end
      | Off_delta hunks ->
        add_be_modified_base_128 tmp_buffer hunks.source;
        add_deflated_hunks tmp_buffer hunks
      | Ref_delta hunks ->
        SHA1.add tmp_buffer hunks.source;
        add_deflated_hunks tmp_buffer hunks
    in
    let kind = match t with
      | Off_delta _ -> 0b110
      | Ref_delta _ -> 0b111
      | Raw_value v ->
        match Value.type_of_inflated (Mstruct.of_bigarray v) with
        | Object_type.Commit -> 0b001
        | Object_type.Tree   -> 0b010
        | Object_type.Blob   -> 0b011
        | Object_type.Tag    -> 0b100 in
    let more = if size > 0x0f then 0x80 else 0 in
    Log.debugf "add kind:%d size:%d (%b %d)"
      kind size (more=0x80) (size land 0x0f);
    let byte = more lor (kind lsl 4) lor (size land 0x0f) in
    Bigbuffer.add_char buf (Char.of_int_exn byte);
    if size > 0x0f then
      add_le_base_128 buf (size lsr 4);
    let str = Misc.buffer_contents tmp_buffer in
    Bigbuffer.add_string buf (Bigstring.to_string str)

  let crc32 t =
    let buf = Misc.with_buffer (fun buf -> add buf t) in
    Misc.crc32 buf

  let pretty = pretty

  include T

end

module V2 = Make(struct let version = 2 end)
module V3 = Make(struct let version = 3 end)

let rev_assoc map d =
  let r = ref None in
  try
    SHA1.Map.iter
      ~f:(fun ~key ~data -> if Int.(data=d) then (r := Some key; raise Exit))
      map;
    None
  with Exit ->
    !r

let add_inflated_value_aux (return, bind) ~read ~index ~offset buf = function
  | Raw_value x ->
    Bigbuffer.add_string buf (Bigstring.to_string x);
    return ()
  | Ref_delta d ->
    bind
      (read d.source)
      (fun source ->
         add_delta buf { d with source };
         return ())
  | Off_delta d ->
    let offset = offset - d.source in
    let base =
      match rev_assoc index.Pack_index.offsets offset with
      | Some k -> k
      | None   ->
        let msg = sprintf
            "inflate: cannot find any object starting at offset %d"
            offset in
        failwith msg in
    bind
      (read base)
      (fun source ->
         add_delta buf { d with source };
         return ())

let lwt_monad = Lwt.return, Lwt.bind
let id_monad = (fun x ->x), (fun x f -> f x)

let add_inflated_value = add_inflated_value_aux lwt_monad
let add_inflated_value_sync = add_inflated_value_aux id_monad

open Lwt

let to_value ~read ~index ~offset packed_value =
  let buf = Bigbuffer.create 1024 in
  add_inflated_value ~read ~index ~offset buf packed_value >>= fun () ->
  let buf = Mstruct.of_bigarray (Misc.buffer_contents buf) in
  let value = Value.input_inflated buf in
  return value
