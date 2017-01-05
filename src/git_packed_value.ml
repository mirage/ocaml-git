(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Log = (val Git_misc.src_log "packed-value" : Logs.LOG)

type copy = { copy_offset: int; copy_length: int; }

type hunk =
  | Insert of string
  | Copy of copy

type 'a delta = {
  source: 'a;
  source_length: int;
  result_length: int;
  hunks: hunk list;
}

type kind =
  | Raw_value of string
  | Ref_delta of Git_hash.t delta
  | Off_delta of int delta

let pp_copy ppf t =
  Format.fprintf ppf "@[off:%d@ len:%d@]" t.copy_offset t.copy_length

let pp_hunk ppf = function
  | Insert str -> Format.fprintf ppf "@[INSERT %S@]" str
  | Copy copy  -> Format.fprintf ppf "@[COPY %a@]" pp_copy copy

let pp_hunks ppf l =
  Format.fprintf ppf "@[";
  List.iter (Format.fprintf ppf "%a@ " pp_hunk) l;
  Format.fprintf ppf "@]"

let _pp_delta ppf d =
  Format.fprintf ppf
    "@[\
     source-length: %d@ \
     result-length: %d@ \
     %a@]"
    d.source_length
    d.result_length
    pp_hunks d.hunks

type t = { kind: kind; offset: int; }
let hash = Hashtbl.hash
let equal = (=)
let compare = compare

let shallow hashes t = match t.kind with
  | Off_delta _
  | Raw_value _ -> false
  | Ref_delta d -> not (Git_hash.Set.mem d.source hashes)

let pp_kind ppf = function
  | Raw_value _ -> Format.pp_print_string ppf "RAW"
  | Ref_delta d -> Format.fprintf ppf "@[ref-delta: %s@@]" (Git_hash.to_hex d.source)
  | Off_delta d -> Format.fprintf ppf "@[off-delta:%d@@]" d.source

let pp ppf { kind; offset } =
  Format.fprintf ppf "offset: %d@,%a" offset pp_kind kind

let create ~offset ~kind = { offset; kind }
let kind t = t.kind
let offset t = t.offset

let is_delta t = match t.kind with
  | Raw_value _ -> false
  | _ -> true

let result_length t = match t.kind with
  | Ref_delta { result_length; _ }
  | Off_delta { result_length; _ } -> result_length
  | Raw_value str -> String.length str

let source_length t = match t.kind with
  | Ref_delta { source_length; _ }
  | Off_delta { source_length; _ } -> source_length
  | Raw_value str -> String.length str

let fail fmt = Fmt.kstrf failwith ("Packed_value." ^^ fmt)
let err_invalid_size = fail "%s is not a valid size"
let err_missing_size source = Mstruct.parse_error_buf source "missing size"

let size_of_string s =
  try int_of_string s with Failure _ -> err_invalid_size s

let err_size_differs size len =
  fail "with_inflated: inflated size differs. Expecting %d, got %d." size len

let add_hunk buf ~source ~pos = function
  | Insert str -> Buffer.add_string buf str
  | Copy c     ->
    Buffer.add_substring buf source (pos + c.copy_offset) c.copy_length

let add_delta buf delta =
  let source = Mstruct.of_string delta.source in
  let object_type = Git_object_type.input source in
  let size = match Mstruct.get_string_delim source Git_misc.nul with
    | None   -> err_missing_size source
    | Some s -> size_of_string s
  in
  if size <> delta.source_length then
    err_size_differs delta.source_length size;
  Buffer.add_string buf (Git_object_type.to_string object_type);
  Buffer.add_char   buf Git_misc.sp;
  Buffer.add_string buf (string_of_int delta.result_length);
  Buffer.add_char   buf Git_misc.nul;
  let pos = Mstruct.offset source in
  List.iter (add_hunk buf ~source:delta.source ~pos) delta.hunks

module PIC = struct

  type kind = Raw of string | Link of t delta

  and t = {
    kind   : kind;
    hash   : Git_hash.t;
    shallow: bool;
    mutable raw: string option;
  }

  let equal x y = Git_hash.equal x.hash y.hash
  let hash x = Git_hash.hash x.hash
  let compare x y = Git_hash.compare x.hash y.hash

  let pp_kind ppf = function
    | Raw _  -> Fmt.string ppf "RAW"
    | Link d -> Fmt.pf ppf "link(%a)" Git_hash.pp @@ d.source.hash

  let pp ppf t = Format.fprintf ppf "@[%a: %a@]" Git_hash.pp t.hash pp_kind t.kind

  let create ?raw ?(shallow=false) hash kind = { hash; kind; shallow; raw }
  let name t = t.hash
  let kind t = t.kind
  let raw t = t.raw
  let shallow t = t.shallow

  let of_raw ?(shallow=false) hash raw =
    { hash; kind = Raw raw; shallow; raw = Some raw; }

  let with_cache f h =
    match Git_value.Cache.find_inflated h with
    | Some x -> x
    | None   ->
      Log.debug (fun l -> l "%a: cache miss!" Git_hash.pp h);
      let x = f () in
      Git_value.Cache.add_inflated h x;
      x

  let rec unpack t = match raw t with
    | Some str -> str
    | None     ->
      Log.debug (fun l -> l "unpack %a" pp t);
      let raw = with_cache (fun () -> unpack_kind @@ kind t) (name t) in
      t.raw <- Some raw;
      raw

  and unpack_kind = function
    | Raw x  -> x
    | Link d ->
      Log.debug (fun l -> l "unpack: hop to %a" Git_hash.pp @@ name d.source);
      let source = unpack d.source in
      Git_misc.with_buffer (fun buf -> add_delta buf { d with source })

end

type pic = PIC.t

module IO (D: Git_hash.DIGEST) (I: Git_inflate.S) = struct

  module Hash_IO = Git_hash.IO(D)
  module Value_IO = Git_value.IO(D)(I)

  let err_inflate () = fail "not a valid compressed object"

  module V (M: sig val version: int end) = struct

    type t = kind
    let compare = compare
    let hash = hash
    let equal = equal

    let isset i bit =
      (i lsr bit) land 1 <> 0

    let input_hunk source_length buf =
      let opcode = Mstruct.get_uint8 buf in
      if opcode = 0 then
        Mstruct.parse_error_buf buf
          "0 as value of the first byte of a hunk is reserved.";
      match opcode land 0x80 with
      | 0 ->
        let contents = Mstruct.get_string buf opcode in
        Insert contents
      | _ ->
        let read bit shift =
          if not (isset opcode bit) then 0
          else Mstruct.get_uint8 buf lsl shift in
        let copy_offset =
          let o0 = read 0 0 in
          let o1 = read 1 8 in
          let o2 = read 2 16 in
          let o3 = read 3 24 in
          o0 lor o1 lor o2 lor o3 in
        let copy_length =
          let l0 = read 4 0 in
          let l1 = read 5 8 in
          let l2 = read 6 16 in
          if M.version = 2 && l2 <> 0 then
            Mstruct.parse_error_buf buf "result fied set in delta hunk";
          l0 lor l1 lor l2 in
        let copy_length =
          if copy_length = 0 then 0x10_000 else copy_length in
        if copy_offset + copy_length > source_length then
          Mstruct.parse_error_buf buf
            "wrong insert hunk (offset:%d length:%d source:%d)"
            copy_offset copy_length source_length;
        Copy { copy_offset; copy_length }

    let add_hunk buf = function
      | Insert contents ->
        let len = String.length contents in
        if len >= 0x80 then fail "invalid hunk: insert too large (%d)" len;
        Buffer.add_char buf (Char.chr (String.length contents));
        Buffer.add_string buf contents
      | Copy { copy_offset; copy_length } ->
        let length = if copy_length = 0x10_000 then 0 else copy_length in
        let offset = copy_offset in
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
          fail "pack version 2 does not support copy hunks of size greater than \
                64K.";
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

    let input_hunks_result_size buf =
      let _source_length = input_le_base_128 buf in
      input_le_base_128 buf

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
      let buf = match I.inflate ~output_size:size buf with
        | Some b -> b
        | None   -> err_inflate ()
      in
      let len = Mstruct.length buf in
      if len <> size then err_size_differs size len;
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

      Log.debug (fun l -> l "input: kind:%d size:%d (more=%b)" kind size more);

      let mk typ str =
        let size = Cstruct.len str in
        let buf = Git_misc.with_buffer (fun buf ->
            Value_IO.add_header buf typ size;
            Buffer.add_string buf (Cstruct.to_string str)
          ) in
        Raw_value buf in

      match kind with
      | 0b000 -> Mstruct.parse_error "invalid: 0 is reserved"
      | 0b001 -> with_inflated_buf buf size (mk Git_object_type.Commit)
      | 0b010 -> with_inflated_buf buf size (mk Git_object_type.Tree)
      | 0b011 -> with_inflated_buf buf size (mk Git_object_type.Blob)
      | 0b100 -> with_inflated_buf buf size (mk Git_object_type.Tag)
      | 0b101 -> Mstruct.parse_error "invalid: 5 is reserved"
      | 0b110 ->
        let base  = input_be_modified_base_128 buf in
        let hunks = with_inflated buf size (input_hunks base) in
        Off_delta hunks
      | 0b111 ->
        let base  = Hash_IO.input buf in
        let hunks = with_inflated buf size (input_hunks base) in
        Ref_delta hunks
      | _     -> assert false

    let size buf =
      let byte = Mstruct.get_uint8 buf in
      let more = (byte land 0x80) <> 0 in
      let kind = (byte land 0x70) lsr 4 in
      let size =
        let low = (byte land 0x0f) in
        if more then
          let ss = input_le_base_128 buf in
          low lor (ss lsl 4)
        else low in
      Log.debug (fun l -> l "size: kind:%d size:%d (more=%b)" kind size more);
      match kind with
      | 0b000 -> Mstruct.parse_error "invalid: 0 is reserved"
      | 0b001 | 0b010 | 0b011 | 0b100 -> size
      | 0b101 -> Mstruct.parse_error "invalid: 5 is reserved"
      | 0b110 ->
        ignore (input_be_modified_base_128 buf);
        (* This is a bit inefficient. We only need to decompress the first few bytes. *)
        with_inflated buf size input_hunks_result_size
      | 0b111 ->
        ignore (Hash_IO.input buf);
        with_inflated buf size input_hunks_result_size
      | _     -> assert false

    let inflated_buffer = Buffer.create 1024
    let with_deflated buf ?level fn =
      Buffer.reset inflated_buffer;
      fn inflated_buffer;
      let inflated = Buffer.contents inflated_buffer in
      let deflated = I.deflate ?level (Cstruct.of_string inflated) in
      Buffer.add_string buf (Cstruct.to_string deflated);
      String.length inflated

    let tmp_buffer = Buffer.create 1024

    let add buf ?level t =
      Buffer.reset tmp_buffer;
      let add_deflated_hunks buf hunks =
        with_deflated buf ?level (fun b -> add_hunks b hunks) in
      let size = match t with
        | Raw_value str ->
          begin
            try
              let i = String.index str Git_misc.nul in
              let s = String.sub str (i+1) (String.length str-i-1) in
              with_deflated tmp_buffer (fun b -> Buffer.add_string b s)
            with Not_found -> fail "add: %S" str
          end
        | Off_delta hunks ->
          add_be_modified_base_128 tmp_buffer hunks.source;
          add_deflated_hunks tmp_buffer hunks
        | Ref_delta hunks ->
          Hash_IO.add tmp_buffer hunks.source;
          add_deflated_hunks tmp_buffer hunks
      in
      let kind = match t with
        | Off_delta _ -> 0b110
        | Ref_delta _ -> 0b111
        | Raw_value v ->
          match Git_value.type_of_inflated (Mstruct.of_string v) with
          | Git_object_type.Commit -> 0b001
          | Git_object_type.Tree   -> 0b010
          | Git_object_type.Blob   -> 0b011
          | Git_object_type.Tag    -> 0b100 in
      let more = if size > 0x0f then 0x80 else 0 in
      Log.debug (fun l ->
          l "add kind:%d size:%d (%b %d)" kind size (more=0x80) (size land 0x0f)
        );
      let byte = more lor (kind lsl 4) lor (size land 0x0f) in
      Buffer.add_char buf (Char.chr byte);
      if size > 0x0f then
        add_le_base_128 buf (size lsr 4);
      let str = Buffer.contents tmp_buffer in
      Buffer.add_string buf str

    let pp = pp_kind

  end

  module V2 = V(struct let version = 2 end)
  module V3 = V(struct let version = 3 end)

  let value_of_pic p =
    Log.debug (fun l -> l "to_value");
    let buf = PIC.unpack p in
    (* FIXME: costly, allocate a bigarray *)
    Value_IO.input_inflated (Mstruct.of_string buf)

  let of_pic ~index ~offset t =
    let return kind = { offset; kind } in
    match PIC.kind t with
    | PIC.Raw x  -> return (Raw_value x)
    | PIC.Link d ->
      let name = PIC.name d.source in
      match index name with
      | None   -> fail "of_pic: cannot find %a" Git_hash.pp name
      | Some o -> return (Off_delta { d with source = offset - o })

  let err_hash_not_found n h = fail "%s: cannot read %a" n Git_hash.pp h
  let err_offset_not_found = fail "%s: cannot find any object at offset %d"

  let to_pic ~read ~offsets ~hashes t =
    let kind = match t.kind with
      | Raw_value x -> Lwt.return (PIC.Raw x)
      | Ref_delta d ->
        begin match hashes d.source with
          | Some pic -> Lwt.return (PIC.Link { d with source = pic })
          | None ->
            read d.source >>= function
            | Some buf ->
              let shallow = PIC.of_raw ~shallow:true d.source buf in
              Lwt.return (PIC.Link { d with source = shallow })
            | None -> err_hash_not_found "to_pic" d.source
        end
      | Off_delta d ->
        let offset = t.offset - d.source in
        match offsets offset with
        | None     -> err_offset_not_found "to_pic" d.source
        | Some pic ->
          let pic = PIC.Link { d with source = pic } in
          Lwt.return pic
    in
    kind >|= fun kind ->
    let raw  = PIC.unpack_kind kind in
    let h = D.string raw in
    Log.debug (fun l ->
        l "to_pic(%a) -> %a:%a" pp_kind t.kind Git_hash.pp h PIC.pp_kind kind
      );
    PIC.create ~raw h kind

  let read_and_add_delta ~read buf delta h =
    read h >>= function
    | None        -> err_hash_not_found "read_and_add_delta" h
    | Some source -> add_delta buf { delta with source }; Lwt.return_unit

  let add_inflated_value ~read ~offsets buf { offset; kind } = match kind with
    | Raw_value x -> Buffer.add_string buf x; Lwt.return_unit
    | Ref_delta d -> read_and_add_delta ~read buf d d.source
    | Off_delta d ->
      let offset = offset - d.source in
      match offsets offset with
      | None      -> err_offset_not_found "add_inflated_value" offset
      | Some base -> read_and_add_delta ~read buf d base

  let input_packed_value = function
    | 2 -> V2.input
    | 3 -> V3.input
    | _ -> fail "pack version should be 2 or 3"

  let rec unpack_ref_delta ~lv ~version ~index ~read ~write ba d =
    Log.debug (fun l ->
        l "unpack-ref-delta[%d]: d.source=%a" lv Git_hash.pp d.source);
    match index d.source with
    | Some offset ->
      let offset = offset - 12 in (* header skipped *)
      let ba_len = Bigarray.Array1.dim ba in
      let len = ba_len - offset in
      let buf = Mstruct.of_bigarray ~off:offset ~len ba in
      let kind = input_packed_value version buf in
      let t = { offset; kind } in
      unpack ~lv:(lv+1) ~read ~write ~version ~index ~ba t >|= fun source ->
      Git_misc.with_buffer (fun b -> add_delta b {d with source})
    | None ->
      read d.source >>= function
      | None     -> fail "unpack: cannot read %a" Git_hash.pp d.source
      | Some buf -> Lwt.return buf

  and unpack_off_delta ~lv ~version ~index ~read ~write ba d offset =
    let offset = offset - d.source in
    Log.debug (fun l ->
        l "unpack-off-delta[%d]: offset=%d-%d=%d" lv offset d.source offset);
    let ba_len = Bigarray.Array1.dim ba in
    let len = ba_len - offset in
    let buf = Mstruct.of_bigarray ~off:offset ~len ba in
    let kind = input_packed_value version buf in
    let t = { offset; kind } in
    unpack ~lv:(lv+1) ~read ~write ~version ~index ~ba t >|= fun source ->
    Git_misc.with_buffer (fun b -> add_delta b {d with source})

  and unpack ~lv ~index ~read ~write ~version ~ba { offset=o; kind } =
    let obj = match kind with
      | Raw_value v -> Lwt.return v
      | Ref_delta d -> unpack_ref_delta ~lv ~version ~index ~read ~write ba d
      | Off_delta d -> unpack_off_delta ~lv ~version ~index ~read ~write ba d o
    in
    obj >>= fun x ->
    write x >|= fun h ->
    Log.debug (fun l -> l "unpack-raw[%d]: %a" lv Git_hash.pp h);
    x

  let unpack = unpack ~lv:0

  let to_value ~index ~read ~write ~version ~ba t =
    Log.debug (fun l -> l "to_value");
    unpack ~version ~read ~write ~index ~ba t >|= fun u ->
    (* FIXME: costly, allocates a bigarray *)
    Value_IO.input_inflated (Mstruct.of_string u)

  module type IO = sig
    include Git_s.IO with type t = kind
    val size: Mstruct.t -> int
  end

end
