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

module Log = (val Misc.src_log "index" : Logs.LOG)

type time = {
  lsb32: int32;
  nsec : int32;
}

type mode = [
    `Normal
  | `Exec
  | `Link
  | `Gitlink
]

let pretty_mode = function
  | `Normal  -> "normal"
  | `Exec    -> "exec"
  | `Link    -> "link"
  | `Gitlink -> "gitlink"

let pp_mode ppf t =
  Format.fprintf ppf "%s" (pretty_mode t)

type stat_info = {
  ctime: time;
  mtime: time;
  dev  : int32;
  inode: int32;
  mode : mode;
  uid  : int32;
  gid  : int32;
  size : int32;
}

let pp_stats ppf t =
  Format.fprintf ppf
    "{@[<hov 2>\
     ctime = (%ld, %ld);@ \
     mtime = (%ld, %ld);@ \
     dev = %ld;@ \
     inode = %ld;@ \
     uid = %ld;@ \
     gid = %ld;@ \
     size = %ld;@ \
     mode = \"%a\"@]}"
    t.ctime.lsb32 t.ctime.nsec
    t.mtime.lsb32 t.mtime.nsec
    t.dev t.inode
    t.uid t.gid
    t.size pp_mode t.mode

type entry = {
  stats : stat_info;
  id    : Hash.Blob.t;
  stage : int;
  name  : string;
}

(* Index entries are sorted by the byte sequence that comprises the
   entry name; with a secondary comparison of the stage bits from the
   <ENTRY_FLAGS> if the entry name byte sequences are identical. *)
let compare_entries e1 e2 =
  match String.compare e1.name e2.name with
  | 0 -> Hash.Blob.compare e2.id e1.id
  | i -> i

let pp_entry ppf t =
  Format.fprintf ppf
    "{@[<hov 2>\
     name = %S@ \
     id = \"%a\"@ \
     stats =@ %a;@ \
     stage = %d;@]}"
    t.name
    Hash.Blob.pp t.id
    pp_stats t.stats
    t.stage

type extension_kind =
  [ `Tree
  | `Reuc (* Reuse undo *)
  | `Link (* split index *)
  | `Other of string ]

let extension_kind_of_string: string -> extension_kind = function
  | "TREE" -> `Tree
  | "REUC" -> `Reuc
  | "link" -> `Link
  | x      -> `Other x

let string_of_extension_kind: extension_kind -> string = function
  | `Tree -> "TREE"
  | `Reuc -> "REUC"
  | `Link -> "link"
  | `Other x -> x

type extension = {
  kind: extension_kind;
  payload: string;
}

type t = {
  entries   : entry list;
  extensions: extension list;
}

let create ?(extensions=[]) entries =
  { entries = List.sort compare_entries entries; extensions }

let empty = { entries = []; extensions = [] }

let hash = Hashtbl.hash

let compare x y =
  let rec aux l1 l2 =
    Printf.printf "XXX\n"; match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _ , [] -> 1
    | h1::t1, h2::t2 ->
      match compare_entries h1 h2 with
      | 0 -> aux t1 t2
      | i -> i
  in
  match aux x.entries y.entries with
  | 0 -> compare x.extensions y.extensions
  | i -> i

let equal x y = compare x y = 0

let pp ppf t =
  Format.fprintf ppf "@[";
  List.iter (fun e ->
      pp_entry ppf e;
      Format.fprintf ppf "@.";
    ) t.entries;
  Format.fprintf ppf "@]"

let pretty t =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf t;
  Buffer.contents buf

let pp ppf t = Format.fprintf ppf "%s" (pretty t)

let input_time buf =
  let lsb32 = Mstruct.get_be_uint32 buf in
  let nsec = Mstruct.get_be_uint32 buf in
  { lsb32; nsec }

let output_time buf t =
  Mstruct.set_be_uint32 buf t.lsb32;
  Mstruct.set_be_uint32 buf t.nsec

let input_mode buf =
  let _zero = Mstruct.get_be_uint16 buf in
  (* XX: check that _zero is full of 0s *)
  let n = Mstruct.get_be_uint16 buf in
  match n lsr 12 with
  | 0b1010 -> `Link
  | 0b1110 -> `Gitlink
  | 0b1000 ->
    begin match n land 0x1FF with
      | 0o755 -> `Exec
      | 0o644 -> `Normal
      | d     -> Mstruct.parse_error_buf buf "mode: invalid permission (%d)" d
    end
  | m -> Mstruct.parse_error_buf buf "mode: invalid (%d)" m

let output_mode buf t =
  let n = match t with
    | `Exec    -> 0b1000__000__111_101_101
    | `Normal  -> 0b1000__000__110_100_100
    | `Link    -> 0b1010__000__000_000_000
    | `Gitlink -> 0b1110__000__000_000_000 in
  Mstruct.set_be_uint16 buf 0;
  Mstruct.set_be_uint16 buf n

let input_stat_info buf =
  Log.debug (fun l -> l "input_stat_info");
  let ctime = input_time buf in
  let mtime = input_time buf in
  let dev = Mstruct.get_be_uint32 buf in
  let inode = Mstruct.get_be_uint32 buf in
  let mode = input_mode buf in
  let uid = Mstruct.get_be_uint32 buf in
  let gid = Mstruct.get_be_uint32 buf in
  let size = Mstruct.get_be_uint32 buf in
  { mtime; ctime; dev; inode; mode; uid; gid; size }

let add_stat_info buf t =
  output_time buf t.ctime;
  output_time buf t.mtime;
  let uint32 = Mstruct.set_be_uint32 buf in
  uint32 t.dev;
  uint32 t.inode;
  output_mode buf t.mode;
  uint32 t.uid;
  uint32 t.gid;
  uint32 t.size

let fail fmt = Fmt.kstrf failwith ("Index: " ^^ fmt)
let err_invalid_version v = fail "Only index version 2 is supported (not %ld)" v
let err_need_more_data = fail "input: need more data! (total:%d current:%d)"

let err_wrong_index_header buf =
  Mstruct.parse_error_buf buf "%s: wrong index header."

let err_wrong_checksum ~got ~expected =
  fail "Wrong checksum! got %a but was expecting %a."
    Hash.pp got Hash.pp expected

let pp_extension ppf e =
  Format.fprintf ppf "@[kind:%s@ size:%d]"
    (string_of_extension_kind e.kind) (String.length e.payload)

module IO (D: Hash.DIGEST) = struct

  module Hash_IO = Hash.IO(D)
  type x = t
  type t = x
  let equal = equal
  let compare = compare
  let pp = pp
  let hash = hash

  let input_entry buf =
    Log.debug (fun l -> l "input_entry");
    let offset0 = Mstruct.offset buf in
    let stats = input_stat_info buf in
    let id = Hash_IO.input buf |> Hash.to_blob in
    let stage, len =
      let i = Mstruct.get_be_uint16 buf in
      (i land 0x3000) lsr 12,
      (i land 0x0FFF)
    in
    Log.debug (fun l -> l "stage:%d len:%d" stage len);
    let name = Mstruct.get_string buf len in
    Mstruct.shift buf 1;
    let bytes = Mstruct.offset buf - offset0 in
    let padding = match bytes mod 8 with
      | 0 -> 0
      | n -> 8-n in
    Mstruct.shift buf padding;
    Log.debug (fun l ->
        l "name:%s id:%a bytes:%d padding:%d" name Hash.Blob.pp id bytes padding
      );
    { stats; id; stage; name }

  let add_entry buf t =
    Log.debug (fun l -> l "add_entry");
    let len = 63 + String.length t.name in
    let pad = match len mod 8 with
      | 0 -> 0
      | n -> 8-n in
    let cstr = Cstruct.create (len+pad) in
    Mstruct.with_mstruct cstr (fun mstr ->
        add_stat_info mstr t.stats;
        Mstruct.set_string mstr (Hash.Blob.to_raw t.id);
        let flags = (t.stage lsl 12 + String.length t.name) land 0x3FFF in
        Mstruct.set_be_uint16 mstr flags;
        Mstruct.set_string mstr t.name;
        Mstruct.set_string mstr (String.make (1+pad) '\x00');
      );
    Buffer.add_string buf (Cstruct.to_string cstr)


  let input_entries buf =
    let n = Mstruct.get_be_uint32 buf in
    Log.debug (fun l ->
        l "input_entries: %ld entries (%db)" n (Mstruct.length buf));
    let rec loop acc n =
      if n = 0l then List.rev acc
      else
        let entry = input_entry buf in
        loop (entry :: acc) Int32.(sub n 1l) in
    loop [] n

  let input_extensions buf =
    let rec aux acc =
      if Mstruct.length buf = 20 then List.rev acc
      else
        let kind = extension_kind_of_string (Mstruct.get_string buf 4) in
        let size = Mstruct.get_be_uint32 buf in
        let payload = Mstruct.get_string buf (Int32.to_int size) in
        let e = { kind; payload } in
        aux (e :: acc)
    in
    aux []

  let input buf =
    let all = Mstruct.to_cstruct buf in
    let offset = Mstruct.offset buf in
    let total_length = Mstruct.length buf in
    let header = Mstruct.get_string buf 4 in
    if header <> "DIRC" then err_wrong_index_header buf header;
    let version = Mstruct.get_be_uint32 buf in
    if version <> 2l then err_invalid_version version;
    let entries = input_entries buf in
    let extensions = input_extensions buf in
    let length = Mstruct.offset buf - offset in
    if length <> total_length - 20 then
      err_need_more_data (total_length - 20) length;
    let got = Cstruct.sub all offset length |> D.cstruct in
    let expected = Hash_IO.input buf in
    if not (Hash.equal got expected) then err_wrong_checksum ~got ~expected;
    { entries; extensions }

  let add buf ?level:_ t =
    let str = Misc.with_buffer (fun buf ->
        let n = List.length t.entries in
        Log.debug (fun l -> l "add %d entries" n);
        let header = Cstruct.create 12 in
        Mstruct.with_mstruct header (fun header ->
            Mstruct.set_string header "DIRC";
            Mstruct.set_be_uint32 header 2l;
            Mstruct.set_be_uint32 header (Int32.of_int n);
          );
        Buffer.add_string buf (Cstruct.to_string header);
        List.iter (add_entry buf) t.entries;
      ) in
    let h = D.string str in
    Buffer.add_string buf str;
    Buffer.add_string buf (Hash.to_raw h)

end

module type IO = S.IO with type t = t
