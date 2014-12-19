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

(* XXX: we only implement index file cache format V2 *)

open Printf

module Log = Log.Make(struct let section = "cache" end)

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

type entry = {
  stats : stat_info;
  id    : SHA.t;
  stage : int;
  name  : string;
}

let pretty_entry t =
  sprintf
    "%s\n\
    \  ctime: %ld:%ld\n\
    \  mtime: %ld:%ld\n\
    \  dev: %ld\tino: %ld\n\
    \  uid: %ld\tgid: %ld\n\
    \  size: %ld\tflags: %d\n\
    \  mode: %s\n"
    t.name
    t.stats.ctime.lsb32 t.stats.ctime.nsec
    t.stats.mtime.lsb32 t.stats.mtime.nsec
    t.stats.dev t.stats.inode
    t.stats.uid t.stats.gid
    t.stats.size t.stage (pretty_mode t.stats.mode)

type t = {
  entries   : entry list;
  extensions: (int32 * string) list;
}

let hash = Hashtbl.hash

let compare = compare

let equal = (=)

let pretty t =
  let buf = Buffer.create 1024 in
  List.iter (fun e ->
      Buffer.add_string buf (pretty_entry e);
      Buffer.add_char buf '\n';
    ) t.entries;
  Buffer.contents buf

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
  Log.debug "input_stat_info";
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

let input_entry buf =
  Log.debug "input_entry";
  let offset0 = Mstruct.offset buf in
  let stats = input_stat_info buf in
  let id = SHA.input buf in
  let stage, len =
    let i = Mstruct.get_be_uint16 buf in
    (i land 0x3000) lsr 12,
    (i land 0x0FFF) in
  Log.debug "stage:%d len:%d" stage len;
  let name = Mstruct.get_string buf len in
  Mstruct.shift buf 1;
  let bytes = Mstruct.offset buf - offset0 in
  let padding = match bytes mod 8 with
    | 0 -> 0
    | n -> 8-n in
  Mstruct.shift buf padding;
  Log.debug "name: %s id: %s bytes:%d padding:%d"
    name (SHA.to_hex id) bytes padding;
  { stats; id; stage; name }

let add_entry buf t =
  Log.debug "add_entry";
  let len = 63 + String.length t.name in
  let pad = match len mod 8 with
    | 0 -> 0
    | n -> 8-n in
  let cstr = Cstruct.create (len+pad) in
  Mstruct.with_mstruct cstr (fun mstr ->
      add_stat_info mstr t.stats;
      Mstruct.set_string mstr (SHA.to_raw t.id);
      let flags = (t.stage lsl 12 + String.length t.name) land 0x3FFF in
      Mstruct.set_be_uint16 mstr flags;
      Mstruct.set_string mstr t.name;
      Mstruct.set_string mstr (String.make (1+pad) '\x00');
    );
  Buffer.add_string buf (Cstruct.to_string cstr)

let input_extensions _buf =
  (* TODO: actually read the extension contents *)
  []

let input buf =
  let all = Mstruct.to_cstruct buf in
  let offset = Mstruct.offset buf in
  let total_length = Mstruct.length buf in
  let header = Mstruct.get_string buf 4 in
  if header <> "DIRC" then
    Mstruct.parse_error_buf buf "%s: wrong cache header." header;
  let version = Mstruct.get_be_uint32 buf in
  if version <> 2l then
    failwith (Printf.sprintf "Only cache version 2 is supported (%ld)" version);
  let n = Mstruct.get_be_uint32 buf in
  Log.debug "input: %ld entries (%db)" n (Mstruct.length buf);
  let entries =
    let rec loop acc n =
      if n = 0l then List.rev acc
      else
        let entry = input_entry buf in
        loop (entry :: acc) Int32.(sub n 1l) in
    loop [] n in
  let extensions = input_extensions buf in
  let length = Mstruct.offset buf - offset in
  if length <> total_length - 20 then (
    eprintf "Cache.input: more data to read! (total:%d current:%d)"
      (total_length - 20) length;
    failwith "Cache.input"
  );
  let actual_checksum =
    Cstruct.sub all offset length
    |> SHA.of_cstruct
  in
  let checksum = SHA.input buf in
  if actual_checksum <> checksum then (
    eprintf "Cach.input: wrong checksum";
    failwith "Cache.input"
  );
  { entries; extensions }

let add buf t =
  let str = Misc.with_buffer (fun buf ->
      let n = List.length t.entries in
      Log.debug "add %d entries" n;
      let header = Cstruct.create 12 in
      Mstruct.with_mstruct header (fun header ->
          Mstruct.set_string header "DIRC";
          Mstruct.set_be_uint32 header 2l;
          Mstruct.set_be_uint32 header (Int32.of_int n);
        );
      Buffer.add_string buf (Cstruct.to_string header);
      List.iter (add_entry buf) t.entries;
    ) in
  let sha1 = SHA.of_string str in
  Buffer.add_string buf str;
  Buffer.add_string buf (SHA.to_raw sha1)
