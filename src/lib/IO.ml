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

(* Input helper functions *)

module Debug = struct

  type t = {
    file : File.Name.t;
    stack: string list;
  }

  let create file = { file; stack = [] }

  let push t s = { t with stack = s :: t.stack }

  let pop t = match t.stack with
    | []       -> t
    | _::stack -> { t with stack }

  let dump t =
    Printf.eprintf
      "File       : %s\n\
       Stack      : [%s]\n"
      (File.Name.to_string t.file)
      (String.concat ":" (List.rev t.stack))

end

module B = struct

  (* same type as Core.Bigstring.t *)
  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let sub buf off len =
    let s = String.create len in
    (* XXX: very inefficient *)
    for i = 0 to len-1 do
      s.[i] <- buf.{off+i}
    done;
    s

  let length buf =
    Bigarray.Array1.dim buf

  let to_string buf =
    sub buf 0 (length buf)

  let index_from buf off c =
    let n = length buf in
    let rec aux i =
      if i >= n then None
      else if buf.{i} = c then Some i
      else aux (i+1) in
    aux off

  let map_file file =
    let fd = Unix.openfile (File.Name.to_string file) [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o644 in
    let stats = Unix.stat (File.Name.to_string file) in
    let len = stats.Unix.st_size in
    (Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false len, len)

  let dump buf off len =
    let size = min (length buf - off) len in
    Printf.eprintf
      "Offset     : %d\n\
       Length     : %d\n\
       Full-length: %d\n\
       Size       : %d\n"
      off len (length buf) size;
    Printf.eprintf "%S\n" (sub buf off size)

  let of_string s =
    let len = String.length s in
    let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
    (* XXX: very ineficient *)
    for i = 0 to len-1 do
      buf.{i} <- s.[i]
    done;
    buf


end

type buffer = {
  buf: B.t;
  dbg: Debug.t;
  off: int ref;
  len: int ref;
}

let file b = b.dbg.Debug.file

let of_file file =
  let buf, len = B.map_file file in
  {
    dbg = Debug.create file;
    buf;
    off = ref 0;
    len = ref len;
  }

let of_string file string =
  let len = String.length string in
  let buf = B.of_string string in
  {
    dbg = Debug.create file;
    buf;
    off = ref 0;
    len = ref len;
  }

let dump buf =
  Debug.dump buf.dbg;
  B.dump buf.buf !(buf.off) !(buf.len)

let length buf =
  !(buf.len)

let clone buf =
  { buf with len = ref !(buf.len);
             off = ref !(buf.off) }

let parse_error buf fmt =
  Printf.kprintf (fun msg ->
    Debug.dump buf.dbg;
    Printf.eprintf "Parse error:%s\n" msg;
    B.dump buf.buf !(buf.off) !(buf.len);
    raise Parsing.Parse_error
  ) fmt

let push buf s =
  { buf with dbg = Debug.push buf.dbg s }

let string_of_buffer b =
  Printf.sprintf "[off=%d len=%d]\n%s" !(b.off) !(b.len) (B.to_string b.buf)

let check buf msg =
  if !(buf.off) + !(buf.len) > B.length buf.buf then
    parse_error buf "inconsistent buffer: %s" msg

let shift buf n =
  if n > !(buf.len)     (* positive shift *)
  || n + !(buf.off) < 0 (* negative shift *)
  then parse_error buf "shift(%d)" n;
  buf.off := !(buf.off) + n;
  buf.len := !(buf.len) - n;
  check buf "shift"

let sub_string buf off len =
  let error msg =
    parse_error buf
      "sub_string(b-off:%d b-len:%d off:%d len:%d): %s"
      !(buf.off) !(buf.len) off len msg in
  if off < 0
  || len < 0
  || off + len > !(buf.len) then
    error "bounds error";
  try B.sub buf.buf (!(buf.off) + off) len
  with _ -> error "sub-string"

let index buf c =
  match B.index_from buf.buf !(buf.off) c with
  | None   -> None
  | Some i ->
    if i <= !(buf.off) + !(buf.len) then Some (i - !(buf.off))
    else None

let input_string buf len =
  let len = match len with
    | Some l -> l
    | None   -> !(buf.len) in
  let str = sub_string buf 0 len in
  shift buf len;
  str

let input_byte buf =
  let byte = int_of_char buf.buf.{!(buf.off)} in
  shift buf 1;
  byte

let input_be_int32 buf =
  let i = EndianBigstring.BigEndian.get_int32 buf.buf !(buf.off) in
  shift buf 4;
  i

let input_be_int64 buf =
  let i = EndianBigstring.BigEndian.get_int64 buf.buf !(buf.off) in
  shift buf 8;
  i

let sub buf off len =
  if off < 0
  || len > !(buf.len) then
    parse_error buf "subview (off=%d len=%d)" off len;
  let buf = {
    buf with dbg = buf.dbg;
             off = ref ( !(buf.off) + off );
             len = ref len } in
  check buf "sub";
  buf

let with_delim buf c =
  match index buf c with
  | None   -> None
  | Some i ->
    Some (sub buf 0 i)

let input_delim buf0 c fn =
  match with_delim buf0 c with
  | None      -> None
  | Some buf1 ->
    let len = !(buf1.len) in
    let s = fn buf1 in
    shift buf0 (len + 1);
    Some s

let input_string_delim buf c =
  input_delim buf c (fun buf -> input_string buf None)

let inflate buf =
  let deflated = input_string buf None in
  let inflated = Misc.inflate_string deflated in
  of_string (file buf) inflated

let deflate buf =
  let inflated = input_string buf None in
  let deflated = Misc.inflate_string inflated in
  of_string (file buf) deflated
