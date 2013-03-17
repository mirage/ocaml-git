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

type dbg = {
  file : File.Name.t;
  trace: string list;
}

let create_dbg file = { file; trace = [] }

let update_dbg dbg t = { dbg with trace = t :: dbg.trace }

type buffer = {
  dbg: dbg;
  buf: string;
  off: int;
  len: int;
}

let file b = b.dbg.file

let dump_buffer buf =
  Printf.printf "-- %s --\n%S\n\n\n"
    (String.concat ":" (List.rev buf.dbg.trace))
    (String.sub buf.buf buf.off buf.len)

let parse_error buf fmt =
  let file = buf.dbg.file in
  let trace = buf.dbg.trace in
  Printf.kprintf (fun msg ->
    Printf.eprintf
      "File %s\n\
       Parse error[%s]:%s\n\
       ---\n\
       %S\n\
       ---\n"
      (File.Name.to_string file)
      (String.concat ":" (List.rev trace))
      msg
      (try String.sub buf.buf buf.off buf.len
      with _ -> "[..]");
    raise Parsing.Parse_error
  ) fmt

let buffer file str = {
  dbg = create_dbg file;
  buf = str;
  off = 0;
  len = String.length str;
}

let update buffer t =
  { buffer with dbg = update_dbg buffer.dbg t }

type 'a result = buffer * 'a

let string_of_buffer b =
  Printf.sprintf "[off=%d len=%d]\n%s" b.off b.len b.buf

let shift buf n =
  if n > buf.len then parse_error buf "shift(%d)" n;
  { buf with off = buf.off + n ;
             len = buf.len - n }

let sub buf off len =
  try String.sub buf.buf off len
  with _ -> parse_error buf "sub(off:%d len:%d)" off len

let index_from buf off c =
  if off < buf.off then
    parse_error buf "index %d out of bounds" off;
  let i = String.index_from buf.buf off c in
  if i <= buf.off + buf.len then i
  else raise Not_found

let input_string buf len =
  shift buf len, sub buf buf.off len

let subview buf c =
  let i =
    try String.index_from buf.buf buf.off c
    with Not_found -> parse_error buf "%S invalid delimiter" (String.make 1 c) in
  { buf with len = i - buf.off }

let with_subview buf0 c fn =
  let buf1 = subview buf0 c in
  let buf2, s = fn buf1 in
  if buf2.len <> 0 then
    parse_error buf2 "imcomplete parsing of subview";
  shift buf0 (buf1.len + 1), s

let input_delim buf c =
  with_subview buf c (fun buf -> input_string buf buf.len)

(* Strings *)

let to_string output t =
  let b = Buffer.create 1024 in
  output b t;
  Buffer.contents b

let of_string input file str =
  let _, r = input (buffer file str) in
  r

