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
module Log = Log.Make(struct let section = "misc" end)

(* From OCaml's stdlib. See [Digest.to_hex] *)
let hex_encode s =
  let n = String.length s in
  let result = String.create (n*2) in
  for i = 0 to n-1 do
    String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result

(* From OCaml's stdlib. See [Digest.from_hex] *)
let hex_decode h =
  let n = String.length h in
  if n mod 2 <> 0 then (
    let msg =
      Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
    raise (Invalid_argument msg)
  );
  let digit c =
    match c with
    | '0'..'9' -> Char.to_int c - Char.to_int '0'
    | 'A'..'F' -> Char.to_int c - Char.to_int 'A' + 10
    | 'a'..'f' -> Char.to_int c - Char.to_int 'a' + 10
    | c ->
      let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
      raise (Invalid_argument msg) in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = String.create (n / 2) in
  for i = 0 to n/2 - 1 do
    result.[i] <- Char.of_int_exn (byte (2 * i));
  done;
  result

(* From Zlib *)
module Zlib_ext = struct

  let buffer_size = 1024
  let uncompress ?(header = true) incr_used_in refill flush =
    let inbuf = String.create buffer_size
    and outbuf = String.create buffer_size in
    let zs = Zlib.inflate_init header in
    let rec uncompr inpos inavail =
      if inavail = 0 then begin
        let incount = refill inbuf in
        if incount = 0 then uncompr_finish true else uncompr 0 incount
      end else begin
        let (finished, used_in, used_out) =
          Zlib.inflate zs inbuf inpos inavail outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
        incr_used_in used_in;
        flush outbuf used_out;
        if not finished then uncompr (inpos + used_in) (inavail - used_in)
      end
    and uncompr_finish first_finish =
      (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
         after the compressed stream in order to complete decompression
         and return finished = true. *)
      let dummy_byte = if first_finish && not header then 1 else 0 in
      let (finished, used_in, used_out) =
        Zlib.inflate zs inbuf 0 dummy_byte outbuf 0 buffer_size Zlib.Z_SYNC_FLUSH in
      incr_used_in used_in;
      flush outbuf used_out;
      if not finished then uncompr_finish false
    in
    uncompr 0 0;
    Zlib.inflate_end zs

end

let uncompress_with_size ?header refill flush =
  let used_in = ref 0 in
  let incr_used_in n =
    used_in := !used_in + n in
  Zlib_ext.uncompress ?header incr_used_in refill flush;
  !used_in

let refill input =
  let n = Bigstring.length input in
  let toread = ref n in
  fun buf ->
    let m = min !toread (String.length buf) in
    Bigstring.To_string.blit input (n - !toread) buf 0 m;
    toread := !toread - m;
    m

let flush output buf len =
  Bigbuffer.add_substring output buf 0 len

let buffer_contents buf =
  let len = Bigbuffer.length buf in
  Bigstring.sub_shared ~len (Bigbuffer.volatile_contents buf)

let bigstring_concat buffers =
  let buf = Bigbuffer.create 1024 in
  List.iter ~f:(fun b ->
      Bigbuffer.add_string buf (Bigstring.to_string b)
    ) buffers;
  buffer_contents buf

let deflate_bigstring input =
  let output = Bigbuffer.create (Bigstring.length input) in
  Zlib.compress (refill input) (flush output);
  buffer_contents output

let deflate_mstruct buf =
  let inflated = Mstruct.to_bigarray buf in
  let deflated = deflate_bigstring inflated in
  Mstruct.of_bigarray deflated

let inflate_mstruct orig_buf =
  let buf = Mstruct.clone orig_buf in
  let output = Bigbuffer.create (Mstruct.length orig_buf) in
  let refill input =
    let n = min (Mstruct.length buf) (String.length input) in
    let s = Mstruct.get_string buf n in
    String.blit s 0 input 0 n;
    n in
  let flush buf len =
    Bigbuffer.add_substring output buf 0 len in
  let size = uncompress_with_size refill flush in
  let inflated = buffer_contents output in
  let res = Mstruct.of_bigarray inflated in
  Mstruct.shift orig_buf size;
  res

let inflate_bigstring str =
  let buf = inflate_mstruct (Mstruct.of_bigarray str) in
  Mstruct.to_bigarray buf

let crc32 str =
  (* XXX: use ocaml-crc ? *)
  Zlib.update_crc 0l str 0 (String.length str)


let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'
let lt  = '<'
let gt  = '>'

let input_key_value buf ~key:expected input_value =
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

let str_buffer = String.create 4
let add_be_uint32 buf i =
  EndianString.BigEndian.set_int32 str_buffer 0 i;
  Bigbuffer.add_string buf str_buffer

let buf = Bigbuffer.create 1024

let with_bigbuffer fn =
  Bigbuffer.reset buf;
  fn buf;
  Bigbuffer.big_contents buf

let with_buffer fn =
  Bigbuffer.reset buf;
  fn buf;
  Bigbuffer.contents buf

open Lwt

let rec list_iter_p ?(width=50) f l =
  match List.split_n l width with
  | [], [] -> return_unit
  | h , t  ->
    Lwt_list.iter_p f h >>= fun () ->
    list_iter_p ~width f t

let list_map_p ?(width=50) f l =
  let res = ref [] in
  list_iter_p ~width (fun x ->
      f x >>= fun y ->
      res := y :: !res;
      return_unit
    ) l
  >>= fun () ->
  return !res

module OP = struct

  let (/) = Filename.concat

end

let map_rev_find map d =
  let r = ref None in
  try
    Map.iter
      ~f:(fun ~key ~data -> if data = d then (r := Some key; raise Exit))
      map;
    None
  with Exit ->
    !r
