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

module Log = (val Misc.src_log "inflate" : Logs.LOG)

module type S = sig
  val inflate: ?output_size:int -> Mstruct.t -> Mstruct.t option
  val deflate: ?level:int -> Cstruct.t -> Cstruct.t
end

module None = struct

  let inflate ?output_size buf =
    match output_size with
    | None   -> Some buf
    | Some n ->
      if Mstruct.length buf < n then None else Some (Mstruct.sub buf 0 n)

  let deflate ?level:_ buf = buf

end

module M = struct
  open Decompress

  let size_buffer   = 0x8000
  let input_buffer  = Bytes.create size_buffer
  let output_buffer = Bytes.create size_buffer
  let window = Window.create ~proof:B.proof_bytes

  exception Deflate_error of Deflate.error

  let deflate ?(level = 4) buff =
    let pos = ref 0 in
    let res = Buffer.create (Cstruct.len buff) in

    Deflate.bytes
      input_buffer output_buffer
      (fun input_buffer -> function
      | Some max ->
        let n = min max (min size_buffer (Cstruct.len buff - !pos)) in
        Cstruct.blit_to_bytes buff !pos input_buffer 0 n;
        pos := !pos + n;
        n
      | None ->
        let n = min size_buffer (Cstruct.len buff - !pos) in
        Cstruct.blit_to_bytes buff !pos input_buffer 0 n;
        pos := !pos + n;
        n)
      (fun output_buffer len ->
      Buffer.add_subbytes res output_buffer 0 len;
      size_buffer)
      (Deflate.default ~proof:B.proof_bytes level)
    |> function
    | Ok _ -> Cstruct.of_string @@ Buffer.contents res
    | Error exn -> raise (Deflate_error exn)

  let inflate ?output_size orig =
    let res = Buffer.create (match output_size with Some len -> len | None -> size_buffer) in

    let open Inflate in

    let rec loop ~refill:rest t =
      match eval (B.from_bytes input_buffer) (B.from_bytes output_buffer) t with
      | `Await t ->
        Mstruct.shift orig (used_in t - rest);

        let len = min size_buffer (Mstruct.length orig) in

        (match Mstruct.pick_string orig len with
        | Some str ->
          Bytes.blit_string str 0 input_buffer 0 len;
          loop ~refill:0 (refill 0 len t)
        | None -> failwith "Inflate.inflate")
      | `Flush t ->
        Mstruct.shift orig (used_in t - rest);

        Buffer.add_subbytes res output_buffer 0 (used_out t);
        loop ~refill:(used_in t) (flush 0 size_buffer t)
      | `Error _ -> None
      | `End t ->
        Mstruct.shift orig (used_in t - rest);

        if used_out t <> 0
        then begin
          Buffer.add_subbytes res output_buffer 0 (used_out t);
          Some (Mstruct.of_string (Buffer.contents res))
        end else Some (Mstruct.of_string (Buffer.contents res))
    in

    loop ~refill:0 (default (Window.reset window))
end

module type ZLIB = sig

  exception Error of string * string

  val compress:
    ?level: int -> ?header: bool ->
    (bytes -> int) -> (bytes -> int -> unit) -> unit

  type stream

  type flush_command =
      Z_NO_FLUSH
    | Z_SYNC_FLUSH
    | Z_FULL_FLUSH
    | Z_FINISH

  val inflate_init: bool -> stream
  val inflate:
    stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
    -> bool * int * int
  val inflate_end: stream -> unit

end

module Make (Zlib: ZLIB) = struct

  module Zlib_ext = struct (* From [camlzip] *)

    (***********************************************************************)
    (*                                                                     *)
    (*                         The CamlZip library                         *)
    (*                                                                     *)
    (*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
    (*                                                                     *)
    (*  Copyright 2001 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed    *)
    (*  under the terms of the GNU Lesser General Public License, with     *)
    (*  the special exception on linking described in file LICENSE.        *)
    (*                                                                     *)
    (***********************************************************************)

    let buffer_size = 1024
    let uncompress ?(header = true) incr_used_in refill flush =
      let inbuf = Bytes.create buffer_size
      and outbuf = Bytes.create buffer_size in
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
    let n = Cstruct.len input in
    let toread = ref n in
    fun buf ->
      let m = min !toread (Bytes.length buf) in
      Cstruct.blit_to_bytes input (n - !toread) buf 0 m;
      toread := !toread - m;
      m

  let flush output buf len =
    Buffer.add_subbytes output buf 0 len

  let deflate ?level input =
    let output = Buffer.create (Cstruct.len input) in
    Zlib.compress ?level (refill input) (flush output);
    Cstruct.of_string (Buffer.contents output)

  let inflate_exn ?output_size orig_buf =
    let buf = Mstruct.clone orig_buf in
    let osz =
      match output_size with
      | None -> Mstruct.length orig_buf
      | Some sz -> sz
    in
    let output = Buffer.create osz in
    let refill input =
      let n = min (Mstruct.length buf) (Bytes.length input) in
      let s = Mstruct.get_string buf n in
      String.blit s 0 input 0 n;
      n in
    let flush buf len =
      Buffer.add_subbytes output buf 0 len in
    let size = uncompress_with_size refill flush in
    Mstruct.shift orig_buf size;
    Mstruct.of_string (Buffer.contents output)

  let inflate ?output_size orig_buf =
    try Some (inflate_exn ?output_size orig_buf)
    with Zlib.Error (s,t) ->
      Log.err (fun l -> l "inflate: error %s %s" s t);
      None

end
