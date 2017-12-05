(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

module String = Buffer

type t =
  { mutable buf : Cstruct.t
  ; mutable pos : int
  ; mutable len : int
  ; init        : Cstruct.t }

type raw = string
type fixe = Cstruct.t

let create n =
  let n = if n < 1 then 1 else n in
  let n = if n > Sys.max_string_length then Sys.max_string_length else n in
  let buf = Cstruct.create n in
  { buf; pos = 0; len = n; init = buf; }

let contents { buf; pos; _ } =
  Cstruct.copy buf 0 pos

let unsafe_contents { buf; pos; _ } =
  Cstruct.sub buf 0 pos

let has { pos; _ } = pos

let resize t more =
  let len = t.len in
  let new_len = ref len in

  while t.pos + more > !new_len do new_len := 2 * !new_len done;

  if !new_len > Sys.max_string_length
  then begin
    if t.pos + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Cstruct_buffer.add: cannot grow buffer"
  end;

  let new_buffer = Cstruct.create !new_len in
  Cstruct.blit t.buf 0 new_buffer 0 t.pos;
  t.buf <- new_buffer;
  t.len <- !new_len

let add t fixe =
  let len = Cstruct.len fixe in
  let new_pos = t.pos + len in

  if new_pos > t.len then resize t len;
  Cstruct.blit fixe 0 t.buf t.pos len;
  t.pos <- new_pos

let clear t = t.pos <- 0
let reset t =
  t.pos <- 0;
  t.buf <- t.init;
  t.len <- Cstruct.len t.init
