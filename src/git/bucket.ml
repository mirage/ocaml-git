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

type 'a t =
  { bulk        : 'a option array
  ; mutable idx : int
  ; len         : int }

let make len =
  let bulk = Array.init len (fun _ -> None) in
  { bulk
  ; idx = 0
  ; len }

let add t obj =
  Array.set t.bulk t.idx (Some obj); (* replace *)
  t.idx <- (t.idx + 1) mod t.len

let iter t f =
  Array.iter (function Some obj -> f obj | None -> ()) t.bulk

exception Find

let find t predicate =
  let obj = ref None in
  try
    iter t
      (fun x ->
        if predicate x
        then begin
          obj := Some x;
          raise Find
        end);
    None
  with Find -> !obj
