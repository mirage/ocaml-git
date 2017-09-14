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

module type KEY =
sig
  type t

  val compare : t -> t -> int
  val get     : t -> int -> char
end

module Make (Key : KEY) =
struct
  type 'a t =
    { fanout : (Key.t * 'a) list array }

  let make () =
    { fanout = Array.make 256 [] }

  (* XXX(dinosaure): if, one day, we find a bug about the
     serialization of the IDX file, may be it's about this function
     (stable/unstable sort). *)
  let merge lst key value =
    List.merge
      (fun (k1, _) (k2, _) -> Key.compare k1 k2)
      lst [ (key, value) ]

  let bind key value { fanout } =
    let idx = Char.code (Key.get key 0) in
    Array.set fanout idx (merge (Array.get fanout idx) key value)

  let length idx { fanout } =
    if idx < 256
    then List.length (Array.get fanout idx)
    else raise (Invalid_argument "Fanout.fanout")

  let get idx { fanout } =
    if idx < 256
    then Array.get fanout idx
    else raise (Invalid_argument "Fanout.get")
end
