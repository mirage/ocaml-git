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

open Sexplib.Std

module Log = Log.Make(struct let section = "sha1" end)

module type S = sig
  include Object.S
  val of_string: string -> t
  val of_cstruct: Cstruct.t -> t
  val to_raw: t -> string
  val of_raw: string -> t
  val to_hex: t -> string
  val of_hex: string -> t
  val input_hex: Mstruct.t -> t
  val add_hex: Buffer.t -> t -> unit
  val zero: t
  val length: t -> int
  val is_short: t -> bool
  val lt: t -> t -> bool
  val is_prefix: t -> t -> bool
  module Set: Misc.Set with type elt = t
  module Map: Misc.Map with type key = t
end

type sha_t = { raw    : string;
               padded : bool;   (* for hex of odd length *)
             }

exception Ambiguous

let sha_to_string t =
  let `Hex h = Hex.of_string t.raw in
  h^(if t.padded then "(PADDED)" else "")

let get_upper c = (Char.code c) land 0xf0

let sha_compare x y = 
  (*Log.debugf "sha_compare: %s vs %s" (sha_to_string x) (sha_to_string y);*)
  let nx = String.length x.raw in
  let ny = String.length y.raw in
  let res =
    if nx = ny && not x.padded && not y.padded then
      String.compare x.raw y.raw
    else begin
      let len = min nx ny in
      let rec scan i =
        if i = len then
          raise Ambiguous
        else
          if (x.padded && y.padded) || i < len then
            let x0 = x.raw.[i] in
            let y0 = y.raw.[i] in
            if x0 < y0 then
              -1
            else if x0 > y0 then
              1
            else
              scan (i + 1)
          else
            let x0 = get_upper x.raw.[i] in
            let y0 = get_upper y.raw.[i] in
            if x0 < y0 then
              -1
            else if x0 > y0 then
              1
            else
              raise Ambiguous
      in
      scan 0
    end
  in
  (*Log.debugf "sha_compare: result=%d" res;*)
  res

module SHA1_String = struct

  type t = sha_t

  let length x = (* 0 <= length <= 40 *)
    let n = (String.length x.raw) * 2 in
    if x.padded then
      n - 1
    else
      n

  let is_short x = (length x) < 40

  let equal x y = (x=y)

  let hash x = Hashtbl.hash x.raw

  let compare = sha_compare

  let is_prefix p x =
    let np = length p in
    let nx = length x in
    if np > nx then
      false
    else if np = nx then
      equal p x
    else 
      let n =
        if p.padded then
          (String.length p.raw) - 1
        else
          String.length p.raw
      in
      try
        for i = 0 to n - 1 do
          if p.raw.[i] <> x.raw.[i] then
            raise Exit
        done;
        if p.padded then
          (get_upper p.raw.[n]) = (get_upper x.raw.[n])
        else
          true
      with
        Exit -> false

  let lt x y = compare x y < 0

  let to_raw x = x.raw

  let of_raw x = { raw=x; padded=false; }

  let of_string str =
    Cstruct.of_string str
    |> Nocrypto.Hash.SHA1.digest
    |> Cstruct.to_string
    |> fun x -> { raw=x; padded=false; }

  let of_cstruct c =
    Nocrypto.Hash.SHA1.digest c
    |> Cstruct.to_string
    |> fun x -> { raw=x; padded=false; }

  let to_hex t =
    let `Hex h = Hex.of_string t.raw in
    if t.padded then
      String.sub h 0 ((String.length h) - 1)
    else
      h

  let of_hex h =
    let len = String.length h in
    let to_be_padded = (len mod 2) = 1 in
    let h' =
      if to_be_padded then
        h ^ "0"
      else
        h
    in
    { raw    = Hex.to_string (`Hex h');
      padded = to_be_padded;
    }

  let zero = of_hex (String.make 40 '0')

  let sexp_of_sha_t t =
    Sexplib.Sexp.Atom (to_hex t)

  let sexp_of_t = sexp_of_sha_t

  let sha_t_of_sexp s =
    of_hex (Sexplib.Conv.string_of_sexp s)

  let t_of_sexp = sha_t_of_sexp

  let pretty = to_hex

  let input buf =
    { raw=Mstruct.get_string buf 20; padded=false; }

  let add buf t =
    Buffer.add_string buf t.raw

  let input_hex buf =
    of_hex (Mstruct.get_string buf (Mstruct.length buf))

  let add_hex buf t =
    Buffer.add_string buf (to_hex t)

  module X = struct
    type t = sha_t with sexp
    let compare = sha_compare
  end
  module Map = Misc.Map(X)
  module Set = Misc.Set(X)
end

include (SHA1_String: S)

module Commit: S = SHA1_String
module Tree: S = SHA1_String
module Blob: S = SHA1_String

let of_commit c = of_raw (Commit.to_raw c)
let to_commit n = Commit.of_raw (to_raw n)
let of_tree t = of_raw (Tree.to_raw t)
let to_tree n = Tree.of_raw (to_raw n)
let of_blob b = of_raw (Blob.to_raw b)
let to_blob n = Blob.of_raw (to_raw n)
