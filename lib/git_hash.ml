(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = (val Git_misc.src_log "hash": Logs.LOG)

module type S = sig
  include Git_s.S
  val to_raw: t -> string
  val of_raw: string -> t
  val to_hex: t -> string
  val hex_length: t -> int
  val lt: t -> t -> bool
  val is_prefix: t -> t -> bool
  module Set: Git_s.Set with type elt = t
  module Map: Git_s.Map with type key = t
end

type sha = {
  raw   : string;
  padded: bool;   (* for hex of odd length *)
}

exception Ambiguous of string

module Hash_string = struct

  type t = sha

  let hex_length x =
    let n = (String.length x.raw) * 2 in
    if x.padded then n - 1 else n

  let to_hex t =
    let `Hex h = Hex.of_string t.raw in
    if t.padded then String.sub h 0 ((String.length h) - 1)
    else h

  let equal x y = x.padded = y.padded && String.compare x.raw y.raw = 0
  let hash x = Hashtbl.hash x.raw
  let ambiguous t = raise (Ambiguous (to_hex t))
  let get_upper c = (Char.code c) land 0xf0

  let compare x y =
    let nx = String.length x.raw in
    let ny = String.length y.raw in
    if nx = ny && not x.padded && not y.padded then String.compare x.raw y.raw
    else
      let len = min nx ny in
      let short = if nx < ny then x else y in
      let rec scan i =
        if i = len then ambiguous short
        else if (x.padded && y.padded) || i < len then
          let x0 = x.raw.[i] in
          let y0 = y.raw.[i] in
          if x0 < y0 then -1
          else if x0 > y0 then 1
          else scan (i + 1)
        else
          let x0 = get_upper x.raw.[i] in
          let y0 = get_upper y.raw.[i] in
          if x0 < y0 then -1
          else if x0 > y0 then 1
          else ambiguous short
      in
      scan 0

  let is_prefix p x =
    let np = hex_length p in
    let nx = hex_length x in
    if np > nx then false
    else if np = nx then equal p x
    else
      let n =
        if p.padded then String.length p.raw - 1
        else String.length p.raw
      in
      try
        for i = 0 to n - 1 do if p.raw.[i] <> x.raw.[i] then raise Exit done;
        if p.padded then get_upper p.raw.[n] = get_upper x.raw.[n] else true
      with
        Exit -> false

  let lt x y = compare x y < 0
  let to_raw x = x.raw
  let of_raw x = { raw=x; padded=false; }
  let pp ppf t = Format.fprintf ppf "%s" (to_hex t)

  module X = struct
    type t = sha
    let compare = compare
    let pp = pp
  end
  module Map = Git_misc.Map(X)
  module Set = Git_misc.Set(X)

end

include (Hash_string: S with type t = sha)
module Commit = Hash_string
module Tree = Hash_string
module Blob = Hash_string

module type H = sig
  include S
  include Git_s.IO with type t := t
  val of_hex: string -> t
  val of_short_hex: string -> t
  val input_hex: Mstruct.t -> t
  val add_hex: Buffer.t -> t -> unit
  val is_short: t -> bool
  val zero: t
end

module type IO = sig
  include H with type t = sha
  module Blob: H with type t = Blob.t
  module Tree: H with type t = Tree.t
  module Commit: H with type t = Commit.t
end

type 'a digest = 'a -> sha

module type DIGEST = sig
  val cstruct: Cstruct.t digest
  val string: string digest
  val length: int
end

module H (D: DIGEST) = struct

  include Hash_string

  let is_short x = hex_length x < D.length * 2

  let of_hex_aux ~strict h =
    let len = String.length h in
    if strict && len <> D.length * 2 then raise (Ambiguous h);
    let to_be_padded = (len mod 2) = 1 in
    let h' = if to_be_padded then h ^ "0" else h in
    { raw = Hex.to_string (`Hex h'); padded = to_be_padded; }

  let of_hex = of_hex_aux ~strict:true
  let of_short_hex = of_hex_aux ~strict:false
  let zero = of_hex (String.make (D.length * 2) '0')
  let input buf = { raw=Mstruct.get_string buf D.length; padded=false; }
  let add buf ?level:_ t = Buffer.add_string buf t.raw
  let input_hex buf = of_hex (Mstruct.get_string buf (Mstruct.length buf))
  let add_hex buf t = Buffer.add_string buf (to_hex t)

end

let of_blob b = of_raw (Blob.to_raw b)
let to_blob n = Blob.of_raw (to_raw n)
let of_commit c = of_raw (Commit.to_raw c)
let to_commit n = Commit.of_raw (to_raw n)
let of_tree t = of_raw (Tree.to_raw t)
let to_tree n = Tree.of_raw (to_raw n)

module IO (D: DIGEST) = struct
  include H(D)
  module Blob = H(D)
  module Tree = H(D)
  module Commit = H(D)
end

module Array (D: DIGEST) = struct

  module Hash = IO(D)

  let get buf n =
    let buf = Cstruct.sub buf (n * D.length) D.length in
    Hash.input (Mstruct.of_cstruct buf)

  let ambiguous t = raise (Ambiguous (to_hex t))

  let only_one h = function
    | []  -> None
    | [x] -> Some x
    | _   -> ambiguous h

  let length buf = Cstruct.len buf / D.length

  let to_list t =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (get t (n-1) :: acc) (n-1)
    in
    loop [] (length t)

  let linear_search buf h =
    let len = length buf in
    let short_sha = Hash.is_short h in
    let rec aux acc = function
      | 0 -> only_one h acc
      | i ->
        let off = len - i in
        let s = get buf off in
        if equal s h then (assert (acc = []); Some off)
        else if (not short_sha) then aux acc (i - 1)
        else
          let acc = if is_prefix h s then off :: acc else acc in
          aux acc (i - 1)
    in
    aux [] len

  let sub buf off len = Cstruct.sub buf (off * D.length) (len * D.length)

  let (++) x y = match y with
    | None   -> None
    | Some y -> Some (x + y)

  let binary_search buf h =
    let short_sha = Hash.is_short h in
    let scan_thresh = 4 in
    let rec aux buf =
      let len = length buf in
      if len < scan_thresh then
        linear_search buf h
      else
        let off = len / 2 in
        let s = get buf off in
        if equal s h then Some off
        else if short_sha && is_prefix h s then
          let is_prefix_of n = is_prefix h (get buf n) in
          let prev_ok = off = 0      || not (is_prefix_of (off-1)) in
          let next_ok = off >= len-1 || not (is_prefix_of (off+1)) in
          if prev_ok && next_ok then Some off
          else ambiguous h
        else if lt h s then aux (sub buf 0 off)
        else 1 + off ++ aux (sub buf (off+1) (len-off-1))
    in
    aux buf

end
