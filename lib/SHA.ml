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

module Log = Log.Make(struct let section = "sha1" end)

module type S = sig
  include Object.S
  val of_string: string -> t
  val of_cstruct: Cstruct.t -> t
  val to_raw: t -> string
  val of_raw: string -> t
  val to_hex: t -> string
  val of_hex: ?strict:bool -> string -> t
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

type sha_t = {
  raw   : string;
  padded: bool;   (* for hex of odd length *)
}

exception Ambiguous of string

module SHA1_String = struct

  type t = sha_t

  let length x = (* 0 <= length <= 40 *)
    let n = (String.length x.raw) * 2 in
    if x.padded then n - 1 else n

  let to_hex t =
    let `Hex h = Hex.of_string t.raw in
    if t.padded then String.sub h 0 ((String.length h) - 1)
    else h

  let is_short x = length x < 40
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
    let np = length p in
    let nx = length x in
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

  let of_string str =
    Cstruct.of_string str
    |> Nocrypto.Hash.SHA1.digest
    |> Cstruct.to_string
    |> fun x -> { raw=x; padded=false; }

  let of_cstruct c =
    Nocrypto.Hash.SHA1.digest c
    |> Cstruct.to_string
    |> fun x -> { raw=x; padded=false; }

  let of_hex ?(strict=true) h =
    let len = String.length h in
    if strict && len <> 40 then raise (Ambiguous h);
    let to_be_padded = (len mod 2) = 1 in
    let h' = if to_be_padded then h ^ "0" else h in
    { raw = Hex.to_string (`Hex h'); padded = to_be_padded; }

  let zero = of_hex (String.make 40 '0')
  let pretty = to_hex
  let pp ppf t = Format.fprintf ppf "%s" (pretty t)
  let input buf = { raw=Mstruct.get_string buf 20; padded=false; }
  let add buf ?level:_ t = Buffer.add_string buf t.raw
  let input_hex buf = of_hex (Mstruct.get_string buf (Mstruct.length buf))
  let add_hex buf t = Buffer.add_string buf (to_hex t)

  module X = struct
    type t = sha_t
    let compare = compare
    let pretty = pretty
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

module Array = struct

  let get buf n =
    let buf = Cstruct.sub buf (n * 20) 20 in
    input (Mstruct.of_cstruct buf)

  let ambiguous t = raise (Ambiguous (to_hex t))

  let only_one sha1 = function
    | []  -> None
    | [x] -> Some x
    | _   -> ambiguous sha1

  let length buf = Cstruct.len buf / 20

  let to_list t =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (get t n :: acc) (n-1)
    in
    loop [] (length t)

  let linear_search buf sha1 =
    let len = length buf in
    let short_sha = is_short sha1 in
    let rec aux acc = function
      | 0 -> only_one sha1 acc
      | i ->
        let off = len - i in
        let s = get buf off in
        if equal s sha1 then (assert (acc = []); Some off)
        else if (not short_sha) then aux acc (len - 1)
        else
          let acc = if is_prefix sha1 s then off :: acc else acc in
          aux acc (len - 1)
    in
    aux [] len

  let sub buf off len = Cstruct.sub buf (off * 20) (len * 20)

  let (++) x y = match y with
    | None   -> None
    | Some y -> Some (x + y)

  let binary_search buf sha1 =
    let short_sha = is_short sha1 in
    let scan_thresh = 8 in
    let rec aux buf =
      let len = length buf in
      if len <= scan_thresh then
        linear_search buf sha1
      else
        let p = len / 2 in
        let s = get buf p in
        if equal s sha1 then Some p
        else if short_sha && is_prefix sha1 s then
          let is_prefix_of n = is_prefix sha1 (get buf n) in
          let prev_ok = p = 0      || not (is_prefix_of (p-1)) in
          let next_ok = p >= len-1 || not (is_prefix_of (p+1)) in
          if prev_ok && next_ok then Some p
          else ambiguous sha1
        else if lt sha1 s then
          aux (sub buf 0 p)
        else
          p ++ aux (sub buf p (len-p-1))
    in
    aux buf

end
