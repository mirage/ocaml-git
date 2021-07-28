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

let src = Logs.Src.create "git.blob" ~doc:"logs git's blob event"

module Log = (val Logs.src_log src : Logs.LOG)

type t = Cstruct.t

external of_cstruct : Cstruct.t -> t = "%identity"
external to_cstruct : t -> Cstruct.t = "%identity"

let of_string x : t = Cstruct.of_string x
let to_string (x : t) = Cstruct.to_string x
let length : t -> int64 = fun t -> Int64.of_int (Cstruct.length t)
let pp ppf blob = Fmt.string ppf (Cstruct.to_string blob)
let equal = Cstruct.equal
let compare = Cstruct.compare
let hash = Hashtbl.hash

module type S = sig
  type hash
  type nonrec t = t

  include S.DIGEST with type t := t and type hash := hash
  include S.BASE with type t := t

  val length : t -> int64
  val of_cstruct : Cstruct.t -> t
  val to_cstruct : t -> Cstruct.t
  val of_string : string -> t
  val to_string : t -> string
end

module Make (Hash : S.HASH) = struct
  type hash = Hash.t
  type nonrec t = t

  external of_cstruct : Cstruct.t -> t = "%identity"
  external to_cstruct : t -> Cstruct.t = "%identity"

  let of_string x : t = Cstruct.of_string x
  let to_string (x : t) = Cstruct.to_string x
  let length : t -> int64 = fun t -> Int64.of_int (Cstruct.length t)

  let digest cs =
    let ctx = Hash.init () in
    let hdr = Fmt.str "blob %Ld\000" (length cs) in
    let ctx = Hash.feed_string ctx hdr in
    let ctx = Hash.feed_cstruct ctx cs in
    Hash.get ctx

  let pp ppf blob = Fmt.string ppf (Cstruct.to_string blob)
  let equal = Cstruct.equal
  let compare = Cstruct.compare
  let hash = Hashtbl.hash

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
