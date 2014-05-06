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
module Log = Log.Make(struct let section = "sha1" end)

module type S = sig
  include Object.S
  val create: string -> t
  val to_hex: t -> string
  val of_hex: string -> t
  val input_hex: Mstruct.t -> t
  val add_hex: Bigbuffer.t -> t -> unit
  val zero: t
end

module SHA1_String = struct

  include (String: Identifiable.S)

  let zero = of_string (String.make 20 '0')

  (* XXX: add the bigstring in chunks using a tmp buffer (to alloc only
     once). *)
  let create str =
    let hash = Cryptokit.Hash.sha1 () in
    hash#add_string str;
    of_string hash#result

  let to_hex t =
    Misc.hex_encode (to_string t)

  let of_hex h =
    of_string (Misc.hex_decode h)

  let sexp_of_t t =
    Sexplib.Sexp.Atom (to_hex t)

  let t_of_sexp s =
    of_hex (Sexplib.Conv.string_of_sexp s)

  let pretty = to_hex

  let input buf =
    Mstruct.get_string buf 20
    |> of_string

  let add buf t =
    Bigbuffer.add_string buf (to_string t)

  let input_hex buf =
    of_hex (Mstruct.get_string buf (Mstruct.length buf))

  let add_hex buf t =
    Bigbuffer.add_string buf (to_hex t)

end

include (SHA1_String: S)

module Commit: S = SHA1_String
module Tree: S = SHA1_String
module Blob: S = SHA1_String

let of_commit c = of_string (Commit.to_string c)
let to_commit n = Commit.of_string (to_string n)
let of_tree t = of_string (Tree.to_string t)
let to_tree n = Tree.of_string (to_string n)
let of_blob b = of_string (Blob.to_string b)
let to_blob n = Blob.of_string (to_string n)
