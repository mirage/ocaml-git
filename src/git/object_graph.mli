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


module Make (S : Store.S
             with type Hash.Digest.buffer = Cstruct.t
              and type Hash.hex = string) :
sig
  module Store : Store.S

  module K : Graph.Sig.I with type V.t = Store.Hash.t

  val keys : K.t -> Store.Hash.t list
  val of_keys : Store.t -> K.t Lwt.t
  val closure : ?full:bool -> Store.t -> min:Store.Hash.Set.t -> max:Store.Hash.Set.t -> K.t Lwt.t
  val pack : Store.t -> min:Store.Hash.Set.t -> max:Store.Hash.Set.t -> (Store.Hash.t * Store.Value.t) list Lwt.t
  val to_dot : Store.t -> Buffer.t -> unit Lwt.t
end with module Store = S
