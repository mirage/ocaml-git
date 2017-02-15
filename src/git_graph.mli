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

module K: Graph.Sig.I with type V.t = Git_hash.t

module Make (S: Git_store.S): sig
  val keys: K.t -> Git_hash.t list
  val of_keys: S.t -> K.t Lwt.t
  val closure: ?full:bool -> S.t -> min:Git_hash.Set.t -> max:Git_hash.Set.t -> K.t Lwt.t
  val pack: S.t -> min:Git_hash.Set.t -> max:Git_hash.Set.t -> (Git_hash.t * Git_value.t) list Lwt.t
  val to_dot: S.t -> Buffer.t -> unit Lwt.t
end
