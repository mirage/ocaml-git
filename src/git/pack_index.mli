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

type t
type f = Hash.t -> int option
type raw = {
  offsets      : int Hash.Map.t;
  crcs         : int32 Hash.Map.t;
  pack_checksum: Hash.t;
}

module Raw: sig
  include S.S with type t = raw
  val keys: t -> Hash.Set.t
  val find_offset: t -> Hash.t -> int option
  val lengths: t -> int option Hash.Map.t
end

module Make (D: Hash.DIGEST): sig
  val input: ?cache_size:int -> Cstruct.buffer -> t
  val find_offset: t -> Hash.t -> int option
  val mem: t -> Hash.t -> bool
  val keys: t -> Hash.t list
  module Raw: S.IO with type t = raw
end
