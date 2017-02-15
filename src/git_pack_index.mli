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
type f = Git_hash.t -> int option
type raw = {
  offsets      : int Git_hash.Map.t;
  crcs         : int32 Git_hash.Map.t;
  pack_checksum: Git_hash.t;
}

module Raw: sig
  include Git_s.S with type t = raw
  val keys: t -> Git_hash.Set.t
  val find_offset: t -> Git_hash.t -> int option
  val lengths: t -> int option Git_hash.Map.t
end

module Make (D: Git_hash.DIGEST): sig
  val input: ?cache_size:int -> Cstruct.buffer -> t
  val find_offset: t -> Git_hash.t -> int option
  val mem: t -> Git_hash.t -> bool
  val keys: t -> Git_hash.t list
  module Raw: Git_s.IO with type t = raw
end
