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

type t = Git_packed_value.pic list

val keys: t -> Git_hash.Set.t

include Git_s.S with type t := t

type raw

module Raw: sig
  include Git_s.S with type t = raw
  val index: t -> Git_pack_index.Raw.t
  val name: t -> Git_hash.t
  val keys: t -> Git_hash.Set.t
  val buffer: t -> Cstruct.t
  val shallow: t -> bool
  val input_header: Mstruct.t -> [`Version of int] * [`Count of int]
end

module type IO = sig
  include Git_s.S with type t = t
  val add: ?level:int -> t -> Git_pack_index.raw * Cstruct.t
  val input: ?progress:(string -> unit) ->
    index:Git_pack_index.f -> keys:Git_hash.Set.t -> read:Git_value.read_inflated ->
    Mstruct.t -> t Lwt.t
  val read: t -> Git_hash.t -> Git_value.t option
  val read_exn: t -> Git_hash.t -> Git_value.t
  val create: (Git_hash.t * Git_value.t) list -> t

  module Raw: sig
    include Git_s.S with type t = raw
    val add: [`Not_defined]
    val input: ?progress:(string -> unit) -> read:Git_value.read_inflated ->
      Mstruct.t -> t Lwt.t
    val unpack: ?progress:(string -> unit) -> write:Git_value.write_inflated ->
      t -> Git_hash.Set.t Lwt.t
    val read: index:Git_pack_index.f ->
      read:Git_value.read_inflated -> write:Git_value.write_inflated ->
      Mstruct.t -> Git_hash.t -> Git_value.t option Lwt.t
    val read_inflated: index:Git_pack_index.f ->
      read:Git_value.read_inflated -> write:Git_value.write_inflated ->
      Mstruct.t -> Git_hash.t -> string option Lwt.t
    val size: index:Git_pack_index.f -> Mstruct.t -> Git_hash.t -> int option Lwt.t
  end

  type raw = Raw.t
  val of_raw: ?progress:(string -> unit) -> Raw.t -> t Lwt.t
  val to_raw: t -> Raw.t
end

module IO (D: Git_hash.DIGEST) (I: Git_inflate.S): IO
