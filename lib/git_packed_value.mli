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

type copy = { copy_offset: int; copy_length: int; }
type hunk = Insert of string | Copy of copy

type 'a delta = {
  source       : 'a;
  source_length: int;
  result_length: int;
  hunks        : hunk list;
}

type kind =
  | Raw_value of string
  | Ref_delta of Git_hash.t delta
  | Off_delta of int delta

type t = { kind: kind; offset: int }

val shallow: Git_hash.Set.t -> t -> bool
val create: offset:int -> kind:kind -> t
val kind: t -> kind
val offset: t -> int
val pp_kind: Format.formatter -> kind -> unit
val is_delta: t -> bool
val result_length: t -> int
val source_length: t -> int

include Git_s.S with type t := t

module PIC: sig

  type kind = Raw of string | Link of t delta

  and t = {
    kind   : kind;
    hash   : Git_hash.t;
    shallow: bool;
    mutable raw: string option;
  }

  include Git_s.S with type t := t

  val create: ?raw:string -> ?shallow:bool -> Git_hash.t -> kind -> t
  val of_raw: ?shallow:bool ->  Git_hash.t -> string -> t
  val kind: t -> kind
  val name: t -> Git_hash.t
  val raw: t -> string option
  val shallow: t -> bool
  val unpack_kind: kind -> string
  val unpack: t -> string

end

type pic = PIC.t

module IO (D: Git_hash.DIGEST) (I: Git_inflate.S): sig

  module type IO = sig
    include Git_s.IO with type t = kind
    val size: Mstruct.t -> int
  end

  module V2: IO
  module V3: IO

  val add_inflated_value:
    read:Git_value.read_inflated -> offsets:(int -> Git_hash.t option) ->
    Buffer.t -> t -> unit Lwt.t

  val to_value:
    index:Git_pack_index.f -> read:Git_value.read_inflated -> version:int ->
    ba:Cstruct.buffer -> t -> Git_value.t Lwt.t

  val unpack:
    index:Git_pack_index.f -> read:Git_value.read_inflated -> version:int ->
    ba:Cstruct.buffer -> t -> string Lwt.t

  val value_of_pic: pic -> Git_value.t

  val to_pic: read:Git_value.read_inflated ->
    offsets:(int -> pic option) -> hashes:(Git_hash.t -> pic option) ->
    t -> pic Lwt.t

  val of_pic: index:Git_pack_index.f -> offset:int -> pic -> t

end
