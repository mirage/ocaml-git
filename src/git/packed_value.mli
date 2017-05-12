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
  | Ref_delta of Hash.t delta
  | Off_delta of int delta

type t = { kind: kind; offset: int }

val shallow: Hash.Set.t -> t -> bool
val create: offset:int -> kind:kind -> t
val kind: t -> kind
val offset: t -> int
val pp_kind: Format.formatter -> kind -> unit
val is_delta: t -> bool
val result_length: t -> int
val source_length: t -> int

include S.S with type t := t

module PIC: sig

  type kind = Raw of string | Link of t delta

  and t = {
    kind   : kind;
    hash   : Hash.t;
    shallow: bool;
    mutable raw: string option;
  }

  include S.S with type t := t

  val create: ?raw:string -> ?shallow:bool -> Hash.t -> kind -> t
  val of_raw: ?shallow:bool ->  Hash.t -> string -> t
  val kind: t -> kind
  val name: t -> Hash.t
  val raw: t -> string option
  val shallow: t -> bool
  val unpack_kind: kind -> string
  val unpack: t -> string

end

type pic = PIC.t

module IO (D: Hash.DIGEST) (I: Inflate.S): sig

  module type IO = sig
    include S.IO with type t = kind
    val size: Mstruct.t -> int
  end

  module V2: IO
  module V3: IO

  val add_inflated_value:
    read:Value.read_inflated -> offsets:(int -> Hash.t option) ->
    Buffer.t -> t -> unit Lwt.t

  val to_value:
    index:Pack_index.f ->
    read:Value.read_inflated -> write:Value.write_inflated ->
    version:int -> ba:Cstruct.buffer -> t -> Value.t Lwt.t

  val unpack:
    index:Pack_index.f ->
    read:Value.read_inflated -> write:Value.write_inflated ->
    version:int -> ba:Cstruct.buffer -> t -> string Lwt.t

  val value_of_pic: pic -> Value.t

  val to_pic: read:Value.read_inflated ->
    offsets:(int -> pic option) -> hashes:(Hash.t -> pic option) ->
    t -> pic Lwt.t

  val of_pic: index:Pack_index.f -> offset:int -> pic -> t

end
