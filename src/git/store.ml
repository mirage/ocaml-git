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

module type S = sig
  type t
  val create: ?root:string -> ?dot_git:string -> ?level:int -> unit -> t Lwt.t
  val dot_git: t -> string
  val root: t -> string
  val level: t -> int
  val dump: t -> unit Lwt.t
  val contents: t -> (Hash.t * Value.t) list Lwt.t
  val size: t -> Hash.t -> int option Lwt.t
  val read: t -> Hash.t -> Value.t option Lwt.t
  val read_exn: t -> Hash.t -> Value.t Lwt.t
  val mem: t -> Hash.t -> bool Lwt.t
  val list: t -> Hash.t list Lwt.t
  val write: t -> Value.t -> Hash.t Lwt.t
  val write_pack: t -> Pack.raw -> Hash.Set.t Lwt.t
  val references: t -> Reference.t list Lwt.t
  val mem_reference: t -> Reference.t -> bool Lwt.t
  val read_reference: t -> Reference.t -> Hash.t option Lwt.t
  val read_reference_exn: t -> Reference.t -> Hash.t Lwt.t
  val write_head: t -> Reference.head_contents -> unit Lwt.t
  val read_head: t -> Reference.head_contents option Lwt.t
  val write_reference: t -> Reference.t -> Hash.t -> unit Lwt.t
  val remove_reference: t -> Reference.t -> unit Lwt.t
  val test_and_set_reference: t -> Reference.t ->
    test:Hash.t option -> set:Hash.t option -> bool Lwt.t
  val read_index: t -> Index.t Lwt.t
  val write_index: t -> ?index:Index.t -> Hash.Commit.t -> unit Lwt.t
  val kind: [`Mem | `Disk]
  val read_inflated: t -> Hash.t -> string option Lwt.t
  val write_inflated: t -> string -> Hash.t Lwt.t
  module Digest: Hash.DIGEST
  module Inflate: Inflate.S
end
