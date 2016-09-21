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

module type S = sig
  type t
  val create: ?root:string -> ?dot_git:string -> ?level:int ->
    unit -> t Lwt.t
  val dot_git: t -> string
  val root: t -> string
  val level: t -> int
  val dump: t -> unit Lwt.t
  val contents: t -> (Git_hash.t * Git_value.t) list Lwt.t
  val size: t -> Git_hash.t -> int option Lwt.t
  val read: t -> Git_hash.t -> Git_value.t option Lwt.t
  val read_exn: t -> Git_hash.t -> Git_value.t Lwt.t
  val mem: t -> Git_hash.t -> bool Lwt.t
  val list: t -> Git_hash.t list Lwt.t
  val write: t -> Git_value.t -> Git_hash.t Lwt.t
  val write_pack: t -> Git_pack.raw -> Git_hash.Set.t Lwt.t
  val references: t -> Git_reference.t list Lwt.t
  val mem_reference: t -> Git_reference.t -> bool Lwt.t
  val read_reference: t -> Git_reference.t -> Git_hash.t option Lwt.t
  val read_reference_exn: t -> Git_reference.t -> Git_hash.t Lwt.t
  val write_head: t -> Git_reference.head_contents -> unit Lwt.t
  val read_head: t -> Git_reference.head_contents option Lwt.t
  val write_reference: t -> Git_reference.t -> Git_hash.t -> unit Lwt.t
  val remove_reference: t -> Git_reference.t -> unit Lwt.t
  val read_index: t -> Git_index.t Lwt.t
  val write_index: t -> ?index:Git_index.t -> Git_hash.Commit.t -> unit Lwt.t
  val kind: [`Memory | `Disk]
  val read_inflated: t -> Git_hash.t -> string option Lwt.t
  val write_inflated: t -> string -> Git_hash.t Lwt.t
  module Digest: Git_hash.DIGEST
  module Inflate: Git_inflate.S
end
