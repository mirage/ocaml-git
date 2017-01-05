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
  include Git_store.S
  val remove: t -> unit Lwt.t
  val create_file: t -> string -> Git_tree.perm -> Git_blob.t -> unit Lwt.t
  val entry_of_file: t -> Git_index.t -> string -> Git_tree.perm ->
    Git_hash.Blob.t -> Git_blob.t -> Git_index.entry option Lwt.t
  val clear: unit -> unit
end

module type IO = sig
  val getcwd: unit -> string Lwt.t
  val realpath: string -> string Lwt.t
  val mkdir: string -> unit Lwt.t
  val remove: string -> unit Lwt.t
  val file_exists: string -> bool Lwt.t
  val directories: string -> string list Lwt.t
  val files: string -> string list Lwt.t
  val rec_files: string -> string list Lwt.t
  val read_file: string -> Cstruct.t Lwt.t
  val write_file: string -> ?temp_dir:string -> Cstruct.t -> unit Lwt.t
  val chmod: string -> int -> unit Lwt.t
  val stat_info: string -> Git_index.stat_info
end

module Make (IO: IO) (D: Git_hash.DIGEST) (I: Git_inflate.S): S
