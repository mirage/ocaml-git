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

(** Store Git objects on the local filesystem. *)

module type S = sig

  include Store.S

  val remove: t -> unit Lwt.t
  (** Remove all the contents of the store. *)

  val create_file: t -> string -> Tree.perm -> Blob.t -> unit Lwt.t
  (** Create a file on the filesystem, with the given mode. *)

  val entry_of_file: t -> Index.t ->
    string -> Tree.perm -> SHA.Blob.t -> Blob.t -> Index.entry option Lwt.t
  (** Generate a cache entry for the file. Create a fresh file if it
      does not already exist. If [root] is not set, use the current
      working directory as repository root. *)

  val clear: unit -> unit
  (** Clear the LRU caches. *)

end

(** {2 Constructor} *)

module type IO = sig

  val getcwd: unit -> string Lwt.t
  (** Get the current working directory. *)

  val realpath: string -> string Lwt.t
  (** Very dumb real-path. Only works for existing directories. *)

  val mkdir: string -> unit Lwt.t
  (** Create a directory (and the parent dirs if needed). *)

  val remove: string -> unit Lwt.t
  (** Remote a file or a directory (even if non-empty). *)

  val file_exists: string -> bool Lwt.t
  (** Does the given file exists? See {!Sys.file_exists}*)

  val directories: string -> string list Lwt.t
  (** List the subdirs. *)

  val files: string -> string list Lwt.t
  (** List the subfiles. *)

  val rec_files: string -> string list Lwt.t
  (** [rec_files dir] is the list of the files recursively present in
      [dir] and all of its sub-directories. Return filenames prefixed
      by [dir]. *)

  val read_file: string -> Cstruct.t Lwt.t
  (** Read a file and return a mutable C-like structure with its
      contents. *)

  val write_file: string -> ?temp_dir:string -> Cstruct.t -> unit Lwt.t
  (** Write a bigarray to a file. Atomicity is guaranteed by writing
      the file first in [temp_dir] and then moving the file to the
      right location. By defaut, [temp_dir] is
      [Filename.get_temp_dir_name]. *)

  val chmod: string -> int -> unit Lwt.t
  (** Change the file mode. *)

  val stat_info: string -> Index.stat_info
  (** Return the stats of the given file. *)

end

module Make (IO: IO) (D: SHA.DIGEST) (I: Inflate.S): S
(** Create an on-disk store implementation. *)
