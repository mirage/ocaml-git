(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Basic file manipulation *)

(** File names. *)
module Name: Abstract.SIG

(** Directory names. *)
module Dirname: sig
  include Abstract.SIG

  (** Basename. *)
  val basename: t -> string
end

(** The current directory. *)
val cwd: unit -> Dirname.t

(** Read a file. *)
val read: Name.t -> string

(** Write a file. *)
val write: Name.t -> string -> unit

(** Does a file exists ? *)
val exists: Name.t -> bool

(** Return the directory name. *)
val dirname: Name.t -> Dirname.t

(** Return the basename. *)
val basename: Name.t -> string

(** Create a directory and its parents. *)
val mkdir: Dirname.t -> unit

(** Return the directories in a path. *)
val directories: Dirname.t -> Dirname.t list

(** Return the files in a path. *)
val files: Dirname.t -> Name.t list

(** Return all the file hierarchy in a path. *)
val rec_files: Dirname.t -> Name.t list

(** Concatenatation. *)
module OP: sig
  val (/) : Dirname.t -> string -> Dirname.t
  val (//): Dirname.t -> string -> Name.t
end
