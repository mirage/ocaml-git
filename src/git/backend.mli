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

(** Git data-model *)

open Lib

(** File contents. *)
module type SIG = sig
  type t

  (** Dump the contents of the file. *)
  val dump: t -> unit

  (** Output the file in a buffer. *)
  val output: Buffer.t -> t -> unit

  (** Output the file in a string. *)
  val to_string: t -> string

  (** [input file buf off len] reads the file from a string at offset
     [off] and length [len]. *)
  val input: IO.buffer -> t IO.result

  (** Read a file from a raw string. *)
  val of_string: File.Name.t -> string -> t

end

(** Blob objects. *)
module Blob: SIG

(** Commit objects. *)
module Commit: SIG

(** Tree objects. *)
module Tree: SIG

(** Tag objects. *)
module Tag: SIG

(** An object is either a tag/tree/commit/blob. *)
module Object: SIG with type t = Model.obj

(** Dump the contents of a git reposity. *)
val dump: Model.t -> unit
