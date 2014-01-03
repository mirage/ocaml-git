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

(** Miscellaneous functions. *)

(** {2 Hexa encoding} *)

val hex_encode: string -> string
(** Encode a string to base16. *)

val hex_decode: string -> string
(** Decode a string from base16. *)

(** {2 Zlib Compression} *)

(** Deflate a string. *)
val deflate_string: string -> string

(** Inflate a buffer. *)
val inflate_mstruct: ?allocator:(int -> Cstruct.t) -> Mstruct.t -> Mstruct.t

(** Deflate a buffer. *)
val deflate_mstruct: Mstruct.t -> Mstruct.t

(** {2 Files} *)

val mkdir: string -> unit
(** Create a directory (and the parent dirs if needed). *)

val directories: string -> string list
(** List the subdirs. *)

val files: string -> string list
(** List the subfiles. *)

val rec_files: string -> string list
(** List of the subfiles, recursively. *)

val mstruct_of_file: string -> Mstruct.t
(** mmap a file and return a mutable C-like structure with its
    contents. *)

module OP: sig

  val (/): string -> string -> string
  (** Same as [Filename.concat]. *)

end
