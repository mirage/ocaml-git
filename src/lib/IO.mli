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

(** IO functions. *)

(** {2 Input functions.} *)

(** Debugging context. *)
type dbg

(** Input buffers. *)
type buffer = {
  dbg: dbg;
  buf: string;
  off: int;
  len: int;
}

(** Create a buffer. *)
val buffer: File.Name.t -> string -> buffer

(** Dump the buffer. *)
val dump_buffer: buffer -> unit

(** Return the file associated to the buffer. *)
val file: buffer -> File.Name.t

(** Result type. *)
type 'a result = (buffer * 'a)

(** Sub-strings. *)
val sub: buffer -> int -> int -> string

(** See [String.index_from]. *)
val index_from: buffer -> int -> char -> int

(** Raise a parsing error. *)
val parse_error: buffer -> ('a, unit, string, 'b) format4 -> 'a

(** Read a string of a given size. *)
val input_string: buffer -> int -> string result

(** Read a string until a given delimiter. *)
val input_delim: buffer -> char -> string result

(** {2 Buffer manipulation} *)

(** Update the debug context. *)
val update: buffer -> string -> buffer

(** Shift a buffer. *)
val shift: buffer -> int -> buffer

(** Apply function to a sub-buffer, by looking at the first occurence
   of a char. *)
val with_subview: buffer -> char -> (buffer -> 'a result) -> 'a result

(** {2 Conversion functions.} *)

(** Read from a string. *)
val of_string: (buffer -> 'a result) -> File.Name.t -> string -> 'a

(** Write to a string. *)
val to_string: (Buffer.t -> 'a -> unit) -> 'a -> string
