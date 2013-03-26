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

(** Optimized input buffers. *)

(** This finally converges to something very similar to the
    [ocaml-cstruct] library, but with few differences:

    - each input function shift the buffer as a side-effect. This is
      maybe a bit less efficient but this is much easier to debug and
      to use.

    - and better context to have nice message: user can push/pop
      symbol on top of the context stack, and we know the filename as
      well.

    - we have operation to find a given delimeter in a string (which
      does not really have sense in the normal cstruct, as everything
      is binary encoded with a known size/pattern).

    I guess at one point I should merge these changes upstream.
*)

(** Input buffers. *)
type buffer

(** Create an input buffer from a file. *)
val of_file: File.Name.t -> buffer

(** Create an input buffer from a string. *)
val of_string: File.Name.t -> string -> buffer

(** Dump the buffer. *)
val dump: buffer -> unit

(** {2 Buffer manipulation} *)

(** Push a new symbol on top of the debug context. *)
val push: buffer -> string -> buffer

(** Return the file associated to the buffer. *)
val file: buffer -> File.Name.t

(** Shift the buffer. Negative shifting is supported. *)
val shift: buffer -> int -> unit

(** Build a subview which share the same contents as the initial
    buffer but have different bounds. *)
val sub: buffer -> int -> int -> buffer

(** Buffer lenght. *)
val length: buffer -> int

(** Clone a buffer. Only the bounds are copied, not the contents which
    is still shared. *)
val clone: buffer -> buffer

(** {2 Input functions} *)

(** All the input function have a side-effect on the buffer: they
    shift it from the number of read bytes. *)

(** Raise a parsing error. *)
val parse_error: buffer -> ('a, unit, string, 'b) format4 -> 'a

(** See [String.index]. [None] means no result*)
val index: buffer -> char -> int option

(** Read a string of a given size. If [None] read the full buffer. *)
val input_string: buffer -> int option -> string

(** Read a byte. *)
val input_byte: buffer -> int

(** Read a big-endian int32. *)
val input_be_int32: buffer -> int32

(** Read a big-endian int64. *)
val input_be_int64: buffer -> int64

(** Read a value by looking at the first occurence of a char. *)
val input_delim: buffer -> char -> (buffer -> 'a)  -> 'a option

(** Read a string by looking at the first occurence of a char. *)
val input_string_delim: buffer -> char -> string option

(** {2 Compression} *)

(** Inflate a buffer. *)
val inflate: buffer -> buffer

(** Deflate a buffer. *)
val deflate: buffer -> buffer
