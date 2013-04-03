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

(** Inflate a string using ZLIB. Return the exact size of the origin
    string. *)
(* val inflate_string: string -> int * string *)

(** Deflate a string using ZLIB. *)
val deflate_string: string -> string

(** Uncompress a string and return the size of the original compressed
    string. *)
val uncompress:
  ?header:bool -> (int -> unit) -> (string -> int) -> (string -> int -> unit) -> unit

(** Encode a string to base16. *)
val hex_encode: string -> string

(** Decode a string from base16. *)
val hex_decode: string -> string

(** {2 Strings} *)

(** Cut a string at the first occurence of a character, starting from
    the left. *)
val cut_at: string -> char -> (string * string) option

(** Cut a string at the first occurence of a character, starting from
    the right. *)
val rcut_at: string -> char -> (string * string) option

(** Split a string using a given char. *)
val split: string -> char -> string list

(** Return the contents of an inchannel. *)
val string_of_in_channel: in_channel -> string
