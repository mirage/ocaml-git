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

(** Miscellaneous functions. *)

(** {2 Hexa encoding} *)

val with_buffer: (Buffer.t -> unit) -> string
(** Create a temporary buffer, apply a function to append stuff to it,
    and return the buffer contents. *)

val with_buffer': (Buffer.t -> unit) -> Cstruct.t
(** Create a temporary buffer, apply a function to append stuff to
    it, and return the buffer contents as a cstruct. *)

(** {2 Zlib Compression} *)

val inflate_cstruct: Cstruct.t -> Cstruct.t
(** Inflate a cstruct. *)

val deflate_cstruct: ?level:int -> Cstruct.t -> Cstruct.t
(** Deflate a cstruct. *)

val inflate_mstruct: ?output_size:int -> Mstruct.t -> Mstruct.t
(** Inflate an mstruct. *)

val deflate_mstruct: ?level:int -> Mstruct.t -> Mstruct.t
(** Deflate an mstruct. *)

(** {2 CRC-32} *)

val crc32: string -> int32
(** Return the CRC-32 value of a bigstring. *)

(** {2 Association lists} *)

val inverse_assoc: ('a * 'b) list -> ('b * 'a) list
(** Inverse the association map. *)

val try_assoc: 'a -> ('a * 'b) list -> 'b option
(** Same as [List.assoc] but returns [None] if no element is found. *)

(** {2 Marshaling helpers} *)

val add_be_uint32: Buffer.t -> int32 -> unit

val input_key_value: Mstruct.t -> key:string -> (Mstruct.t -> 'a) -> 'a

val sp: char
val nul: char
val lf: char
val lt: char
val gt: char

module type OrderedType = sig
  include Set.OrderedType
  val pretty: t -> string
end

module I: OrderedType with type t = int
module S: OrderedType with type t = string

module type Set = sig
  include Set.S
  val pretty: t -> string
  val to_list: t -> elt list
  val of_list: elt list -> t
end

module type Map = sig
  include Map.S
  val pretty: ('a -> string) -> 'a t -> string
  val keys: 'a t -> key list
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val add_multi: key -> 'a -> 'a list t -> 'a list t
end

module Set (X: OrderedType): Set with type elt = X.t
module Map (X: OrderedType): Map with type key = X.t

module IntMap: Map with type key = int

val string_split: string -> on:char -> string list
val string_lsplit2: string -> on:char -> (string * string) option
val string_forall: (char -> bool) -> string -> bool
val string_exists: (char -> bool) -> string -> bool
val string_mem: char -> string -> bool
val string_chop_prefix: string -> prefix:string -> string option
val string_chop_suffix: string -> suffix:string -> string option

val list_filter_map: ('a -> 'b option) -> 'a list -> 'b list

module OP: sig

  val (/): string -> string -> string
  (** Same as [Filename.concat]. *)

end
