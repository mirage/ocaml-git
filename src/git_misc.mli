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

(** Miscellaneous functions. *)

(** {2 Hexa encoding} *)

val with_buffer: (Buffer.t -> unit) -> string
(** Create a temporary buffer, apply a function to append stuff to it,
    and return the buffer contents. *)

val with_buffer': (Buffer.t -> unit) -> Cstruct.t
(** Create a temporary buffer, apply a function to append stuff to
    it, and return the buffer contents as a cstruct. *)

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
val sp_str: string
val nul_str: string

module type OrderedType = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module I: OrderedType with type t = int
module S: OrderedType with type t = string

module Set (X: OrderedType): Git_s.Set with type elt = X.t
module Map (X: OrderedType): Git_s.Map with type key = X.t

module IntMap: Git_s.Map with type key = int

val list_filter_map: ('a -> 'b option) -> 'a list -> 'b list

module OP: sig

  val (/): string -> string -> string
  (** Same as [Filename.concat]. *)

end

val src_log: string -> (module Logs.LOG)
