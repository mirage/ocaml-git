(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

type t
(** The type of the CRC-32 checksum. *)

val digest : t -> ?off:int -> ?len:int -> Cstruct.t -> t
(** [digest crc cs] digests the buffer [cs] and produce a new CRC-32 checksum
   (derived from [crc]). The default value of [off] is 0 and the default value
   of [len] is the length of [cs]. *)

val digestv : t -> Cstruct.t list -> t
(** [digest crc css] applies {!digest} for each [Cstruct.t] elements of [css]
   with the default argument and return the new CRC-32 checksum.

    Is equivalent to: [(digest .. (digest crc cs1) csN)]. *)

val digestc : t -> int -> t
(** [digestc crc byte] derives the checksum CRC-32 [crc] with the byte [byte].
*)

val digests : t -> ?off:int -> ?len:int -> Bytes.t -> t

val of_int32 : int32 -> t
(** Casts an [int32] value to a CRC-32 checksum. *)

val to_int32 : t -> int32
(** Casts the CRC-32 checksum to an [int32] value. *)

val pp : t Fmt.t
(** A pretty-print for {!t}. *)

val default : t
(** A default and first (without derivation) of the CRC-32 checksum. *)

val eq : t -> t -> bool
(** [eq a b] returns [true] only when [a] equal [b]. Otherwise, it returns
   [false]. *)

val neq : t -> t -> bool
(** [neq a b] returns [true] only when [a] is different to [b]. Otherwise, it
   returns [true]. Is equivalent to: [not (eq a b)]. *)
