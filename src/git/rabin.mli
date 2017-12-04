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

(** Rabin's fingerprint. *)

(** Common Value for the [libXdiff] algorithm. *)
module type VALUE =
sig
  val window: int
  (** Length of the Rabin Window. *)

  val shift: int
  (** How many bits we need to shift to get a bounded index to [T] (this value must be [23]). *)

  val limit: int
  (** Limit the number of entries per hashes (default = 64). *)
end

(** Index module. *)
module Index (V: VALUE) :
sig
  type t
  (** The type of the index. *)

  val pp: t Fmt.t
  (** Pretty-printer for {!t}. *)

  val memory_size: t -> int
  (** [memory_size t] returns how many word(s) is needed to store [t]. *)

  val make: ?copy:bool -> Cstruct.t -> t
  (** [make ?copy raw] returns a Rabin's fingerprint of [raw]. [?copy] signals
      to copy the input buffer [raw] or not because [make] expects to take the
      ownership. *)
end

(** The type of the compression. *)
type t =
  | Copy of (int * int)
  (** It's the copy opcode to copy the byte range from the source to the
      target. *)
  | Insert of (int * int)
  (** It's the insert opcode to keep a specific byte range of the target. *)

val pp: t Fmt.t
(** Pretty-printer for {!e}. *)

module Delta (V: VALUE):
sig
  module Index: module type of Index(V)

  val delta: Index.t -> Cstruct.t -> t list
  (** [delta index trg] returns a compression list between the Rabin's fingerprint
      of the source with the target [trg]. *)
end

module Default: module type of Delta(struct let window, shift, limit = 16, 23, 64 end)
