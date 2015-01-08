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

(** Signatures *)

module type S = sig

  type t
  (** The type for the given Git object. *)

  val equal: t -> t -> bool
  (** Are two objects equal? *)

  val hash: t -> int
  (** Hash an object. *)

  val compare: t -> t -> int
  (** Compare two objects. *)

  val pretty: t -> string
  (** Human readable represenation of the object. *)

  val input: Mstruct.t -> t
  (** Build a value from an inflated contents. *)

  val add: Buffer.t -> ?level:int -> t -> unit
  (** Add the serialization of the value to an already existing
      buffer.

      The compression [level] must be between 0 and 9: 1 gives best
      speed, 9 gives best compression, 0 gives no compression at all
      (the input data is simply copied a block at a time). The default
      value (currently equivalent to level 6) requests a default
      compromise between speed and compression. *)
end
