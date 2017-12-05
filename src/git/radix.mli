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

(** The Radix tree. *)

module type KEY =
sig
  type t

  val get: t -> int -> char
  val length: t -> int
end

(** A concrete iterable structure. *)
type 'a sequence = ('a -> unit) -> unit

(** A Radix tree is a optimized container to bind a [Key.t] with a
    value. *)
module Make (K: KEY) :
sig
  type 'a t
  (** The radix-tree. *)

  module Key : KEY

  type nonrec 'a sequence = 'a sequence
  (** An abstract representation of a iterative container. *)

  val empty: 'a t
  (** The empty radix-tree. *)

  val is_empty: 'a t -> bool
  (** Test whether is radix-tree is empty or not. *)

  val bind: 'a t -> Key.t -> 'a -> 'a t
  (** [bind t k v] returns a radix-tree containing the same binding as
      [t], plus a binding [k] to [v]. *)

  val lookup: 'a t -> Key.t -> 'a option
  (** [lookup t k] returns the current binding of [k] in [t] or
      returns [None] if no such binding exists. *)

  val mem: 'a t -> Key.t -> bool
  (** [mem t k] checks if at least we have one binding with the key
      [k]. *)

  val fold: (Key.t * 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** [fold f a t] computes [(f kN dN (f k1 d1 a))], where [k1 .. kN]
      are the keys of all bindings in [t], and [d1 .. dN] are the
      associated data. *)

  val iter: (Key.t * 'a -> unit) -> 'a t -> unit
  (** [iter f t] applies [f] to all bindings in radix-tree [t]. [f]
      receives a pair which contains the key and the associated
      value. *)

  val to_sequence: 'a t -> (Key.t * 'a) sequence
  (** [to_sequence t] makes a abstract representation of the
      radix-tree. *)

  val to_list: 'a t -> (Key.t * 'a) list
  (** [to_list t] makes an associated list from the radix-tree. *)

  val pp: (Key.t * 'a) Fmt.t -> 'a t Fmt.t
  (** A pretty-printer for the radix-tree. *)
end with module Key = K
