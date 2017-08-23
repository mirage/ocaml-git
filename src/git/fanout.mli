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

module type KEY =
sig
  type t

  val compare : t -> t -> int
  val get     : t -> int -> char
end

(** A Fanout table is a container to associate a key [Key.t] with a value.
    Internally, we order bindings by the first byte of the [Key.t]. *)
module Make (Key : KEY) :
sig
  type 'a t
  (** The fanout table. *)

  val make : unit -> 'a t
  (** Make a new fanout table. *)

  val bind : Key.t -> 'a -> 'a t -> unit
  (** [bind k v t] adds the binding [k] and [v] to [t]. *)

  val length : int -> 'a t -> int
  (** [length t] returns the length of [t]. *)

  val get : int -> 'a t -> (Key.t * 'a) list
  (** [get byte t] gets a list of bindings where the key start with the byte
      [byte]. *)
end
