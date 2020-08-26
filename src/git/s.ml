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

module type BASE = sig
  type t

  val pp : t Fmt.t
  (** Pretty-printer of {!t}. *)

  val compare : t -> t -> int
  (** The comparison function for {!t}. *)

  val hash : t -> int
  (** [hash blob] associates a non-negative integer to any value of {!t}. It is
      guaranteed that if [x = y] or [compare x y = 0], then [hash x = hash y]. *)

  val equal : t -> t -> bool
  (** The equal function for {!t}. *)

  (** {2 Sets and Maps.} *)

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module type CONVERTER = sig
  type t
  type buffer

  val to_t : buffer -> t
  val of_t : t -> buffer
end

module type HASH = sig
  include Digestif.S

  val feed_cstruct : ctx -> Cstruct.t -> ctx
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val read : t -> int -> int
  val null : t
  val length : int
  val feed : ctx -> ?off:int -> ?len:int -> Bigstringaf.t -> ctx

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module type DIGEST = sig
  type t

  type hash
  (** Type of digests. *)

  val digest : t -> hash
  (** [digest t] associates a [hash] to any value of {!t}. It is guaranteed that
      if [x = y] or [compare x y = 0], then [digest x = digest y]. *)
end
