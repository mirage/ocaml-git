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

module type BASE =
sig
  type t

  val pp      : t Fmt.t
  val compare : t -> t -> int
  val hash    : t -> int
  val equal   : t -> t -> bool

  module Set  : Set.S with type elt = t
  module Map  : Map.S with type key = t
end

module type CONVERTER =
sig
  type t
  type buffer

  val to_t : buffer -> t
  val of_t : t -> buffer
end

module type ENCODER =
sig
  type t
  type raw
  type init
  type error
  type encoder

  val pp_error  : error Fmt.t

  val default   : init -> encoder
  val eval      : raw -> encoder -> [ `Flush of encoder | `End of (encoder * int) | `Error of error ]
  val flush     : int -> int -> encoder -> encoder
  val used      : encoder -> int
end

module type DECODER =
sig
  type t
  type raw
  type init
  type error
  type decoder

  val pp_error  : error Fmt.t

  val to_result : raw -> (t, error) result

  val default   : init -> decoder
  val eval      : decoder -> [ `Await of decoder | `End of (raw * t) | `Error of (raw * error) ]
  val refill    : raw -> decoder -> (decoder, error) result
  val finish    : decoder -> decoder
end

module type ANGSTROM =
sig
  type t

  val decoder : t Angstrom.t
end

module type FARADAY =
sig
  type t

  val encoder : t Farfadet.t
  val length  : t -> int64
end

module type MINIENC =
sig
  type t

  val encoder : t -> (Minienc.encoder -> 'a Minienc.state) -> Minienc.encoder -> 'a Minienc.state
end

(* XXX(dinosaure): non blocking interface comes from [Decompress] but we can
   produce the same with [Camlzip]. *)
module type INFLATE =
sig
  type t
  type error
  type window

  val pp_error     : error Fmt.t
  val pp           : t Fmt.t

  val window_reset : window -> window
  val window       : unit -> window

  val default      : window -> t
  val eval         : src:Cstruct.t -> dst:Cstruct.t -> t -> [ `Await of t | `Flush of t | `Error of (t * error) | `End of t ]
  val used_in      : t -> int
  val used_out     : t -> int
  val write        : t -> int
  val flush        : int -> int -> t -> t
  val refill       : int -> int -> t -> t
end

(* XXX(dinosaure): non blocking interface comes from [Decompress] but we can
   produce the same with [Camlzip]. *)
module type DEFLATE =
sig
  type t
  type error

  val pp_error   : error Fmt.t

  val default    : int -> t
  val flush      : int -> int -> t -> t
  val no_flush   : int -> int -> t -> t
  val sync_flush : int -> int -> t -> t
  val finish     : t -> t
  val used_in    : t -> int
  val used_out   : t -> int
  val eval       : src:Cstruct.t -> dst:Cstruct.t -> t -> [ `Flush of t | `Await of t | `Error of (t * error) | `End of t ]
end
