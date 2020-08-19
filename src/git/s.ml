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

  val compare : t -> t -> int

  val hash : t -> int

  val equal : t -> t -> bool

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type CONVERTER = sig
  type t

  type buffer

  val to_t : buffer -> t

  val of_t : t -> buffer
end

module type ENCODER = sig
  type t

  type init

  type error

  type encoder

  val pp_error : error Fmt.t

  val default : init -> encoder

  val eval :
    Cstruct.t ->
    encoder ->
    [ `Flush of encoder | `End of encoder * int | `Error of error ]

  val flush : int -> int -> encoder -> encoder

  val used : encoder -> int
end

module type DECODER = sig
  type t

  type init

  type error

  type decoder

  val pp_error : error Fmt.t

  val to_result : Cstruct.t -> (t, error) result

  val default : init -> decoder

  val eval :
    decoder ->
    [ `Await of decoder | `End of Cstruct.t * t | `Error of Cstruct.t * error ]

  val refill : Cstruct.t -> decoder -> (decoder, error) result

  val finish : decoder -> decoder
end

module type META = functor (Meta : Encore.Meta.S) -> sig
  type e

  val p : e Meta.t
end

module type DESC = sig
  type 'a t

  type e

  val p : e t
end

module type INFLATE = sig
  type t

  type error

  type window

  val pp_error : error Fmt.t

  val pp : t Fmt.t

  val window_reset : window -> window

  val window : unit -> window

  val default : window -> t

  val eval :
    src:Cstruct.t ->
    dst:Cstruct.t ->
    t ->
    [ `Await of t | `Flush of t | `Error of t * error | `End of t ]

  val used_in : t -> int

  val used_out : t -> int

  val write : t -> int

  val flush : int -> int -> t -> t

  val refill : int -> int -> t -> t
end

module type DEFLATE = sig
  type t

  type error

  val pp_error : error Fmt.t

  val default : int -> t

  val flush : int -> int -> t -> t

  val no_flush : int -> int -> t -> t

  val finish : t -> t

  val used_in : t -> int

  val used_out : t -> int

  val eval :
    src:Cstruct.t ->
    dst:Cstruct.t ->
    t ->
    [ `Flush of t | `Await of t | `Error of t * error | `End of t ]
end

module type HASH = sig
  include Digestif.S

  val feed_cstruct : ctx -> Cstruct.t -> ctx

  val compare : t -> t -> int

  val hash : t -> int

  val equal : t -> t -> bool

  val read : t -> int -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type DIGEST = sig
  type t

  type hash

  val digest : t -> hash
end

module type FILE = sig
  type error

  type t

  val exists : t -> Fpath.t -> (bool, error) result Lwt.t

  val delete : t -> Fpath.t -> (unit, error) result Lwt.t

  val move : t -> Fpath.t -> Fpath.t -> (unit, error) result Lwt.t
  (** [move] should be be atomic *)

  type 'a fd constraint 'a = [< `Read | `Write ]

  val open_w : t -> Fpath.t -> ([ `Write ] fd, error) result Lwt.t

  val open_r : t -> Fpath.t -> ([ `Read ] fd, error) result Lwt.t

  val write :
    Cstruct.t ->
    ?off:int ->
    ?len:int ->
    [> `Write ] fd ->
    (int, error) result Lwt.t

  val read :
    Cstruct.t ->
    ?off:int ->
    ?len:int ->
    [> `Read ] fd ->
    (int, error) result Lwt.t

  val close : 'a fd -> (unit, error) result Lwt.t
end

module type MAPPER = sig
  type fd

  type error

  type t

  val pp_error : error Fmt.t

  val openfile : t -> Fpath.t -> (fd, error) result Lwt.t

  val length : fd -> (int64, error) result Lwt.t

  val map : fd -> ?pos:int64 -> int -> (Cstruct.t, error) result Lwt.t

  val close : fd -> (unit, error) result Lwt.t
end

module type DIR = sig
  type error

  type t

  val exists : t -> Fpath.t -> (bool, error) result Lwt.t

  val create : t -> Fpath.t -> (bool, error) result Lwt.t

  val delete : t -> Fpath.t -> (unit, error) result Lwt.t

  val contents : t -> ?rel:bool -> Fpath.t -> (Fpath.t list, error) result Lwt.t

  val current : t -> (Fpath.t, error) result Lwt.t
end

module type FS = sig
  type error

  type t

  val pp_error : error Fmt.t

  val is_dir : t -> Fpath.t -> (bool, error) result Lwt.t

  val is_file : t -> Fpath.t -> (bool, error) result Lwt.t

  module File : FILE with type t = t and type error = error

  module Dir : DIR with type t = t and type error = error

  module Mapper : MAPPER with type t = t and type error = error

  val has_global_watches : bool

  val has_global_checkout : bool
end
