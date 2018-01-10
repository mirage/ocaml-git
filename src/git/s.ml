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

  val pp      : t Fmt.t
  val compare : t -> t -> int
  val hash    : t -> int
  val equal   : t -> t -> bool

  module Set  : Set.S with type elt = t
  module Map  : Map.S with type key = t
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

  val pp_error  : error Fmt.t

  val default   : init -> encoder
  val eval      : Cstruct.t -> encoder -> [ `Flush of encoder | `End of (encoder * int) | `Error of error ]
  val flush     : int -> int -> encoder -> encoder
  val used      : encoder -> int
end

module type DECODER = sig
  type t
  type init
  type error
  type decoder

  val pp_error  : error Fmt.t

  val to_result : Cstruct.t -> (t, error) result

  val default   : init -> decoder
  val eval      : decoder -> [ `Await of decoder | `End of (Cstruct.t * t) | `Error of (Cstruct.t * error) ]
  val refill    : Cstruct.t -> decoder -> (decoder, error) result
  val finish    : decoder -> decoder
end

module type ANGSTROM = sig
  type t

  val decoder : t Angstrom.t
end

module type FARADAY = sig
  type t

  val encoder : t Farfadet.t
  val length  : t -> int64
end

module type MINIENC = sig
  type t

  val encoder : t -> (Minienc.encoder -> 'a Minienc.state) -> Minienc.encoder -> 'a Minienc.state
end

module type INFLATE = sig
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

module type DEFLATE = sig
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

module type FDIGEST = sig
  type t
  type buffer

  type 'a digest = 'a -> t

  val cstruct : Cstruct.t digest
  val string  : string digest
  val bytes   : Bytes.t digest
  val digest  : buffer digest
  val digestv : buffer list digest

  val length  : int
end

module type IDIGEST = sig
  type t
  type ctx
  type buffer = Cstruct.t

  val init   : unit -> ctx
  val feed   : ctx -> buffer -> unit
  val get    : ctx -> t

  val length : int
end

module type HASH = sig
  include BASE with type t = Hash.t

  module Digest : IDIGEST with type t = t

  val get : t -> int -> char

  val to_string : t -> string
  val of_string : string -> t

  type hex = string

  val to_hex : t -> hex
  val of_hex : hex -> t
end

module type DIGEST = sig
  type t
  type hash

  val digest : t -> hash
end

module type FILE = sig
  type error
  val exists : Fpath.t -> (bool, error) result Lwt.t
  val delete : Fpath.t -> (unit, error) result Lwt.t
  val move : Fpath.t -> Fpath.t -> (unit, error) result Lwt.t
  (** [move] should be be atomic *)

  type 'a fd constraint 'a = [< `Read | `Write ]

  val open_w: Fpath.t -> mode:int -> ([ `Write ] fd, error) result Lwt.t
  val open_r: Fpath.t -> mode:int -> ([ `Read ] fd, error) result Lwt.t
  val write: Cstruct.t -> ?off:int -> ?len:int -> [> `Write ] fd
    -> (int, error) result Lwt.t
  val read: Cstruct.t -> ?off:int -> ?len:int -> [> `Read ] fd
    -> (int, error) result Lwt.t
  val close : 'a fd -> (unit, error) result Lwt.t
end

module type MAPPER = sig
  type fd
  type error
  val pp_error: error Fmt.t
  val openfile: Fpath.t -> (fd, error) result Lwt.t
  val length: fd -> (int64, error) result Lwt.t
  val map: fd -> ?pos:int64 -> share:bool -> int -> (Cstruct.t, error) result Lwt.t
  val close : fd -> (unit, error) result Lwt.t
end

module type DIR = sig
  type error
  val exists: Fpath.t -> (bool, error) result Lwt.t
  val create: ?path:bool -> ?mode:int -> Fpath.t -> (bool, error) result Lwt.t
  val delete: ?recurse:bool -> Fpath.t -> (unit, error) result Lwt.t
  val contents: ?dotfiles:bool -> ?rel:bool -> Fpath.t ->
    (Fpath.t list, error) result Lwt.t
  val current: unit -> (Fpath.t, error) result Lwt.t
  val temp: unit -> Fpath.t Lwt.t
end

module type FS = sig
  type error
  val pp_error: error Fmt.t
  val is_dir: Fpath.t -> (bool, error) result Lwt.t
  val is_file: Fpath.t -> (bool, error) result Lwt.t

  module File  : FILE with type error := error
  module Dir   : DIR with type error := error
  module Mapper: MAPPER with type error := error

  val has_global_watches: bool
  val has_global_checkout: bool
end
