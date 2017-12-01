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
  type raw = Cstruct.t
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
  type raw = Cstruct.t
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

module type FDIGEST =
sig
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

module type IDIGEST =
sig
  type t
  type ctx
  type buffer = Cstruct.t

  val init   : unit -> ctx
  val feed   : ctx -> buffer -> unit
  val get    : ctx -> t

  val length : int
end

module type HASH =
sig
  include BASE with type t = Bytes.t

  module Digest : IDIGEST with type t = t

  val get : t -> int -> char

  val to_string : t -> string
  val of_string : string -> t

  type hex = string

  val to_hex : t -> hex
  val of_hex : hex -> t
end

module type DIGEST =
sig
  type t
  type hash

  val digest : t -> hash
end

module type LOCK =
sig
  type +'a io

  type key
  type elt
  type t

  val unlock    : elt -> unit io
  val lock      : elt -> unit io
  val with_lock : elt option -> (unit -> 'a Lwt.t) -> 'a io

  val make      : t -> key -> elt
end

module type PATH =
sig
  include module type of Fpath
end

module type WEB =
sig
  type +'a io

  type req
  (** The type for HTTP requests. *)

  type resp
  (** The type for HTTP responses. *)

  type raw
  (** The type of the buffer. *)

  type uri

  module HTTP :
  sig
    type headers
    (** The type for header maps.
        A header map represents a list of HTTP headers. It maps header field
        names to their value. *)

    type path = string list
    (** The type for HTTP [absolute_path]s represented by its non-empty list of
        URI [segment]s. Note that URI segments can be empty; in particular the
        absolute path ["/"] is the list with a single empty segment and is thus
        represented by the list [[""]]. *)

    type version = int * int
    (** The type for representing [HTTP-Version] fields. Both integers must be
        in the interval [\[0;9\]]. *)

    type meth =
      [ `GET
      | `HEAD
      | `POST
      | `PUT
      | `DELETE
      | `CONNECT
      | `OPTIONS
      | `TRACE
      | `PATCH
      | `Other of string ]
    (** The type for HTTP request methods. *)

    (** Headers.
        FIXME(dbuenzli): The semantic of this module considers than a field can contain
        multiple values. However, it's should be the wrong way to abstract
        values of headers - and let the user to consider than specific field can
        have multiple values. *)
    module Headers :
    sig
      type name
      (** The type for lower-cased HTTP header [field-name]s. *)

      val name : string -> name
      (** [name s] is a header name from [s].
          @raise Invalid_argument if [s] is not a field-name. *)

      val name_equal : name -> name -> bool
      (** [name_equal n n'] is [true] iff [n] and [n'] are equal. *)

      (** {1:map Header maps} *)

      val empty : headers
      (** [empty] is the empty header map. *)

      val is_empty : headers -> bool
      (** [is_empty hs] is [true] iff [hs] is the empty header map. *)

      val find : name -> headers -> string option
      (** [find n hs] is the value of header [n] in [hs] (if defined). If [n] is
          multi-valued and defined, we return [String.concat "," vs] where [vs]
          are values related to [n] in [hs]. *)

      val get : name -> headers -> string
      (** [get n hs] is like {!find} but @raise Invalid_argument if [n] is
          undefined in [hs]. *)

      (** {1:hcst Header name values} *)

      val user_agent : name
      (** User-Agent. *)

      val content_type : name
      (** Content-Type. *)

      val access_control_allow_origin : name
      (** Access-Control-Allow-Origin. *)

      val access_control_allow_methods : name
      (** Access-Control-Allow-Methods. *)

      val access_control_allow_headers : name
      (** Access-Control-Allow-Headers. *)

      val def : name -> string -> headers -> headers
      (** [def n v hs] is [hs] with [n] bound to [v]. *)

      val def_multi : name -> string list -> headers -> headers
      (** [def_multi n vs hs] is [hs] with [n] bound to the multi-value [vs].
          @raise Invalid_argument if [vs] is [[]]. *)

      val merge : headers -> headers -> headers
    end
  end

  (** {1:status_codes Status codes} *)

  type status = int
  (** The type for HTTP status code. *)

  val s100_continue : status
  (** [100]. *)

  val s200_ok : status
  (** [200]. *)

  module Request :
  sig
    type body
    (** The type for request bodies. *)

    val body : req -> body

    val meth : req -> HTTP.meth

    val uri : req -> uri

    val headers : req -> HTTP.headers
    (** [headers r] is [r]'s information. Initially empty it can be used be
        services and layers to store and share data. *)

    val with_headers : req -> HTTP.headers -> req
    (** [with_headers req hs] is [req] with headers [hs]. *)

    val with_path : req -> HTTP.path -> req
    (** [with_path req p] is [req] with path [p]. *)

    val with_body : req -> body -> req
    (** [with_body req body] is [req] with body [body]. *)

    val v : ?version:HTTP.version -> HTTP.meth -> path:HTTP.path -> ?query:(string * string list) list -> HTTP.headers -> body -> req
    (** [v meth ~path headers body] is an HTTP request with the given
        components. [query] defaults to [None]. *)
  end

  module Response :
  sig
    type body (* = unit -> (raw * int * int) option io *)
    (** The type for request bodies.contents
        A body is a function that yields byte chunks of the request body as
        [Some (bytes, pos, len)] values. The bytes value must not be modified
        and is readable from [pos] to [pos+len] until the next call to the
        function. The function returns [None] at the end of stream. *)

    val body : resp -> body
    (** [body r] is [r]'s body. *)

    val status : resp -> status
    (** [status r] is [r]'s headers. *)
  end
end

module type FILE =
sig
  type +'a io

  type path
  type error
  type lock

  val pp_error : error Fmt.t

  val exists : path -> (bool, error) result io
  val delete : ?lock:lock -> path -> (unit, error) result io
  val move : ?lock:lock -> path -> path -> (unit, error) result io

  type raw
  type 'a fd constraint 'a = [< `Read | `Write ]

  val test_and_set : ?lock:lock -> ?temp:path -> path -> test:Cstruct.t option -> set:Cstruct.t option -> (bool, error) result io
  val open_w : ?lock:lock -> path -> mode:int -> ([ `Write ] fd, error) result io
  val open_r : ?lock:lock -> path -> mode:int -> ([ `Read ] fd, error) result io
  val write : raw -> ?off:int -> ?len:int -> [> `Write ] fd -> (int, error) result io
  val read : raw -> ?off:int -> ?len:int -> [> `Read ] fd -> (int, error) result io
  val close : 'a fd -> (unit, error) result io
end

module type MAPPER =
sig
  type +'a io

  type fd
  type raw
  type path
  type error

  val pp_error : error Fmt.t

  val openfile : path -> (fd, error) result io
  val length : fd -> (int64, error) result io
  val map : fd -> ?pos:int64 -> share:bool -> int -> (raw, error) result io
  val close : fd -> (unit, error) result io
end

module type DIR =
sig
  type +'a io

  type path
  type error

  val pp_error : error Fmt.t

  val exists : path -> (bool, error) result io
  val create : ?path:bool -> ?mode:int -> path -> (bool, error) result io
  val delete : ?recurse:bool -> path -> (unit, error) result io
  val contents : ?dotfiles:bool -> ?rel:bool -> path -> (path list, error) result io
  val current : unit -> (path, error) result io
  val temp : unit -> path io
end

module type FS =
sig
  type +'a io

  type path
  type error

  val pp_error : error Fmt.t

  val is_dir : path -> (bool, error) result io
  val is_file : path -> (bool, error) result io

  module File : FILE with type path = path and type +'a io = 'a io
  module Dir : DIR with type path = path and type +'a io = 'a io
  module Mapper : MAPPER with type path = path and type +'a io = 'a io
end
