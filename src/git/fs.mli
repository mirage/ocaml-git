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

module type FILE =
sig
  type path
  type error
  type lock

  val pp_error : error Fmt.t

  val exists   : path -> (bool, error) result Lwt.t
  val delete   : ?lock:lock -> path -> (unit, error) result Lwt.t
  val move     : ?lock:lock -> path -> path -> (unit, error) result Lwt.t

  val lock     : path -> lock

  type raw
  type 'a fd constraint 'a = [< `Read | `Write ]

  val test_and_set : ?lock:lock -> ?temp:path -> path -> test:Cstruct.t option -> set:Cstruct.t option -> (bool, error) result Lwt.t
  val open_w       : ?lock:lock -> path -> mode:int -> ([ `Write ] fd, error) result Lwt.t
  val open_r       : ?lock:lock -> path -> mode:int -> ([ `Read ] fd, error) result Lwt.t
  val write        : raw -> ?off:int -> ?len:int -> [> `Write ] fd -> (int, error) result Lwt.t
  val read         : raw -> ?off:int -> ?len:int -> [> `Read ] fd -> (int, error) result Lwt.t
  val close        : 'a fd -> (unit, error) result Lwt.t
end

module type MAPPER =
sig
  type fd
  type raw
  type path
  type error

  val pp_error : error Fmt.t

  val openfile : path -> (fd, error) result Lwt.t
  val length   : fd -> (int64, error) result Lwt.t
  val map      : fd -> ?pos:int64 -> share:bool -> int -> (raw, error) result Lwt.t
  val close    : fd -> (unit, error) result Lwt.t
end

module type DIR =
sig
  type path
  type error

  val pp_error : error Fmt.t

  val exists   : path -> (bool, error) result Lwt.t
  val create   : ?path:bool -> ?mode:int -> path -> (bool, error) result Lwt.t
  val delete   : ?recurse:bool -> path -> (unit, error) result Lwt.t
  val contents : ?dotfiles:bool -> ?rel:bool -> path -> (path list, error) result Lwt.t
  val current  : unit -> (path, error) result Lwt.t
  val temp     : unit -> path Lwt.t
end

module type S =
sig
  type path
  type error

  val pp_error  : error Fmt.t

  val is_dir    : path -> (bool, error) result Lwt.t
  val is_file   : path -> (bool, error) result Lwt.t

  module File   : FILE   with type path = path
  module Dir    : DIR    with type path = path
  module Mapper : MAPPER with type path = path
end
