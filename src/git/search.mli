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
module Make (Hash : Digestif.S) (Store : Minimal.S with type hash = Hash.t) : sig
  type hash = Hash.t

  type store = Store.t

  type pred =
    [ `Commit of hash
    | `Tag of string * hash
    | `Tree of string * hash
    | `Tree_root of hash ]

  val pred : store -> ?full:bool -> hash -> pred list Lwt.t

  type path = [ `Tag of string * path | `Commit of path | `Path of string list ]

  val mem : store -> hash -> path -> bool Lwt.t

  val find : store -> hash -> path -> hash option Lwt.t
end
