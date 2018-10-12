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

(** Purpose of this module is little bit hard to understand. Indeed, in the
    previous version of ocaml-git, we used [Fpath] to have a good abstraction
    of path in UNIX and Windows systems. However, Git keeps in some ways (INDEX
    file specially) the UNIX path semantic (tree and reference use it too). So,
    [Gpath] is an abstraction (Git-path) to keep UNIX path semantic (and the
    separator "/"). However, we used it in few cases.

    Indeed, any interation with a FS use an [Fpath] and [Gpath] is used only
    when we talk with the Git kernel (for example, the [S.fold] function). The
    biggest differences between [Fpath] and [Gpath] is:

    - The empty element, that means, any [Gpath] __is__ (must be) relative with
    a [Fpath].

    - The UNIX separator in any platform, so we need to cast a [Gpath] to an
    [Fpath] then when we want to talk with the FS.

    It's really annoying to handle it like this but Windows is a shit. *)

type t

val empty : t
val root : t
val v : string -> t
val add : t -> string -> t
val ( / ) : t -> string -> t
val append : t -> t -> t
val ( // ) : t -> t -> t
val ( + ) : Fpath.t -> t -> Fpath.t
val segs : t -> string list
val of_segs : string list -> t
val basename : t -> string
val parent : t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val pp : t Fmt.t
val to_string : t -> string
val has_ext : string -> t -> bool
val rem_ext : ?multi:bool -> t -> t
