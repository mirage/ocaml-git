(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Git ressource identifiers (GRI). *)

(** GRIs are similar to URIs, but Git also allows some blend of URL
    and SSH remote files such as:

      git@github.com:samoht/ocaml-git.git

*)

type t
(** Git ressource identifier values. *)

val of_string: string -> t
(** Create a GRI from a string. *)

val to_string: t -> string
(** Convert a string to a GRI. *)

val to_uri: t -> Uri.t
(** Cast to [Uri.t]. *)

val of_uri: Uri.t -> t
(** Cast from [Uri.t]. *)
