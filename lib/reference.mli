(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Branch references. *)

open Core_kernel.Std

include Identifiable.S

val head: t
(** The repository HEAD. *)

val master: t
(** The master branch. *)

val is_head: t -> bool
(** Is the given reference the HEAD ? *)

type head_contents =
  | SHA1 of SHA1.Commit.t
  | Ref of t
(** The possible HEAD contents. *)

val head_contents: SHA1.Commit.t Map.t -> SHA1.Commit.t -> head_contents
(** Compute the head contents. The result is either the hex
    representation of the SHA1 or something like {i ref: <ref>} if the
    SHA1 has already a reference pointing to it. *)

val is_valid: t -> bool
(** Check if a reference can be stored on disk properly. *)
