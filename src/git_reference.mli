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

include Git_s.IO

val to_raw: t -> string
val of_raw: string -> t

module Map: Git_s.Map with type key = t

val head: t

val master: t

val is_head: t -> bool

type head_contents =
  | Hash of Git_hash.Commit.t
  | Ref of t

val head_contents_of_string: of_hex:(string -> Git_hash.t) -> string -> head_contents

val head_contents_of_commit: Git_hash.Commit.t Map.t -> Git_hash.Commit.t -> head_contents

val pp_head_contents: Format.formatter -> head_contents -> unit

val equal_head_contents: head_contents -> head_contents -> bool

val is_valid: t -> bool
