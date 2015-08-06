(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Search over the graph of Git objects. *)

type pred = [
  | `Commit of SHA.t
  | `Tag of string * SHA.t
  | `Tree of string * SHA.t
  | `Tree_root of SHA.t
]
(** The type for {!Value.t} predecessors. *)

module Make (S: Store.S): sig

  val pred: S.t -> ?full:bool -> SHA.t -> pred list Lwt.t
  (** [pred t s] is the list of [s]'s predecessors in the graph
      [t]. If [full] is not set (by default it is) only consider
      commits and their history relation. *)

  type path = [
    | `Tag of string * path
    | `Commit of path
    | `Path of string list
  ]
  (** The type for path values. See {!find} for details. *)

  val mem: S.t -> SHA.t -> path -> bool Lwt.t
  (** [mem t s p] check wether there exists a path [p] from [s] in
      [t]. *)

  val find: S.t -> SHA.t -> path -> SHA.t option Lwt.t
  (** [find t s p] follows the path [p] from [s] in [t]. *)

end
