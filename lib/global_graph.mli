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

(** Manage object graphs. *)

module C: Graph.Sig.I with type V.t = (SHA1.t * Value.t)
                       and type E.label = string
(** Contents graph. *)

module K: Graph.Sig.I with type V.t = SHA1.t
(** Key graph. *)

module Make (S: Store.S): sig

  val create_contents: S.t -> C.t Lwt.t
  (** Create a graph of contents. *)

  val create_key: S.t -> K.t Lwt.t
  (** Create a graph of keys. *)

  val pack: S.t -> ?min:SHA1.Set.t -> SHA1.Set.t -> Pack.t Lwt.t
  (** Find a consistent cut in the graph of keys with [min] as minimal
      elements and [max] as maximal elements. If [min] is empty,
      return all the predecessors of [max]. *)

  val to_dot: S.t -> string -> unit Lwt.t
  (** Export the Git store as a "graphviz" file. *)

end
