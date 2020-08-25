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

module type S = sig
  type hash

  type store

  module S : Set.S with type elt = hash
  (** An imperative Graph of the store. *)

  module K : Graph.Sig.I with type V.t = hash

  val keys : K.t -> hash list
  (** [keys graph] returns all hashes recheables in the graph [graph]. *)

  val of_keys : store -> K.t Lwt.t
  (** [of_keys store] makes a new graph from all values of a [store]. *)

  val of_commits : store -> K.t Lwt.t
  (** [of_commits store] makes a new graph from all commits of a [store]. *)

  val closure : ?full:bool -> store -> min:S.t -> max:S.t -> K.t Lwt.t

  val pack : store -> min:S.t -> max:S.t -> (hash * hash Value.t) list Lwt.t

  val to_dot : store -> Format.formatter -> unit Lwt.t
end

module Make (S : Minimal.S) : S with module Store = S
