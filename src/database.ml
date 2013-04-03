(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type BACKEND = sig

  (** Database handler *)
  type t

  (** The type of stored Values. *)
  type value

  (** The type of keys used to lookup stored values. *)
  type key

  (** Every object in the database is a node. *)
  type node

  (** Every [commit] operation generates a new revision. Thus
      [revision] has to be comparable, and they can be seen as a marker
      of time. NB: we are assuming a branching time, ie. we allow a
      partial order on revisions. *)
  type revision

  (** Tags are short-cut to specific revisions. Usually they come from
      a global namespace or are generated following
      conventions. Examples of useful tags: the current local state,
      transaction states, remote states, etc. *)
  type tag

  (** Create a new node. *)
  val write: t -> value -> node

  (** Read a node contents. *)
  val read: t -> node -> value option

  (** List all the nodes. *)
  val list: t -> node list

  (** Get the node corresponding to a given key for a given revision.
      In case the node does not exists, return None.*)
  val find: t -> revision -> key -> node option

  (** Merge two revisions. If the merge does not succeed, return [None]. *)
  val merge: t -> (value -> value -> value) -> revision -> revision -> revision option

  (** Implicit graph of nodes. *)
  val succ: t -> node -> node list

end
