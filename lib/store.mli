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

module type S = sig

  (** Signature for Git stores. *)

  type t
  (** Git store handlers. *)

  val create: ?root:string -> unit -> t Lwt.t
  (** Create a store handler for the given path. If [root] is not set,
      use the current directory. *)

  val clear: t -> unit Lwt.t
  (** Remove all the contents of the Git store. *)

  val root: t -> string
  (** The state root (or any other meaningful name to be displayed to
      the user). *)

  val dump: t -> unit Lwt.t
  (** Dump the store contents to stderr. *)

  val contents: t -> (SHA.t * Value.t) list Lwt.t
  (** Get the full store contents. *)

  (** {2 Objects} *)

  val read: t -> SHA.t -> Value.t option Lwt.t
  (** Return the object having the given SHA name. *)

  val read_exn: t -> SHA.t -> Value.t Lwt.t
  (** Same as [read] but raises [Not_found] if no object with the given
      SHA is found. *)

  val mem: t -> SHA.t -> bool Lwt.t
  (** Check whether a key belongs to the store. *)

  val list: t -> SHA.t list Lwt.t
  (** Return the list of SHA names. *)

  val write: t -> Value.t -> SHA.t Lwt.t
  (** Write a value and return the SHA of its serialized contents. *)

  val write_pack: t -> Pack.Raw.t -> SHA.Set.t Lwt.t
  (** Write a raw pack file and the corresponding index. Return the
      objects IDs which have been written. *)

  (** {2 References} *)

  val references: t -> Reference.t list Lwt.t
  (** Return the list of references (ie. tags and branches). *)

  val mem_reference: t -> Reference.t -> bool Lwt.t
  (** Check if a reference exists. *)

  val read_reference: t -> Reference.t -> SHA.Commit.t option Lwt.t
  (** Read a given reference. *)

  val read_reference_exn: t -> Reference.t -> SHA.Commit.t Lwt.t
  (** Read a given reference. *)

  val write_head: t -> Reference.head_contents -> unit Lwt.t
  (** Write the HEAD. *)

  val read_head: t -> Reference.head_contents option Lwt.t
  (** Read the head contents. *)

  val write_reference: t -> Reference.t -> SHA.Commit.t -> unit Lwt.t
  (** Write a reference. *)

  val remove_reference: t -> Reference.t -> unit Lwt.t
  (** Remove a refernce. *)

  (** {2 Cache file} *)

  val read_cache: t -> Cache.t Lwt.t
  (** Return the cache of files. *)

  val write_cache: t -> SHA.Commit.t -> unit Lwt.t
  (** Update the cache of files for the given revision. XXX: need a
      merge stategy. *)

  val kind: [`Memory | `Disk]
  (** The kind of backend. *)

end
