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

open Core_kernel.Std

module type S = sig

  (** Signature for Git stores. *)

  type t
  (** Git store handlers. *)

  val create: ?root:string -> unit -> t Lwt.t
  (** Create a store handler for the given path. If [root] is not set,
      use the current directory. *)

  val root: t -> string
  (** The state root (or any other meaningful name to be displayed to
      the user). *)

  val dump: t -> unit Lwt.t
  (** Dump the store contents of cache to stderr. *)

  (** {2 Objects} *)

  val read: t -> SHA1.t -> Value.t option Lwt.t
  (** Return the object having the given SHA1 name. *)

  val read_exn: t -> SHA1.t -> Value.t Lwt.t
  (** Same as [read] but raises [Not_found] if no object with the given
      SHA1 is found. *)

  val mem: t -> SHA1.t -> bool Lwt.t
  (** Check whether a key belongs to the store. *)

  val read_inflated: t -> SHA1.t -> Bigstring.t option Lwt.t
  (** Return the inflated contents of object having the given SHA1
      name. *)

  val read_inflated_exn: t -> SHA1.t -> Bigstring.t Lwt.t
  (** Same as [read_inflated] but raise [Not_found] if the key does
      not exist. *)

  val list: t -> SHA1.t list Lwt.t
  (** Return the list of SHA1 names. *)

  val write: t -> Value.t -> SHA1.t Lwt.t
  (** Write a value and return the SHA1 of its serialized contents. *)

  val write_and_check_inflated: t -> SHA1.t -> Bigstring.t -> unit Lwt.t
  (** Compress a binary value and write it in the store. Check that
      the SHA1 of the uncompressed contents is the one that is
      expected. *)

  val write_pack: t -> Pack.t -> Pack_index.t Lwt.t
  (** Write a raw pack file and its corresponding index. *)

  val write_raw_pack: t -> Bigstring.t -> Pack_index.t Lwt.t
  (** Same as [write_pack] but with a raw pack file. *)

  (** {2 References} *)

  val references: t -> Reference.t list Lwt.t
  (** Return the list of references (ie. tags and branches). *)

  val mem_reference: t -> Reference.t -> bool Lwt.t
  (** Check if a reference exists. *)

  val read_reference: t -> Reference.t -> SHA1.Commit.t option Lwt.t
  (** Read a given reference. *)

  val read_reference_exn: t -> Reference.t -> SHA1.Commit.t Lwt.t
  (** Read a given reference. *)

  val write_reference: t -> Reference.t -> SHA1.Commit.t -> unit Lwt.t
  (** Write a reference. *)

  val remove_reference: t -> Reference.t -> unit Lwt.t
  (** Remove a refernce. *)

  (** {2 Type Inspection} *)

  val type_of: t -> SHA1.t -> Object_type.t option Lwt.t
  (** Return the object type corresponding to the given SHA1. *)

  val succ: t -> SHA1.t -> Value.successor list Lwt.t
  (** Return the list of [successors] of a given object. *)

  (** {2 Cache file} *)

  val read_cache: t -> Cache.t Lwt.t
  (** Return the cache of files. *)

  val write_cache: t -> SHA1.Commit.t -> unit Lwt.t
  (** Update the cache of files for the given revision. XXX: need a
      merge stategy. *)

end
