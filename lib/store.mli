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

module type S = sig

  (** Signature for Git stores. *)

  type t
  (** Git store handlers. *)

  val create: ?root:string -> ?dot_git:string -> ?level:int ->
    unit -> t Lwt.t
  (** Create a store handler for the given path. See {!root} and
      {!level}.*)

  val dot_git: t -> string
  (** The location of the [.git] directory. By defaut it is
      {!root}[/.git]. *)

  val root: t -> string
  (** The location of the repository root (or any other meaningful
      name to be displayed to the user). By default, it is the current
      directory. *)

  val level: t -> int
  (** The compression [level] used when creating new Git objects. must
      be between 0 and 9: 1 gives best speed, 9 gives best
      compression, 0 gives no compression at all (the input data is
      simply copied a block at a time). The default value (currently
      equivalent to level 6) requests a default compromise between
      speed and compression. *)

  val dump: t -> unit Lwt.t
  (** Dump the store contents to stderr. *)

  val contents: t -> (Hash.t * Value.t) list Lwt.t
  (** Get the full store contents. *)

  (** {2 Objects} *)

  val read: t -> Hash.t -> Value.t option Lwt.t
  (** Return the object having the given Hash name. *)

  val read_exn: t -> Hash.t -> Value.t Lwt.t
  (** Same as [read] but raises [Not_found] if no object with the given
      Hash is found. *)

  val mem: t -> Hash.t -> bool Lwt.t
  (** Check whether a key belongs to the store. *)

  val list: t -> Hash.t list Lwt.t
  (** Return the list of Hash names. *)

  val write: t -> Value.t -> Hash.t Lwt.t
  (** Write a value and return the Hash of its serialized contents. *)

  val write_pack: t -> Pack.raw -> Hash.Set.t Lwt.t
  (** Write a raw pack file and the corresponding index. Return the
      objects IDs which have been written. *)

  (** {2 References} *)

  val references: t -> Reference.t list Lwt.t
  (** Return the list of references (ie. tags and branches). *)

  val mem_reference: t -> Reference.t -> bool Lwt.t
  (** Check if a reference exists. *)

  val read_reference: t -> Reference.t -> Hash.Commit.t option Lwt.t
  (** Read a given reference. *)

  val read_reference_exn: t -> Reference.t -> Hash.Commit.t Lwt.t
  (** Read a given reference. *)

  val write_head: t -> Reference.head_contents -> unit Lwt.t
  (** Write the HEAD. *)

  val read_head: t -> Reference.head_contents option Lwt.t
  (** Read the head contents. *)

  val write_reference: t -> Reference.t -> Hash.Commit.t -> unit Lwt.t
  (** Write a reference. *)

  val remove_reference: t -> Reference.t -> unit Lwt.t
  (** Remove a refernce. *)

  (** {2 Git index files} *)

  val read_index: t -> Index.t Lwt.t
  (** Return the index file. *)

  val write_index: t -> ?index:Index.t -> Hash.Commit.t -> unit Lwt.t
  (** Update the index file for the given revision. A side-effect of
      this operation is that the blobs are expanded into the
      filesystem.

      {b Note:} It is the user responsability to ensure that filenames
      are valid. No sanitazition is done by the library -- the Git
      format does not impose a filename format as this is a constraint
      of the underlying filesystem.

      If [index] is not set, read the current index and update it with
      the current state of the filesystem. *)

  (** {2 Backend kind} *)

  val kind: [`Memory | `Disk]
  (** The kind of backend. *)

  (** {2 Raw values} *)

  val read_inflated: t -> Hash.t -> string option Lwt.t
  (** Read a raw buffer from the store. *)

  val write_inflated: t -> string -> Hash.t Lwt.t
  (** Write a raw buffer in the store. *)

  module Digest: Hash.DIGEST
  (** Digest functions that the store is using. *)

  module Inflate: Inflate.S
  (** Inflate functions that the store is using. *)

end
