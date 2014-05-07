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

(** Clone/Fecth/Push protocol *)


type fetch_result = {
  head      : SHA1.Commit.t option;
  references: SHA1.Commit.t Reference.Map.t;
  sha1s     : SHA1.t list;
}
(** The resulting sha1s and references. *)

type ok_or_error = Ok | Error of string

type push_result = {
  result: ok_or_error;
  commands: (Reference.t * ok_or_error) list;
}
(** The result of a push operation. *)

module type IO = sig

  (** Channel abstraction. XXX: reuse something existing ... *)

  type ic
  (** Type for input channels. *)

  type oc
  (** Type for output channels. *)

  val with_connection: Uri.t -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  (** Connect to a remote server, get the corresponding input and
      output channels and apply a function on them. Close the channel
      once the function returns. *)

  val read_all: ic -> string Lwt.t
  (** Read all the channel contents (until the channel is closed by
      the other side). *)

  val read_exactly: ic -> int -> string Lwt.t
  (** Read a given number of bits. *)

  val write: oc -> string -> unit Lwt.t
  (** Write a string on a channel. *)

  val flush: oc -> unit Lwt.t
  (** Flush the channel. *)

end

module type S = sig

  (** Remote operation, abstracted over the bakend type. *)

  type t
  (** Abstract value for stores. *)

  val ls: t -> Uri.t -> SHA1.Commit.t Reference.Map.t Lwt.t
  (** List the references of the remote repository. *)

  val push: t -> branch:Reference.t -> Uri.t -> push_result Lwt.t
  (** Push a local branch to a remote store. *)

  val clone: t -> ?bare:bool -> ?deepen:int -> ?unpack:bool -> Uri.t -> fetch_result Lwt.t
  (** [clone t address] clones the contents of [address] into the
      store [t]. *)

  val fetch: t -> ?deepen:int -> ?unpack:bool -> Uri.t -> fetch_result Lwt.t
  (** [fetch t address] fetches the missing contents of [address] into
      the store [t]. *)

end

module Make (IO: IO) (S: Store.S): S with type t = S.t
