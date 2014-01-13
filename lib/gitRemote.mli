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

(** Clone/Fecth/Push protocol *)

open GitTypes

type result = {
  references: (sha1 * reference) list;
  sha1s     : sha1 list;
}
(** The resulting sha1s and references. *)

val set_expand_hook: (string list -> perm -> blob -> unit Lwt.t) -> unit
(** Set up the expand hook (to avoid cyclic deps) *)

module type IO = sig

  (** Channel abstraction. XXX: reuse something existing ... *)

  type ic
  (** Type for input channels. *)

  type oc
  (** Type for output channels. *)

  val with_connection: string -> int option -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
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

  val ls: t -> string -> (sha1 * reference) list Lwt.t
  (** List the references in the remote repository. *)

  val clone: t -> ?bare:bool -> ?deepen:int -> string -> result Lwt.t
  (** [clone t address] clones the contents of [address] into the
      store [t]. *)

  val fetch: t -> ?deepen:int -> string -> result Lwt.t
  (** [fetch t address] fetches the missing contents of [address] into
      the store [t]. *)

end

module Make (IO: IO) (S: GitTypes.S): S with type t = S.t
