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

(** Clone/Fecth/Push protocol *)

type protocol = [ `SSH | `Git | `Smart_HTTP ]
(** The type for the different Git protocols. *)

val protocol: Uri.t -> [`Ok of protocol | `Not_supported of string | `Unknown]
(** [protocol uri] is the Git protocol associated to [uri]. *)

type capability =
  [ `Multi_ack
  | `Thin_pack
  | `Side_band
  | `Side_band_64k
  | `Ofs_delta
  | `Shallow
  | `No_progress
  | `Include_tag
  | `Report_status
  | `Delete_refs
  | `Agent of string
  | `Other of string ]

module Result: sig

  type fetch = {
    head      : SHA.Commit.t option;
    references: SHA.Commit.t Reference.Map.t;
    sha1s     : SHA.t list;
  }
  (** The resulting sha1s and references. *)

  val pretty_fetch: fetch -> string
  (** Pretty print a fetch result. *)

  type ok_or_error = [`Ok | `Error of string]

  type push = {
    result  : ok_or_error;
    commands: (Reference.t * ok_or_error) list;
  }
  (** The result of a push operation. *)

  val pretty_push: push -> string
  (** Pretty print a push status. *)

end

module type S = sig

  type t
  (** Abstract value for stores. *)

  type ctx
  (** Connection context *)

  (** {1 The base Git protocol and Git+SSH} *)

  val ls: ?ctx:ctx -> t -> Gri.t -> SHA.Commit.t Reference.Map.t Lwt.t
  (** List the references of the remote repository. *)

  val push: ?ctx:ctx -> t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
  (** Push a local branch to a remote store. *)

  val clone: ?ctx:ctx -> t -> ?deepen:int -> ?unpack:bool ->
    ?capabilities:capability list -> ?head:Reference.head_contents ->
    ?progress:(string -> unit) ->
    Gri.t -> Result.fetch Lwt.t
  (** [clone t address] clones the contents of [address] into the
      store [t]. If [head] is set, only the history of the given SHA1
      or references will be downloaded. If [head] is not set
      (default), all the whole history (corresponding to {i all} the
      remote heads) will be downloaded. *)

  val fetch: ?ctx:ctx -> t -> ?deepen:int -> ?unpack:bool ->
    ?capabilities:capability list -> ?progress:(string -> unit) ->
    Gri.t -> Result.fetch Lwt.t
  (** [fetch t address] fetches the missing contents of [address] into
      the store [t]. *)

end

(** {2 Constructor} *)

module type IO = sig

  (** Channel abstraction. *)

  type ic
  (** Type for input channels. *)

  type oc
  (** Type for output channels. *)

  type ctx
  (** Connection context. *)

  val with_connection: ?ctx:ctx -> Uri.t -> ?init:string ->
    (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  (** Connect to a remote server, get the corresponding input and
      output channels and apply a function on them. Close the channel
      once the function returns. The [init] corresponds to an optional
      first message sent on the connection to set-it up. *)

  val read_all: ic -> string list Lwt.t
  (** Read all the channel contents (until the channel is closed by
      the other side). *)

  val read_exactly: ic -> int -> string Lwt.t
  (** Read a given number of bytes. *)

  val write: oc -> string -> unit Lwt.t
  (** Write a string on a channel. *)

  val flush: oc -> unit Lwt.t
  (** Flush the channel. *)

end

module Make (IO: IO) (S: Store.S): S with type t = S.t and type ctx = IO.ctx
