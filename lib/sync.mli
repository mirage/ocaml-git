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
  | `No_thin
  | `Side_band
  | `Side_band_64k
  | `Ofs_delta
  | `Shallow
  | `No_progress
  | `Include_tag
  | `Report_status
  | `Delete_refs
  | `Allow_reachable_sha1_in_want (* in Git 2.5 only *)
  | `Agent of string
  | `Other of string ]

module Result: sig

  type fetch
  (** The resulting hashes and references. *)

  val head_contents: fetch -> Reference.head_contents option
  (** [head_contents f] is [f]'s head contents (the value of the
      remote {i .git/HEAD}). *)

  val head: fetch -> Hash.Commit.t option
  (** [head f] is [f]'s head commit. *)

  val references: fetch -> Hash.Commit.t Reference.Map.t
  (** [references f] are [f]'s references. *)

  val hashes: fetch -> Hash.Set.t
  (** [hashes f] are [f]'s object hashes. *)

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

type want = [ `Ref of Reference.t | `Commit of Hash.Commit.t ]
(** The type for values wanted. See {!fetch} for details. *)

module type S = sig

  type t
  (** Abstract value for stores. *)

  type ctx
  (** Connection context *)

  (** {1 The base Git protocol and Git+SSH} *)

  val ls: ?ctx:ctx -> t -> Gri.t -> Hash.Commit.t Reference.Map.t Lwt.t
  (** List the references of the remote repository. *)

  val push: ?ctx:ctx -> t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
  (** Push a local branch to a remote store. *)

  val fetch:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?wants:want list ->
    ?progress:(string -> unit) ->
    t -> Gri.t -> Result.fetch Lwt.t
  (** [fetch t uri] fetches the contents of [uri] into the store [t].

      By default, all the remote references are updated. This behavior
      can be changed by using the [wants] parameter:

      {ul
      {- if [wants] is not specified, the objects corresponding to {b
         all} the remote references are downloaded. This is useful
         when cloning a new repository as the remote references are
         not yet known.}
      {- If a reference (e.g. a [`Ref] {!want} value) appears in the
         list, the object corresponding to that remote reference are
         fetched. This works only if the server exports the
         "allow-reachable-sha1-in-want" (available in Git 2.5.0)}
      {- If a commit hash (e.g. a [`Commit] {!want} value) in the
         list, the objects corresponding to the that remote commits
         are fetched..}  }

      {b Note:} the local HEAD is not modified when doing a fetch. To do so
      (for instance when doing a clone) do the following:

      {[
        fetch t gri >>= fun r ->
        match Result.head_contents r with
        | Some h -> Store.write_head t h
        | None   -> Lwt.return_unit
      ]}
  *)

  (** Similar to {!fetch} but also initialise [HEAD] and a the
      specified [branch]. If [branch] is not specified, initialise all
      the references that the remote repository exposes. *)
  val clone:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?branch:want ->
    ?progress:(string -> unit) ->
    t -> checkout:bool -> Gri.t -> Result.fetch Lwt.t

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
