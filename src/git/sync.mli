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

(** The synchronization commands to a git repository. *)

(** This interface describes the minimal I/O operations to a git
    repository. *)
module type NET = sig
  type socket

  val read: socket -> Bytes.t -> int -> int -> int Lwt.t
  val write: socket -> Bytes.t -> int -> int -> int Lwt.t
  val socket: string -> int -> socket Lwt.t
  val close: socket -> unit Lwt.t
end

module type S = sig

  module Store: Minimal.S
  module Net: NET
  module Client: Smart.CLIENT with module Hash = Store.Hash

  (** The error type. *)
  type error =
    [ `SmartPack of string (** Appear when we retrieve a decoder's error about Smart protocol. *)
    | `Pack      of Store.Pack.error (** Appear when we retrieve a PACK error from the {!Store.Pack}. *)
    | `Clone     of string (** Appear when we don't follow operations on the clone command. *)
    | `Fetch     of string (** Appear when we don't follow operations on the fetch command. *)
    | `Ls        of string (** Appear when we don't follow operations on the ls-remote command. *)
    | `Push      of string (** Appear when we don't follow operations on the push command. *)
    | `Ref       of Store.Ref.error (** Appear when we retrieve a reference I/O error from the {!Store.Ref}. *)
    | `Not_found (** Appear when we don't find the reference requested by the client on the server. *) ]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** A push command to interact with the server. *)
  type command =
    [ `Create of (Store.Hash.t * string)
    (** To create a new reference on the server. *)
    | `Delete of (Store.Hash.t * string)
    (** To delete an existing reference on the server -
        [`Delete_refs] needs to be available in both side as
        a {!Capability.t}. *)
    | `Update of (Store.Hash.t * Store.Hash.t * string)
    (** To update a reference from a commit hash to a new commit hash. *) ]

  val push:
    Store.t
    -> push:(Store.t -> (Store.Hash.t * string * bool) list -> (Store.Hash.t list * command list) Lwt.t)
    -> ?port:int
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> ((string, string * string) result list, error) result Lwt.t

  val ls :
    Store.t
    -> ?port:int
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> ((Store.Hash.t * string * bool) list, error) result Lwt.t

  val fetch_ext:
    Store.t
    -> ?shallow:Store.Hash.t list
    -> ?capabilities:Capability.t list
    -> notify:(Client.Decoder.shallow_update -> unit Lwt.t)
    -> negociate:((Client.Decoder.acks -> 'state -> ([ `Ready | `Done | `Again of Store.Hash.t list ] * 'state) Lwt.t) * 'state)
    -> has:Store.Hash.t list
    -> want:((Store.Hash.t * string * bool) list -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[ `Depth of int | `Timestamp of int64 | `Ref of string ]
    -> ?port:int
    -> string
    -> string
    -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

  val clone_ext :
    Store.t
    -> ?port:int
    -> ?reference:Store.Reference.t
    -> ?capabilities:Capability.t list
    -> string
    -> string
    -> (Store.Hash.t, error) result Lwt.t

  val fetch_all: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    Uri.t ->
    (unit, error) result Lwt.t

  val fetch_one: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    reference:Reference.t ->
    Uri.t -> (unit, error) result Lwt.t

  val clone: Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Capability.t list ->
    reference:Reference.t -> Uri.t ->
    (unit, error) result Lwt.t

  val update: Store.t -> ?capabilities:Capability.t list ->
    reference:Reference.t -> Uri.t ->
    ((Reference.t, Reference.t * string) result list, error) result Lwt.t

end

module Make (N: NET) (S: Minimal.S)
  : S with module Store = S
       and module Net = N
