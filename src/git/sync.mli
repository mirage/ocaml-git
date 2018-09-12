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

(** This interface describes the minimal I/O operations to a git repository. *)
module type NET = sig
  type socket
  type error

  val pp_error : Format.formatter -> error -> unit
  val read : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val write : socket -> Bytes.t -> int -> int -> (int, error) result Lwt.t
  val socket : Uri.t -> socket Lwt.t
  val close : socket -> unit Lwt.t
end

module type S = sig
  module Store : Minimal.S
  module Net : NET

  module Client :
    Smart.CLIENT
    with module Hash := Store.Hash
     and module Reference := Store.Reference

  (** The error type. *)
  type error =
    [ `Sync of string
      (** Appear when we retrieve an unexpected response from server. *)
    | `Store of Store.error
      (** Appear when we retrieve a PACK error from the {!Store.Pack}. *)
    | `Net of Net.error  (** Appear when we retrieve an I/O error. *)
    | `Smart of Client.Decoder.error
      (** Appear when internal parser got an error. *)
    | `Not_found
      (** Appear when we don't find the reference requested by the client on
          the server. *) ]

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  (** A push command to interact with the server. *)
  type command =
    [ `Create of Store.Hash.t * Store.Reference.t
      (** To create a new reference on the server. *)
    | `Delete of Store.Hash.t * Store.Reference.t
      (** To delete an existing reference on the server - [`Delete_refs] needs
          to be available in both side as a {!Capability.t}. *)
    | `Update of Store.Hash.t * Store.Hash.t * Store.Reference.t
      (** To update a reference from a commit hash to a new commit hash. *) ]

  val pp_command : command Fmt.t
  (** Pretty-printer of {!command}. *)

  val push :
       Store.t
    -> push:(   (Store.Hash.t * Store.Reference.t * bool) list
             -> (Store.Hash.t list * command list) Lwt.t)
    -> ?capabilities:Capability.t list
    -> Uri.t
    -> ( (Store.Reference.t, Store.Reference.t * string) result list
       , error )
       result
       Lwt.t

  val ls :
       Store.t
    -> ?capabilities:Capability.t list
    -> Uri.t
    -> ((Store.Hash.t * Store.Reference.t * bool) list, error) result Lwt.t

  val fetch_ext :
       Store.t
    -> ?shallow:Store.Hash.t list
    -> ?capabilities:Capability.t list
    -> notify:(Client.Common.shallow_update -> unit Lwt.t)
    -> negociate:(   Client.Common.acks
                  -> 'state
                  -> ([`Ready | `Done | `Again of Store.Hash.Set.t] * 'state)
                     Lwt.t)
                 * 'state
    -> have:Store.Hash.Set.t
    -> want:(   (Store.Hash.t * Store.Reference.t * bool) list
             -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[`Depth of int | `Timestamp of int64 | `Ref of Reference.t]
    -> Uri.t
    -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

  val clone_ext :
       Store.t
    -> ?reference:Store.Reference.t
    -> ?capabilities:Capability.t list
    -> Uri.t
    -> (Store.Hash.t, error) result Lwt.t

  val fetch_some :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Uri.t
    -> ( Store.Hash.t Store.Reference.Map.t
         * Store.Reference.t list Store.Reference.Map.t
       , error )
       result
       Lwt.t
  (** [fetch_some git ?capabilities ~references repository] will fetch some
      remote references specified by [references].

      [references] is a map which: {ul {- the key is the {b remote} reference.}
      {- the value is a list of {b local} references - which may not exist
      yet.}}

      Then, the function will try to download all of these remote references
      and returns 2 maps:

      {ul {- the first map contains all local references updated by the new
      hash. This new hash is come from the server as the downloaded remote
      reference asked by the client by [references]. Then, from associated
      local references with remote references, we updated them with the
      associated hash.

      For example, if [references] is: {[ { "refs/heads/master": [
      "refs/remotes/origin/master" ; "refs/heads/master" ] } ]}

      We will update (or create) "refs/remotes/origin/master" and
      "refs/heads/master" with the new hash downloaded from the remote
      reference "refs/heads/master" only if it's necessary (only if we did not
      find the hash referenced by "refs/heads/master" in the local store).}

      {- the second map is a {b subset} of [references] which contains all
      binder of:

      {ul {- remote references which does not exist on the server side.} {-
      remote references which references to an already existing in the local
      store hash.}}}}

      The client should not put the same local reference as a value of some
      remote references. The client can define non-existing remote references
      (then, they appear on the second map). The client can want to set
      non-existing local references - we will create them.

      If the processus encountered an error when it updates references, it
      leaves but, it did partially some update on some local references. *)

  val fetch_all :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Uri.t
    -> ( Store.Hash.t Store.Reference.Map.t
         * Store.Reference.t list Store.Reference.Map.t
         * Store.Hash.t Store.Reference.Map.t
       , error )
       result
       Lwt.t
  (** [fetch_all git ?capabilities ~references repository] has the same
      semantic than {!fetch_some} for any remote references found on
      [references]. However, [fetch all] will download all remote references
      available on the server (and whose hash is not available on the local
      store). If these remote references are not associated with some local
      references, we return a third map which contains these remote references
      binded with the new hash downloaded.

      We {b don't} notice any non-downloaded remote references not found on the
      [references] map and whose hash already exists on the local store.

      Then, the client can bind these new hashes with specific local references
      or just give up. *)

  val fetch_one :
       Store.t
    -> ?capabilities:Capability.t list
    -> reference:Store.Reference.t * Store.Reference.t list
    -> Uri.t
    -> ( [`AlreadySync | `Sync of Store.Hash.t Store.Reference.Map.t]
       , error )
       result
       Lwt.t
  (** [fetch_one git ?capabilities ~reference repository] is a specific call of
      {!fetch_some} with only one reference. Then, it retuns:

      {ul {- [`AlreadySync] if the hash of the requested reference already
      exists on the local store} {- [`Sync updated] if we downloaded [new_hash]
      and set [local_ref] with this new hash.}} *)

  val clone :
       Store.t
    -> ?capabilities:Capability.t list
    -> reference:Store.Reference.t * Store.Reference.t
    -> Uri.t
    -> (unit, error) result Lwt.t

  val update_and_create :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Uri.t
    -> ( (Store.Reference.t, Store.Reference.t * string) result list
       , error )
       result
       Lwt.t
  (** As {!fetch_some}, [update git ?capabilities ~references repository] is
      the other side of the communication with a Git server and update and
      create remote references when it uploads local hashes.

      [reference] is a map which: {ul {- the key is the {b local} reference.}
      {- the value is a list of {b remote} references - which may not exist
      yet.}}

      Then, the function will try to upload all of these local references to
      the binded remote references. If binded remote reference does not exist
      on the server, we ask to the server to create and set it to the local
      hash.

      For each update action, we check if the local store has the remote hash.
      In other case, we miss this action - that means, the local store is not
      synchronized with the server (and the client probably needs to
      {!fetch_some} before).

      Then, it returns a list of results. The [Ok] case with the remote
      reference which the server updated correctly and the [Error] case with
      the remote reference which the server encountered an error with a
      description of this error.

      At this final stage, the function does not encountered any error during
      the commmunication - if it's the case, we did not do any modification on
      the server and return an {!error}. *)
end

module Common (G : Minimal.S) :
  sig
    module Store : Minimal.S

    type command =
      [ `Create of Store.Hash.t * Store.Reference.t
      | `Delete of Store.Hash.t * Store.Reference.t
      | `Update of Store.Hash.t * Store.Hash.t * Store.Reference.t ]

    val pp_command : command Fmt.t

    val packer :
         ?window:[`Object of int | `Memory of int]
      -> ?depth:int
      -> Store.t
      -> ofs_delta:bool
      -> (Store.Hash.t * Store.Reference.t * bool) list
      -> command list
      -> ( Store.Pack.stream * (Crc32.t * int64) Store.Hash.Map.t Lwt_mvar.t
         , Store.error )
         result
         Lwt.t

    val want_handler :
         Store.t
      -> (Store.Reference.t -> bool Lwt.t)
      -> (Store.Hash.t * Store.Reference.t * bool) list
      -> (Store.Reference.t * Store.Hash.t) list Lwt.t

    val update_and_create :
         Store.t
      -> references:Store.Reference.t list Store.Reference.Map.t
      -> (Store.Reference.t * Store.Hash.t) list
      -> ( Store.Hash.t Store.Reference.Map.t
           * Store.Reference.t list Store.Reference.Map.t
           * Store.Hash.t Store.Reference.Map.t
         , Store.error )
         result
         Lwt.t

    val push_handler :
         Store.t
      -> Store.Reference.t list Store.Reference.Map.t
      -> (Store.Hash.t * Store.Reference.t * bool) list
      -> command list Lwt.t
  end
  with module Store = G

module Make (N : NET) (S : Minimal.S) :
  S with module Store = S and module Net = N
