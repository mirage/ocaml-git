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

module type CLIENT = sig
  type headers
  type body
  type resp
  type meth
  type uri

  type +'a io

  val call: ?headers:headers -> ?body:body -> meth -> uri -> resp io
end

module type FLOW = sig
  type raw

  type +'a io

  type i = (raw * int * int) option -> unit io
  type o = unit -> (raw * int * int) option io
end

module Lwt_cstruct_flow:
  FLOW with type raw = Cstruct.t and type +'a io = 'a Lwt.t

module type S_EXT = sig
  module Web: Web.S
  module Client: CLIENT
    with type headers = Web.HTTP.headers
     and type meth = Web.HTTP.meth
     and type uri = Web.uri
     and type resp = Web.resp
  module Store: Git.S

  module Decoder: Git.Smart.DECODER
    with module Hash = Store.Hash
  module Encoder: Git.Smart.ENCODER
    with module Hash = Store.Hash

  module PACKDecoder: Git.Unpack.P
    with module Hash = Store.Hash
     and module Inflate = Store.Inflate

  type error =
    [ `SmartDecoder of Decoder.error
    | `StorePack of Store.Pack.error
    | `Clone of string
    | `ReportStatus of string
    | `Ref of Store.Ref.error ]

  val pp_error: error Fmt.t

  val ls:
       Store.t
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?capabilities:Git.Capability.t list
    -> string -> string -> (Decoder.advertised_refs, error) result Lwt.t

  type command =
    [ `Create of (Store.Hash.t * Store.Reference.t)
    | `Delete of (Store.Hash.t * Store.Reference.t)
    | `Update of (Store.Hash.t * Store.Hash.t * Store.Reference.t) ]

  val push:
    Store.t
    -> push:((Store.Hash.t * Store.Reference.t * bool) list -> (Store.Hash.t list * command list) Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?capabilities:Git.Capability.t list
    -> string -> string -> ((string, string * string) result list, error) result Lwt.t

  val fetch:
    Store.t
    -> ?shallow:Store.Hash.t list
    -> ?stdout:(Cstruct.t -> unit Lwt.t)
    -> ?stderr:(Cstruct.t -> unit Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?capabilities:Git.Capability.t list
    -> negociate:((Decoder.acks -> 'state -> ([ `Ready | `Done | `Again of Store.Hash.t list ] * 'state) Lwt.t) * 'state)
    -> has:Store.Hash.t list
    -> want:((Store.Hash.t * Store.Reference.t * bool) list -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[ `Depth of int | `Timestamp of int64 | `Ref of string ]
    -> ?port:int
    -> string -> string -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

  val clone_ext:
    Store.t
    -> ?stdout:(Cstruct.t -> unit Lwt.t)
    -> ?stderr:(Cstruct.t -> unit Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?reference:Store.Reference.t
    -> ?capabilities:Git.Capability.t list
    -> string -> string -> (Store.Hash.t, error) result Lwt.t

  val fetch_some:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    ?headers:Web.HTTP.headers ->
    references:Store.Reference.t list Store.Reference.Map.t ->
    Uri.t -> (Store.Hash.t Store.Reference.Map.t
              * Store.Reference.t list Store.Reference.Map.t, error) result Lwt.t
  (** [fetch_some git ?locks ?capabilities ~references repository] will
      fetch some remote references specified by [references].

      [references] is a map which:
      {ul
      {- the key is the {b remote} reference.}
      {- the value is a list of {b local} references - which may not
      exist yet.}}

      Then, the function will try to download all of these remote
      references and returns 2 maps:

      {ul
      {- the first map contains all local references updated by the
      new hash. This new hash is come from the server as the
      downloaded remote reference asked by the client by [references].
      Then, from associated local references with remote references,
      we updated them with the associated hash.

      For example, if [references] is:
      {[ { "refs/heads/master": [ "refs/remotes/origin/master"
                                ; "refs/heads/master" ] } ]}

      We will update (or create) "refs/remotes/origin/master" and
      "refs/heads/master" with the new hash downloaded from the remote
      reference "refs/heads/master" only if it's necessary (only if we
      did not find the hash referenced by "refs/heads/master" in the
      local store).}

      {- the second map is a {b subset} of [references] which contains
      all binder of:

      {ul
      {- remote references which does not exist on the server side.}
      {- remote references which references to an already existing in
      the local store hash.}}}}

      The client should not put the same local reference as a value of
      some remote references. The client can define non-existing
      remote references (then, they appear on the second map). The
      client can want to set non-existing local references - we will
      create them.

      If the processus encountered an error when it updates
      references, it leaves but, it did partially some update on some
      local references. *)

  val fetch_all:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    ?headers:Web.HTTP.headers ->
    references:Store.Reference.t list Store.Reference.Map.t ->
    Uri.t -> (Store.Hash.t Store.Reference.Map.t
              * Store.Reference.t list Store.Reference.Map.t
              * Store.Hash.t Store.Reference.Map.t, error) result Lwt.t
  (** [fetch_all git ?locks ?capabilities ~references repository] has
      the same semantic than {!fetch_some} for any remote references found
      on [references]. However, [fetch all] will download all remote
      references available on the server (and whose hash is not available
      on the local store). If these remote references are not associated
      with some local references, we return a third map which contains
      these remote references binded with the new hash downloaded.

      We {b don't} notice any non-downloaded remote references not
      found on the [references] map and whose hash already exists on
      the local store.

      Then, the client can bind these new hashes with specific local
      references or just give up. *)

  val fetch_one:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    ?headers:Web.HTTP.headers ->
    reference:(Store.Reference.t * Store.Reference.t list) ->
    Uri.t -> ([ `AlreadySync | `Sync of Store.Hash.t Store.Reference.Map.t ], error) result Lwt.t
  (** [fetch_one git ?locks ?capabilities ~reference repository] is a
      specific call of {!fetch_some} with only one reference. Then, it
      retuns:

      {ul
      {- [`AlreadySync] if the hash of the requested reference already
      exists on the local store}
      {- [`Sync updated] if we downloaded [new_hash] and
      set [local_ref] with this new hash.}} *)

  val clone:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    ?headers:Web.HTTP.headers ->
    reference:(Store.Reference.t * Store.Reference.t) ->
    Uri.t -> (unit, error) result Lwt.t

  val update_and_create: Store.t ->
    ?capabilities:Git.Capability.t list ->
    ?headers:Web.HTTP.headers ->
    references:Store.Reference.t list Store.Reference.Map.t-> Uri.t ->
    ((Store.Reference.t, Store.Reference.t * string) result list, error) result Lwt.t
    (** As {!fetch_some}, [update git ?capabilities ~references
        repository] is the other side of the communication with a Git
        server and update and create remote references when it
        uploads local hashes.

        [reference] is a map which:
        {ul
        {- the key is the {b local} reference.}
        {- the value is a list of {b remote} references - which may
        not exist yet.}}

        Then, the function will try to upload all of these local
        references to the binded remote references. If binded remote
        reference does not exist on the server, we ask to the server
        to create and set it to the local hash.

        For each update action, we check if the local store has the
        remote hash. In other case, we miss this action - that means,
        the local store is not synchronized with the server (and the
        client probably needs to {!fetch_some} before).

        Then, it returns a list of results. The [Ok] case with the
        remote reference which the server updated correctly and the
        [Error] case with the remote reference which the server
        encountered an error with a description of this error.

        At this final stage, the function does not encountered any
        error during the commmunication - if it's the case, we did not
        do any modification on the server and return an {!error}. *)
end

module Make_ext
    (W: Web.S
     with type +'a io = 'a Lwt.t
      and type raw = Cstruct.t
      and type uri = Uri.t
      and type Request.body = Lwt_cstruct_flow.i
      and type Response.body = Lwt_cstruct_flow.o)
    (C: CLIENT
     with type +'a io = 'a W.io
      and type headers = W.HTTP.headers
      and type body = Lwt_cstruct_flow.o
      and type meth = W.HTTP.meth
      and type uri = W.uri
      and type resp = W.resp)
    (G: Git.S)
  : S_EXT with module Web = W
           and module Client = C
           and module Store = G

module type S = S_EXT
  with type Web.req = Web_cohttp_lwt.req
   and type Web.resp = Web_cohttp_lwt.resp
   and type 'a Web.io = 'a Web_cohttp_lwt.io
   and type Web.raw = Web_cohttp_lwt.raw
   and type Web.uri = Web_cohttp_lwt.uri
   and type Web.Request.body = Web_cohttp_lwt.Request.body
   and type Web.Response.body = Web_cohttp_lwt.Response.body
   and type Web.HTTP.headers = Web_cohttp_lwt.HTTP.headers

module Make
    (C: CLIENT
     with type +'a io = 'a Lwt.t
      and type headers = Web_cohttp_lwt.HTTP.headers
      and type body = Lwt_cstruct_flow.o
      and type meth = Web_cohttp_lwt.HTTP.meth
      and type uri = Web_cohttp_lwt.uri
      and type resp = Web_cohttp_lwt.resp)
    (S: Git.S)
  : S with module Client = C
       and module Store  = S
