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
    -> push:(Store.t -> (Store.Hash.t * Store.Reference.t * bool) list -> (Store.Hash.t list * command list) Lwt.t)
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
    references:(Store.Reference.t * Store.Reference.t) list ->
    Uri.t -> (unit, error) result Lwt.t
  (** [fetch_some git ?locks ?capabilities ~references repository] will
      fetch some references specified by [references].

      [references] is an associated list which:
      {ul
      {- the key is the local reference where the client wants to
      store the updated hash.}
      {- the value is the remote reference when the client wants to be
      synchronized with the server.}}

      If a reference inside [references] does not appear on the server
      side, we miss it.

      This function compares the hash pointed by the local reference
      binded with the remote branch to see if the update is necessary
      or not. Then, it updates local references by the hash noticed by
      the server and associated to the remote reference. For example:

      > references = [refs/heads/master, refs/heads/master]

      Will update the reference [refs/heads/master] to the last
      version of the [refs/heads/master] reference on the server side.
      However, usually, Git launch the fetch command with:

      > references = [refs/remotes/master, refs/heads/master]

      Then, it updateds [refs/heads/master] to point indirectly to
      [refs/remotes/master] and finally set the [HEAD] reference to
      point to [refs/heads/master].

      [locks] is the place which is a {i data-structure} which
      contains lock representation. By {i data-structure} we can mean
      a directory or a data-structure whcih contains {i mutex}.

      [capabilities] is a list of {!Capabilities.t} which describe how
      to communicate with the server. If you don't understand what I
      mean, it's better to not precise (and use the default value)
      this argument. In other side, it's better to use directly
      {!fetch} than this helper function.

      Finally, we need to repository address which needs to have the
      [http] or the [https] scheme. The process to send a request (and
      handle a redirection for example) is not the part of
      [fetch_some] but described on the {!Client} module. *)

  val fetch_all:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    references:(Store.Reference.t * Store.Reference.t) list ->
    Uri.t -> ((Store.Reference.t * Store.Hash.t) list, error) result Lwt.t
  (** [fetch_all git ?locks ?capabilities ~references repository] is a
      specific call of {!fetch_some}. This function will download all
      references from the server.

      Then, for all references founded in the associated list
      [references], we updated the binded local reference. This list
      could not be exhaustive. So for all references downloaded from
      the server, if we not find them in the [references] associated
      list and only if they have this format:

      > refs/heads/{branch}

      We create/{b replace} a new local references on this form:

      > refs/remotes/{branch}

      To keep updated hashes locally. For any other references, we
      miss this update but return an associated list between these
      remote references and binded hashes. *)

  val fetch_one:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    reference:(Store.Reference.t * Store.Reference.t) ->
    Uri.t -> (unit, error) result Lwt.t
  (** [fetch_one git ?locks //capabilities ~reference repository] is a
      specific call of {!fetch_some} with only one reference. *)

  val clone:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    reference:(Store.Reference.t * Store.Reference.t) ->
    Uri.t -> (unit, error) result Lwt.t

  val update: Store.t ->
    ?capabilities:Git.Capability.t list ->
    reference:Store.Reference.t -> Uri.t ->
    ((Store.Reference.t, Store.Reference.t * string) result list, error) result Lwt.t

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
