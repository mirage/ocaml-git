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
  module Web  : Web.S
  module Client: CLIENT
    with type headers = Web.HTTP.headers
     and type meth    = Web.HTTP.meth
     and type uri     = Web.uri
     and type resp    = Web.resp
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

  val ls :
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

  val push :
    Store.t
    -> push:(Store.t -> (Store.Hash.t * Store.Reference.t * bool) list -> (Store.Hash.t list * command list) Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?capabilities:Git.Capability.t list
    -> string -> string -> ((string, string * string) result list, error) result Lwt.t

  val fetch :
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

  val clone_ext :
    Store.t
    -> ?stdout:(Cstruct.t -> unit Lwt.t)
    -> ?stderr:(Cstruct.t -> unit Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?reference:Store.Reference.t
    -> ?capabilities:Git.Capability.t list
    -> string -> string -> (Store.Hash.t, error) result Lwt.t

  val fetch_all :
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    Uri.t -> (unit, error) result Lwt.t

  val fetch_one :
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    reference:Store.Reference.t -> Uri.t -> (unit, error) result Lwt.t

  val fetch_some:
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    references:Store.Reference.t list -> Uri.t -> (unit, error) result Lwt.t

  val clone :
    Store.t -> ?locks:Store.Lock.t ->
    ?capabilities:Git.Capability.t list ->
    reference:Store.Reference.t -> Uri.t -> (unit, error) result Lwt.t

  val update : Store.t ->
    ?capabilities:Git.Capability.t list ->
    reference:Store.Reference.t -> Uri.t ->
    ((Store.Reference.t, Store.Reference.t * string) result list, error) result Lwt.t

end

module Make_ext
    (W: Web.S with type +'a io = 'a Lwt.t
               and type raw = Cstruct.t
               and type uri = Uri.t
               and type Request.body = Lwt_cstruct_flow.i
               and type Response.body = Lwt_cstruct_flow.o)
    (C: CLIENT with type +'a io = 'a W.io
                and type headers = W.HTTP.headers
                and type body = Lwt_cstruct_flow.o
                and type meth = W.HTTP.meth
                and type uri = W.uri
                and type resp = W.resp)
    (G: Git.S)
  : S_EXT with module Web     = W
           and module Client  = C
           and module Store   = G

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
    (C: CLIENT with type +'a io = 'a Lwt.t
                and type headers = Web_cohttp_lwt.HTTP.headers
                and type body = Lwt_cstruct_flow.o
                and type meth = Web_cohttp_lwt.HTTP.meth
                and type uri = Web_cohttp_lwt.uri
                and type resp = Web_cohttp_lwt.resp)
    (S: Git.S)
  : S with module Client = C
       and module Store  = S
