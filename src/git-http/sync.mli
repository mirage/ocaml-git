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

  val call : ?headers:headers -> ?body:body -> meth -> uri -> resp io
end

module type FLOW = sig
  type raw
  type +'a io
  type i = (raw * int * int) option -> unit io
  type o = unit -> (raw * int * int) option io
end

module Lwt_cstruct_flow :
  FLOW with type raw = Cstruct.t and type +'a io = 'a Lwt.t

module type S = sig
  module Web : Web.S

  module Client :
    CLIENT
    with type headers = Web.HTTP.headers
     and type meth = Web.HTTP.meth
     and type uri = Web.uri
     and type resp = Web.resp

  module Endpoint : sig
    type t = {uri: Uri.t; headers: Web.HTTP.headers}

    include Git.Sync.ENDPOINT with type t := t
  end

  include Git.Sync.S with module Endpoint := Endpoint
end

module Make
    (W : Web.S
         with type +'a io = 'a Lwt.t
          and type raw = Cstruct.t
          and type uri = Uri.t
          and type Request.body = Lwt_cstruct_flow.i
          and type Response.body = Lwt_cstruct_flow.o)
    (C : CLIENT
         with type +'a io = 'a W.io
          and type headers = W.HTTP.headers
          and type body = Lwt_cstruct_flow.o
          and type meth = W.HTTP.meth
          and type uri = W.uri
          and type resp = W.resp)
    (G : Git.S) :
  S with module Web = W and module Client = C and module Store = G

module type COHTTP_S =
  S
  with type Web.req = Web_cohttp_lwt.req
   and type Web.resp = Web_cohttp_lwt.resp
   and type 'a Web.io = 'a Web_cohttp_lwt.io
   and type Web.raw = Web_cohttp_lwt.raw
   and type Web.uri = Web_cohttp_lwt.uri
   and type Web.Request.body = Web_cohttp_lwt.Request.body
   and type Web.Response.body = Web_cohttp_lwt.Response.body
   and type Web.HTTP.headers = Web_cohttp_lwt.HTTP.headers

module CohttpMake
    (C : CLIENT
         with type +'a io = 'a Lwt.t
          and type headers = Web_cohttp_lwt.HTTP.headers
          and type body = Lwt_cstruct_flow.o
          and type meth = Web_cohttp_lwt.HTTP.meth
          and type uri = Web_cohttp_lwt.uri
          and type resp = Web_cohttp_lwt.resp)
    (S : Git.S) : COHTTP_S with module Client = C and module Store = S
