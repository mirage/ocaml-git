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

(** HTTP helpers to implement Git's Smart-HTTP protocol.

    The "smart" HTTP connection simulate normal channel by a sequence
    of RPC calls (the HTTP requests and responses) by:

    {ul
    {- allowing a read only after a write: the write is in the HTTP
    request, the read is in the HTTP response.}
    {- replaying all the previous communication in both direction
    everytime. The new thing to read or write is appended to the
    history and part of a new RPC.}}

    This looks like a terrible idea, but in practice there are very
    few round-trips between the client and the server with relatively
    small amount of data. These round trips correspond to the
    negociation phase, where the client and the server needs to find
    which hashes they have in common. Once this is done, the final RPC
    response contains the pack file, which is where most of the data
    are.

    The good thing with that scheme (I guess) is that the server
    doesn't have to keep any client state. Note: there are more
    clever way to do this ...

    That module implements a "restartable" HTTP channels, which hides
    all that reconnection and replay complexity behind an usual
    channel interface.
*)

module type CLIENT = sig
  include Cohttp_lwt.Client
    with type 'a IO.t = 'a Lwt.t               (* FIMXE in cohttp *)

  module Request : Cohttp.S.Http_io with module IO = IO and type t = Cohttp.Request.t
  module Response : Cohttp.S.Http_io with module IO = IO and type t = Cohttp.Response.t

  val close_out: IO.oc -> unit                 (* FIXME in cohttp *)
  val close_in: IO.ic -> unit                  (* FIXME in cohttp *)
  val oc: IO.oc -> Request.IO.oc               (* FIXME in cohttp *)
  val ic: IO.ic -> Response.IO.ic              (* FIXME in cohttp *)
end

module type CHAN = sig
  type t
  val make:
    ?close:(unit -> unit Lwt.t) -> (Cstruct.t -> int -> int -> int Lwt.t) -> t
end
(** The module type for buildable channels (see {!CHAN.make}). *)

module Flow (HTTP: CLIENT) (IC: CHAN) (OC: CHAN): sig

  type 'a http_callback =
      Uri.t -> (HTTP.IO.ic * HTTP.IO.oc -> 'a Lwt.t) -> 'a Lwt.t

  type 'a callback = Uri.t -> (IC.t * OC.t -> 'a Lwt.t) -> 'a Lwt.t

  val with_http: ?init:string -> 'a http_callback -> 'a callback
  (** Connect to an HTTP-endpoint using conduit. The method is infered
      from the URI by the Git protocol, the [init] string contains a
      marshaled S-expression representation of the actual header to
      use (which are set by {!Git.Sync}. *)

end
