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

(** HTTP helpers to implement Git's Smart-HTTP protocol. *)

module type CLIENT = sig
  include Cohttp_lwt.Client
    with type 'a IO.t = 'a Lwt.t               (* FIMXE in cohttp *)
     and type 'a Request.IO.t = 'a Lwt.t       (* FIXME in cohttp *)
     and type 'a Response.IO.t = 'a Lwt.t      (* FIXME in cohttp *)
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
