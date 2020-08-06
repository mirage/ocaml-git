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

module type STORE = sig
  module Hash : S.HASH

  module Value : Value.S with type hash = Hash.t

  type t

  val root : t -> Fpath.t

  val read_exn : t -> Hash.t -> Value.t Lwt.t
end

module Make (Store : STORE) : sig
  val fold :
    Store.t ->
    ('a ->
    ?name:Fpath.t ->
    length:int64 ->
    Store.Hash.t ->
    Store.Value.t ->
    'a Lwt.t) ->
    path:Fpath.t ->
    'a ->
    Store.Hash.t ->
    'a Lwt.t

  val iter :
    Store.t ->
    (Store.Hash.t -> Store.Value.t -> unit Lwt.t) ->
    Store.Hash.t ->
    unit Lwt.t
end
