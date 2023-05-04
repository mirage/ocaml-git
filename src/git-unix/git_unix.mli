(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Unix backend. *)

module Make (Digestif : Digestif.S) : sig
  include Git.S with type hash = Digestif.t

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

val ctx : Happy_eyeballs_lwt.t -> Mimic.ctx Lwt.t

module Sync (Store : Git.S) :
  Git.Sync.S with type hash = Store.hash and type store = Store.t

module Store : sig
  include Git.S with type hash = Digestif.SHA1.t

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t

  val close_pack_files : t -> unit Lwt.t
  (** [close_pack_files t] closes all {!type:Mj.fd}.

      {b NOTE}: Any subsequent use of the [t] has undefined behavior! This
      function should be registered with [at_exit] to clean pending
      file-descriptors. *)
end
