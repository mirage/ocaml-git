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

(** The memory back-end of Git.

    The memory back-end means, we never use any I/O operations to read or write
    a Git value. All of your repository is stored on your memory. For some
    specific purposes, it's useful. However, keep in your mind, when your
    program stops, you lost your repository obviously.

    Because we don't need to handle any I/O operations, this store could be more
    fast than the Unix/Mirage store and relevant for testing. However, we still
    use the [Lwt] monad to protect mutable reference value against data-race
    condition.

    Finally, this module respects the same API {!Minimal.S} and can handle PACK
    file - and, by this way, can be used on the Smart protocol. Obviously, some
    ext. modules like INDEX could not be used with this store (because thay
    interact with a file-system back-end). *)

type 'hash t

module Make (Digestif : Digestif.S) : sig
  type nonrec t = Digestif.t t

  include Minimal.S with type hash = Digestif.t and type t := t

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
  (** [create ?root ?dotgit ?compression ()] creates a new store represented by
      the path [root] (default is ["."]), where the Git objects are located in
      [dotgit] (default is [root / ".git"] and when Git objects are compressed
      by the [level] (default is [4]). *)
end

module Store : sig
  include
    Minimal.S with type t = Digestif.SHA1.t t and type hash = Digestif.SHA1.t

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

module Sync (Store : Minimal.S) :
  Sync.S with type hash = Store.hash and type store = Store.t
