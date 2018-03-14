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

    The memory back-end means, we never use any I/O operations to read
    or write a Git value. All of your repository is stored on your
    memory. For some specific purposes, it's useful. However, keep in
    your mind, when your program stops, you lost your repository
    obviously.

    Because we don't need to handle any I/O operations, this store
    could be more fast than the Unix/Mirage store and relevant for
    testing. However, we still use the [Lwt] monad to protect mutable
    reference value against data-race condition.

    Finally, this module respects the same API {!Minimal.S} and can
    handle PACK file - and, by this way, can be used on the Smart
    protocol. Obviously, some ext. modules like {!Index} could not be
    used with this store (because thay interact with a file-system
    back-end).
*)

module Make (H: S.HASH) (I: S.INFLATE) (D: S.DEFLATE): sig

(** The {i functor} needs 4 modules:

    {ul
    {- The hash algorithm like [SHA1].}
    {- A lock implementation to protect references against data-race
    condition - this module expects a [Lwt] monad.}
    {- An inflate algorithm implementation - usually [zlib].}
    {- A deflate algorithm implementation - usually [zlib].}}

    From the Inflate and the Deflate module, usually, we use a [zlib]
    implementation provided by [camlzip] or [decompress]. However, you
    can use an other (better) implementation which needs to respect
    these interfaces - or provide an implementation which does not
    inflate/deflate a stream (it's possible). The only {i
    non-described by type} constraint is the Inflate module needs to
    understand the Deflate module. For example, use [zlib] to inflate
    and [brotli] to deflate does not work. *)

  include Minimal.S with module Hash = H
                     and module Inflate = I
                     and module Deflate = D

  val v:
    ?root:Fpath.t ->
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t) ->
    unit -> (t, error) result Lwt.t
  (** [create ?root ?dotgit ?compression ()] creates a new store
      represented by the path [root] (default is ["."]), where the Git
      objects are located in [dotgit] (default is [root / ".git"] and
      when Git objects are compressed by the [level] (default is
      [4]). *)

end

module Store (H : Digestif_sig.S): sig
  include Minimal.S with module Hash    = Hash.Make(H)
                     and module Inflate = Inflate
                     and module Deflate = Deflate

  val v:
    ?root:Fpath.t ->
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t) ->
    unit -> (t, error) result Lwt.t
end
