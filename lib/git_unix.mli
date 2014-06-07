(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Lwt_unix IO module. *)

open Core_kernel.Std
open Git

(** {2 Synchronisation primitives} *)

module Sync: sig
  module Result: (module type of Sync.Result with type fetch = Sync.Result.fetch
                                              and type push  = Sync.Result.push)
  module Make (S: Store.S): Sync.S with type t = S.t
(** Implementation of the Git protocol using [Lwt_unix] and [Lwt_io]
    IO functions. *)
end

(** Filesystem. *)

module FS: FS.S
(** Implementation of the on-disk Git protocol. *)
