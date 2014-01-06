(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Clone/Fecth/Push protocol *)

open GitTypes

type backend = {
  write_reference: t -> string -> sha1 -> unit Lwt.t;
  write_and_check_inflated: t -> sha1 -> string -> unit Lwt.t;
  write_filesystem: t -> SHA1.Commit.t -> unit Lwt.t;
}
(** Abstraction for the backend type (could be on-disk or
    in-memory. *)

val local: backend
(** Local Git store. *)

val clone: ?bare:bool -> ?deepen:int -> backend -> t -> string -> unit Lwt.t
(** Clone a remote repository. *)

val clone_on_disk: ?bare:bool -> ?deepen:int -> t -> string -> unit Lwt.t
(** Same as [clone] by populate the filesystem with the result of the clone. *)
