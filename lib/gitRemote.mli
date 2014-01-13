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

type result = {
  references: (sha1 * reference) list;
  sha1s     : sha1 list;
}
(** The resulting sha1s and references. *)

val set_expand_hook: (string list -> perm -> blob -> unit Lwt.t) -> unit
(** Set up the expand hook (to avoid cyclic deps) *)

module Make (Store: S): sig

  (** Remote operation, abstracted over the bakend type. *)

  val ls: Store.t -> string -> (sha1 * reference) list Lwt.t
  (** List the references in the remote repository. *)

  val clone: Store.t -> ?bare:bool -> ?deepen:int -> string -> result Lwt.t
  (** [clone t address] clones the contents of [address] into the
      store [t]. *)

  val fetch: Store.t -> ?deepen:int -> string -> result Lwt.t
  (** [fetch t address] fetches the missing contents of [address] into
      the store [t]. *)

end
