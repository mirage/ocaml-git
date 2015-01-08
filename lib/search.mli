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

(** {2 Search over the Git objects} *)

type succ =
  [ `Commit of SHA.t
  | `Tag of string * SHA.t
  | `Tree of string * SHA.t ]
(** [Value.t] successors. *)

val sha1_of_succ: succ -> SHA.t
(** Return the SHA of the successor value. *)

module Make (S: Store.S): sig

  type path = string list
  (** A path can cross different type of Git object boundaries. By
      convention, the empty string [""] denotes a [Commit.t] to [Tree.t]
      transition, so that [mem head ["";"a";"b"]] will look into the
      Tree/Blob object located under {i a/b} for the head revision. *)

  val succ: S.t -> SHA.t -> succ list Lwt.t
  (** Compute the successor values. *)

  val mem: S.t -> SHA.t -> string list -> bool Lwt.t
  (** [mem t sha1 path] check wether we can go from the object named
      [sha1] to an other object following the [path] labels.*)

  val find: S.t -> SHA.t -> string list -> SHA.t option Lwt.t
  (** [find t sha1 path] returns (if it exists) the object named
      [sha1] to an other object following the [path] labels. *)

  val find_exn: S.t -> SHA.t -> string list -> SHA.t Lwt.t
  (** [find t sha1 path] returns (if it exists) the object named
      [sha1] to an other object following the [path] labels. Raise
      [Not_found] if no such object exist.*)

end
