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
module Make (Store : Minimal.S) : sig
  type t =
    | Reference of Store.Reference.t
    (* <refname> *)
    | Commit of id
    (* <id> *)
    | Parent of t * int
    (* <rev>^<n> *)
    | Ancestor of t * int

  (* <rev>~<n> *)
  and id = Incomplete of string | Complete of Store.Hash.t

  val head : t
  val parent : t -> int -> t
  val ancestor : t -> int -> t
  val from_hash : Store.Hash.t -> t

  type error

  val normalize : Store.t -> t -> (Store.Hash.t, error) result Lwt.t

  module Range : sig
    type nonrec t =
      | Include of t
      (* <rev> *)
      | Exclude of t
      (* ^<rev> *)
      | Diff of t * t
      (* <rev1>..<rev2> différence semantically, it's the same than [^<rev1>
         <rev2>] *)
      | Delta of t * t
      (* <rev1>...<rev2> différence symétrique *)
      | Parents of t
      (* <rev>^@ *)
      | PParents of t

    (* <rev>^! *)

    val normalize : Store.t -> t -> Store.Hash.Set.t Lwt.t
  end
end
