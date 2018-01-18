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

(* This file is part of Ocaml-aliases.

   Ocaml-quadtree is free software: you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation.

   Ocaml-quadtree is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Ocaml-aliases. If not, see <http://www.gnu.org/licenses/>.

   Copyright 2012 Be Sport

   XXX(dinosaure): This module is originally provided by
   https://github.com/khigia/ocaml-stringset but Ludo (aka khigia) did
   not put a LICENSE. Then, Hugo (from BeSport) improved the project.
   So, this module is a mix between ocaml-stringset and ocaml-aliases.
*)

module type KEY =
sig
  type t

  val get: t -> int -> char
  val length: t -> int
end

type 'a sequence = ('a -> unit) -> unit

module type S =
sig
  type key
  type 'a t

  type nonrec 'a sequence = 'a sequence

  val empty: 'a t
  val is_empty: 'a t -> bool
  val bind: 'a t -> key -> 'a -> 'a t
  val lookup: 'a t -> key -> 'a option
  val mem: 'a t -> key -> bool
  val fold: (key * 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val iter: (key * 'a -> unit) -> 'a t -> unit
  val to_sequence: 'a t -> (key * 'a) sequence
  val to_list: 'a t -> (key * 'a) list
  val pp: (key * 'a) Fmt.t -> 'a t Fmt.t
end

module Make (Key: KEY): S with type key = Key.t =
struct
  type key = Key.t

  module Compare =
  struct
    type t =
      | Eq
      | Prefix
      | Contain
      | Inf of int
      | Sup of int

    let rec compare value1 off1 len1 value2 off2 len2 =
      if off1 = len1
      then if off2 = len2
          then Eq
          else Prefix
      else if off2 = len2
          then Contain
          else
            let c1 = Key.get value1 off1 in
            let c2 = Key.get value2 off2 in

            if c1 = c2
            then compare value1 (off1 + 1) len1 value2 (off2 + 1) len2
            else if c1 < c2
            then Inf off1
            else (* c1 > c2 *) Sup off1
  end

  let critbit c1 c2 =
    let rec aux p c1 c2 =
      if (c1 land 128) <> (c2 land 128)
      then p
      else aux (p - 1) (c1 lsl 1) (c2 lsl 1)
    in

    if c1 = c2
    then raise Not_found
    else aux 7 (Char.code c1) (Char.code c2)
  (* XXX(dinosaure): a XOR and find first bit should be more
     efficient. *)

  type 'a node =
    | L of Key.t * 'a
    | T of 'a node * Key.t * 'a
    | B of 'a node * 'a node * int * int

  type 'a t = 'a node option
  type nonrec 'a sequence = 'a sequence

  let empty = None

  let is_empty = function
    | None -> true
    | Some _ -> false

  let rec first_key = function
    | L (k, _) -> k
    | T (_, k, _) -> k
    | B (l, _, _, _) ->
      first_key l (* XXX(dinosaure): could take the shortest path if
                     [B] node embed he size of tree. *)

  let rec bind key off keylen value tree = match tree with
    | L (k, v) ->
      (let kl = Key.length k in
      match Compare.compare key off keylen k off kl with
      | Compare.Eq -> L (k, value) (* replace *)
      | Compare.Prefix -> T (tree, key, value)
      | Compare.Contain -> T (L (key, value), k, v)
      | Compare.Inf p ->
        let b = critbit (Key.get key p) (Key.get k p) in
        B (L (key, value), tree, p, b)
      | Compare.Sup p ->
        let b = critbit (Key.get key p) (Key.get k p) in
        B (tree, L (key, value), p, b))
    | T (m, k, v) ->
      (let kl = Key.length k in
      match Compare.compare key off keylen k off kl with
      | Compare.Eq -> T (m, k, value) (* replace *)
      | Compare.Prefix -> T (tree, key, value)
      | Compare.Contain -> T (bind key kl keylen value m, k, v)
      | Compare.Inf p ->
        let b = critbit (Key.get key p) (Key.get k p) in
        B (L (key, value), tree, p, b)
      | Compare.Sup p ->
        let b = critbit (Key.get key p) (Key.get k p) in
        B (tree, L (key, value), p, b))
    | B (l, r, i, b) ->
      if keylen > i
      then if ((Char.code (Key.get key i)) land (1 lsl b)) = 0
          then B (bind key i keylen value l, r, i, b)
          else B (l, bind key i keylen value r, i, b)
      else let k = first_key l in
          match Compare.compare key off keylen k off keylen with
          | Compare.Eq | Compare.Prefix -> T (tree, key, value)
          | Compare.Contain -> B (bind key i keylen value l, r, i, b)
          | Compare.Inf p ->
            if p = i
            then B (bind key i keylen value l, r, i, b)
            else let bn = critbit (Key.get key p) (Key.get k p) in
                  B (L (key, value), tree, p, bn)
          | Compare.Sup p ->
            if p = i
            then B (l, bind key i keylen value r, i, b)
            else let bn = critbit (Key.get key p) (Key.get k p) in
                  B (tree, L (key, value), p, bn)

  let bind tree key value =
    match tree with
    | None -> Some (L (key, value))
    | Some tree ->
      let keylen = Key.length key in
      Some (bind key 0 keylen value tree)

  let rec lookup key off keylen tree = match tree with
    | L (k, v) ->
      if key = k then Some v else None
    | T (m, k, v) ->
      (let kl = Key.length k in
      match Compare.compare key off keylen k off kl with
      | Compare.Eq | Compare.Prefix -> Some v
      | Compare.Contain -> lookup key kl keylen m
      | Compare.Inf _ | Compare.Sup _ -> None)
    | B (l, r, i, b) ->
      if keylen > i
      then let dir = if ((Char.code (Key.get key i)) land (1 lsl b)) = 0
                     then l else r in
          lookup key i keylen dir
      else None

  let lookup tree key =
    match tree with
    | None -> None
    | Some tree ->
      let keylen = Key.length key in
      lookup key 0 keylen tree

  let mem tree key =
    match lookup tree key with
    | None -> false
    | Some _ -> true

  let rec fold f acc tree = match tree with
    | L (k, v) -> f (k, v) acc
    | T (m, k, v) ->
      let acc' = f (k, v) acc in
      fold f acc' m
    | B (l, r, _, _) ->
      let acc' = fold f acc l in
      fold f acc' r

  let fold f acc = function
    | None -> acc
    | Some tree -> fold f acc tree

  let rec iter f tree = match tree with
    | L (k, v) -> f (k, v)
    | T (m, k, v) -> let () = f (k, v) in iter f m
    | B (l, r, _, _) ->
      let () = iter f l in iter f r

  let iter f = function
    | None -> ()
    | Some tree -> iter f tree

  let to_sequence
      : 'a t -> (Key.t * 'a) sequence
      = fun tree k -> iter k tree

  let to_list t = fold (fun (k, v) acc -> (k, v) :: acc) [] t

  let pp ppv ppf radix =
    Fmt.pf ppf "[ %a ]"
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") (fun ppf x -> ppv ppf x)))
      (to_list radix)
end
