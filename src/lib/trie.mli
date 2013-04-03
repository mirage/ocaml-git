(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Lazy tries based on lists *)

type ('a, 'b) t
(** Type of tries mapping from ['a list] to ['b] *)

val empty : ('a,'b) t

val create :
  ?children: ('a * ('a,'b) t) list Lazy.t ->
  ?value: 'b ->
  unit ->
  ('a,'b) t
(** Create a new trie with the given components *)

val mem : ('a,'b) t -> 'a list -> bool
(** Returns true if there is a value associated with the given path *)

val find : ('a, 'b) t -> 'a list -> 'b
(** Returns the value associated with the given path.
    @raise [Not_found] *)

val set : ('a, 'b) t -> 'a list -> 'b -> ('a, 'b) t
(** Associates a value with the given path, or replaces if there was already
    one *)

val set_lazy : ('a, 'b) t -> 'a list -> 'b Lazy.t -> ('a, 'b) t
(** The same but taking a lazy value *)

val unset : ('a, 'b) t -> 'a list -> ('a, 'b) t
(** Removes an association from the trie. Warning: doesn't cleanup branches that
    don't point to anything anymore *)

val map_subtree :
  ('a, 'b) t -> 'a list ->
  (('a, 'b) t -> ('a, 'b) t) ->
  ('a, 'b) t
(** [map_subtree tree path f] applies [f] on value and children of the node
    found at [path] in [tree], and bind the returned node back at that position
    in the tree *)

(*
val set_lazy_subtree :
  ('a, 'b) t -> 'a list -> ('a * ('a,'b) t) list Lazy.t -> ('a, 'b) t
(** [set_lazy_subtree t path lazy_children] sets the sub-tree of [t] pointed to
    by [path] to have [lazy_children] *)
*)

val iter : ('a list -> 'b -> unit) -> ('a, 'b) t -> unit
(** iters over all the bindings in the trie, top-down *)

val fold : ('acc -> 'a list -> 'b -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
(** folds over all bindings of the trie, bottom-up *)

val map_filter_values : ('b -> 'c option) -> ('a,'b) t -> ('a,'c) t
(** Maps and filters over all values in the trie, removing the value if [None]
    is returned *)

val sub : ('a, 'b) t -> 'a list -> ('a,'b) t
(** [sub t p] returns the sub-trie associated with the path [p] in the trie
    [t].  If [p] is not a valid path of [t], it returns an empty trie. *)

val filter_keys : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
(** [filter f t] returns t with all subtrees for which [f key = false] pruned *)

val graft : ('a, 'b) t -> 'a list -> ('a, 'b) t -> ('a, 'b) t
(** [graft tree path node] grafts [node] in [tree] at [path] *)

val graft_lazy : ('a, 'b) t -> 'a list -> ('a, 'b) t Lazy.t -> ('a, 'b) t
