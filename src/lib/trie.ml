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

let (!!) = Lazy.force

type ('a,'b) t =  {
  value: 'b option;
  children: ('a * ('a,'b) t) list Lazy.t;
}

let create ?(children = lazy []) ?value () =
  { children; value; }

let empty = create ()

let rec list_map_filter f = function
  | [] -> []
  | h :: tl -> match f h with
      | Some h -> h :: list_map_filter f tl
      | None -> list_map_filter f tl

(* actually a map_filter, which causes it to force all the lazies (it's
   otherwise impossible to know which branches to prune) *)
let map_filter_values f tree =
  let rec aux value children = {
    value = (
      match value with
      | None -> None
      | Some value -> f value
    );
    children = lazy (
      list_map_filter
        (fun (key, {value; children}) -> match aux value children with
          | { value = None; children = lazy [] } -> None
          | r -> Some (key, r))
        !!children
    )
  }
  in
  aux tree.value tree.children

let iter f tree =
  let rec aux rev_path tree =
    (match tree.value with Some v -> f (List.rev rev_path) v | None -> ());
    List.iter (fun (k,v) -> aux (k::rev_path) v) !!(tree.children)
  in
  aux [] tree

let fold f tree acc =
  let rec aux acc t rev_path =
    let acc =
      List.fold_left
        (fun acc (key,n) -> aux acc n (key::rev_path))
        acc
        !!(t.children)
    in
    match t.value with Some v -> f acc (List.rev rev_path) v | None -> acc
  in
  aux acc tree []

let sub tree path =
  let rec aux tree = function
  | [] -> tree
  | h :: tl -> aux (List.assoc h !!(tree.children)) tl
  in
  try aux tree path with Not_found -> empty

let rec find tree = function
  | h :: tl -> find (List.assoc h !!(tree.children)) tl
  | [] -> match tree.value with
      | Some v -> v
      | None -> raise Not_found

let mem tree path =
  let rec aux tree = function
    | h :: tl -> aux (List.assoc h !!(tree.children)) tl
    | [] -> tree.value <> None
  in
  try aux tree path with Not_found -> false

(* maps f on the element of assoc list children with key [key], appending a
   new empty child if necessary *)
let list_map_assoc f children key empty =
  let rec aux acc = function
    | [] -> List.rev_append acc [key, f empty]
    | (k,v) as child :: children ->
        if k = key then
          List.rev_append acc ((key, f v) :: children)
        else
          aux (child::acc) children
  in
  aux [] children

let rec map_subtree tree path f = match path with
  | [] -> f tree
  | h :: tl ->
      let children = lazy (
        list_map_assoc (fun n -> map_subtree n tl f) !!(tree.children) h empty
      ) in
      { tree with children }

let set tree path value =
  map_subtree tree path (fun t -> { t with value = Some value })

let set_lazy tree path lazy_value =
  map_subtree tree path (fun t -> { t with value = Some !!lazy_value })

let unset tree path =
  map_subtree tree path (fun t -> { t with value = None })

let rec filter_keys f tree =
  { tree with
    children = lazy (
      list_map_filter
        (fun (key,n) -> if f key then Some (key, filter_keys f n) else None)
        !!(tree.children)
    )}

let graft tree path node = map_subtree tree path (fun _ -> node)

let graft_lazy tree path lazy_node =
  map_subtree tree path (fun _ -> !!lazy_node)
