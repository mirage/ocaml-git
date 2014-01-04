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

open Core_kernel.Std

open GitTypes

let to_string node =
  let hex = GitMisc.hex_encode (SHA1.to_string node) in
  String.sub hex 0 8

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = (sha1 * value)
      let compare (x,_) (y,_) = String.compare (SHA1.to_string x) (SHA1.to_string y)
      let hash (x,_) = Hashtbl.hash (SHA1.to_string x)
      let equal (x,_) (y,_) = (x = y)
    end)
    (struct
      type t = string
      let default = ""
      let compare = String.compare
    end)

module Dot = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name (x,o) =
      let hex = to_string x in
      let p = match o with
        | Blob s   -> "B"
        | Commit _ -> "C"
        | Tree _   -> "Tr"
        | Tag  _   -> "Ta" in
      Printf.sprintf "\"%s-%s\"" p hex
    let vertex_attributes (_,o) =
      match o with
      | Commit _ -> [`Shape `Doublecircle]
      | _ -> []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes (_,l,_) =
      match l with
      | "" -> []
      | _  ->[`Label l]
  end)

let create_graph t =

  let g = G.create () in
  let nodes = GitLocal.list t in
  let read n =
    match GitLocal.read t n with
    | None   -> failwith (Printf.sprintf "Cannot find %s" (to_string n))
    | Some v -> v in
  let nodes = List.map ~f:(fun n -> (n, read n)) nodes in

  (* Add all the vertices *)
  List.iter ~f:(G.add_vertex g) nodes;

  List.iter ~f:(fun (id, obj as src) ->
    let succs = GitLocal.succ t id in
    List.iter ~f:(fun (l, succ) ->
      let l = match l with
        | `parent -> ""
        | `tag t  -> "TAG-" ^ t
        | `file f -> f in
      G.add_edge_e g (src, l, (succ, read succ))
    ) succs
  ) nodes;

  (* Return the graph *)
  g

let to_dot t file =
  Out_channel.with_file file ~f:(fun oc ->
      let g = create_graph t in
      Dot.output_graph oc g;
    )
