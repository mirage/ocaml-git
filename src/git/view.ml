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

open Lib
open Model

let to_string node =
  let hex = Misc.hex_encode (Node.to_string node) in
  String.sub hex 0 8

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = (node * value)
      let compare (x,_) (y,_) = String.compare (Node.to_string x) (Node.to_string y)
      let hash (x,_) = Hashtbl.hash (Node.to_string x)
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
  let nodes = Store.list t in
  let read n =
    match Store.read t n with
    | None   -> failwith (Printf.sprintf "Cannot find %s" (to_string n))
    | Some v -> v in
  let nodes = List.map (fun n -> (n, read n)) nodes in

  (* Add all the vertices *)
  List.iter (G.add_vertex g) nodes;

  List.iter (fun (id, obj as src) ->
    let succs = Store.succ t id in
    List.iter (fun (l, succ) ->
      G.add_edge_e g (src, l, (succ, read succ))
    ) succs
  ) nodes;

  (* Return the graph *)
  g

let to_dot t file =
  let oc = open_out_bin (File.Name.to_string file) in
  let g = create_graph t in
  Dot.output_graph oc g;
  close_out oc

let current file =
  let t = Store.create (File.Dirname.of_string ".git") in
  Store.dump t;
  to_dot t file
