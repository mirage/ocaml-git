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

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Hex.Object)

module Dot = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name x = Hex.Object.to_string x
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end)

let create_graph objects =
  let g = G.create () in
  List.iter Hex.Object.(fun o -> G.add_vertex g o) objects;
  (* XXX: add the edges *)
  g

let to_dot objects file =
  let oc = open_out_bin (File.Name.to_string file) in
  let g = create_graph objects in
  Dot.output_graph oc g
