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

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = (SHA1.t * Model.obj)
      let compare (x,_) (y,_) = String.compare (SHA1.to_string x) (SHA1.to_string y)
      let hash (x,_) = Hashtbl.hash (SHA1.to_string x)
      let equal (x,_) (y,_) = (x = y)
    end)
    (struct
      type t = string
      let default = ""
      let compare = String.compare
    end)

let hex_of_sha1 sha1 =
  Hex.of_string (Misc.hex_encode (SHA1.to_string sha1))

module Dot = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name (x,o) =
      let hex = hex_of_sha1 x in
      let p = match o with
        | Blob s   -> "B"
        | Commit _ -> "C"
        | Tree _   -> "Tr"
        | Tag  _   -> "Ta" in
      Printf.sprintf "\"%s-%s\"" p (String.sub (Hex.to_string hex) 0 8)
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

  (* Add all the vertices *)
  let add_vertex fn (s,x) = G.add_vertex g (s, fn x) in
  let blob b = Blob b and commit c = Commit c
  and tag t = Tag t and tree t = Tree t in
  List.iter (add_vertex blob) t.blobs;
  List.iter (add_vertex commit) t.commits;
  List.iter (add_vertex tag) t.tags;
  List.iter (add_vertex tree) t.trees;

  (* Add the commit edges *)
  List.iter (fun (sha1, commit) ->
    let src = sha1, Commit commit in
    let tree =
      let s = Hex.Tree.to_sha1 commit.tree in
      s, Tree (List.assoc s t.trees) in
    let parent p =
      let s = Hex.Commit.to_sha1 p in
      s, Commit (List.assoc s t.commits) in
    G.add_edge g src tree;
    List.iter (fun p -> G.add_edge g (parent p) src) commit.parents
  ) t.commits;

  (* Add the tree edges *)
  List.iter (fun (sha1, tree) ->
    let src = sha1, Tree tree in
    List.iter (fun e ->
      let dst = e.sha1, Model.find e.sha1 t in
      G.add_edge_e g (src, e.file, dst)
    ) tree
  ) t.trees;

  (* Add the tag edges *)
  List.iter (fun (sha1, tag) ->
    let src = sha1, Tag tag in
    let dst =
      let s = Hex.Commit.to_sha1 tag.commit in
      s, Commit (List.assoc s t.commits) in
    G.add_edge_e g (src, tag.tag, dst)
  ) t.tags;

  (* Return the graph *)
  g

let to_dot t file =
  let oc = open_out_bin (File.Name.to_string file) in
  let g = create_graph t in
  Dot.output_graph oc g;
  close_out oc
