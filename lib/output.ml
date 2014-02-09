(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Lwt

let to_string node =
  let hex = SHA1.to_hex node in
  String.sub hex 0 8

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = (SHA1.t * Value.t)
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
        | Value.Blob _   -> "B"
        | Value.Commit _ -> "C"
        | Value.Tree _   -> "Tr"
        | Value.Tag  _   -> "Ta" in
      Printf.sprintf "\"%s-%s\"" p hex
    let vertex_attributes (_,o) =
      match o with
      | Value.Commit _ -> [`Shape `Doublecircle]
      | _ -> []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes (_,l,_) =
      match l with
      | "" -> []
      | _  ->[`Label l]
  end)

module Graph (Store: Store.S) = struct

  module Log = Log.Make(struct let section = "graph" end)

  module Search = struct
    include Search.Make(Store)
    include Search
  end

  let create_graph t =
    let g = G.create () in
    Store.contents t >>= fun nodes ->

    (* Add all the vertices *)
    List.iter ~f:(G.add_vertex g) nodes;

    begin
      Misc.list_iter_p (fun (id, _ as src) ->
          Search.succ t id >>= fun succs ->
          Misc.list_iter_p (fun s ->
              let l = match s with
                | `Commit _   -> ""
                | `Tag (t,_)  -> "TAG-" ^ t
                | `Tree (f,_) -> f in
              let sha1 = Search.sha1_of_succ s in
              Store.read_exn t sha1 >>= fun v ->
              G.add_edge_e g (src, l, (sha1, v));
              return_unit
            ) succs
        ) nodes
    end >>= fun () ->

    (* Return the graph *)
    return g

  let to_dot t file =
    create_graph t >>= fun g ->
    Out_channel.with_file file ~f:(fun oc -> Dot.output_graph oc g);
    return_unit

end
