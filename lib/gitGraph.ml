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
open Lwt

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

  let read n =
    GitLocal.read t n >>= function
    | None   -> fail (Failure (Printf.sprintf "Cannot find %s" (to_string n)))
    | Some v -> return v in

  let g = G.create () in

  GitLocal.list t >>= fun nodes ->
  Lwt_list.map_p (fun n ->
      read n >>= fun v -> return (n, v)
    ) nodes
  >>= fun nodes ->

  (* Add all the vertices *)
  List.iter ~f:(G.add_vertex g) nodes;

  begin
    Lwt_list.iter_s (fun (id, obj as src) ->
      GitLocal.succ t id >>= fun succs ->
      Lwt_list.iter_s (fun (l, succ) ->
          let l = match l with
            | `Parent -> ""
            | `Tag t  -> "TAG-" ^ t
            | `File f -> f in
          read succ >>= fun v ->
          G.add_edge_e g (src, l, (succ, v));
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
