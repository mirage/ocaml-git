(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let to_string node =
  let hex = Hash.to_hex node in
  String.sub hex 0 8

module C =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = (Hash.t * Value.t)
      let compare (x,_) (y,_) = Hash.compare x y
      let hash (x,_) = Hash.hash x
      let equal (x,_) (y,_) = Hash.equal x y
    end)
    (struct
      type t = string
      let default = ""
      let compare = String.compare
    end)

module Dot = Graph.Graphviz.Dot(struct
    include C
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
      | _  ->[`Label (String.escaped l)]
  end)

module K = Graph.Imperative.Digraph.ConcreteBidirectional(Hash)
module T = Graph.Topological.Make(K)
module KO = Graph.Oper.I(K)

module Log = (val Misc.src_log "graph" : Logs.LOG)

module Make (Store: Store.S) = struct

  module Search = struct
    include Search.Make(Store)
    include Search
  end

  let label = function
    | `Commit s    -> "commit"  , s
    | `Tag (t,s)   -> "TAG-" ^ t, s
    | `Tree (f,s)  -> f         , s
    | `Tree_root s -> "/"       , s

  let of_store t =
    Log.debug (fun l -> l "of_store");
    let g = C.create () in
    Store.contents t >>= fun nodes ->
    List.iter (C.add_vertex g) nodes;
    begin
      Lwt_list.iter_p (fun (id, _ as src) ->
          Search.pred t id >>= fun preds ->
          Lwt_list.iter_p (fun s ->
              let l, h = label s in
              Store.read_exn t h >>= fun v ->
              C.add_edge_e g (src, l, (h, v));
              Lwt.return_unit
            ) preds
        ) nodes
    end >>= fun () ->
    Lwt.return g

  let of_keys t =
    Log.debug (fun l -> l "of_keys");
    let g = K.create () in
    Store.contents t >>= fun nodes ->
    List.iter (fun (k, _) -> K.add_vertex g k) nodes;
    begin
      Lwt_list.iter_p (fun (src, _) ->
          Search.pred t src >>= fun succs ->
          Lwt_list.iter_p (fun s ->
              let _, h = label s in
              if K.mem_vertex g h then K.add_edge g src h;
              Lwt.return_unit
            ) succs
        ) nodes
    end >>= fun () ->
    Lwt.return g

  let to_dot t buf =
    Log.debug (fun l -> l "to_dot");
    let fmt = Format.formatter_of_buffer buf in
    of_store t >>= fun g ->
    Dot.fprint_graph fmt g;
    Lwt.return_unit

  let closure ?(full=true) t ~min ~max =
    Log.debug (fun l -> l "closure");
    let g = K.create ~size:1024 () in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let min = Hash.Set.to_list min in
    Lwt_list.iter_p (fun k ->
        Store.mem t k >>= function
        | false -> Lwt.return_unit
        | true  ->
          mark k;
          K.add_vertex g k;
          Lwt.return_unit
      ) min >>= fun () ->
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        Log.debug (fun l -> l "ADD %a" Hash.pp key);
        Store.mem t key >>= function
        | false -> Lwt.return_unit
        | true  ->
          if not (K.mem_vertex g key) then K.add_vertex g key;
          Search.pred ~full t key >>= fun preds ->
          let keys = List.map (fun x -> snd (label x)) preds in
          List.iter (fun k -> K.add_edge g k key) keys;
          Lwt_list.iter_p add keys
      ) in
    let max = Hash.Set.to_list max in
    Lwt_list.iter_p add max >>= fun () ->
    Lwt.return g

  let keys g = T.fold (fun k l -> k :: l) g [] |> List.rev

  let pack t ~min ~max =
    closure t ~min ~max >>= fun g ->
    let keys = keys g in
    Lwt_list.map_p (fun k -> Store.read_exn t k >|= fun v -> (k, v)) keys

end
