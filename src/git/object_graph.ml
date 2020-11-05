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

let src =
  Logs.Src.create "git.object_graph"
    ~doc:"logs git's internal graph computation"

module Log = (val Logs.src_log src : Logs.LOG)
open Lwt.Infix

module type S = sig
  type hash
  type store

  module S : Set.S with type elt = hash
  module K : Graph.Sig.I with type V.t = hash

  val keys : K.t -> hash list
  val of_keys : store -> K.t Lwt.t
  val of_commits : store -> K.t Lwt.t
  val closure : ?full:bool -> store -> min:S.t -> max:S.t -> K.t Lwt.t
  val pack : store -> min:S.t -> max:S.t -> (hash * hash Value.t) list Lwt.t
  val to_dot : store -> Format.formatter -> unit Lwt.t
end

module Make (Hash : Digestif.S) (Store : Minimal.S with type hash = Hash.t) =
struct
  type hash = Hash.t
  type store = Store.t

  let to_string node =
    let hex = Hash.to_hex node in
    String.sub hex 0 8

  module C =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (struct
        type t = hash * hash Value.t

        let compare (x, _) (y, _) = Hash.unsafe_compare x y
        let hash (x, _) = Hashtbl.hash x
        let equal (x, _) (y, _) = Hash.unsafe_compare x y = 0
      end)
      (struct
        type t = string

        let default = ""
        let compare = String.compare
      end)

  module Dot = Graph.Graphviz.Dot (struct
    include C

    let graph_attributes _ = []
    let default_vertex_attributes _ = []

    let vertex_name (x, o) =
      let hex = to_string x in
      let p =
        match o with
        | Value.Blob _ -> "B"
        | Value.Commit _ -> "C"
        | Value.Tree _ -> "Tr"
        | Value.Tag _ -> "Ta"
      in
      Printf.sprintf "\"%s-%s\"" p hex

    let vertex_attributes (_, o) =
      match o with
      | Value.Commit _ -> [ `Shape `Doublecircle ]
      | Value.Blob _ | Value.Tree _ | Value.Tag _ -> []

    let get_subgraph _ = None
    let default_edge_attributes _ = []

    let edge_attributes (_, l, _) =
      match l with "" -> [] | _ -> [ `Label (String.escaped l) ]
  end)

  module Ordered = struct
    type t = hash

    let compare a b = Hash.unsafe_compare a b
  end

  module S = Set.Make (Ordered)

  module K = Graph.Imperative.Digraph.ConcreteBidirectional (struct
    type t = hash

    let equal x y = Hash.unsafe_compare x y = 0
    let hash = Hashtbl.hash
    let compare x y = Hash.unsafe_compare x y
  end)

  module T = Graph.Topological.Make (K)
  module Search = struct include Search.Make (Hash) (Store) include Search end

  let label = function
    | `Commit s -> "commit", s
    | `Tag (t, s) -> "TAG-" ^ t, s
    | `Tree (f, s) -> f, s
    | `Tree_root s -> "/", s

  let of_store t =
    let header = "of_store" in
    let g = C.create () in
    Log.debug (fun l -> l ~header "Loading the current Git repository.");
    Store.contents t >>= fun nodes ->
    Log.debug (fun l -> l ~header "Loading vertex in the graph.");
    List.iter (C.add_vertex g) nodes;
    Lwt_list.iter_s
      (fun ((id, _) as src) ->
        Log.debug (fun l -> l ~header "Search predecessors of %a." Hash.pp id);
        Search.pred t id >>= fun preds ->
        Lwt_list.iter_s
          (fun s ->
            let l, h = label s in
            Log.debug (fun l -> l ~header "Read the object: %a." Hash.pp h);
            Store.read_exn t h >>= fun v ->
            C.add_edge_e g (src, l, (h, v));
            Lwt.return ())
          preds)
      nodes
    >>= fun () -> Lwt.return g

  let of_keys t =
    let g = K.create () in
    Store.contents t >>= fun nodes ->
    List.iter (fun (k, _) -> K.add_vertex g k) nodes;
    Lwt_list.iter_p
      (fun (src, _) ->
        Search.pred t src >>= fun succs ->
        Lwt_list.iter_p
          (fun s ->
            let _, h = label s in
            if K.mem_vertex g h then K.add_edge g src h;
            Lwt.return_unit)
          succs)
      nodes
    >>= fun () -> Lwt.return g

  let of_commits t =
    let g = K.create () in
    Store.contents t >>= fun nodes ->
    List.iter
      (function
        | k, Value.Commit _ -> K.add_vertex g k
        | _, (Value.Tree _ | Value.Tag _ | Value.Blob _) -> ())
      nodes;
    Lwt_list.iter_p
      (fun (src, _) ->
        Search.pred ~full:false t src >>= fun succs ->
        Lwt_list.iter_p
          (fun s ->
            let _, h = label s in
            if K.mem_vertex g h then K.add_edge g src h;
            Lwt.return ())
          succs)
      nodes
    >>= fun () -> Lwt.return g

  let to_dot t ppf =
    of_store t >>= fun g ->
    Dot.fprint_graph ppf g;
    Lwt.return_unit

  let closure ?(full = true) t ~min ~max =
    let g = K.create ~size:1024 () in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let min = S.fold (fun x a -> x :: a) min [] in
    Lwt_list.iter_p
      (fun k ->
        Store.mem t k >>= function
        | false -> Lwt.return_unit
        | true ->
            mark k;
            K.add_vertex g k;
            Lwt.return_unit)
      min
    >>= fun () ->
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        Store.mem t key >>= function
        | false -> Lwt.return_unit
        | true ->
            if not (K.mem_vertex g key) then K.add_vertex g key;
            Search.pred ~full t key >>= fun preds ->
            let keys = List.map (fun x -> snd (label x)) preds in
            List.iter (fun k -> K.add_edge g k key) keys;
            Lwt_list.iter_p add keys)
    in
    let max = S.fold (fun x a -> x :: a) max [] in
    Lwt_list.iter_p add max >>= fun () -> Lwt.return g

  let keys g = T.fold (fun k l -> k :: l) g [] |> List.rev

  let pack t ~min ~max =
    closure t ~min ~max >>= fun g ->
    let keys = keys g in
    Lwt_list.fold_left_s
      (fun a k -> Store.read_exn t k >|= fun v -> (k, v) :: a)
      [] keys
end
