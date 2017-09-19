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

open Lwt.Infix

module Make (S : Minimal.S with type Hash.Digest.buffer = Cstruct.t
                              and type Hash.hex = string)
= struct
  module Store = S

  let to_string node =
    let hex = Store.Hash.to_hex node in
    String.sub hex 0 8

  module C =
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
      (struct
        type t = (Store.Hash.t * Store.Value.t)
        let compare (x,_) (y,_) = Store.Hash.compare x y
        let hash (x,_) = Store.Hash.hash x
        let equal (x,_) (y,_) = Store.Hash.equal x y
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
          | Store.Value.Blob _   -> "B"
          | Store.Value.Commit _ -> "C"
          | Store.Value.Tree _   -> "Tr"
          | Store.Value.Tag  _   -> "Ta" in
        Printf.sprintf "\"%s-%s\"" p hex
      let vertex_attributes (_,o) =
        match o with
        | Store.Value.Commit _ -> [`Shape `Doublecircle]
        | Store.Value.Blob _ | Store.Value.Tree _ | Store.Value.Tag _ -> []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes (_,l,_) =
        match l with
        | "" -> []
        | _  ->[`Label (String.escaped l)]
    end)

  module K = Graph.Imperative.Digraph.ConcreteBidirectional(Store.Hash)
  module T = Graph.Topological.Make(K)

  module Log =
  struct
    let src = Logs.Src.create "git.object_graph" ~doc:"logs git's internal graph computation"
    include (val Logs.src_log src : Logs.LOG)
  end

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
    let header = "of_store" in
    let g = C.create () in

    Log.debug (fun l -> l ~header "Loading the current Git repository.");

    Store.contents t >>= function
    | Error err ->
      Log.err (fun l -> l ~header "Retrieve an error when we list the git repository: %a." Store.pp_error err);
      Lwt.return g
    | Ok nodes ->
      Log.debug (fun l -> l ~header "Loading vertex in the graph.");
      List.iter (C.add_vertex g) nodes;

      begin
        Lwt_list.iter_s (fun (id, _ as src) ->
            Log.debug (fun l -> l ~header "Search predecessors of %a." Store.Hash.pp id);

            Search.pred t id >>= fun preds ->
            Lwt_list.iter_s (fun s ->
                let l, h = label s in

                Log.debug (fun l -> l ~header "Read the object: %a." Store.Hash.pp h);

                Store.read t h >>= function
                | Ok v -> C.add_edge_e g (src, l, (h, v)); Lwt.return ()
                | Error err ->
                  Log.err (fun l -> l ~header "Retrieve an error when we try to read %a: %a." Store.Hash.pp h Store.pp_error err);
                  Lwt.return () (* XXX(dinosaure): quiet this error? *)
              ) preds
          ) nodes
      end >>= fun () -> Lwt.return g

  let of_keys t =
    let g = K.create () in
    Store.contents t >>= function
    | Error _ -> Lwt.return g
    | Ok nodes ->
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

  let of_commits t =
    let g = K.create () in
    Store.contents t >>=function
    | Error _ -> Lwt.return g
    | Ok nodes ->
      List.iter (function
          | (k, Store.Value.Commit _) -> K.add_vertex g k
          | (_, (Store.Value.Tree _ | Store.Value.Tag _ | Store.Value.Blob _ )) -> ()) nodes;
      begin
        Lwt_list.iter_p (fun (src, _) ->
            Search.pred ~full:false t src >>= fun succs ->
            Lwt_list.iter_p (fun s ->
                let _, h = label s in
                if K.mem_vertex g h then K.add_edge g src h;
                Lwt.return ())
              succs
          ) nodes
      end >>= fun () -> Lwt.return g

  let to_dot t buf =
    let fmt = Format.formatter_of_buffer buf in
    of_store t >>= fun g ->
    Dot.fprint_graph fmt g;
    Lwt.return_unit

  let closure ?(full=true) t ~min ~max =
    let g = K.create ~size:1024 () in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let min = Store.Hash.Set.fold (fun x a -> x :: a) min [] in
    Lwt_list.iter_p (fun k ->
        Store.exists t k >>= function
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
        Store.exists t key >>= function
        | false -> Lwt.return_unit
        | true  ->
          if not (K.mem_vertex g key) then K.add_vertex g key;
          Search.pred ~full t key >>= fun preds ->
          let keys = List.map (fun x -> snd (label x)) preds in
          List.iter (fun k -> K.add_edge g k key) keys;
          Lwt_list.iter_p add keys
      ) in
    let max = Store.Hash.Set.fold (fun x a -> x :: a) max [] in
    Lwt_list.iter_p add max >>= fun () ->
    Lwt.return g

  let keys g = T.fold (fun k l -> k :: l) g [] |> List.rev

  let pack t ~min ~max =
    closure t ~min ~max >>= fun g ->
    let keys = keys g in
    Lwt_list.fold_left_s
      (fun a k -> Store.read t k >|= function Ok v -> (k, v) :: a
                                            | Error _ -> a)
      [] keys
    (* XXX(dinosaure): needed to use [map_p]. *)
end
