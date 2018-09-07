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

module Make (S : Minimal.S) = struct
  module Store = S

  module Log = struct
    let src =
      Logs.Src.create "git.search"
        ~doc:"logs git's internal search computation"

    include (val Logs.src_log src : Logs.LOG)
  end

  type pred =
    [ `Commit of Store.Hash.t
    | `Tag of string * Store.Hash.t
    | `Tree of string * Store.Hash.t
    | `Tree_root of Store.Hash.t ]

  let pred t ?(full = true) h =
    let tag t = `Tag (Store.Value.Tag.tag t, Store.Value.Tag.obj t) in
    Log.debug (fun l ->
        l ~header:"predecessor" "Read the object: %a." Store.Hash.pp h ) ;
    Store.read t h
    >|= function
    | Error err ->
        Log.err (fun l ->
            l ~header:"predecessor"
              "Retrieve an error when the search engine try to read %a: %a."
              Store.Hash.pp h Store.pp_error err ) ;
        []
    | Ok (Store.Value.Blob _) -> []
    | Ok (Store.Value.Commit c) ->
        (if full then [`Tree_root (Store.Value.Commit.tree c)] else [])
        @ List.map (fun x -> `Commit x) (Store.Value.Commit.parents c)
    | Ok (Store.Value.Tag t) -> if full then [tag t] else []
    | Ok (Store.Value.Tree t) ->
        if full then
          List.map
            (fun {Store.Value.Tree.name; node; _} -> `Tree (name, node))
            (Store.Value.Tree.to_list t)
        else []

  type path = [`Tag of string * path | `Commit of path | `Path of string list]

  let find_list f l =
    List.fold_left
      (fun acc x -> match acc with Some _ -> acc | None -> f x)
      None l

  let _find_commit = find_list (function `Commit x -> Some x | _ -> None)
  let find_tree_root = find_list (function `Tree_root x -> Some x | _ -> None)

  let find_tag l =
    find_list (function
      | `Tag (s, x) -> if l = s then Some x else None
      | _ -> None )

  let find_tree l =
    find_list (function
      | `Tree (s, x) -> if s = l then Some x else None
      | _ -> None )

  (* XXX: not tail-rec *)
  let rec find t hash path =
    match path with
    | `Path [] -> Lwt.return (Some hash)
    | `Tag (l, p) -> (
        pred t hash
        >>= fun preds ->
        match find_tag l preds with
        | None -> Lwt.return_none
        | Some s -> find t s p )
    | `Commit p -> (
        pred t hash
        >>= fun preds ->
        match find_tree_root preds with
        | None -> Lwt.return_none
        | Some s -> find t s p )
    | `Path (h :: p) -> (
        pred t hash
        >>= fun preds ->
        match find_tree h preds with
        | None -> Lwt.return_none
        | Some s -> find t s (`Path p) )

  (* XXX: can do one less look-up *)
  let mem t h path =
    find t h path
    >>= function None -> Lwt.return false | Some _ -> Lwt.return true
end
