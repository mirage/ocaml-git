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

module Make (Hash : Digestif.S) (Store : Minimal.S with type hash = Hash.t) =
struct
  type hash = Hash.t
  type store = Store.t

  module Log = struct
    let src =
      Logs.Src.create "git.search" ~doc:"logs git's internal search computation"

    include (val Logs.src_log src : Logs.LOG)
  end

  type pred =
    [ `Commit of hash
    | `Tag of string * hash
    | `Tree of string * hash
    | `Tree_root of hash ]

  let pred t ?(full = true) h =
    let tag t = `Tag (Store.Value.Tag.tag t, Store.Value.Tag.obj t) in
    Log.debug (fun l ->
        l ~header:"predecessor" "Read the object: %a." Hash.pp h);
    Store.read_exn t h >>= function
    | Value.Blob _ -> Lwt.return []
    | Value.Commit c -> (
        Store.is_shallowed t h >|= function
        | true ->
            if full then [ `Tree_root (Store.Value.Commit.tree c) ] else []
        | false ->
            (if full then [ `Tree_root (Store.Value.Commit.tree c) ] else [])
            @ List.map (fun x -> `Commit x) (Store.Value.Commit.parents c) )
    | Value.Tag t -> if full then Lwt.return [ tag t ] else Lwt.return []
    | Value.Tree t ->
        if full then
          let lst =
            List.map
              (fun { Tree.name; node; _ } -> `Tree (name, node))
              (Store.Value.Tree.to_list t)
          in
          Lwt.return lst
        else Lwt.return []

  type path = [ `Tag of string * path | `Commit of path | `Path of string list ]

  let find_list f l =
    List.fold_left
      (fun acc x -> match acc with Some _ -> acc | None -> f x)
      None l

  let _find_commit = find_list (function `Commit x -> Some x | _ -> None)
  let find_tree_root = find_list (function `Tree_root x -> Some x | _ -> None)

  let find_tag l =
    find_list (function
      | `Tag (s, x) -> if l = s then Some x else None
      | _ -> None)

  let find_tree l =
    find_list (function
      | `Tree (s, x) -> if s = l then Some x else None
      | _ -> None)

  let rec find t hash path =
    match path with
    | `Path [] -> Lwt.return (Some hash)
    | `Tag (l, p) -> (
        pred t hash >>= fun preds ->
        match find_tag l preds with
        | None -> Lwt.return_none
        | Some s -> (find [@tailcall]) t s p )
    | `Commit p -> (
        pred t hash >>= fun preds ->
        match find_tree_root preds with
        | None -> Lwt.return_none
        | Some s -> (find [@tailcall]) t s p )
    | `Path (h :: p) -> (
        pred t hash >>= fun preds ->
        match find_tree h preds with
        | None -> Lwt.return_none
        | Some s -> (find [@tailcall]) t s (`Path p) )

  (* XXX: can do one less look-up *)
  let mem t h path =
    find t h path >>= function
    | None -> Lwt.return false
    | Some _ -> Lwt.return true
end
