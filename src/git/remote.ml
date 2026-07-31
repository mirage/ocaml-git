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
  Logs.Src.create "git.remote" ~doc:"logs git's store-less remote operations"

module Log = (val Logs.src_log src : Logs.LOG)
module Store = Mem.Store
module Sync = Mem.Sync (Mem.Store)

let ( >>? ) = Lwt_result.bind

let msgf fmt = Fmt.kstr (fun err -> `Msg err) fmt
let reword pp x = Lwt.map (Result.map_error (fun err -> msgf "%a" pp err)) x
let store_error x = reword Store.pp_error x
let sync_error x = reword Sync.pp_error x

(* The [git] library has no clock, so the default author is dated from the
   epoch. The documentation tells the caller to give a real one. *)
let anonymous =
  { User.name = "ocaml-git"; email = "ocaml-git@nowhere.invalid"; date = (0L, None) }

(* [path] is split into the directories leading to the new file and the name of
   the file itself, so that the rest of the module never has to handle an empty
   list of segments. *)
let segments path =
  let reserved = [ ""; "."; ".." ] in
  let segs = Fpath.segs path in
  if Fpath.is_abs path then
    Error (msgf "%a: not a relative path" Fpath.pp path)
  else if List.exists (fun seg -> List.mem seg reserved) segs then
    Error (msgf "%a: invalid path segment" Fpath.pp path)
  else
    match List.rev segs with
    | [] -> Error (msgf "%a: empty path" Fpath.pp path)
    | name :: rev_dirs -> Ok (List.rev rev_dirs, name)

let entry_named tree name =
  List.find_opt
    (fun entry -> String.equal entry.Tree.name name)
    (Tree.to_list tree)

let write store value = Store.write store value |> store_error

let write_tree store tree =
  write store (Store.Value.tree tree) >>? fun (hash, _) -> Lwt.return_ok hash

let read_tree store hash =
  Store.read store hash |> store_error >>? function
  | Value.Tree tree -> Lwt.return_ok tree
  | Value.Blob _ | Value.Commit _ | Value.Tag _ ->
      Lwt.return_error (msgf "%a: not a tree" Store.Hash.pp hash)

let read_commit store hash =
  Store.read store hash |> store_error >>? function
  | Value.Commit commit -> Lwt.return_ok commit
  | Value.Blob _ | Value.Tree _ | Value.Tag _ ->
      Lwt.return_error (msgf "%a: not a commit" Store.Hash.pp hash)

(* [fresh store entry dirs] writes the chain of brand-new trees [dirs] whose
   deepest one holds [entry], and returns the hash of its root. *)
let rec fresh store entry = function
  | [] -> write_tree store (Tree.v [ entry ])
  | dir :: dirs ->
      fresh store entry dirs >>? fun node ->
      write_tree store (Tree.v [ Tree.entry ~name:dir `Dir node ])

(* [graft store entry tree dirs] rewrites the existing [tree] so that [entry]
   sits at [dirs], and returns the hash of the rewritten root. It fails with
   [`Conflict] as soon as a segment of the path is already taken - by a file
   where a directory is needed, or by anything at all where the new file goes.
   Nothing is ever replaced nor removed. *)
let rec graft store entry tree = function
  | [] ->
      Option.fold
        ~none:(fun () -> write_tree store (Tree.add entry tree))
        ~some:(fun _taken () -> Lwt.return_error `Conflict)
        (entry_named tree entry.Tree.name)
        ()
  | dir :: dirs ->
      let replace node =
        write_tree store
          (Tree.add (Tree.entry ~name:dir `Dir node) (Tree.remove ~name:dir tree))
      in
      Option.fold
        ~none:(fun () -> fresh store entry dirs >>? replace)
        ~some:(fun taken () ->
          match taken.Tree.perm with
          | `Dir ->
              read_tree store taken.Tree.node >>? fun subtree ->
              graft store entry subtree dirs >>? replace
          | `Normal | `Everybody | `Exec | `Link | `Commit ->
              Lwt.return_error `Conflict)
        (entry_named tree dir)
        ()

let push_new_file ?(branch = Reference.master) ?(author = anonymous) ?message
    ~ctx edn ~path ~contents =
  let message =
    Option.value message ~default:(Fmt.str "Add %a" Fpath.pp path)
  in
  Lwt.return (segments path) >>? fun (dirs, name) ->
  Store.v (Fpath.v ".") |> store_error >>? fun store ->
  Sync.fetch ~ctx edn store ~deepen:(`Depth 1) `All |> sync_error >>? fun fetched ->
  (* A [`Depth 1] fetch brings the whole snapshot of the tip, so the conflict
     below is decided without any further exchange with the remote peer. *)
  let tip =
    Option.bind fetched (fun (_, refs) ->
        List.find_opt (fun (reference, _) -> Reference.equal reference branch) refs
        |> Option.map snd)
  in
  Log.debug (fun m ->
      m "%a exists remotely: %b" Reference.pp branch (Option.is_some tip));
  write store (Store.Value.blob (Blob.of_string contents))
  >>? fun (blob, _) ->
  let entry = Tree.entry ~name `Normal blob in
  Option.fold
    ~none:(fun () ->
      fresh store entry dirs >>? fun tree -> Lwt.return_ok (tree, []))
    ~some:(fun hash () ->
      read_commit store hash >>? fun commit ->
      read_tree store (Store.Value.Commit.tree commit) >>? fun root ->
      graft store entry root dirs >>? fun tree -> Lwt.return_ok (tree, [ hash ]))
    tip ()
  >>? fun (tree, parents) ->
  write store
    (Store.Value.commit
       (Store.Value.Commit.make ~tree ~author ~committer:author ~parents
          (Some message)))
  >>? fun (commit, _) ->
  Store.Ref.write store branch (Reference.uid commit) |> store_error
  >>? fun () ->
  (* [`Update] degrades to a ref creation when [branch] does not exist
     remotely, which is what we want for an empty repository. *)
  Sync.push ~ctx edn store [ `Update (branch, branch) ] |> sync_error
