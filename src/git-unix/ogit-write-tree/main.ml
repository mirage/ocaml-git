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

let () = Random.self_init ()

open Git_unix

let option_map f = function
  | Some v -> Some (f v)
  | None -> None

let option_map_default v f = function
  | Some v -> f v
  | None -> v

let option_value_exn f = function
  | Some v -> v
  | None -> f ()

let pad n x =
  if String.length x > n
  then x
  else x ^ String.make (n - String.length x) ' '

let pp_header ppf (level, header) =
  let level_style =
    match level with
    | Logs.App -> Logs_fmt.app_style
    | Logs.Debug -> Logs_fmt.debug_style
    | Logs.Warning -> Logs_fmt.warn_style
    | Logs.Error -> Logs_fmt.err_style
    | Logs.Info -> Logs_fmt.info_style
  in
  let level = Logs.level_to_string (Some level) in

  Fmt.pf ppf "[%a][%a]"
    (Fmt.styled level_style Fmt.string) level
    (Fmt.option Fmt.string) (option_map (pad 10) header)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 10 (Fmt.strf "%+04.0fus" dt))
        pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_src_and_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stdout);
  let quiet = match style_renderer with Some _ -> true | None -> false in
  quiet, Fmt.stdout

module Entry = Index.Entry(SHA1)
module Index = Index.Make(SHA1)(Store)(Fs)(Entry)

type error =
  [ `Store of Store.error
  | `Index of Index.error
  | `App of string ]

let store_err err = Lwt.return (`Store err)
let index_err err = Lwt.return (`Index err)
let app_err err = Lwt.return (`App err)

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "%a" Store.pp_error err
  | `Index err -> Fmt.pf ppf "%a" Index.pp_error err
  | `App msg -> Fmt.string ppf msg

open Lwt.Infix
let ( >!= ) = Lwt_result.bind_lwt_err
let ( >?= ) = Lwt_result.bind

let perm_of_entry { Index.Entry.info = { mode; _ }; _ } =
  match mode with
  | Entry.Exec -> `Exec
  | Entry.Normal -> `Normal
  | Entry.Everybody -> `Everybody
  | Entry.Symlink -> `Link
  | Entry.Gitlink -> `Commit

let hash_of_blob entry = entry.Index.Entry.hash

let rec index_entries_to_tree_entries ~bucket entries =
  List.map (fun (name, entry) -> match entry with
      | Index.Blob entry ->
        Store.Value.Tree.entry name (perm_of_entry entry) (hash_of_blob entry)
      | Index.Tree entries ->
        Store.Value.Tree.entry name `Dir (hash_of_tree ~bucket entries)
    ) (Index.StringMap.bindings entries)

and hash_of_tree ~bucket entries =
  let entries = index_entries_to_tree_entries ~bucket entries in
  let tree = Store.Value.Tree.of_list entries in
  let hash = Store.Value.Tree.digest tree in
  let () = Hashtbl.add bucket hash tree in
  hash

let hash_of_root ~bucket (Index.Root entries) =
  let entries = index_entries_to_tree_entries ~bucket entries in
  let root_tree = Store.Value.Tree.of_list entries in
  let root_hash = Store.Value.Tree.digest root_tree in
  let () = Hashtbl.add bucket root_hash root_tree in
  root_hash

let main ?prefix:_ _ =
  let dtmp = Cstruct.create 0x8000 in
  let root = Fpath.(v (Sys.getcwd ())) in

  Store.v root >!= store_err >?= fun t ->
    Index.IO.load () ~root:(Store.dotgit t) ~dtmp >!= index_err >?= fun (entries, _) ->
      let root = Index.of_entries entries in
      let tree = Hashtbl.create 128 in
      let root_hash = hash_of_root ~bucket:tree root in
      let todo = Hashtbl.fold (fun hash tree acc -> (hash, tree) :: acc) tree [] in

      Lwt_list.fold_left_s
        (fun s (hash, tree) -> match s with
           | Error _ as err -> Lwt.return err
           | Ok () ->
             Store.write t (Store.Value.tree tree) >>= function
             | Ok (hash', _) ->
               if Store.Hash.equal hash hash'
               then Lwt.return (Ok ())
               else
                 Lwt.return
                   (Error
                     (`App
                      (Fmt.strf "Produced bad hash from the tree object: %a. We expected %a and we have %a."
                         Store.Value.Tree.pp tree
                         Store.Hash.pp hash
                         Store.Hash.pp hash')))
             | Error err -> Lwt.return (Error (`Store err)))
        (Ok ()) todo >?= fun () ->
        Fmt.(pf stdout) "%a\n" Store.Hash.pp root_hash;
        Lwt.return (Ok ())

open Cmdliner

module Flag =
struct
  let prefix =
    let doc = "Writes a tree object that represents a subdirectory <prefix>. \
               This can be used to write the tree object for a subproject \
               that is in the named subdirectory." in
    Arg.(value
         & opt (some dir) None
         & info [ "prefix" ] ~doc ~docv:"<prefix>")

  let missing_ok =
    let doc = "Normally git write-tree ensures that the objects referenced by \
               the directory exist in the object database. This option disables this check." in
    Arg.(value
         & flag
         & info [ "missing-ok" ] ~doc)
end

let setup_log =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main prefix missing_ok _ =
  match Lwt_main.run (main ?prefix missing_ok) with
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Fmt.strf "%a" pp_error err)

let command =
  let doc = "Create a tree object from the current index" in
  let exits = Term.default_exits in
  Term.(ret (const main $ Flag.prefix $ Flag.missing_ok $ setup_log)),
  Term.info "ogit-write-tree" ~version:"v0.1" ~doc ~exits

let () = Term.(exit @@ eval command)
