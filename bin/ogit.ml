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

open Lwt
open Cmdliner
open Printf
open Git
open Git_unix

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ocaml-git/issues.";
]

(* Global options *)
type global = {
  verbose: bool;
  color  : bool;
}

let app_global g =
  if g.color then
    Log.color_on ();
  if g.verbose then
    Log.set_log_level Log.DEBUG

let color_tri_state =
  try match Sys.getenv "GITCOLOR" with
    | "always" -> `Always
    | "never"  -> `Never
    | _        -> `Auto
  with
  | Not_found  -> `Auto

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section
        ~doc:"Be more verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let color =
    let doc = Arg.info ~docv:"WHEN"
        ~doc:"Colorize the output. $(docv) must be `always', `never' or `auto'."
        ["color"] in
    let choices = Arg.enum [ "always", `Always; "never", `Never; "auto", `Auto ] in
    let arg = Arg.(value & opt choices color_tri_state & doc) in
    let to_bool = function
      | `Always -> true
      | `Never  -> false
      | `Auto   -> Unix.isatty Unix.stdout in
    Term.(pure to_bool $ arg)
  in
  Term.(pure (fun verbose color -> { verbose; color }) $ verbose $ color)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun global -> app_global global; fn) $ global)

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt conv default & doc)

let mk_required ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(required & opt conv default & doc)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(non_empty & pos_all conv [] & doc)

let gri =
  let parse str = `Ok (Gri.of_string str) in
  let print ppf name = Format.pp_print_string ppf (Gri.to_string name) in
  parse, print

let remote =
  let doc = Arg.info ~docv:"REPOSITORY"
      ~doc:"Location of the remote repository." [] in
  Arg.(required & pos 0 (some gri) None & doc)

let directory =
  let doc = Arg.info ~docv:"DIRECTORY"
      ~doc:"The name of the directory to clone into." [] in
  Arg.(value & pos 1 (some string) None & doc)

let reference =
  let parse str = `Ok (Reference.of_raw str) in
  let print ppf name = Format.pp_print_string ppf (Reference.to_raw name) in
  parse, print

let branch =
  let doc = Arg.info ~docv:"BRANCH"
      ~doc:"The name of the branch to update remotely." [] in
  Arg.(value & pos 1 reference Reference.master & doc)

let backend =
  let memory = mk_flag ["m";"in-memory"] "Use an in-memory store." in
  let create = function
    | true  -> (module Memory: Store.S)
    | false -> (module FS    : Store.S)
  in
  Term.(pure create $ memory)

let unpack =
  mk_flag ["unpack"] "Unpack the received pack archive."

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

(* CAT *)
(*
let cat = {
  name = "cat-file";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
    let file =
      let doc = Arg.info ~docv:"FILE"
          ~doc:"The name of the file to show the contents." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let cat_file file =
      run begin
        Lwt_io.with_file ~mode:Lwt_io.input file Lwt_io.read >>= fun buf ->
        let v = Value.input (Mstruct.of_string buf) in
        Printf.printf "%s%!" (Value.pretty v);
        return_unit
      end in
    Term.(mk cat_file $ file)
}
*)
(* CAT-FILE *)
let cat_file = {
  name = "cat-file";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
  let ty_flag = mk_flag ["t"] "Instead of the content, show the object type." in
  let sz_flag = mk_flag ["s"] "Instead of the content, show the object size." in
  let id =
    let doc = Arg.info ~docv:"ID" ~doc:"The id of the repository object." [] in
    Arg.(required & pos 0 (some string) None & doc) 
  in
  let cat_file (module S: Store.S) ty_flag sz_flag id =
    run begin
      S.create () >>= fun t ->
	S.read_exn t (SHA.of_hex id) >>= fun v -> begin
	  let t, c, s =
	    match v with
	    | Value.Blob blob -> 
		let c = Blob.pretty blob in
		"blob", c, String.length c
	    | Value.Commit commit ->
		let c = Commit.pretty commit in
		"commit", c, String.length c
	    | Value.Tree tree ->
		let c = Tree.pretty tree in
		"tree", c, String.length c
	    | Value.Tag tag ->
		let c = Tag.pretty tag in
		"tag", c, String.length c
	  in
	  if ty_flag then
	    Printf.printf "%s%!" t;

	  if sz_flag then
	    Printf.printf "%d%!" s;

	  if not ty_flag && not sz_flag then
	    Printf.printf "%s%!" c;

	  return_unit
	end 
    end
  in
  Term.(mk cat_file $ backend $ ty_flag $ sz_flag $ id)
}

(* LS-REMOTE *)
let ls_remote = {
  name = "ls-remote";
  doc  = "List references in a remote repository.";
  man  = [];
  term =
    let ls (module S: Store.S) remote =
      let module Sync = Sync.Make(S) in
      run begin
        S.create ()  >>= fun t ->
        Sync.ls t remote >>= fun references ->
        Printf.printf "From %s\n" (Gri.to_string remote);
        let print ref sha1 =
          Printf.printf "%s        %s\n"
            (SHA.Commit.to_hex sha1)
            (Reference.to_raw ref) in
        Reference.Map.iter print references;
        return_unit
      end in
    Term.(mk ls $ backend $ remote)
}

(* LS-FILES *)
let ls_files = {
  name = "ls-files";
  doc  = "Show information about files in the index and the working tree.";
  man  = [];
  term =
    let debug = mk_flag ["debug"]
        "After each line that describes a file, add more data about its cache entry. \
         This is intended to show as much information as possible for manual inspection; \
         the exact format may change at any time." in
    let ls (module S: Store.S) debug =
      run begin
        S.create ()    >>= fun t ->
        S.read_cache t >>= fun cache ->
        if debug then
          printf "%s" (Cache.pretty cache)
        else
          List.iter
            (fun e -> Printf.printf "%s\n" e.Cache.name)
            cache.Cache.entries;
        return_unit
      end in
    Term.(mk ls $ backend $ debug)
}

(* LS-TREE *)
let ls_tree = {
  name = "ls-tree";
  doc  = "List the contents of a tree object.";
  man  = [];
  term =
  let recurse_flag = mk_flag ["r"] "Recurse into sub-trees." in
  let oid =
    let doc = Arg.info [] ~docv:"ID"
        ~doc:"The id of the commit." 
    in
    Arg.(required & pos 0 (some string) None & doc ) 
  in
  let ls (module S: Store.S) recurse_flag oid =
    run begin
      S.create () >>= fun t ->

        let get_kind = function
          | `Dir    -> "tree",   true
          | `Commit -> "commit", false
          | _       -> "blob",   false
        in

        let rec walk recurse path sha1 =
          S.read_exn t sha1 >>= fun v -> begin
            match v with
            | Value.Blob blob -> begin
                printf "blob %s %s\n" (SHA.to_hex sha1) path;
                return_unit
            end
            | Value.Tree tree -> begin
                Lwt_list.iter_s
                  (fun e -> 
                    let path' = Filename.concat path e.Tree.name in
                    let kind, is_dir = get_kind e.Tree.perm in
                    let mode = Tree.string_of_perm e.Tree.perm in
                    printf "%s %s %s\t%s\n" mode kind (SHA.to_hex e.Tree.node) path';
                    if is_dir && recurse then
                      walk recurse path' e.Tree.node
                    else
                      return_unit
                  ) tree
            end
            | Value.Tag tag -> begin
                printf "tag %s %s\n" (SHA.to_hex sha1) path;
                return_unit
            end
            | Value.Commit commit -> begin
                printf "commit %s %s\n" (SHA.to_hex sha1) path;
                walk recurse path (SHA.of_tree commit.Commit.tree)
            end
          end
        in
        let sha1 = SHA.of_hex oid in
        walk recurse_flag "" sha1
    end 
  in
  Term.(mk ls $ backend $ recurse_flag $ oid)
}

(* READ-TREE *)
let read_tree = {
  name = "read-tree";
  doc  = "Reads tree information into the index.";
  man  = [];
  term =
    let commit =
      let doc = Arg.info [] ~docv:"COMMIT"
          ~doc:"The commit to set the index to. Use any valid tag \
                name as well." in
      Arg.(required & pos 0 (some string) None & doc ) in
    let read (module S: Store.S) commit_str =
      run begin
        S.create ()    >>= fun t ->
        S.references t >>= fun refs ->
        begin
          let (/) = Filename.concat in
          let ref = "refs" / "heads" / commit_str in
          if List.exists (fun r -> Reference.to_raw r = ref) refs then
            S.read_reference_exn t (Reference.of_raw ref)
          else
            return (SHA.Commit.of_hex commit_str)
        end >>= fun commit ->
        S.write_cache t commit >>= fun () ->
        printf "The index file has been update to %s\n%!" commit_str;
        return_unit
      end in
    Term.(mk read $ backend $ commit)
}

(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Clone a remote repository.";
  man  = [];
  term =
    let depth =
      mk_opt ["depth"] "DEPTH"
        "Create a shallow clone with a history truncated to the specified number \
         of revisions. Refer to $(b,git clone --help) for more information."
        Arg.(some int) None in
    let bare =
      mk_flag ["bare"] "Do not expand the filesystem." in
    let clone (module S: Store.S) deepen bare unpack remote dir =
      let dir = match dir with
        | Some d -> d
        | None   ->
          let str = Uri.path (Gri.to_uri remote) in
          let dir = Filename.basename str in
          if Filename.check_suffix dir ".git" then
            Filename.chop_extension dir
          else
            dir
      in
      if Sys.file_exists dir && Array.length (Sys.readdir dir) > 0 then (
        eprintf "fatal: destination path '%s' already exists and is not an empty directory.\n"
          dir;
        exit 128
      );
      let module Result = Sync.Result in
      let module Sync = Sync.Make(S) in
      run begin
        S.create ~root:dir ()   >>= fun t ->
        printf "Cloning into '%s' ...\n%!" (Filename.basename (S.root t));
        Sync.clone t ?deepen ~unpack remote >>= fun r ->
        if not bare then match r.Result.head with
          | None      -> return_unit
          | Some head ->
            S.write_cache t head >>= fun () ->
            printf "HEAD is now at %s\n" (SHA.Commit.to_hex head);
            return_unit
        else
          return_unit
      end in
    Term.(mk clone $ backend $ depth $ bare $ unpack $ remote $ directory)
}

(* FETCH *)
let fetch = {
  name = "fetch";
  doc  = "Fetch a remote Git repository.";
  man  = [];
  term =
    let fetch (module S: Store.S) unpack remote =
      let module Sync = Sync.Make(S) in
      run begin
        S.create ()                  >>= fun t ->
        Sync.fetch t ~unpack remote >>= fun _ ->
        return_unit
      end in
    Term.(mk fetch $ backend $ unpack $ remote)
}


(* PUSH *)
let push = {
  name = "push";
  doc  = "Update remote refs along with associated objects.";
  man  = [];
  term =
    let push (module S: Store.S) remote branch =
      let module Result = Sync.Result in
      let module Sync = Sync.Make(S) in
      run begin
        S.create ()                 >>= fun t ->
        S.read_reference t branch   >>= fun b ->
        let branch = match b with
          | None   -> Reference.of_raw
                        ("refs/heads/" ^ Reference.to_raw branch)
          | Some _ -> branch in
        Sync.push t ~branch remote >>= fun s ->
        printf "%s\n" (Result.pretty_push s);
        return_unit
      end in
    Term.(mk push $ backend $ remote $ branch)
}

(* GRAPH *)
let graph = {
  name = "graph";
  doc  = "Display a graph of the objects.";
  man  = [];
  term =
    let file =
      mk_required ["o";"output"] "FILE" "Output file."
        Arg.(some string) None in
    let graph (module S: Store.S) file =
      let module Graph = Global_graph.Make(S) in
      run begin
        S.create () >>= fun t ->
        let buf = Buffer.create 1024 in
        Graph.to_dot t buf >>= fun () ->
        Lwt_io.with_file ~mode:Lwt_io.output file (fun oc ->
            Lwt_io.write oc (Buffer.contents buf)
          )
      end in
    Term.(mk graph $ backend $ file)
}

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about ogit and ogit commands.";
  man  = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ];
  term =
    let topic =
      let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
      Arg.(value & pos 0 (some string) None & doc )
    in
    let help man_format cmds topic = match topic with
      | None       -> `Help (`Pager, None)
      | Some topic ->
        let topics = "topics" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
        | `Ok t                   -> `Help (man_format, Some t) in
  Term.(ret (pure help $Term.man_format $Term.choice_names $topic))
}

let default =
  let doc = "Mirage application builder" in
  let man = [
    `S "DESCRIPTION";
    `P "ogit is a small tool to experiement with the pure-OCaml implementation \
        of the Git format and protocol. Very few options are available, and it  \
        is not expected that this number grows very much in a near future.";
    `P "ogit is a prototype, so use it at your own risk (eg. it might corrupt \
        your data if you are particulary unlucky).";
    `P "Use either $(b,ogit <command> --help) or $(b,ogit help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: ogit [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
      \n\
      The most commonly used ogit commands are:\n\
      \    clone       %s\n\
      \    fetch       %s\n\
      \    push        %s\n\
      \    graph       %s\n\
      \n\
      See 'ogit help <command>' for more information on a specific command.\n%!"
      clone.doc fetch.doc push.doc graph.doc in
  Term.(pure usage $ (pure ())),
  Term.info "ogit"
    ~version:Git.Version.current
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map command [
    cat_file;
    ls_remote;
    ls_files;
    ls_tree;
    read_tree;
    clone;
    fetch;
    graph;
    push;
    help;
  ]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
