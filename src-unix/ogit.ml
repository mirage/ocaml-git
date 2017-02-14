(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("\r%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

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

let mk (fn:'a): 'a Term.t = Term.(pure (fun () -> fn) $ setup_log)

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
  let doc =
    Arg.info ~docv:"BRANCH" ~doc:"The name of the branch to update remotely." []
  in
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
  Lwt_main.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

(* CAT *)
let cat = {
  name = "cat";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
    let file =
      let doc = Arg.info ~docv:"FILE"
          ~doc:"The name of the file to show the contents." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let cat_file file =
      run begin
        Lwt_io.with_file ~mode:Lwt_io.input file (fun x -> Lwt_io.read x)
        >>= fun buf ->
        let v = Value_IO.input (Mstruct.of_string buf) in
        Fmt.(pf stdout) "%a%!\n" Value.pp v;
        Lwt.return_unit
      end in
    Term.(mk cat_file $ file)
}

let catch_ambiguous f =
  Lwt.catch f (function
      | Hash.Ambiguous s -> eprintf "%s: ambiguous argument\n%!" s; exit 1
      | Not_found ->
        eprintf "unknown revision or path not in the working tree\n%!";
        exit 1
      | e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1
    )

(* CAT-FILE *)
let cat_file = {
  name = "cat-file";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
    let ty_flag = mk_flag ["t"] "Instead of the content, show the object type." in
    let sz_flag = mk_flag ["s"] "Instead of the content, show the object size." in
    let id =
      let doc = Arg.info ~docv:"Hash1" ~doc:"The Hash1 of the repository object." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let cat_file (module S: Store.S) ty_flag sz_flag id =
      run begin
        S.create ~root:(Sys.getcwd ()) () >>= fun t ->
        catch_ambiguous (fun () ->
            S.read_exn t (Hash_IO.of_short_hex id) >>= fun v ->
            let t, c, s = match v with
              | Value.Blob blob ->
                let c = Blob.to_raw blob in
                "blob", c, String.length c
              | Value.Commit commit ->
                let c = Fmt.to_to_string Commit.pp commit in
                "commit", c, String.length c
              | Value.Tree tree ->
                let c = Fmt.to_to_string Tree.pp tree in
                "tree", c, String.length c
              | Value.Tag tag ->
                let c = Fmt.to_to_string Tag.pp tag in
                "tag", c, String.length c
            in
            if ty_flag then Printf.printf "%s%!\n" t;
            if sz_flag then Printf.printf "%d%!\n" s;
            if not ty_flag && not sz_flag then Printf.printf "%s%!\n" c;
            Lwt.return_unit)
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
        S.create ~root:(Sys.getcwd ()) ()  >>= fun t ->
        Sync.ls t remote >>= fun references ->
        Printf.printf "From %s\n" (Gri.to_string remote);
        let print r h =
          Printf.printf "%s        %s\n" (Hash.to_hex h) (Reference.to_raw r)
        in
        Reference.Map.iter print references;
        Lwt.return_unit
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
        "After each line that describes a file, add more data about its cache \
         entry. This is intended to show as much information as possible for \
         manual inspection; the exact format may change at any time."
    in
    let ls (module S: Store.S) debug =
      run begin
        S.create ~root:(Sys.getcwd ()) ()    >>= fun t ->
        S.read_index t >>= fun cache ->
        if debug then
          Fmt.(pf stdout) "%a" Index.pp cache
        else
          List.iter
            (fun e -> Printf.printf "%s\n" e.Index.name)
            cache.Index.entries;
        Lwt.return_unit
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
    let show_tree_flag =
      mk_flag ["t"] "Show tree entries even when going to recurse them."
    in
    let only_tree_flag = mk_flag ["d"] "Show only the named tree entry itself." in
    let oid =
      let doc = Arg.info [] ~docv:"Hash1"
          ~doc:"The Hash1 of the tree."
      in
      Arg.(required & pos 0 (some string) None & doc )
    in
    let get_kind = function
      | `Dir    -> "tree",   true
      | `Commit -> "commit", false
      | _       -> "blob",   false
    in
    let ls (module S: Store.S) recurse show_tree only_tree oid =
      let pp_blob path h =
        printf "blob %s %s\n" (Hash.to_hex h) path;
        Lwt.return_unit
      in
      let pp_tree mode kind path e =
        printf "%s %s %s\t%s\n" mode kind (Hash.to_hex e.Tree.node) path
      in
      let pp_tag path h =
        printf "tag %s %s\n" (Hash.to_hex h) path;
        Lwt.return_unit
      in
      let rec walk t path h =
        S.read_exn t h >>= function
        | Value.Commit c  -> walk t path (Hash.of_tree c.Commit.tree)
        | Value.Blob _    -> pp_blob path h
        | Value.Tag _     -> pp_tag path h
        | Value.Tree tree ->
          Lwt_list.iter_s (fun e ->
              let path = Filename.concat path e.Tree.name in
              let kind, is_dir = get_kind e.Tree.perm in
              let mode = Tree.fixed_length_string_of_perm e.Tree.perm in
              let show =
                if is_dir then not recurse || show_tree || only_tree
                else not only_tree
              in
              if show then pp_tree mode kind path e;
              if is_dir && recurse then walk t path e.Tree.node
              else Lwt.return_unit
            ) tree
      in
      run begin
        S.create ~root:(Sys.getcwd ()) () >>= fun t ->
        let h = Hash_IO.of_short_hex oid in
        catch_ambiguous (fun () -> walk t "" h)
      end in
    Term.(mk ls $ backend $ recurse_flag $ show_tree_flag
          $ only_tree_flag $ oid)
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
        S.create ~root:(Sys.getcwd ()) ()    >>= fun t ->
        S.references t >>= fun refs ->
        begin
          let (/) = Filename.concat in
          let ref = "refs" / "heads" / commit_str in
          if List.exists (fun r -> Reference.to_raw r = ref) refs then
            S.read_reference_exn t (Reference.of_raw ref)
          else
            Lwt.return (Hash_IO.of_short_hex commit_str)
        end >>= fun commit ->
        S.write_index t (Hash.to_commit commit) >>= fun () ->
        printf "The index file has been update to %s\n%!" commit_str;
        Lwt.return_unit
      end in
    Term.(mk read $ backend $ commit)
}

let reference_of_raw branch =
  Reference.of_raw ("refs/heads/" ^ Reference.to_raw branch)

let progress s = printf "\r%s%!" s

let err_not_a_directory dir =
  eprintf "fatal: %s is not a directory.\n" dir;
  exit 128

let err_not_empty dir =
  eprintf "fatal: destination path '%s' already exists and is not an \
           empty directory.\n" dir;
  exit 128

let (>+=) x f = match x with None -> None | Some x -> Some (f x)

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
      mk_flag ["bare"]
        "Make a bare Git repository. That is, instead of creating <directory> \
         and placing the administrative files in <directory>/.git, make the \
         $(i, <directory>) itself the $(i,$GIT_DIR). This obviously implies \
         the -n because there is nowhere to check out the working tree."
    in
    let no_checkout =
      mk_flag ["n"; "no-checkout"]
        "No checkout of HEAD is performed after the clone is complete."
    in
    let branch =
      mk_opt ["b"; "branch"] "BRANCH"
        "Instead of pointing the newly created HEAD to the branch pointed to by \
         the cloned repository's HEAD, point to $(b, name) branch instead. In a \
         non-bare repository, this is the branch that will be checked out."
        Arg.(some reference) None
    in
    let clone (module S: Store.S)
        deepen bare no_checkout branch unpack remote dir =
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
      if Sys.file_exists dir && not (Sys.is_directory dir) then
        err_not_a_directory dir;
      if Sys.file_exists dir && Array.length (Sys.readdir dir) > 0 then
        err_not_empty dir;
      let module Result = Sync.Result in
      let module Sync = Sync.Make(S) in
      run begin
        let dot_git = if bare then Some dir else None in
        S.create ~root:dir ?dot_git () >>= fun t ->
        let branch = branch >+= fun b -> `Ref (reference_of_raw b) in
        printf "Cloning into '%s' ...\n%!" (Filename.basename (S.root t));
        let checkout = not (bare || no_checkout) in
        Sync.clone t ?deepen ~unpack ?branch ~progress ~checkout remote
        >|= fun _ -> ()
      end in
    Term.(mk clone $ backend $ depth $ bare $ no_checkout $ branch $
          unpack $ remote $ directory)
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
        S.create ~root:(Sys.getcwd ()) ()     >>= fun t ->
        Sync.fetch t ~unpack ~progress remote >>= fun _ ->
        Lwt.return_unit
      end in
    Term.(mk fetch $ backend $ unpack $ remote)
}

(* PULL *)
let pull = {
  name = "pull";
  doc  = "Pull a remote Git repository (and reset --hard to the new head).";
  man  = [];
  term =
    let pull (module S: Store.S) unpack remote =
      let module Sy = Sync.Make(S) in
      run begin
        S.create ~root:(Sys.getcwd ()) () >>= fun t ->
        S.read_head t >>= fun h ->
        Sy.fetch t ~unpack remote >>= fun r ->
        match h with
        | None
        | Some (Reference.Hash _) -> Lwt.return_unit
        | Some (Reference.Ref b)  ->
          let refs = Sync.Result.references r in
          if Reference.Map.mem b refs then (
            let commit = Reference.Map.find b refs in
            S.write_reference t b commit >>= fun () ->
            S.write_index t (Hash.to_commit commit)
          ) else
            Lwt.return_unit
      end
    in
    Term.(mk pull $ backend $ unpack $ remote)
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
        S.create ~root:(Sys.getcwd ()) () >>= fun t ->
        S.read_reference t branch >>= fun b ->
        let branch = match b with
          | None   -> reference_of_raw branch
          | Some _ -> branch in
        Sync.push t ~branch remote >>= fun s ->
        Fmt.(pf stdout) "%a\n" Result.pp_push s;
        Lwt.return_unit
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
      let module Graph = Git.Graph.Make(S) in
      run begin
        S.create ~root:(Sys.getcwd ()) () >>= fun t ->
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
    let help man_format cmds topic () = match topic with
      | None       -> `Help (`Pager, None)
      | Some topic ->
        let topics = "topics" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
        | `Ok t                   -> `Help (man_format, Some t) in
    Term.(ret (pure help $Term.man_format $Term.choice_names $topic $setup_log))
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
       The most commonly used commands are:\n\
      \    clone       %s\n\
      \    fetch       %s\n\
      \    pull        %s\n\
      \    push        %s\n\
      \    graph       %s\n\
       \n\
       See 'ogit help <command>' for more information on a specific command.\n%!"
      clone.doc fetch.doc pull.doc push.doc graph.doc in
  Term.(pure usage $ setup_log),
  Term.info "ogit"
    ~version:"%%VERSION%%"
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map command [
    cat;
    cat_file;
    ls_remote;
    ls_files;
    ls_tree;
    read_tree;
    clone;
    fetch;
    pull;
    graph;
    push;
    help;
  ]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
