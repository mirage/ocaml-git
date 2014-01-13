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

open Lwt
open Core_kernel.Std
open Cmdliner

open GitTypes
module Remote = GitRemote.Make(GitUnix)

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
}

let app_global g =
  Log.color_on ();
  if g.verbose then
    Log.set_log_level Log.DEBUG

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be more verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  Term.(pure (fun verbose -> { verbose }) $ verbose)

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

let repository =
  let doc = Arg.info ~docv:"REPOSITORY" ~doc:"Location of the remote repository." [] in
  Arg.(required & pos 0 (some string) None & doc)

let directory =
  let doc = Arg.info ~docv:"DIRECTORY" ~doc:"The name of the directory to clone into." [] in
  Arg.(value & pos 1 (some string) None & doc)

let backend =
  let memory = mk_flag ["m";"in-memory"] "Use an in-memory store." in
  let create = function
    | true  -> (module GitMemory: S)
    | false -> (module GitLocal: S)
  in
  Term.(pure create $ memory)

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Exn.to_string e); exit 1)
  )

(* CAT *)
let cat = {
  name = "cat-file";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
    let file =
      let doc = Arg.info ~docv:"FILE"  ~doc:"The name of the file to show the contents." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let cat_file file =
      run begin
        GitUnix.mstruct_of_file file >>= fun buf ->
        let v = Git.input buf in
        Printf.printf "%s%!" (Git.pretty v);
        return_unit
      end in
    Term.(mk cat_file $ file)
}

(* LS *)
let ls = {
  name = "ls-remote";
  doc  = "List references in a remote repository.";
  man  = [];
  term =
    let ls (module S: S) repo =
      let module Remote = Remote(S) in
      run begin
        S.create ()  >>= fun t ->
        Remote.ls t repo >>= fun references ->
        Printf.printf "From %s\n" repo;
        let print (sha1, ref) =
          Printf.printf "%s        %s\n"
            (SHA1.to_hex sha1)
            (Reference.to_string ref) in
        List.iter ~f:print references;
        return_unit
      end in
    Term.(mk ls $ backend $ repository)
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
    let clone (module S: S) deepen bare repo dir =
      let dir = match dir with
        | Some d -> d
        | None   ->
          let dir = Filename.basename repo in
          if Filename.check_suffix dir ".git" then
            Filename.chop_extension dir
          else
            dir
      in
      if Sys.file_exists dir && Array.length (Sys.readdir dir) > 0 then (
        eprintf "fatal: destination path '%s' already exists and is not an empty directory.\n" dir;
        exit 128
      );
      let module Remote = Remote(S) in
      run begin
        S.create ~root:dir ()   >>= fun t ->
        printf "Cloning into '%s' ...\n%!" (Filename.basename (S.root t));
        Remote.clone t ?deepen repo >>= fun _ ->
        return_unit
      end in
    Term.(mk clone $ backend $ depth $ bare $ repository $ directory)
}

(* FETCH *)
let fetch = {
  name = "fetch";
  doc  = "Fetch a remote Git repository.";
  man  = [];
  term =
    let fetch (module S: S) repo =
      let module Remote = Remote(S) in
      run begin
        S.create ()     >>= fun t ->
        Remote.fetch t repo >>= fun _ ->
        return_unit
      end in
    Term.(mk fetch $ backend $ repository)
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
    let graph (module S: S) file =
      let module GitGraph = GitGraph.Make(S) in
      run begin
        S.create () >>= fun t ->
        GitGraph.to_dot t file
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
        let conv, _ = Arg.enum (List.rev_map ~f:(fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter ~f:print_endline cmds; `Ok ()
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
      \    graph       %s\n\
      \n\
      See 'ogit help <command>' for more information on a specific command.\n%!"
      clone.doc fetch.doc graph.doc in
  Term.(pure usage $ (pure ())),
  Term.info "ogit"
    ~version:"0.10.0"
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map ~f:command [
    cat;
    ls;
    clone;
    fetch;
    graph;
    help;
  ]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
