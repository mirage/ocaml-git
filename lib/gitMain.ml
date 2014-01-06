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

open Cmdliner

open GitTypes

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
]

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

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

(* CLONE *)
let clone_doc = "Clone a remote Git repository."
let clone =
  let doc = clone_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Clone a remote repository."
  ] in
  let depth =
    mk_opt ["depth"] "DEPTH"
      "Create a shallow clone with a history truncated to the specified number \
       of revisions. Refer to $(b,git clone --help) for more information."
      Arg.(some int) None in
  let bare =
    mk_flag ["bare"] "Do not expand the filesystem." in
  let address =
    arg_list "ADDRESS" "Address of the remote repository." Arg.string in
  let clone deepen bare = function
    | [a] ->
      let name = Filename.basename a in
      let name = Filename.chop_extension name in
      let t = GitLocal.create ~root:name () in
      run (GitRemote.clone_on_disk t ?deepen a);
      `Ok ()
    | _   -> `Error (true, "Too many address") in
  Term.(ret (pure clone $ depth $ bare $ address)),
  term_info "clone" ~doc ~man

(* GRAPH *)
let graph_doc = "Display a graph of the objects."
let graph =
  let doc = graph_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Output a graph on the objects in the current repository."
  ] in
  let file =
    mk_required ["o";"output"] "FILE" "Output file."
      Arg.(some string) None in
  let graph file =
    let t = GitLocal.create () in
    run (GitGraph.to_dot t file) in
  Term.(pure graph $ file),
  term_info "graph" ~doc ~man

(* HELP *)
let help =
  let doc = "Display help about Mirari and Mirari commands." in
  let man = [
    `S "DESCRIPTION";
     `P "Prints help about Mirari commands.";
     `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
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

  Term.(ret (pure help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default =
  let doc = "Mirage application builder" in
  let man = [
    `S "DESCRIPTION";
    `P "Cagit is a small tool to experiement with the pure-OCaml implementation \
        of the Git format and protocol. Very few options are available, and it  \
        is not expected that this number grows very much in a near future.";
    `P "Cagit is a prototype, so use it at your own risk (eg. it might corrupt \
        your data if you are particulary unlucky).";
    `P "Use either $(b,cagit <command> --help) or $(b,cagit help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: cagit [--version]\n\
      \              [--help]\n\
      \              <command> [<args>]\n\
      \n\
      The most commonly used cagit commands are:\n\
      \    clone       %s\n\
      \    graph       %s\n\
      \n\
      See 'cagit help <command>' for more information on a specific command.\n%!"
      clone_doc graph_doc in
  Term.(pure usage $ (pure ())),
  Term.info "cagit"
    ~version:"0.9.0"
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  clone;
  graph;
  help;
]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
