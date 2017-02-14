#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let includes = function
  | "git"        -> ["src"]
  | "git-mirage" -> ["src-mirage"]
  | "git-http"   -> ["src-http"]
  | "git-unix"   -> ["src-unix"]
  | x -> failwith ("Unknown includes for package: " ^ x)

let extra_deps c = match Conf.pkg_name c with
  | "git"        -> []
  | "git-mirage" -> ["git"; "git-http"]
  | "git-http"   -> ["git"]
  | "git-unix"   -> ["git"; "git-http"]
  | x -> failwith ("Unknown includes for package: " ^ x)

let build =
  let cmd c os =
    let includes = match includes (Conf.pkg_name c) with
      | [] -> Cmd.empty
      | is -> Cmd.(v "-Is" % String.concat "," is)
    in
    let extra_deps = match extra_deps c with
      | [] -> Cmd.empty
      | ed -> Cmd.(v "-package" % String.concat "," ed)
    in
    Cmd.(Pkg.build_cmd c os %% includes %% extra_deps)
  in
  let cmd c os files = OS.Cmd.run @@ Cmd.(cmd c os %% of_list files) in
  Pkg.build ~cmd ()

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.http";
  Pkg.meta_file ~install:false "pkg/META.unix";
  Pkg.meta_file ~install:false "pkg/META.mirage";
]

let opams =
  let opam name =
    Pkg.opam_file ~lint_deps_excluding:None ~install:false name
  in
  [
    opam "git.opam";
    opam "git-http.opam";
    opam "git-unix.opam";
    opam "git-mirage.opam";
  ]

let () =
  Pkg.describe ~build ~opams ~metas "git" @@ fun c ->
  match Conf.pkg_name c with
  | "git" ->
    Ok [
      Pkg.lib "pkg/META";
      Pkg.lib "git.opam" ~dst:"opam";
      Pkg.mllib ~api:["Git"] "src/git.mllib";
      Pkg.mllib "src-top/git-top.mllib";
    ]
  | "git-unix" ->
    Ok [
      Pkg.lib "pkg/META.unix" ~dst:"META";
      Pkg.lib "git-unix.opam" ~dst:"opam";
      Pkg.mllib "src-unix/git-unix.mllib";
      Pkg.bin "src-unix/ogit" ~dst:"ogit";
      Pkg.test ~dir:"_build" "test/test" ~args:(Cmd.v "-q");
    ]
  | "git-mirage" ->
    Ok [
      Pkg.lib "pkg/META.mirage" ~dst:"META";
      Pkg.lib "git-mirage.opam" ~dst:"opam";
      Pkg.mllib "src-mirage/git-mirage.mllib";
      Pkg.test ~dir:"_build" "test/test_mirage" ~args:(Cmd.v "-q");
    ]
  | "git-http"   ->
    Ok [
      Pkg.lib "pkg/META.http" ~dst:"META";
      Pkg.lib "git-http.opam" ~dst:"opam";
      Pkg.mllib "src-http/git-http.mllib";
    ]
  | other -> R.error_msgf "unknown package name: %s" other
