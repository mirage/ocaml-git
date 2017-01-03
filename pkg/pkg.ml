#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

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
    opam "opam";
    opam "git-http.opam";
    opam "git-unix.opam";
    opam "git-mirage.opam";
    opam "git-tests.opam";
  ]

let () =
  Pkg.describe ~opams ~metas "git" @@ fun c ->
  match Conf.pkg_name c with
  | "git" ->
    Ok [
      Pkg.lib "pkg/META";
      Pkg.lib "opam";
      Pkg.mllib ~api:["Git"] "src/git.mllib";
      Pkg.mllib "src/top/git-top.mllib";
    ]
  | "git-unix" ->
    Ok [
      Pkg.lib "pkg/META.unix" ~dst:"META";
      Pkg.lib "git-unix.opam" ~dst:"opam";
      Pkg.mllib "src/unix/git-unix.mllib";
      Pkg.bin "src/unix/ogit" ~dst:"ogit";
    ]
  | "git-mirage" ->
    Ok [
      Pkg.lib "pkg/META.mirage" ~dst:"META";
      Pkg.lib "git-mirage.opam" ~dst:"opam";
      Pkg.mllib "src/mirage/git-mirage.mllib";
    ]
  | "git-http"   ->
    Ok [
      Pkg.lib "pkg/META.http" ~dst:"META";
      Pkg.lib "git-http.opam" ~dst:"opam";
      Pkg.mllib "src/http/git-http.mllib";
    ]
  | "git-tests" ->
    Ok [
      Pkg.test ~dir:"_build" "test/test" ~args:(Cmd.v "-q");
    ]
  | other -> R.error_msgf "unknown package name: %s" other
