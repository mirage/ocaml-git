#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let http = Conf.with_pkg "http"
let unix = Conf.with_pkg "unix"
let mirage = Conf.with_pkg "mirage"

let opam = Pkg.opam_file ~lint_deps_excluding:None "opam"

let () =
  Pkg.describe ~opams:[opam] "git" @@ fun c ->
  let http = Conf.value c http in
  let unix = Conf.value c unix in
  let mirage = Conf.value c mirage in
  let test = unix && http && mirage in
  Ok [
    Pkg.mllib ~api:["Git"] "lib/git.mllib";
    Pkg.mllib "lib/top/git-top.mllib";
    Pkg.mllib ~cond:mirage "lib/mirage/git-mirage.mllib";
    Pkg.mllib ~cond:http   "lib/http/git-http.mllib";
    Pkg.mllib ~cond:unix   "lib/unix/git-unix.mllib";
    Pkg.bin   ~cond:unix   "bin/ogit" ~dst:"ogit";
    Pkg.test  ~cond:test ~dir:"lib_test" "lib_test/test" ~args:(Cmd.v "-q");
  ]
