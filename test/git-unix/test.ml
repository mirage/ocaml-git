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

open Git
open Lwt.Infix
open Test_common

let root = "test-db"

module Make (Store: Store.S) = struct

  module Sync = Git_unix.Sync.Make(Store)
  module T = Test_store.Make(Store)
  include T

  let test_basic_remote x () =
    let test () =
      let gri = Gri.of_string "git://localhost/" in
      create () >>= fun t ->
      Store.read_head t >>= fun head ->
      Alcotest.(check (option head_contents)) "no head" None head;
      Sync.fetch t gri >>= fun _ ->
      Sync.push t gri ~branch:Reference.master >>= fun _ ->
      Lwt.return_unit
    in
    run x test

  let test_clone x () =
    let test () =
      Store.create ~root () >>= fun t ->
      let clone ?depth ?(bare=true) ?branch gri =
        x.init () >>= fun () ->
        Store.list t >>= fun l ->
        Alcotest.(check (list sha1))
          (Printf.sprintf "empty depth=%s branch=%s uri=%s"
             (match depth with None -> "<none>" | Some d -> string_of_int d)
             (match branch with
              | None   -> "<none>"
              | Some h -> Fmt.to_to_string Git.Sync.pp_want h)
             (Gri.to_string gri))
          l [];
        Sync.clone t ?deepen:depth ?branch ~checkout:(not bare) gri >>= fun _ ->
        if x.shell then (
          let cmd = Printf.sprintf "cd %s && git fsck" @@ Store.root t in
          Alcotest.(check int) "fsck" 0 (Sys.command cmd)
        );
        let master = Reference.master in
        let e = match branch with
          | None             -> Reference.Ref master
          | Some (`Ref b)    -> Reference.Ref b
          | Some (`Commit h) -> Reference.Hash h
        in
        Store.list t >>= fun sha1s ->
        Lwt_list.iter_s (fun sha1 ->
            Store.read_exn t sha1 >>= fun _v ->
            Lwt.return_unit
          ) sha1s
        >>= fun () ->
        Store.read_head t >>= function
        | None   -> Alcotest.fail "empty clone!"
        | Some h ->
          Alcotest.(check head_contents) "correct head contents" e h;
          Lwt.return_unit
      in
      let git = Gri.of_string "git://github.com/mirage/ocaml-git.git" in
      let https = Gri.of_string "https://github.com/mirage/ocaml-git.git" in
      (* let large = Gri.of_string "https://github.com/ocaml/opam-repository.git" in *)
      let gh_pages = `Ref (Reference.of_raw "refs/heads/gh-pages") in
      let commit =
        `Commit (Hash_IO.Commit.of_hex "f7a8f077e4d880db173f3f48a74d5a3fc9210b4e")
      in
      clone git   >>= fun () ->
      clone https >>= fun () ->
      clone git   ~branch:gh_pages >>= fun () ->
      clone https ~branch:gh_pages >>= fun () ->
      clone https ~depth:1 ~branch:commit   >>= fun () ->
      clone git   ~depth:3 ~branch:gh_pages >>= fun () ->
      (* clone large ~bare:false  >>= fun () -> *)

      Lwt.return_unit
    in
    run x test

  let test_fetch x () =
    let test_one gri c0 c1 ?(update=false) diff =
      x.init () >>= fun () ->
      Store.create ~root () >>= fun t ->
      Store.list t >>= fun l ->
      Alcotest.(check (list sha1)) "empty" [] l;
      let fetch gri (branch, commit) =
        let b = Reference.of_raw ("refs/heads/" ^ branch) in
        let c = Hash_IO.of_hex commit in
        Sync.clone t ~branch:(`Commit (Hash.to_commit c)) ~checkout:false gri
        >>= fun r ->
        Store.write_reference t b c >>= fun () ->
        if x.shell then (
          let cmd = Printf.sprintf "cd %s && git fsck" @@ Store.root t in
          Alcotest.(check int) "fsck" 0 (Sys.command cmd)
        );
        begin if update then (
            Store.read_exn t c >>= function
            | Value.Commit parent ->
              let parents = [Hash.to_commit c] in
              let c' = { parent with Commit.message = "foo"; parents } in
              Store.write t (Value.Commit c') >>= fun k ->
              Store.write_reference t b k
            | _ -> assert false
          ) else
            Lwt.return_unit
        end >|= fun () ->
        Git.Sync.Result.hashes r
      in
      fetch gri c0 >>= fun _ ->
      fetch gri c1 >>= fun r ->
      Alcotest.(check sha1s) "diff" diff r;
      Lwt.return_unit
    in
    let test () =
      let git   = Gri.of_string "git://github.com/mirage/irmin-rt.git" in
      let https = Gri.of_string "https://github.com/mirage/irmin-rt.git" in
      let c1 = "test-fetch-2", "64beec7402efc772363f4e0a7dfeb0ad2a667367" in
      let c0 = "test-fetch-1", "348199320dc33614bc5d101b1e0e22eaea25b36b" in
      let diff  =
        let x = Hash_IO.of_hex in
        Hash.Set.of_list [
          x (snd c1); (* commit *)
          x "1700d5cbd2ac8f5faf491a3c07c4562f9e43f016"; (* / *)
          x "2c6368156f499eaee397c5c2d698fabcb1b3114c"; (* overhead/ *)
          x "54e0af2928da25353fb2a9ecc87530202e940580"; (* overhead/overhead.ml *)
        ] in
      test_one git c0 c1 diff   >>= fun () ->
      test_one https c0 c1 diff >>= fun () ->
      test_one https ~update:true c0 c1 diff >>= fun () ->
      Lwt.return_unit
    in
    run x test

  let test_leaks x () =
    let runs =
      try int_of_string (Sys.getenv "TESTRUNS")
      with Not_found -> 10_000
    in
    let test () =
      create () >>= fun t ->
      let rec aux = function
        | 0 -> Lwt.return_unit
        | i ->
          check_write t "v1" kv1 v1 >>= fun () ->
          check_write t "v2" kv2 v2 >>= fun () ->
          aux (i-1)
      in
      aux runs
    in
    run x test

  let test_index x () =
    let test () =
      let test_fs () =
        (* scan the local filesystem *)
        if x.name = "FS" then (
          if Sys.file_exists ".git" then
            failwith "Tests should not run at the root of a Git repository";
          files "." >>= fun files ->
          Git_unix.FS.create ~root () >>= fun t ->
          Lwt_list.map_s (fun file ->
              Git_unix.FS.IO.read_file file >>= function
              | None     -> Alcotest.fail "empty read"
              | Some str ->
                let blob = Blob.of_raw (Cstruct.to_string str) in
                let sha1 = Hash.to_blob (Value_IO.name (Value.Blob blob)) in
                let mode =
                  let p = (Unix.stat file).Unix.st_perm in
                  if p land 0o100 = 0o100 then `Exec else `Normal
                in
                Git_unix.FS.entry_of_file t Index.empty file mode sha1 blob
            ) files >>= fun entries ->
          let entries = list_filter_map (fun x -> x) entries in
          let cache = Index.create entries in
          let buf = with_buffer' (fun buf -> Index_IO.add buf cache) in
          let cache2 = Index_IO.input (Mstruct.of_cstruct buf) in
          Common.assert_index_equal "index" cache cache2;
          Lwt.return_unit
        ) else
          Lwt.return_unit
      in
      test_fs () >>= fun () ->

      (* test random entries *)
      create ~index:true () >>= fun t ->
      Store.write_index t (Hash.to_commit kc2) >>= fun () ->
      Store.read_index t >>= fun _ ->
      Lwt.return_unit
    in
    run x test

end

let test_read_writes () =
  Lwt_main.run begin
    let file = "/tmp/test-git" in
    let payload = Cstruct.of_string "boo!" in
    let rec write = function
      | 0 -> Lwt.return_unit
      | i -> Git_unix.FS.IO.write_file file payload <&> write (i-1)
    in
    let rec read = function
      | 0 -> Lwt.return_unit
      | i ->
        Git_unix.FS.IO.read_file file >>= function
        | None   -> Alcotest.fail "no file"
        | Some r ->
          Alcotest.(check string) "concurrent read/write"
            (Cstruct.to_string payload) (Cstruct.to_string r);
          read (i-1)
    in
    write 1
    >>= fun () -> Lwt.join [ write 500; read 1000; write 1000; read 500; ]
  end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Operations on blobs"       , speed, T.test_blobs x;
    "Operations on trees"       , speed, T.test_trees x;
    "Operations on commits"     , speed, T.test_commits x;
    "Operations on tags"        , speed, T.test_tags x;
    "Operations on references"  , speed, T.test_refs x;
    "Operations on index"       , speed, T.test_index x;
    "Operations on pack files"  , speed, T.test_packs x;
    "Search"                    , speed, T.test_search x;
    "Resource leaks"            , `Slow, T.test_leaks x;
    "Basic Remote operations"   , `Slow, T.test_basic_remote x;
    "Fetching remote repos"     , `Slow, T.test_fetch x;
    "Cloning remote repos"      , `Slow, T.test_clone x;
  ]

let extra = [
  "OPS"        , ["Concurrent read/writes", `Quick, test_read_writes];
  "SHA1-unix"  , Test_store.array (module Git_unix.SHA1);
  "SHA256-unix", Test_store.array (module Git_unix.SHA256);
]

module Memory = Git.Mem.Make(Git_unix.SHA1)(Git_unix.Zlib)

let mem_init () =
  Git.Value.Cache.clear ();
  Memory.clear_all ();
  Lwt.return_unit

let mem_suite = {
  name  = "MEM";
  init  = mem_init;
  clean = unit;
  store = (module Memory);
  shell = false;
}

module FS = Git_unix.FS

let fs_init () =
  FS.clear ();
  FS.create ~root () >>= fun t ->
  FS.reset t

let fs_suite =
  {
    name  = "FS";
    init  = fs_init;
    clean = unit;
    store = (module FS);
    shell = true;
  }

let () =
  Test_store.run ~extra "git" [
    `Quick, mem_suite;
    `Quick, fs_suite;
  ]
