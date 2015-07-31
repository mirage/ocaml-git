(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Test_common
open Lwt.Infix
open Git
open Git_unix

type t = {
  name  : string;
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  store : (module Store.S);
  mirage: bool;
}

let unit () = Lwt.return_unit

let long_random_string () =
  let t  = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs;
  Cstruct.to_string (Nocrypto.Rng.generate 1024)

module Make (Store: Store.S) = struct

  module Common = Make(Store)
  open Common
  module Search = Search.Make(Store)

  let run x test =
    try Lwt_main.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_main.run (x.clean ());
      raise e

  let () = quiet ()

  let v0 = Value.blob (Blob.of_raw @@ long_random_string ())
  let kv0 = Value.sha1 v0

  let v1  = Value.blob (Blob.of_raw "hoho")
  let kv1 = Value.sha1 v1

  let v2 = Value.blob (Blob.of_raw "")
  let kv2 = Value.sha1 v2

  (* Create a node containing t1 -w-> v1 *)
  let w = "a\000bbb\047"
  let t0 = Value.tree ([
      { Tree.perm = `Normal;
        name = w;
        node = kv1 }
    ])
  let kt0 = Value.sha1 t0

  let t1 = Value.tree ([
      { Tree.perm = `Normal;
        name = "x";
        node = kv1 }
    ])
  let kt1 = Value.sha1 t1

  (* Create the tree t2 -b-> t1 -a-> v1 *)
  let t2 = Value.tree ([
      { Tree.perm = `Dir;
        name = "b";
        node = kt1 }
    ])
  let kt2 = Value.sha1 t2

  (* Create the tree t3 -a-> t2 -b-> t1 -a-> v1 *)
  let t3 = Value.tree ([
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt3 = Value.sha1 t3

  (* Create the tree t4 -a-> t2 -b-> t1 -a-> v1
                       \-c-> v2 *)
  let t4 = Value.tree ([
      { Tree.perm = `Exec;
        name = "c";
        node = kv2; };
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt4 = Value.sha1 t4

  let t5 = Value.tree ([
      { Tree.perm = `Normal;
        name = long_random_string ();
        node = kv2; };
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt5 = Value.sha1 t5

  let john_doe = {
    User.name  = "John Doe";
    email = "jon@doe.com";
    date  = 0L, None;
  }

  (* c1 : t2 *)
  let c1 = Value.commit {
      Commit.tree = SHA.to_tree kt2;
      parents     = [];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!";
    }
  let kc1 = Value.sha1 c1

  (* c1 -> c2 : t4 *)
  let c2 = Value.commit {
      Commit.tree = SHA.to_tree kt4;
      parents     = [SHA.to_commit kc1];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!"
    }
  let kc2 = Value.sha1 c2

  let c3 =
    let c2 = match c2 with Value.Commit x -> x | _ -> assert false in
    Value.commit { c2 with Commit.tree = SHA.to_tree kt5 }
  let kc3 = Value.sha1 c3

  (* tag1: c1 *)
  let tag1 = Value.tag {
      Tag.sha1 = kc1;
      typ      = Object_type.Commit;
      tag      = "foo";
      tagger   = john_doe;
      message  = "Ho yeah!";
    }
  let ktag1 = Value.sha1 tag1

  (* tag2: c2 *)
  let tag2 = Value.tag {
      Tag.sha1 = kc2;
      typ      = Object_type.Commit;
      tag      = "bar";
      tagger   = john_doe;
      message  = "Hahah!";
    }
  let ktag2 = Value.sha1 tag2

  (* r1: t4 *)
  let r1 = Reference.of_raw "refs/origin/head"

  (* r2: c2 *)
  let r2 = Reference.of_raw "refs/upstream/head"

  let () = verbose ()

  let check_write t name k v =
    Store.write t v    >>= fun k' ->
    assert_key_equal (name ^ "-key-1") k k';
    Store.read_exn t k >>= fun v' ->
    assert_value_equal name v v';
    Store.write t v'   >>= fun k'' ->
    assert_key_equal (name ^ "-key-2") k k'';
    Lwt.return_unit

  let check_find t name k path e =
    Search.find t k path >>= fun k' ->
    assert_key_opt_equal (name ^ "-find") (Some e) k';
    Lwt.return_unit

  let root = "test-db"

  let create ?(index=false) () =
    Store.create ~root () >>= fun t  ->
    Lwt_list.iter_p
      (fun v -> Store.write t v >>= fun _ -> Lwt.return_unit)
      (if not index then [
          v0; v1; v2;
          t0; t1; t2; t3; t4;
          c1; c2; c3;
        ] else [
         v1; v2;
         t1; t2; t4;
         c1; c2;
       ])
    >>= fun () ->
    Lwt.return t

  let is_ typ t k =
    Store.read t k >>= function
    | None   -> Lwt.return false
    | Some v ->
      Lwt.return (typ = Value.type_of v)

  let check_keys t name typ expected =
    Store.list t                     >>= fun ks ->
    Lwt_list.filter_p (is_ typ t) ks >>= fun ks ->
    Lwt.return (assert_keys_equal name expected ks)

  let test_blobs x () =
    let test () =
      create ()                 >>= fun t     ->
      check_write t "v1" kv1 v1 >>= fun () ->
      check_write t "v2" kv2 v2 >>= fun () ->

      check_keys t "blobs" Object_type.Blob [kv0; kv1; kv2] >>= fun () ->
      Lwt.return_unit
    in
    run x test

  let test_trees x () =
    let test () =
      create ()                 >>= fun t  ->
      check_write t "t1" kt1 t1 >>= fun () ->
      check_write t "t2" kt2 t2 >>= fun () ->
      check_write t "t3" kt3 t3 >>= fun () ->
      check_write t "t4" kt4 t4 >>= fun () ->

      check_find t "kt0:w"     kt0 [w]           kv1 >>= fun () ->
      check_find t "kt1:w"     kt1 ["x"]         kv1 >>= fun () ->
      check_find t "kt2:b"     kt2 ["b"]         kt1 >>= fun () ->
      check_find t "kt2:b/x"   kt2 ["b";"x"]     kv1 >>= fun () ->
      check_find t "kt3:a"     kt3 ["a"]         kt2 >>= fun () ->
      check_find t "kt3:a/b"   kt3 ["a";"b"]     kt1 >>= fun () ->
      check_find t "kt3:a/b/x" kt3 ["a";"b";"x"] kv1 >>= fun () ->
      check_find t "kt4:c"     kt4 ["c"]         kv2 >>= fun () ->

      check_keys t "trees" Object_type.Tree [kt0; kt1; kt2; kt3; kt4] >>=
      fun () ->

      Lwt.return_unit
    in
    run x test

  let test_commits x () =
    let c =
      let tree =
        Git.SHA.Tree.of_hex "3aadeb4d06f2a149e06350e4dab2c7eff117addc"
      in
      let parents = [] in
      let author = {
        Git.User.name="Thomas Gazagnaire"; email="thomas@gazagnaire.org";
        date= (1435873834L, Some { Git.User.sign = `Plus; hours = 1; min = 0 })}
      in
      let message = "Initial commit" in
      Git.Value.Commit
        { Git.Commit.tree; parents; author; committer = author; message }
    in
    let test () =
      let buf = Git.Misc.with_buffer (fun buf -> Git.Value.add_inflated buf c) in
      let buf = Mstruct.of_string buf in
      let c' = Git.Value.input_inflated buf in
      assert_value_equal "commits: convert" c c';

      create ()                 >>= fun t   ->
      check_write t "c1" kc1 c1 >>= fun () ->
      check_write t "c2" kc2 c2 >>= fun () ->

      check_find t "c1:b"     kc1 ["";"b"]          kt1 >>= fun () ->
      check_find t "c1:b/x"   kc1 ["";"b"; "x"]     kv1 >>= fun () ->
      check_find t "c2:a/b/x" kc2 ["";"a";"b"; "x"] kv1 >>= fun () ->
      check_find t "c2:c"     kc2 ["";"c"]          kv2 >>= fun () ->

      check_keys t "commits" Object_type.Commit [kc1; kc2; kc3] >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_tags x () =
    let test () =
      create () >>= fun t   ->
      check_write t "tag1" ktag1 tag1 >>= fun () ->
      check_write t "tag2" ktag2 tag2 >>= fun () ->

      check_find t "tag1:b" ktag1 ["foo";"";"b"] kt1 >>= fun () ->
      check_find t "tag2:a" ktag2 ["bar";"";"a"] kt2 >>= fun () ->
      check_find t "tag2:c" ktag2 ["bar";"";"c"] kv2 >>= fun () ->

      check_keys t "tags" Object_type.Tag [ktag1; ktag2] >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_refs x () =
    let test () =
      create () >>= fun t ->
      let c = SHA.to_commit in
      let ko = function
        | None   -> None
        | Some x -> Some (SHA.of_commit x) in
      Store.write_reference t r1 (c kt4) >>= fun ()   ->
      Store.read_reference t r1          >>= fun kt4' ->
      assert_key_opt_equal "r1" (Some kt4) (ko kt4');

      Store.write_reference t r2 (c kc2) >>= fun ()   ->
      Store.read_reference t r2          >>= fun kc2' ->
      assert_key_opt_equal "r2" (Some kc2) (ko kc2');

      Store.references t                 >>= fun rs   ->
      assert_refs_equal "refs" [r1; r2] rs;

      let commit =
        Git.Reference.SHA (
          SHA.Commit.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a"
        ) in
      Store.write_head t ( commit) >>= fun () ->
      Store.read_head t >>= fun head ->
      let () = match head with
        | None   -> Alcotest.fail "no head"
        | Some h -> Alcotest.(check head_contents) "head" commit h
      in
      Lwt.return_unit
    in
    run x test

  let test_index x () =
    let test () =
      let test_fs () =
        (* scan the local filesystem *)
        if x.name = "FS" then (
          if Filename.basename (Sys.getcwd ()) <> "lib_test" then
            failwith "Tests should run in lib_test/";
          files "." >>= fun files ->
          FS.create ~root () >>= fun t ->
          Lwt_list.map_s (fun file ->
              Git_unix.FS.IO.read_file file >>= fun str ->
              let blob = Blob.of_raw (Cstruct.to_string str) in
              let sha1 = SHA.to_blob (Value.sha1 (Value.Blob blob)) in
              let mode =
                let p = (Unix.stat file).Unix.st_perm in
                if p land 0o100 = 0o100 then `Exec else `Normal
              in
              FS.entry_of_file t Index.empty file mode sha1 blob >>= fun f ->
              Lwt.return f
            ) files >>= fun entries ->
          let entries = Misc.list_filter_map (fun x -> x) entries in
          let cache = Index.create entries in
          let buf = Misc.with_buffer' (fun buf -> Index.add buf cache) in
          let cache2 = Index.input (Mstruct.of_cstruct buf) in
          assert_index_equal "index" cache cache2;
          Lwt.return_unit
        ) else
          Lwt.return_unit
      in
      test_fs () >>= fun () ->

      (* test random entries *)
      create ~index:true () >>= fun t ->
      Store.write_index t (SHA.to_commit kc2) >>= fun () ->
      Store.read_index t >>= fun _ ->
      Lwt.return_unit
    in
    run x test

  let test_packs x () =
    if x.name = "FS" then
      let test () =
        files "data/" >>= fun files ->
        if files = [] then
          failwith "Please run that test in lib_test/";
        let files = List.filter (fun file ->
            match Misc.string_chop_suffix file ~suffix:".pack" with
            | None   -> false
            | Some _ -> true
          ) files in
        let files = List.map (fun file ->
            match Misc.string_chop_prefix file ~prefix:"data/pack-" with
            | None      -> failwith ("chop prefix " ^ file)
            | Some name ->
              match Misc.string_chop_suffix name ~suffix:".pack" with
              | None      -> failwith ("chop suffix " ^ name)
              | Some name -> file, "data/pack-" ^ name ^ ".idx"
          ) files in
        let read_file file =
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            Cstruct.of_bigarray ba
        in
        Lwt_list.iter_s (fun (pack, index) ->

            (* basic serialization of pack files *)
            let pstr1 = read_file pack in
            let read _ = failwith "shallow pack" in
            Pack.Raw.input (Mstruct.of_cstruct pstr1) ~read >>= fun rp1 ->
            let i3 = Pack.Raw.index rp1 in

            (* basic serialization of index files *)
            begin if Sys.file_exists index then
                let istr1 = read_file index in
                let i1    = Pack_index.Raw.input (Mstruct.of_cstruct istr1) in
                let istr2 = Misc.with_buffer' (fun buf -> Pack_index.Raw.add buf i1) in
                let i2    = Pack_index.Raw.input (Mstruct.of_cstruct istr2) in
                assert_pack_index_equal "pack-index" i1 i2;
                assert_pack_index_equal "raw-pack-->>--pack-index" i1 i3;
                Lwt.return_unit
              else
                Lwt.return_unit
            end >>= fun () ->

            let pstr2 = Pack.Raw.buffer rp1 in
            Pack.Raw.input (Mstruct.of_cstruct pstr2) ~read >>= fun rp2 ->
            Pack.of_raw rp1 >>= fun pic1 ->
            Pack.of_raw rp2 >>= fun pic2 ->
            assert_pack_equal "pack" pic1 pic2;

            Lwt.return_unit

          ) files
      in
      run x test

  module Sync = Sync.Make(Store)

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

  let head_contents =
    let module M = struct
      type t = Git.Reference.head_contents
      let equal = Git.Reference.equal_head_contents
      let pp = Git.Reference.pp_head_contents
    end
    in (module M: Alcotest.TESTABLE with type t = M.t)

  let test_clones x () =
    let test () =
      Store.create ~root () >>= fun t  ->
      let clone ?depth ?(bare=true) gri head =
        x.init () >>= fun () ->
        Sync.clone t ?head ?deepen:depth gri >>= fun _ ->
        if Store.kind = `Disk && not x.mirage then (
          let cmd = Printf.sprintf "cd %s && git fsck" @@ Store.root t in
          Alcotest.(check int) "fsck" 0 (Sys.command cmd)
        );
        let master = Git.Reference.of_raw "refs/heads/master" in
        let e = match head with
          | None   -> Git.Reference.Ref master
          | Some h -> h
        in
        Store.read_head t >>= function
        | None   -> Alcotest.fail "empty clone!"
        | Some h ->
          Alcotest.(check head_contents) "correct head contents" e h;
          if not bare then
            Store.read_reference_exn t master >>= fun h ->
            Store.write_index t h
          else
            Lwt.return_unit
      in
      let git = Gri.of_string "git://github.com/mirage/ocaml-git.git" in
      let https = Gri.of_string "https://github.com/mirage/ocaml-git.git" in
      let large = Gri.of_string "https://github.com/ocaml/opam-repository.git" in
      let gh_pages =
        Some (Git.Reference.(Ref (of_raw "refs/heads/gh-pages")))
      in
      let commit =
        let h = SHA.Commit.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a" in
        Some (Git.Reference.SHA h)
      in
      clone git   None     >>= fun () ->
      clone https None     >>= fun () ->
      clone git   gh_pages >>= fun () ->
      clone https gh_pages >>= fun () ->
      clone https ~depth:1 commit >>= fun () ->
      clone git   ~depth:3 commit >>= fun () ->
      clone large ~bare:false None >>= fun () ->

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
        Git_unix.FS.IO.read_file file >>= fun r ->
        Alcotest.(check string) "concurrent read/write"
          (Cstruct.to_string payload) (Cstruct.to_string r);
        read (i-1)
    in
    write 1
    >>= fun () -> Lwt.join [ write 500; read 1000; write 1000; read 500; ]
  end

module Array = struct

  let create len =
    let mk _i = long_random_string () |> Git.SHA.of_string in
    let a = Array.init len mk in
    Array.sort Git.SHA.compare a;
    let hsize = Git.SHA.(hex_length zero) / 2 in
    let b = Cstruct.create (len * hsize) in
    for i = 0 to len-1 do
      let raw = Git.SHA.to_raw a.(i) in
      Cstruct.blit_from_string raw 0 b (i * hsize) hsize
    done;
    a, b

  let test_get () =
    let a, b = create 127 in
    for i = 0 to Array.length a - 1 do
      let msg = Printf.sprintf "get%d" i in
      Alcotest.(check sha1) msg a.(i) (Git.SHA.Array.get b i);
    done

  let test_lenght () =
    for _i = 0 to 10 do
      let len = 10 + Random.int 1024 in
      let _, b = create len in
      let msg = Printf.sprintf "len%d" len in
      Alcotest.(check int) msg len (Git.SHA.Array.length b)
    done

  let test_linear_search () =
    let a, b = create 127 in
    for i = 0 to 126 do
      let j = Git.SHA.Array.linear_search b a.(i) in
      let msg = Printf.sprintf "linear-seach %d" i in
      Alcotest.(check (option int)) msg (Some i) j
    done

  let test_binary_search () =
    let a, b = create 127 in
    for i = 0 to 126 do
      let j = Git.SHA.Array.binary_search b a.(i) in
      let msg = Printf.sprintf "binary-seach %d" i in
      Alcotest.(check (option int)) msg (Some i) j
    done

end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Operations on blobs"       , speed, T.test_blobs    x;
    "Operations on trees"       , speed, T.test_trees    x;
    "Operations on commits"     , speed, T.test_commits  x;
    "Operations on tags"        , speed, T.test_tags     x;
    "Operations on references"  , speed, T.test_refs     x;
    "Operations on index"       , speed, T.test_index    x;
    "Operations on pack files"  , speed, T.test_packs    x;
    "Resource leaks"            , `Slow, T.test_leaks    x;
    "Basic Remote operations"   , `Slow, T.test_basic_remote x;
    "Cloning remote repos"      , `Slow, T.test_clones   x;
  ]

let generic = [
  "OPS"       , ["Concurrent read/writes", `Quick, test_read_writes];
  "SHA1-ARRAY", [
    ("get"          , `Quick, Array.test_get);
    ("length"       , `Quick, Array.test_lenght);
    ("linear search", `Quick, Array.test_linear_search);
    ("binary search", `Quick, Array.test_binary_search);
  ]
]

let run name tl =
  verbose ();
  Alcotest.run name (generic @ List.map suite tl)
