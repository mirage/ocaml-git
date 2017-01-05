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

open Test_common
open Lwt.Infix
open Git
open Astring

(* From Git_misc *)

let list_filter_map f l =
  List.fold_left (fun l elt ->
      match f elt with
      | None   -> l
      | Some x -> x :: l
    ) [] l
  |> List.rev

let with_buffer fn =
  let buf = Buffer.create 1024 in
  fn buf;
  Buffer.contents buf

let with_buffer' fn =
  Cstruct.of_string (with_buffer fn)

(*****************)

type t = {
  name  : string;
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  store : (module Store.S);
  shell : bool;
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

  module Value_IO = Value.IO(Store.Digest)(Store.Inflate)
  module Hash_IO = Hash.IO(Store.Digest)
  module Index_IO = Index.IO(Store.Digest)
  module Pack_IO = Pack.IO(Store.Digest)(Store.Inflate)
  module Pack_index = Pack_index.Make(Store.Digest)

  let run x test =
    try Lwt_main.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_main.run (x.clean ());
      raise e

  let () = quiet ()

  let v0 = Value.blob (Blob.of_raw @@ long_random_string ())
  let kv0 = Value_IO.name v0

  let v1  = Value.blob (Blob.of_raw "hoho")
  let kv1 = Value_IO.name v1

  let v2 = Value.blob (Blob.of_raw "")
  let kv2 = Value_IO.name v2

  (* Create a node containing t1 -w-> v1 *)
  let w = "a\000bbb\047"
  let t0 = Value.tree ([
      { Tree.perm = `Normal;
        name = w;
        node = kv1 }
    ])
  let kt0 = Value_IO.name t0

  let t1 = Value.tree ([
      { Tree.perm = `Normal;
        name = "x";
        node = kv1 }
    ])
  let kt1 = Value_IO.name t1

  (* Create the tree t2 -b-> t1 -x-> v1 *)
  let t2 = Value.tree ([
      { Tree.perm = `Dir;
        name = "b";
        node = kt1 }
    ])
  let kt2 = Value_IO.name t2

  (* Create the tree t3 -a-> t2 -b-> t1 -x-> v1 *)
  let t3 = Value.tree ([
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt3 = Value_IO.name t3

  (* Create the tree t4 -a-> t2 -b-> t1 -x-> v1
                       \-c-> v2 *)
  let t4 = Value.tree ([
      { Tree.perm = `Exec;
        name = "c";
        node = kv2; };
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt4 = Value_IO.name t4

  let t5 = Value.tree ([
      { Tree.perm = `Normal;
        name = long_random_string ();
        node = kv2; };
      { Tree.perm = `Dir;
        name = "a";
        node = kt2; }
    ])
  let kt5 = Value_IO.name t5

  let john_doe = {
    User.name  = "John Doe";
    email = "jon@doe.com";
    date  = 0L, None;
  }

  (* c1 : t2 *)
  let c1 = Value.commit {
      Commit.tree = Hash.to_tree kt2;
      parents     = [];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!";
    }
  let kc1 = Value_IO.name c1

  (* c1 -> c2 : t4 *)
  let c2 = Value.commit {
      Commit.tree = Hash.to_tree kt4;
      parents     = [Hash.to_commit kc1];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!"
    }
  let kc2 = Value_IO.name c2

  let c3 =
    let c2 = match c2 with Value.Commit x -> x | _ -> assert false in
    Value.commit { c2 with Commit.tree = Hash.to_tree kt5 }
  let kc3 = Value_IO.name c3

  (* tag1: c1 *)
  let tag1 = Value.tag {
      Tag.obj  = kc1;
      typ      = Object_type.Commit;
      tag      = "foo";
      tagger   = john_doe;
      message  = "Ho yeah!";
    }
  let ktag1 = Value_IO.name tag1

  (* tag2: c2 *)
  let tag2 = Value.tag {
      Tag.obj  = kc2;
      typ      = Object_type.Commit;
      tag      = "bar";
      tagger   = john_doe;
      message  = "Hahah!";
    }
  let ktag2 = Value_IO.name tag2

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

      let p x = `Path x in
      check_find t "kt0:w"     kt0 (p [w])           kv1 >>= fun () ->
      check_find t "kt1:w"     kt1 (p ["x"])         kv1 >>= fun () ->
      check_find t "kt2:b"     kt2 (p ["b"])         kt1 >>= fun () ->
      check_find t "kt2:b/x"   kt2 (p ["b";"x"])     kv1 >>= fun () ->
      check_find t "kt3:a"     kt3 (p ["a"])         kt2 >>= fun () ->
      check_find t "kt3:a/b"   kt3 (p ["a";"b"])     kt1 >>= fun () ->
      check_find t "kt3:a/b/x" kt3 (p ["a";"b";"x"]) kv1 >>= fun () ->
      check_find t "kt4:c"     kt4 (p ["c"])         kv2 >>= fun () ->

      check_keys t "trees" Object_type.Tree [kt0; kt1; kt2; kt3; kt4] >>=
      fun () ->

      Lwt.return_unit
    in
    run x test

  let test_commits x () =
    let c =
      let tree =
        Hash_IO.Tree.of_hex "3aadeb4d06f2a149e06350e4dab2c7eff117addc"
      in
      let parents = [] in
      let author = {
        User.name="Thomas Gazagnaire"; email="thomas@gazagnaire.org";
        date= (1435873834L, Some { User.sign = `Plus; hours = 1; min = 0 })}
      in
      let message = "Initial commit" in
      Value.Commit { Commit.tree; parents; author; committer = author; message }
    in
    let test () =
      let buf = with_buffer (fun buf -> Value_IO.add_inflated buf c) in
      let buf = Mstruct.of_string buf in
      let c' = Value_IO.input_inflated buf in
      assert_value_equal "commits: convert" c c';

      create ()                 >>= fun t   ->
      check_write t "c1" kc1 c1 >>= fun () ->
      check_write t "c2" kc2 c2 >>= fun () ->

      let p x = `Commit (`Path x) in
      check_find t "c1:b"     kc1 (p ["b"])          kt1 >>= fun () ->
      check_find t "c1:b/x"   kc1 (p ["b"; "x"])     kv1 >>= fun () ->
      check_find t "c2:a/b/x" kc2 (p ["a";"b"; "x"]) kv1 >>= fun () ->
      check_find t "c2:c"     kc2 (p ["c"])          kv2 >>= fun () ->

      check_keys t "commits" Object_type.Commit [kc1; kc2; kc3] >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_tags x () =
    let test () =
      create () >>= fun t   ->
      check_write t "tag1" ktag1 tag1 >>= fun () ->
      check_write t "tag2" ktag2 tag2 >>= fun () ->

      let p l x = `Tag (l, `Commit (`Path x)) in
      check_find t "tag1:b" ktag1 (p "foo" ["b"]) kt1 >>= fun () ->
      check_find t "tag2:a" ktag2 (p "bar" ["a"]) kt2 >>= fun () ->
      check_find t "tag2:c" ktag2 (p "bar" ["c"]) kv2 >>= fun () ->

      check_keys t "tags" Object_type.Tag [ktag1; ktag2] >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_refs x () =
    let test () =
      create () >>= fun t ->
      Store.write_reference t r1 kt4 >>= fun ()   ->
      Store.read_reference t r1      >>= fun kt4' ->
      assert_key_opt_equal "r1" (Some kt4) kt4';

      Store.write_reference t r2 kc2 >>= fun ()   ->
      Store.read_reference t r2      >>= fun kc2' ->
      assert_key_opt_equal "r2" (Some kc2) kc2';

      Store.references t                 >>= fun rs   ->
      assert_refs_equal "refs" [r1; r2] rs;

      let commit =
        Reference.Hash (
          Hash_IO.Commit.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a"
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

  let test_search x () =
    let test () =
      create () >>= fun t ->
      let check k path v =
        Search.find t k path >>= fun v' ->
        Alcotest.(check (option sha1)) "search" (Some v) v';
        Lwt.return_unit
      in
      check kt4 (`Path ["a";"b";"x"]) kv1 >>= fun () ->
      check kc2 (`Commit (`Path ["a";"b";"x"])) kv1 >>= fun () ->
      check kc2 (`Commit (`Path ["a"])) kt2 >>= fun () ->
      Lwt.return_unit

    in
    run x test

  let test_index x () =
    let test () =
      let test_fs () =
        (* scan the local filesystem *)
        if x.name = "FS" then (
          if Filename.basename (Sys.getcwd ()) <> "_build" then
            failwith "Tests should run in _build/";
          files "." >>= fun files ->
          Git_unix.FS.create ~root () >>= fun t ->
          Lwt_list.map_s (fun file ->
              Git_unix.FS.IO.read_file file >>= fun str ->
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
          assert_index_equal "index" cache cache2;
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

  let test_packs x () =
    if x.name = "FS" then
      let test () =
        files "../test/data/" >>= fun files ->
        if files = [] then
          failwith "Please run that test in _build/";
        let files = List.filter (fun file ->
            String.is_suffix file ~affix:".pack"
          ) files in
        let files = List.map (fun file ->
            let name =
              String.with_range file ~first:(String.length "data/pack-")
            in
            let name =
              String.with_range name
                ~len:(String.length name - String.length ".pack")
            in
            file, "data/pack-" ^ name ^ ".idx"
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
            Pack_IO.Raw.input (Mstruct.of_cstruct pstr1) ~read >>= fun rp1 ->
            let i3 = Pack.Raw.index rp1 in

            (* basic serialization of index files *)
            begin if Sys.file_exists index then
                let istr1 = read_file index in
                let i1    = Pack_index.Raw.input (Mstruct.of_cstruct istr1) in
                let istr2 =
                  with_buffer' (fun buf -> Pack_index.Raw.add buf i1)
                in
                let i2    = Pack_index.Raw.input (Mstruct.of_cstruct istr2) in
                assert_pack_index_equal "pack-index" i1 i2;
                assert_pack_index_equal "raw-pack-->>--pack-index" i1 i3;
                Lwt.return_unit
              else
                Lwt.return_unit
            end >>= fun () ->

            let pstr2 = Pack.Raw.buffer rp1 in
            Pack_IO.Raw.input (Mstruct.of_cstruct pstr2) ~read >>= fun rp2 ->
            Pack_IO.of_raw rp1 >>= fun pic1 ->
            Pack_IO.of_raw rp2 >>= fun pic2 ->
            assert_pack_equal "pack" pic1 pic2;

            Lwt.return_unit

          ) files
      in
      run x test

  module Sync = Git_unix.Sync.Make(Store)

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

module Test_array (D: Hash.DIGEST) = struct

  module Hash_IO = Hash.IO(D)
  module Hash_Array = Hash.Array(D)

  let create len =
    let mk _i = long_random_string () |> D.string in
    let a = Array.init len mk in
    Array.sort Hash.compare a;
    let hsize = Hash.(hex_length Hash_IO.zero) / 2 in
    let b = Cstruct.create (len * hsize) in
    for i = 0 to len-1 do
      let raw = Hash.to_raw a.(i) in
      Cstruct.blit_from_string raw 0 b (i * hsize) hsize
    done;
    a, b

  let test_get () =
    let a, b = create 127 in
    for i = 0 to Array.length a - 1 do
      let msg = Printf.sprintf "get%d" i in
      Alcotest.(check sha1) msg a.(i) (Hash_Array.get b i);
    done

  let test_lenght () =
    for _i = 0 to 10 do
      let len = 10 + Random.int 1024 in
      let _, b = create len in
      let msg = Printf.sprintf "len%d" len in
      Alcotest.(check int) msg len (Hash_Array.length b)
    done

  let test_linear_search () =
    let a, b = create 127 in
    for i = 0 to 126 do
      let j = Hash_Array.linear_search b a.(i) in
      let msg = Printf.sprintf "linear-seach %d" i in
      Alcotest.(check (option int)) msg (Some i) j
    done

  let test_binary_search () =
    let a, b = create 127 in
    for i = 0 to 126 do
      let j = Hash_Array.binary_search b a.(i) in
      let msg = Printf.sprintf "binary-seach %d" i in
      Alcotest.(check (option int)) msg (Some i) j
    done

  let test_to_list () =
    let a, b = create 127 in
    let c = Hash_Array.to_list b in
    Alcotest.(check (list sha1)) "to_list" (Array.to_list a) c

end

let array (module D: Hash.DIGEST) =
  let module Array = Test_array(D) in [
    ("get"          , `Quick, Array.test_get);
    ("length"       , `Quick, Array.test_lenght);
    ("linear search", `Quick, Array.test_linear_search);
    ("binary search", `Quick, Array.test_binary_search);
    ("to_list"      , `Quick, Array.test_to_list);
  ]

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

let generic = [
  "OPS"        , ["Concurrent read/writes", `Quick, test_read_writes];
  "SHA1-unix"  , array (module Git_unix.SHA1);
  "SHA1-mirage", array (module Git_mirage.SHA1_slow);
  "SHA256-unix", array (module Git_unix.SHA256);
]

let run name tl =
  verbose ();
  Alcotest.run name (generic @ List.map suite tl)
