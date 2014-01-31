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

open OUnit
open Test_common
open Lwt
open GitTypes

type t = {
  name : string;
  init : unit -> unit Lwt.t;
  clean: unit -> unit Lwt.t;
  store: (module S);
}

let unit () =
  return_unit

module Make (S: S) = struct

  module Common = Make(S)
  open Common
  open S

  let run x test =
    try Lwt_unix.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_unix.run (x.clean ());
      raise e

  let long_random_string = Cryptokit.(Random.string (Random.device_rng "/dev/urandom") 1024)
  let v1 = blob (Blob.of_string long_random_string)
  let kv1 = key v1

  let v2 = blob (Blob.of_string "")
  let kv2 = key v2

  (* Create a node containing t1 -a-> v1 *)
  let t1 = tree (Tree.create [
      { Tree.perm = `normal;
        name = "a";
        node = kv1 }
    ])
  let kt1 = key t1

  (* Create the tree t2 -b-> t1 -a-> v1 *)
  let t2 = tree (Tree.create [
      { Tree.perm = `dir;
        name = "b";
        node = kt1 }
    ])
  let kt2 = key t2

  (* Create the tree t3 -a-> t2 -b-> t1 -a-> v1 *)
  let t3 = tree (Tree.create [
      { Tree.perm = `dir;
        name = "a";
        node = kt2; }
    ])
  let kt3 = key t3

  (* Create the tree t4 -a-> t2 -b-> t1 -a-> v1
                       \-c-> v2 *)
  let t4 = tree (Tree.create [
      { Tree.perm = `exec;
        name = "c";
        node = kv2; };
      { Tree.perm = `dir;
        name = "a";
        node = kt2; }
    ])
  let kt4 = key t4

  let john_doe = User.({
      name  = "John Doe";
      email = "jon@doe.com";
      date  = "today";
    })

  (* c1 : t2 *)
  let c1 = commit {
      Commit.tree = SHA1.to_tree kt2;
      parents     = [];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!";
    }
  let kc1 = key c1

  (* c1 -> c2 : t4 *)
  let c2 = commit {
      Commit.tree = SHA1.to_tree kt4;
      parents     = [SHA1.to_commit kc1];
      author      = john_doe;
      committer   = john_doe;
      message     = "hello r1!"
    }
  let kc2 = key c2

  (* tag1: c1 *)
  let tag1 = tag {
      Tag.sha1 = kc1;
      typ      = `Commit;
      tag      = "foo";
      tagger   = john_doe;
      message  = "Ho yeah!";
    }
  let ktag1 = key tag1

  (* tag2: c2 *)
  let tag2 = tag {
      Tag.sha1 = kc2;
      typ      = `Commit;
      tag      = "bar";
      tagger   = john_doe;
      message  = "Hahah!";
    }
  let ktag2 = key tag2

  (* r1: t4 *)
  let r1 = Reference.of_string "refs/origin/head"

  (* r2: c2 *)
  let r2 = Reference.of_string "refs/upstream/head"

  let check_write t name k v =
    write t v    >>= fun k' ->
    assert_key_equal (name ^ "-key-1") k k';
    read_exn t k >>= fun v' ->
    assert_value_equal name v v';
    write t v'   >>= fun k''->
    assert_key_equal (name ^ "-key-2") k k'';
    return_unit

  let check_find t name k path e =
    Git.find ~succ:(succ t) k path >>= fun k' ->
    assert_key_opt_equal (name ^ "-find") (Some e) k';
    return_unit

  let create () =
    create ~root:"test-db" () >>= fun t ->
    Lwt_list.iter_p
      (fun v -> write t v >>= fun _ -> return_unit)
      [
        v1; v2;
        t1; t2; t3; t4;
        c1; c2
      ] >>= fun () ->
    return t

  let is_ typ t k =
    type_of t k >>= function
    | Some x -> return (x = typ)
    | None    -> return false

  let check_keys t name typ expected =
    list t                           >>= fun ks ->
    Lwt_list.filter_p (is_ typ t) ks >>= fun ks ->
    return (assert_keys_equal name expected ks)

  let test_blobs x () =
    let test () =
      create ()                 >>= fun t     ->
      check_write t "v1" kv1 v1 >>= fun () ->
      check_write t "v2" kv2 v2 >>= fun () ->

      check_keys  t "blobs" `Blob [kv1; kv2] >>= fun () ->
      return_unit
    in
    run x test

  let test_trees x () =
    let test () =
      create ()                 >>= fun t  ->
      check_write t "t1" kt1 t1 >>= fun () ->
      check_write t "t2" kt2 t2 >>= fun () ->
      check_write t "t3" kt3 t3 >>= fun () ->
      check_write t "t4" kt4 t4 >>= fun () ->

      check_find t "kt1:a"     kt1 ["a"]         kv1 >>= fun () ->
      check_find t "kt2:b"     kt2 ["b"]         kt1 >>= fun () ->
      check_find t "kt2:b/a"   kt2 ["b";"a"]     kv1 >>= fun () ->
      check_find t "kt3:a"     kt3 ["a"]         kt2 >>= fun () ->
      check_find t "kt3:a/b"   kt3 ["a";"b"]     kt1 >>= fun () ->
      check_find t "kt3:a/b/a" kt3 ["a";"b";"a"] kv1 >>= fun () ->
      check_find t "kt4:c"     kt4 ["c"]         kv2 >>= fun () ->

      check_keys t "trees" `Tree [kt1; kt2; kt3; kt4] >>= fun () ->

      return_unit
    in
    run x test

  let test_commits x () =
    let test () =
      create ()                 >>= fun t   ->
      check_write t "c1" kc1 c1 >>= fun () ->
      check_write t "c2" kc2 c2 >>= fun () ->

      check_find t "c1:b"     kc1 ["";"b"]         kt1 >>= fun () ->
      check_find t "c1:b/a"   kc1 ["";"b";"a"]     kv1 >>= fun () ->
      check_find t "c2:a/b/a" kc2 ["";"a";"b";"a"] kv1 >>= fun () ->
      check_find t "c2:c"     kc2 ["";"c"]         kv2 >>= fun () ->

      check_keys t "commits" `Commit [kc1; kc2] >>= fun () ->

      return_unit
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

      check_keys t "tags" `Tag [ktag1; ktag2] >>= fun () ->

      return_unit
    in
    run x test

  let test_refs x () =
    let test () =
      create () >>= fun t ->
      write_reference t r1 kt4 >>= fun ()   ->
      read_reference t r1      >>= fun kt4' ->
      assert_key_opt_equal "r1" (Some kt4) kt4';

      write_reference t r2 kc2 >>= fun ()   ->
      read_reference t r2      >>= fun kc2' ->
      assert_key_opt_equal "r2" (Some kc2) kc2';

      references t             >>= fun rs   ->
      assert_refs_equal "refs" [r1; r2] rs;

      return_unit
    in
    run x test

(*
  let test_sync x () =
    let test () =
      create ()              >>= fun t1 ->
      update t1 ["a";"b"] v1 >>= fun () ->
      snapshot t1            >>= fun r1 ->
      update t1 ["a";"c"] v2 >>= fun () ->
      snapshot t1            >>= fun r2 ->
      update t1 ["a";"d"] v1 >>= fun () ->
      snapshot t1            >>= fun r3 ->
      output t1 "full"       >>= fun () ->
      export t1 [r3]         >>= fun partial ->
      export t1 []           >>= fun full    ->

      (* Restart a fresh store and import everything in there. *)
      x.clean ()             >>= fun () ->
      x.init ()              >>= fun () ->
      create ~root:"test-db2" () >>= fun t2 ->

      import t2 partial      >>= fun () ->
      revert t2 r3           >>= fun () ->
      output t2 "partial"    >>= fun () ->

      mem t2 ["a";"b"]       >>= fun b1 ->
      assert_bool_equal "mem-ab" true b1;

      mem t2 ["a";"c"]       >>= fun b2 ->
      assert_bool_equal "mem-ac" true b2;

      mem t2 ["a";"d"]       >>= fun b3  ->
      assert_bool_equal "mem-ad" true b3;
      read_exn t2 ["a";"d"]  >>= fun v1' ->
      assert_value_equal "v1" v1' v1;

      catch
        (fun () ->
           revert t2 r2      >>= fun () ->
           OUnit.assert_bool "revert" false;
           return_unit)
        (fun e ->
           import t2 full    >>= fun () ->
           revert t2 r2      >>= fun () ->
           mem t2 ["a";"d"]  >>= fun b4 ->
           assert_bool_equal "mem-ab" false b4;
           return_unit
        ) in
    run x test
*)
end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Basic operations on blobs"       , speed, T.test_blobs    x;
    "Basic operations on trees"       , speed, T.test_trees    x;
    "Basic operations on commits"     , speed, T.test_commits  x;
    "Basic operations on tags"        , speed, T.test_tags     x;
    "Basic operations on references"  , speed, T.test_refs     x;
(*
    "High-level store synchronisation", speed, T.test_sync     x;
*)
  ]

let run name tl =
  let tl = List.map suite tl in
  Alcotest.run name tl
