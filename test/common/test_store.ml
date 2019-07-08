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

let () = Printexc.record_backtrace true
let ( >>?= ) = Lwt_result.( >>= )
let ( >|?= ) = Lwt_result.( >|= )

open Test_common
open Lwt.Infix
open Git

let random_cstruct len =
  let t = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs ;
  Nocrypto.Rng.generate len

let long_random_cstruct () =
  random_cstruct 1024

let long_random_string () = Cstruct.to_string (long_random_cstruct ())

module type S = sig
  include Git.S

  val v : Fpath.t -> (t, error) result Lwt.t
end

module Make (Store : S) = struct
  module Common = Make (Store)
  open Common
  module Search = Search.Make (Store)

  let reset t =
    Store.reset t
    >|= function
    | Ok () -> ()
    | Error e -> Alcotest.failf "reset failed: %a" Store.pp_error e

  let check_err = function
    | Ok x -> Lwt.return x
    | Error e -> Alcotest.failf "error: %a" Store.pp_error e

  let run test = Lwt_main.run (test ())
  let ( !! ) = Lazy.force

  let v0 =
    lazy
      (Store.Value.blob (Store.Value.Blob.of_cstruct (long_random_cstruct ())))

  let kv0 = lazy (Store.Value.digest !!v0)
  let v1 = lazy (Store.Value.blob (Store.Value.Blob.of_string "hoho"))
  let kv1 = lazy (Store.Value.digest !!v1)
  let v2 = lazy (Store.Value.blob (Store.Value.Blob.of_string ""))
  let kv2 = lazy (Store.Value.digest !!v2)

  (* Create a node containing t1 -w-> v1 *)
  let w = "a\042bbb\047"

  let tree l =
    Store.Value.tree
    @@ Store.Value.Tree.of_list
    @@ List.map
         (fun (name, perm, node) -> Store.Value.Tree.entry name perm node)
         l

  let t0 = lazy (tree [w, `Normal, !!kv1])
  let kt0 = lazy (Store.Value.digest !!t0)
  let t1 = lazy (tree ["x", `Normal, !!kv1])
  let kt1 = lazy (Store.Value.digest !!t1)

  (* Create the tree t2 -b-> t1 -x-> v1 *)
  let t2 = lazy (tree ["b", `Dir, !!kt1])
  let kt2 = lazy (Store.Value.digest !!t2)

  (* Create the tree t3 -a-> t2 -b-> t1 -x-> v1 *)
  let t3 = lazy (tree ["a", `Dir, !!kt2])
  let kt3 = lazy (Store.Value.digest !!t3)

  (* Create the tree t4 -a-> t2 -b-> t1 -x-> v1 \-c-> v2 *)
  let t4 = lazy (tree ["c", `Exec, !!kv2; "a", `Dir, !!kt2])
  let kt4 = lazy (Store.Value.digest !!t4)

  let t5 =
    lazy
      (tree
         [ ( Astring.String.map
               (function '\000' -> '\001' | chr -> chr)
               (long_random_string ())
           , (* XXX(dinosaure): impossible to store an entry with \000 *)
             `Normal
           , !!kv2 )
         ; "a", `Dir, !!kt2 ])

  let kt5 = lazy (Store.Value.digest !!t5)
  let john_doe = {User.name= "John Doe"; email= "jon@doe.com"; date= 0L, None}

  (* c1 : t2 *)
  let c1 =
    lazy
      (Store.Value.commit
         (Store.Value.Commit.make ~author:john_doe ~committer:john_doe
            ~tree:!!kt2 "hello r1"))

  let kc1 = lazy (Store.Value.digest !!c1)

  (* c1 -> c2 : t4 *)
  let c2 =
    lazy
      (Store.Value.commit
         (Store.Value.Commit.make ~author:john_doe ~committer:john_doe
            ~parents:[!!kc1] ~tree:!!kt4 "hello r1!"))

  let kc2 = lazy (Store.Value.digest !!c2)

  let c3 =
    lazy
      (let c2 =
         match !!c2 with Store.Value.Commit x -> x | _ -> assert false
       in
       Store.Value.(
         commit
           (Commit.make ~author:(Commit.author c2)
              ~committer:(Commit.committer c2) ~parents:(Commit.parents c2)
              ~tree:!!kt5 (Commit.message c2))))

  let kc3 = lazy (Store.Value.digest !!c3)

  let thomas =
    Git.User.
      { name= "Thomas Gazagnaire"
      ; email= "thomas.@gazagnaire.com"
      ; date=
          ( Int64.of_float @@ Unix.gettimeofday ()
          , Some {sign= `Plus; hours= 1; minutes= 1} ) }

  let c4 =
    lazy
      Store.Value.(
        commit
          (Commit.make ~author:thomas ~committer:thomas ~parents:[!!kc3]
             ~tree:!!kt5
             ~extra:["simple-key", ["simple value"]]
             "with extra-fields"))

  let kc4 = lazy (Store.Value.digest !!c4)

  let gpg =
    [ "-----BEGIN PGP SIGNATURE-----"
    ; "wsBcBAABCAAQBQJaV7TOCRBK7hj4Ov3rIwAAdHIIABBfj02NsDB4x2KU1uMSs8l+"
    ; "kTF7a7onxdgoSvWzckXmM2o+uzBtBdnHzK24Sr2uJXq+WQvuVP35io32Qc72TdmX"
    ; "0r8TUt6eXnqu1mlXnTNiCZZady8tL3SiWXsTwx6AFNk59bH59cQy/dF5K0RKaT+W"
    ; "RPCv03yBx9vEAbTVe4kj1jS+FAcYHTyd+zqKio8kjLgL1KyjIO7GRsjRW1q+VLIX"
    ; "ZffaDvLU6hRdHhxxsZ6tA9sLWgfHv0Z+tgpafQrAJkwZc/zRpITA4U54xxEvrKaP"
    ; "BwpFgFK4IlgPC7h1ZxJMJyOL6R+dXpFTtY0vK7Apat886p+nbUJho/8Pn5OuVb8="
    ; "=DCpq"; "-----END PGP SIGNATURE-----" ]

  let c5 =
    lazy
      Store.Value.(
        commit
          (Commit.make ~author:thomas ~committer:thomas ~parents:[!!kc3]
             ~tree:!!kt5 ~extra:["gpgsig", gpg] "with extra-fields"))

  let kc5 = lazy (Store.Value.digest !!c5)

  (* tag1: c1 *)
  let tag1 =
    lazy
      Store.Value.(
        tag (Tag.make !!kc1 Tag.Commit ~tag:"foo" ~tagger:john_doe "Ho yeah!"))

  let ktag1 = lazy (Store.Value.digest !!tag1)

  (* tag2: c2 *)
  let tag2 =
    lazy
      Store.Value.(
        tag (Tag.make !!kc2 Tag.Commit ~tag:"bar" ~tagger:john_doe "Haha!"))

  let ktag2 = lazy (Store.Value.digest !!tag2)

  (* r1: t4 *)
  let r1 = Store.Reference.of_string "refs/origin/head"

  (* r2: c2 *)
  let r2 = Store.Reference.of_string "refs/upstream/head"

  let check_write t name k v =
    Store.write t v
    >>= check_err
    >>= fun (k', _) ->
    assert_key_equal (name ^ "-key-1") k k' ;
    Store.read t k
    >>= check_err
    >>= fun v' ->
    assert_value_equal name v v' ;
    Store.write t v'
    >>= check_err
    >|= fun (k'', _) -> assert_key_equal (name ^ "-key-2") k k''

  let check_find t name k path e =
    Search.find t k path
    >|= fun k' -> assert_key_opt_equal (name ^ "-find") (Some e) k'

  let root = Fpath.v "test-git-store"

  let create ~root ?(index = false) () =
    Store.v root
    >>= check_err
    >>= fun t ->
    reset t
    >>= fun () ->
    Lwt_list.iter_s
      (fun v ->
        Store.write t v
        >|= function
        | Error e -> Alcotest.failf "create: %a" Store.pp_error e | Ok _ -> ()
        )
      ( if not index then
        [ !!v0; !!v1; !!v2; !!t0; !!t1; !!t2; !!t3; !!t4; !!c1; !!c2; !!c3; !!c4
        ; !!c5 ]
      else [!!v1; !!v2; !!t1; !!t2; !!t4; !!c1; !!c2] )
    >|= fun () -> t

  let is_ typ t k =
    Store.read t k
    >>= function
    | Error _ -> Lwt.return false
    | Ok v -> Lwt.return (typ = Store.Value.kind v)

  let check_keys t name typ expected =
    Store.list t
    >>= fun ks ->
    Lwt_list.filter_s (is_ typ t) ks
    >|= fun ks -> assert_keys_equal name expected ks

  let test_blobs () =
    let test () =
      create ~root ()
      >>= fun t ->
      check_write t "v1" !!kv1 !!v1
      >>= fun () ->
      check_write t "v2" !!kv2 !!v2
      >>= fun () -> check_keys t "blobs" `Blob [!!kv0; !!kv1; !!kv2]
    in
    run test

  let test_trees () =
    let test () =
      create ~root ()
      >>= fun t ->
      check_write t "t1" !!kt1 !!t1
      >>= fun () ->
      check_write t "t2" !!kt2 !!t2
      >>= fun () ->
      check_write t "t3" !!kt3 !!t3
      >>= fun () ->
      check_write t "t4" !!kt4 !!t4
      >>= fun () ->
      let p x = `Path x in
      check_find t "kt0:w" !!kt0 (p [w]) !!kv1
      >>= fun () ->
      check_find t "kt1:w" !!kt1 (p ["x"]) !!kv1
      >>= fun () ->
      check_find t "kt2:b" !!kt2 (p ["b"]) !!kt1
      >>= fun () ->
      check_find t "kt2:b/x" !!kt2 (p ["b"; "x"]) !!kv1
      >>= fun () ->
      check_find t "kt3:a" !!kt3 (p ["a"]) !!kt2
      >>= fun () ->
      check_find t "kt3:a/b" !!kt3 (p ["a"; "b"]) !!kt1
      >>= fun () ->
      check_find t "kt3:a/b/x" !!kt3 (p ["a"; "b"; "x"]) !!kv1
      >>= fun () ->
      check_find t "kt4:c" !!kt4 (p ["c"]) !!kv2
      >>= fun () ->
      check_keys t "trees" `Tree [!!kt0; !!kt1; !!kt2; !!kt3; !!kt4]
    in
    run test

  module ValueIO = Git.Value.Raw (Store.Hash) (Store.Inflate) (Store.Deflate)

  let head_contents =
    let open Store.Reference in
    Alcotest.testable pp_head_contents equal_head_contents

  let hash = Alcotest.testable Store.Hash.pp Store.Hash.equal

  let test_commits () =
    let c =
      let root =
        Store.Hash.of_hex "3aadeb4d06f2a149e06350e4dab2c7eff117addc"
      in
      let thomas =
        { User.name= "Thomas Gazagnaire"
        ; email= "thomas@gazagnaire.org"
        ; date= 1435873834L, Some {User.sign= `Plus; hours= 1; minutes= 0} }
      in
      let msg = "Initial commit" in
      Store.Value.Commit.make ~tree:root ~author:thomas ~committer:thomas msg
      |> Store.Value.commit
    in
    let test () =
      let raw = Cstruct.create 0x100 in
      let etmp = Cstruct.create 0x100 in
      match ValueIO.to_raw ~raw ~etmp c with
      | Error e -> Alcotest.failf "%a" ValueIO.EncoderRaw.pp_error e
      | Ok raw -> (
        match ValueIO.of_raw_with_header (Cstruct.of_string raw) with
        | Error err ->
            Alcotest.failf "decoder: %a" Git.Error.Decoder.pp_error err
        | Ok c' ->
            assert_value_equal "commits: convert" c c' ;
            create ~root ()
            >>= fun t ->
            check_write t "c1" !!kc1 !!c1
            >>= fun () ->
            check_write t "c2" !!kc2 !!c2
            >>= fun () ->
            check_write t "c4" !!kc4 !!c4
            >>= fun () ->
            check_write t "c5" !!kc5 !!c5
            >>= fun () ->
            let p x = `Commit (`Path x) in
            check_find t "c1:b" !!kc1 (p ["b"]) !!kt1
            >>= fun () ->
            check_find t "c1:b/x" !!kc1 (p ["b"; "x"]) !!kv1
            >>= fun () ->
            check_find t "c2:a/b/x" !!kc2 (p ["a"; "b"; "x"]) !!kv1
            >>= fun () ->
            check_find t "c2:c" !!kc2 (p ["c"]) !!kv2
            >>= fun () ->
            check_keys t "commits" `Commit [!!kc1; !!kc2; !!kc3; !!kc4; !!kc5]
        )
    in
    run test

  let test_tags () =
    let test () =
      create ~root ()
      >>= fun t ->
      check_write t "tag1" !!ktag1 !!tag1
      >>= fun () ->
      check_write t "tag2" !!ktag2 !!tag2
      >>= fun () ->
      let p l x = `Tag (l, `Commit (`Path x)) in
      check_find t "tag1:b" !!ktag1 (p "foo" ["b"]) !!kt1
      >>= fun () ->
      check_find t "tag2:a" !!ktag2 (p "bar" ["a"]) !!kt2
      >>= fun () ->
      check_find t "tag2:c" !!ktag2 (p "bar" ["c"]) !!kv2
      >>= fun () -> check_keys t "tags" `Tag [!!ktag1; !!ktag2]
    in
    run test

  let test_refs () =
    let test () =
      create ~root ()
      >>= fun t ->
      Store.Ref.write t r1 (Store.Reference.Hash !!kt4)
      >>= check_err
      >>= fun () ->
      Store.Ref.read t r1
      >>= check_err
      >>= fun kt4' ->
      assert_head_contents_equal "r1" (Store.Reference.Hash !!kt4) kt4' ;
      Store.Ref.write t r2 (Store.Reference.Hash !!kc2)
      >>= check_err
      >>= fun () ->
      Store.Ref.read t r2
      >>= check_err
      >>= fun kc2' ->
      assert_head_contents_equal "r2" (Store.Reference.Hash !!kc2) kc2' ;
      Store.Ref.list t
      >>= fun rs ->
      assert_refs_and_hashes_equal "refs" [r1, !!kt4; r2, !!kc2] rs ;
      let commit =
        Store.Hash.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a"
      in
      Store.Ref.write t Store.Reference.head (Store.Reference.Hash commit)
      >>= check_err
      >>= fun () ->
      Store.Ref.read t Store.Reference.head
      >>= check_err
      >|= fun value ->
      Alcotest.(check head_contents) "head" (Store.Reference.Hash commit) value
    in
    run test

  let random_hash () =
    Store.Hash.of_raw_string (Cstruct.to_string (random_cstruct Store.Hash.digest_size))

  let test_order_trees () =
    let lst =
      (* lexicographic order *)
      [ Store.Value.Tree.entry "foo.c" `Normal (random_hash ())
      ; Store.Value.Tree.entry "foo" `Dir (random_hash ())
      ; Store.Value.Tree.entry "foo1" `Exec (random_hash ()) ] in
    let equal_entry a b = a = b in (* XXX(dinosaure): lazy. *)
    let test () =
      let tree = Alcotest.testable Store.Value.Tree.pp Store.Value.Tree.equal in
      let entry = Alcotest.testable Store.Value.Tree.pp_entry equal_entry in
      let r0 = Store.Value.Tree.of_list lst in
      let r1 = Store.Value.Tree.to_list r0 in
      Alcotest.(check (list entry)) "of_list -> to_list" r1 lst ;
      let r2 = Store.Value.Tree.of_list [] (* empty *) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 2) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 0) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 1) in
      Alcotest.(check tree) "add" r2 r0 ;
      let r2 = Store.Value.Tree.of_list [] (* empty *) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 2) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 1) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 1) in
      let r2 = Store.Value.Tree.add r2 (List.nth lst 0) in
      Alcotest.(check tree) "add (doublon)" r2 r0 ;
      let empty = Store.Value.Tree.of_list [] in
      let r3 = List.fold_left Store.Value.Tree.add empty lst in
      let r4 = List.fold_left Store.Value.Tree.add empty (Store.Value.Tree.to_list r0) in
      Alcotest.(check tree) "add (fold)" r3 r0 ;
      Alcotest.(check tree) "add (fold)" r4 r0 ;
      Lwt.return () in
    run test

  let test_search () =
    let test () =
      create ~root ()
      >>= fun t ->
      let check k path v =
        Search.find t k path
        >|= fun v' -> Alcotest.(check (option hash)) "search" (Some v) v'
      in
      check !!kt4 (`Path ["a"; "b"; "x"]) !!kv1
      >>= fun () ->
      check !!kc2 (`Commit (`Path ["a"; "b"; "x"])) !!kv1
      >>= fun () -> check !!kc2 (`Commit (`Path ["a"])) !!kt2
    in
    run test
end

let suite name (module S : S) =
  let module T = Make (S) in
  ( name
  , [ "Operations on blobs", `Quick, T.test_blobs
    ; "Operations on trees", `Quick, T.test_trees
    ; "Operations on trees (order)", `Quick, T.test_order_trees
    ; "Operations on commits", `Quick, T.test_commits
    ; "Operations on tags", `Quick, T.test_tags
    ; "Operations on references", `Quick, T.test_refs
    ; "Search", `Quick, T.test_search ] )
