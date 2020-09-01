let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (Random.int 256))
  done;
  Bytes.unsafe_to_string res

let is_not_refname = function
  | ' ' | '~' | '^' | ':' | '?' | '*' | '|' -> true
  | chr -> if Char.code chr < 32 || Char.code chr > 126 then true else false

let random_reference () =
  let res = Bytes.create 16 in
  let idx = ref 0 in
  while !idx < 16 do
    let chr = Char.chr (Random.int 256) in
    if not (is_not_refname chr) then (
      Bytes.set res !idx chr;
      incr idx )
  done;
  Git.Reference.v (Bytes.unsafe_to_string res)

module Make (Digestif : Digestif.S) (Store : Git.S with type hash = Digestif.t) =
struct
  let hash = Alcotest.testable Digestif.pp Digestif.equal
  let value = Alcotest.testable Store.Value.pp Store.Value.equal
  let v0 = Git.Blob.of_string (random_string 0x1000)
  let v1 = Git.Blob.of_string "hoho"
  let v2 = Git.Blob.of_string ""

  let tree lst =
    let lst =
      List.map
        (fun (name, perm, node) -> Store.Value.Tree.entry ~name perm node)
        lst
    in
    Store.Value.Tree.of_list lst

  let name0 = "a\042bbb\047"
  let t0 = tree [ name0, `Normal, Store.Value.Blob.digest v1 ]
  let t1 = tree [ "x", `Normal, Store.Value.Blob.digest v1 ]
  let t2 = tree [ "b", `Dir, Store.Value.Tree.digest t1 ]
  let t3 = tree [ "a", `Dir, Store.Value.Tree.digest t2 ]

  let t4 =
    tree
      [
        "c", `Exec, Store.Value.Blob.digest v2;
        "a", `Dir, Store.Value.Tree.digest t2;
      ]

  let replace_zero = function '\000' -> '\042' | chr -> chr

  let t5 =
    tree
      [
        ( Astring.String.map replace_zero (random_string 256),
          `Normal,
          Store.Value.Blob.digest v2 ); "a", `Dir, Store.Value.Tree.digest t2;
      ]

  let john_doe =
    {
      Git.User.name = "John Doe";
      email = "john@doe.org";
      date = Random.int64 Int64.max_int, None;
    }

  let c1 =
    Store.Value.Commit.make
      ~tree:(Store.Value.Tree.digest t2)
      ~author:john_doe ~committer:john_doe "hello r1"

  let c2 =
    Store.Value.Commit.make
      ~tree:(Store.Value.Tree.digest t4)
      ~parents:[ Store.Value.Commit.digest c1 ]
      ~author:john_doe ~committer:john_doe "hello r1!"

  let c3 =
    Store.Value.Commit.make
      ~tree:(Store.Value.Tree.digest t5)
      ~parents:(Store.Value.Commit.parents c2)
      ~author:(Store.Value.Commit.author c2)
      ~committer:(Store.Value.Commit.committer c2)
      (Store.Value.Commit.message c2)

  let thomas =
    {
      Git.User.name = "Thomas Gazagnaire";
      email = "thomas@gazagnaire.org";
      date = Random.int64 Int64.max_int, None;
    }

  let c4 =
    Store.Value.Commit.make
      ~tree:(Store.Value.Tree.digest t5)
      ~parents:[ Store.Value.Commit.digest c3 ]
      ~author:thomas ~committer:thomas
      ~extra:[ "simple-key", [ "simple value" ] ]
      "with extra-fields"

  let gpg =
    [
      "-----BEGIN PGP SIGNATURE-----";
      "wsBcBAABCAAQBQJaV7TOCRBK7hj4Ov3rIwAAdHIIABBfj02NsDB4x2KU1uMSs8l+";
      "kTF7a7onxdgoSvWzckXmM2o+uzBtBdnHzK24Sr2uJXq+WQvuVP35io32Qc72TdmX";
      "0r8TUt6eXnqu1mlXnTNiCZZady8tL3SiWXsTwx6AFNk59bH59cQy/dF5K0RKaT+W";
      "RPCv03yBx9vEAbTVe4kj1jS+FAcYHTyd+zqKio8kjLgL1KyjIO7GRsjRW1q+VLIX";
      "ZffaDvLU6hRdHhxxsZ6tA9sLWgfHv0Z+tgpafQrAJkwZc/zRpITA4U54xxEvrKaP";
      "BwpFgFK4IlgPC7h1ZxJMJyOL6R+dXpFTtY0vK7Apat886p+nbUJho/8Pn5OuVb8=";
      "=DCpq"; "-----END PGP SIGNATURE-----";
    ]

  let c5 =
    Store.Value.Commit.make
      ~tree:(Store.Value.Tree.digest t5)
      ~parents:[ Store.Value.Commit.digest c3 ]
      ~author:thomas ~committer:thomas ~extra:[ "gpgsig", gpg ] "with GPG"

  let tt1 =
    Store.Value.Tag.make
      (Store.Value.Commit.digest c1)
      Git.Tag.Commit ~tag:"foo" ~tagger:john_doe "Ho yeah!"

  let tt2 =
    Store.Value.Tag.make
      (Store.Value.Commit.digest c2)
      Git.Tag.Commit ~tag:"bar" ~tagger:john_doe "Haha!"

  let r1 = Git.Reference.v "refs/origin/head"
  let r2 = Git.Reference.v "refs/upstream/head"

  let ( >>? ) x f =
    let open Lwt.Infix in
    x >>= function
    | Ok x -> f x
    | Error err ->
        Fmt.kstrf (fun err -> Lwt.fail (Failure err)) "%a" Store.pp_error err

  let check_write store name k v =
    let open Lwt.Infix in
    Store.write store v >>? fun (k', _) ->
    Alcotest.(check hash) (Fmt.strf "set %s" name) k k';
    Store.read_exn store k' >>= fun v' ->
    Alcotest.(check value) (Fmt.strf "get %s" name) v v';
    Store.write store v >>? fun (k'', _) ->
    Alcotest.(check hash) (Fmt.strf "set %s" name) k' k'';
    Lwt.return_unit

  module Search = Git.Search.Make (Digestif) (Store)

  let check_find store name k p r =
    let open Lwt.Infix in
    Search.find store k p >>= fun k' ->
    Alcotest.(check (option hash)) name r k';
    Lwt.return_unit

  let test_blobs =
    Alcotest_lwt.test_case "blobs" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_write store "v0" (Store.Value.Blob.digest v0) (Store.Value.blob v0)
    >>= fun () ->
    check_write store "v1" (Store.Value.Blob.digest v1) (Store.Value.blob v1)
    >>= fun () ->
    check_write store "v2" (Store.Value.Blob.digest v2) (Store.Value.blob v2)
    >>= fun () ->
    Alcotest.(check pass) "blobs" () ();
    Lwt.return_unit

  let test_trees =
    Alcotest_lwt.test_case "trees" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_write store "t0" (Store.Value.Tree.digest t0) (Store.Value.tree t0)
    >>= fun () ->
    check_write store "t1" (Store.Value.Tree.digest t1) (Store.Value.tree t1)
    >>= fun () ->
    check_write store "t2" (Store.Value.Tree.digest t2) (Store.Value.tree t2)
    >>= fun () ->
    check_write store "t3" (Store.Value.Tree.digest t3) (Store.Value.tree t3)
    >>= fun () ->
    check_write store "t4" (Store.Value.Tree.digest t4) (Store.Value.tree t4)
    >>= fun () ->
    check_write store "t5" (Store.Value.Tree.digest t5) (Store.Value.tree t5)
    >>= fun () ->
    Alcotest.(check pass) "trees" () ();
    Lwt.return_unit

  let test_commits =
    Alcotest_lwt.test_case "commits" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_write store "c1"
      (Store.Value.Commit.digest c1)
      (Store.Value.commit c1)
    >>= fun () ->
    check_write store "c2"
      (Store.Value.Commit.digest c2)
      (Store.Value.commit c2)
    >>= fun () ->
    check_write store "c3"
      (Store.Value.Commit.digest c3)
      (Store.Value.commit c3)
    >>= fun () ->
    check_write store "c4"
      (Store.Value.Commit.digest c4)
      (Store.Value.commit c4)
    >>= fun () ->
    check_write store "c5"
      (Store.Value.Commit.digest c5)
      (Store.Value.commit c5)
    >>= fun () ->
    Alcotest.(check pass) "commits" () ();
    Lwt.return_unit

  let test_tags =
    Alcotest_lwt.test_case "tags" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_write store "tt1" (Store.Value.Tag.digest tt1) (Store.Value.tag tt1)
    >>= fun () ->
    check_write store "tt2" (Store.Value.Tag.digest tt2) (Store.Value.tag tt2)
    >>= fun () ->
    Alcotest.(check pass) "tags" () ();
    Lwt.return_unit

  let test_find_trees =
    Alcotest_lwt.test_case "find trees" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_find store "t0:[name0]"
      (Store.Value.Tree.digest t0)
      (`Path [ name0 ])
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "t1:x"
      (Store.Value.Tree.digest t1)
      (`Path [ "x" ])
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "t2:b"
      (Store.Value.Tree.digest t2)
      (`Path [ "b" ])
      (Some (Store.Value.Tree.digest t1))
    >>= fun () ->
    check_find store "t2:b/x"
      (Store.Value.Tree.digest t2)
      (`Path [ "b"; "x" ])
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "t3:a"
      (Store.Value.Tree.digest t3)
      (`Path [ "a" ])
      (Some (Store.Value.Tree.digest t2))
    >>= fun () ->
    check_find store "t3:a/b"
      (Store.Value.Tree.digest t3)
      (`Path [ "a"; "b" ])
      (Some (Store.Value.Tree.digest t1))
    >>= fun () ->
    check_find store "t3:a/b/x"
      (Store.Value.Tree.digest t3)
      (`Path [ "a"; "b"; "x" ])
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "t4:c"
      (Store.Value.Tree.digest t4)
      (`Path [ "c" ])
      (Some (Store.Value.Blob.digest v2))
    >>= fun () -> Lwt.return_unit

  let test_find_commits =
    Alcotest_lwt.test_case "find commits" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_find store "c1:b"
      (Store.Value.Commit.digest c1)
      (`Commit (`Path [ "b" ]))
      (Some (Store.Value.Tree.digest t1))
    >>= fun () ->
    check_find store "c1:b/x"
      (Store.Value.Commit.digest c1)
      (`Commit (`Path [ "b"; "x" ]))
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "c2:a/b/x"
      (Store.Value.Commit.digest c2)
      (`Commit (`Path [ "a"; "b"; "x" ]))
      (Some (Store.Value.Blob.digest v1))
    >>= fun () ->
    check_find store "c2:c"
      (Store.Value.Commit.digest c2)
      (`Commit (`Path [ "c" ]))
      (Some (Store.Value.Blob.digest v2))
    >>= fun () -> Lwt.return_unit

  let test_find_tags =
    Alcotest_lwt.test_case "find tags" `Quick @@ fun _sw store ->
    let open Lwt.Infix in
    check_find store "tt1:b"
      (Store.Value.Tag.digest tt1)
      (`Tag ("foo", `Commit (`Path [ "b" ])))
      (Some (Store.Value.Tree.digest t1))
    >>= fun () ->
    check_find store "tt2:a"
      (Store.Value.Tag.digest tt2)
      (`Tag ("bar", `Commit (`Path [ "a" ])))
      (Some (Store.Value.Tree.digest t2))
    >>= fun () ->
    check_find store "tt2:a"
      (Store.Value.Tag.digest tt2)
      (`Tag ("bar", `Commit (`Path [ "c" ])))
      (Some (Store.Value.Blob.digest v2))
    >>= fun () -> Lwt.return_unit

  let test_references =
    Alcotest_lwt.test_case "references" `Quick @@ fun _sw store ->
    Store.Ref.write store r1 (Git.Reference.Uid (Store.Value.Commit.digest c1))
    >>? fun () ->
    Store.Ref.resolve store r1 >>? fun k ->
    Alcotest.(check hash) "r1" k (Store.Value.Commit.digest c1);
    Store.Ref.write store r2 (Git.Reference.Uid (Store.Value.Commit.digest c2))
    >>? fun () ->
    Store.Ref.resolve store r2 >>? fun k ->
    Alcotest.(check hash) "r2" k (Store.Value.Commit.digest c2);
    Lwt.return_unit

  let _err_cycle = "Got a reference cycle"

  let test_cycle =
    let open Lwt.Infix in
    Alcotest_lwt.test_case "cycle" `Quick @@ fun _sw store ->
    let refa = Git.Reference.v "a" in
    let refb = Git.Reference.v "b" in
    Store.Ref.write store refa (Git.Reference.Ref refb) >>? fun () ->
    Store.Ref.write store refb (Git.Reference.Ref refa) >>? fun () ->
    Store.Ref.resolve store refa >>= function
    | Ok _ -> Alcotest.failf "Unexpected ok value"
    | Error err ->
        let err = Fmt.strf "%a" Store.pp_error err in
        Alcotest.(check string) "cycle" err _err_cycle;
        Lwt.return_unit

  let random_hashes len =
    Array.init len (fun _ ->
        let str = random_string Store.Hash.length in
        Store.Hash.of_raw_string str)

  let test_atomic =
    let open Lwt.Infix in
    Alcotest_lwt.test_case "atomic write" `Quick @@ fun _sw store ->
    let reference = random_reference () in
    let hashes = random_hashes 500 in
    let atomic_wr hash =
      Store.Ref.write store reference (Git.Reference.Uid hash) >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> Alcotest.failf "%a" Store.pp_error err
    in
    let atomic_rd _ =
      Store.Ref.resolve store reference >>= function
      | Ok hash ->
          let res = Array.exists (Store.Hash.equal hash) hashes in
          Alcotest.(check bool) "hash exists" res true;
          Lwt.return_unit
      | Error err -> Alcotest.failf "%a" Store.pp_error err
    in
    Store.Ref.write store reference (Git.Reference.Uid hashes.(0)) >>? fun () ->
    let lst0 = Array.to_list hashes in
    let lst1 = Array.to_list hashes in
    Lwt.both (Lwt_list.iter_p atomic_wr lst0) (Lwt_list.iter_p atomic_rd lst1)
    >>= fun ((), ()) ->
    Store.Ref.resolve store reference >>? fun hash ->
    let res = Array.exists (Store.Hash.equal hash) hashes in
    Alcotest.(check bool) "hash exists" res true;
    Lwt.return_unit

  let check_search store k path v =
    let open Lwt.Infix in
    Search.find store k path >>= fun v' ->
    Alcotest.(check (option hash)) "search" (Some v) v';
    Lwt.return_ok ()

  let test_search =
    Alcotest_lwt.test_case "search" `Quick @@ fun _sw store ->
    check_search store
      (Store.Value.Tree.digest t4)
      (`Path [ "a"; "b"; "x" ])
      (Store.Value.Blob.digest v1)
    >>? fun () ->
    check_search store
      (Store.Value.Commit.digest c2)
      (`Commit (`Path [ "a"; "b"; "x" ]))
      (Store.Value.Blob.digest v1)
    >>? fun () ->
    check_search store
      (Store.Value.Commit.digest c2)
      (`Commit (`Path [ "a" ]))
      (Store.Value.Tree.digest t2)
    >>? fun () -> Lwt.return_unit

  let test store =
    Alcotest_lwt.run_with_args ~and_exit:false "store" store
      [
        "write", [ test_blobs; test_trees; test_commits; test_tags ];
        "find", [ test_find_trees; test_find_commits; test_find_tags ];
        "reference", [ test_references; test_cycle; test_atomic ];
        "search", [ test_search ];
      ]
end
