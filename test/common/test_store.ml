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

open Test_common
open Lwt.Infix
open Git

let long_random_cstruct () =
  let t  = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs;
  Nocrypto.Rng.generate 1024

let long_random_string () =
  Cstruct.to_string (long_random_cstruct ())

module Make
    (Store : Minimal.S
     with type Hash.Digest.buffer = Cstruct.t
      and type Hash.hex = string)
= struct
  module Common = Make(Store)

  open Common

  module Search = Search.Make(Store)

  exception Reset of [ `Store of Store.error | `Ref of Store.Ref.error ]

  let run _ test =
    Lwt_main.run (test () >>= fun t -> Store.reset t >>= function
      | Ok () -> Lwt.return ()
      | Error err -> Lwt.fail (Reset err))

  let v0  = Store.Value.blob (Store.Value.Blob.of_cstruct (long_random_cstruct ()))
  let kv0 = Store.Value.digest v0

  let v1  = Store.Value.blob (Store.Value.Blob.of_string "hoho")
  let kv1 = Store.Value.digest v1

  let v2  = Store.Value.blob (Store.Value.Blob.of_string "")
  let kv2 = Store.Value.digest v2

  (* Create a node containing t1 -w-> v1 *)
  let w = "a\042bbb\047"
  let t0 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Normal
           ; name = w
           ; node = kv1 } ])
  let kt0 = Store.Value.digest t0

  let t1 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Normal
           ; name = "x"
           ; node = kv1 } ])
  let kt1 = Store.Value.digest t1

  (* Create the tree t2 -b-> t1 -x-> v1 *)
  let t2 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Dir
           ; name = "b"
           ; node = kt1 } ])
  let kt2 = Store.Value.digest t2

  (* Create the tree t3 -a-> t2 -b-> t1 -x-> v1 *)
  let t3 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Dir
           ; name = "a"
           ; node = kt2 } ])
  let kt3 = Store.Value.digest t3

  (* Create the tree t4 -a-> t2 -b-> t1 -x-> v1
                       \-c-> v2 *)
  let t4 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Exec
           ; name = "c"
           ; node = kv2 }
         ; { Store.Value.Tree.perm = `Dir
           ; name = "a"
           ; node = kt2 } ])
  let kt4 = Store.Value.digest t4

  let t5 = Store.Value.tree
      (Store.Value.Tree.of_list
         [ { Store.Value.Tree.perm = `Normal
           ; name = long_random_string ()
           ; node = kv2 }
         ; { Store.Value.Tree.perm = `Dir
           ; name = "a"
           ; node = kt2 } ])
  let kt5 = Store.Value.digest t5

  let john_doe =
    { User.name  = "John Doe"
    ; email = "jon@doe.com"
    ; date  = 0L, None }

  (* c1 : t2 *)
  let c1 =
    Store.Value.commit
      (Store.Value.Commit.make
         ~author:john_doe
         ~committer:john_doe
         ~tree:kt2
         "hello r1")
  let kc1 = Store.Value.digest c1

  (* c1 -> c2 : t4 *)
  let c2 =
    Store.Value.commit
      (Store.Value.Commit.make
         ~author:john_doe
         ~committer:john_doe
         ~parents:[ kc1 ]
         ~tree:kt4
         "hello r1!")
  let kc2 = Store.Value.digest c2

  let c3 =
    let c2 = match c2 with Store.Value.Commit x -> x | _ -> assert false in
    Store.Value.(commit
                   (Commit.make
                      ~author:(Commit.author c2)
                      ~committer:(Commit.committer c2)
                      ~parents:(Commit.parents c2)
                      ~tree:kt5
                      (Commit.message c2)))
  let kc3 = Store.Value.digest c3

  (* tag1: c1 *)
  let tag1 =
    Store.Value.(tag
                   (Tag.make
                      kc1 Tag.Commit ~tag:"foo" ~tagger:john_doe "Ho yeah!"))
  let ktag1 = Store.Value.digest tag1

  (* tag2: c2 *)
  let tag2 =
    Store.Value.(tag
                   (Tag.make
                      kc2 Tag.Commit ~tag:"bar" ~tagger:john_doe "Haha!"))
  let ktag2 = Store.Value.digest tag2

  (* r1: t4 *)
  let r1 = Store.Reference.of_string "refs/origin/head"

  (* r2: c2 *)
  let r2 = Store.Reference.of_string "refs/upstream/head"

  let check_write t name k v =
    let open Lwt_result in

    Store.write t v
    >>= fun (k', _) ->
    assert_key_equal (name ^ "-key-1") k k';
    Store.read t k     >>= fun v' ->
    assert_value_equal name v v';
    Store.write t v'
    >>= fun (k'', _) ->
    assert_key_equal (name ^ "-key-2") k k'';
    Lwt.return (Ok ())

  let check_find t name k path e =
    Search.find t k path >>= fun k' ->
    assert_key_opt_equal (name ^ "-find") (Some e) k';
    Lwt.return (Ok ())

  let root = Store.Path.v "test-git-store"

  let create ~root ?(index=false) () =
    let ( >/= ) = Lwt_result.bind_lwt in

    Store.create ~root () >/= fun t  ->
      Lwt_list.iter_s
        (fun v -> Store.write t v >>= fun _ -> Lwt.return ())
        (if not index then [
            v0; v1; v2;
            t0; t1; t2; t3; t4;
            c1; c2; c3;
          ] else [
           v1; v2;
           t1; t2; t4;
           c1; c2;
         ])
      >>= fun () -> Lwt.return t

  let is_ typ t k =
    Store.read t k >>= function
    | Error _ -> Lwt.return false
    | Ok v    ->
      Lwt.return (typ = Store.Value.kind v)

  let check_keys t name typ expected =
    Store.list t                     >>= fun ks ->
    Lwt_list.filter_s (is_ typ t) ks >>= fun ks ->
    Lwt.return (Ok (assert_keys_equal name expected ks))

  module IndexDecoder = Git.Index_pack.Decoder(Store.Hash)
  module IndexEncoder = Git.Index_pack.Encoder(Store.Hash)
  module PackDecoder  = Git.Unpack.MakePACKDecoder(Store.Hash)(Store.Inflate)
  module PackEncoder  = Git.Pack.MakePACKEncoder(Store.Hash)(Store.Deflate)

  exception Store of Store.error
  exception Ref of Store.Ref.error
  exception Pack of Store.Pack.error

  exception IndexDecoder of IndexDecoder.error
  exception IndexEncoder of IndexEncoder.error
  exception PackDecoder of PackDecoder.error
  exception PackEncoder of PackEncoder.error

  let test_blobs x () =
    let open Lwt_result in

    let test () =
      create ~root ()           >>= fun t  ->
      check_write t "v1" kv1 v1 >>= fun () ->
      check_write t "v2" kv2 v2 >>= fun () ->

      check_keys t "blobs" `Blob [kv0; kv1; kv2] >>= fun () ->
      Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let test_trees x () =
    let test () =
      let open Lwt_result in

      create ~root ()           >>= fun t  ->
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

      check_keys t "trees" `Tree [kt0; kt1; kt2; kt3; kt4] >>=
      fun () ->

      Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  module ValueIO
    : Git.Value.RAW
      with module Hash = Store.Hash
       and module Blob = Store.Value.Blob
       and module Commit = Store.Value.Commit
       and module Tree = Store.Value.Tree
       and module Tag = Store.Value.Tag
       and type t = Store.Value.t
    = Git.Value.Raw(Store.Hash)(Store.Inflate)(Store.Deflate)

  let head_contents =
    let module M = struct
      type t = Store.Reference.head_contents

      let equal = Store.Reference.equal_head_contents
      let pp = Store.Reference.pp_head_contents
    end in (module M : Alcotest.TESTABLE with type t = M.t)

  let hash =
    let module M = struct
      type t = Store.Hash.t

      let equal = Store.Hash.equal
      let pp = Store.Hash.pp
    end in (module M : Alcotest.TESTABLE with type t = M.t)


  let test_commits x () =
    let c =
      let root = Store.Hash.of_hex "3aadeb4d06f2a149e06350e4dab2c7eff117addc" in
      let thomas =
        { User.name = "Thomas Gazagnaire"
        ; email = "thomas@gazagnaire.org"
        ; date= (1435873834L, Some { User.sign = `Plus; hours = 1; minutes = 0 })}
      in
      let message = "Initial commit" in
      Store.Value.(commit (Commit.make ~tree:root ~author:thomas ~committer:thomas message))
    in
    let test () =
      match ValueIO.to_raw c with
      | Error _ -> assert false
      | Ok raw -> match ValueIO.of_raw_with_header (Cstruct.of_string raw) with
        | Ok c' -> assert_value_equal "commits: convert" c c';

          let open Lwt_result in

          create ~root ()           >>= fun t   ->
          check_write t "c1" kc1 c1 >>= fun () ->
          check_write t "c2" kc2 c2 >>= fun () ->

          let p x = `Commit (`Path x) in
          check_find t "c1:b"     kc1 (p ["b"])          kt1 >>= fun () ->
          check_find t "c1:b/x"   kc1 (p ["b"; "x"])     kv1 >>= fun () ->
          check_find t "c2:a/b/x" kc2 (p ["a";"b"; "x"]) kv1 >>= fun () ->
          check_find t "c2:c"     kc2 (p ["c"])          kv2 >>= fun () ->

          check_keys t "commits" `Commit [kc1; kc2; kc3] >>= fun () ->

          Lwt.return (Ok t)
        | Error (`Decoder err) ->
          Alcotest.fail (Fmt.strf "(`Decoder %s)" err)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let test_tags x () =
    let test () =
      let open Lwt_result in

      create ~root ()                 >>= fun t   ->
      check_write t "tag1" ktag1 tag1 >>= fun () ->
      check_write t "tag2" ktag2 tag2 >>= fun () ->

      let p l x = `Tag (l, `Commit (`Path x)) in
      check_find t "tag1:b" ktag1 (p "foo" ["b"]) kt1 >>= fun () ->
      check_find t "tag2:a" ktag2 (p "bar" ["a"]) kt2 >>= fun () ->
      check_find t "tag2:c" ktag2 (p "bar" ["c"]) kv2 >>= fun () ->

      check_keys t "tags" `Tag [ktag1; ktag2] >>= fun () ->

      Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let test_refs x () =
    let test () =
      let open Lwt_result in
      let ( >!= ) = Lwt.bind in

      create ~root () >!= function
        | Error err -> Lwt.fail (Store err)
        | Ok t ->
          Store.Ref.write t r1 (Store.Reference.Hash kt4) >>= fun ()   ->
          Store.Ref.read  t r1      >>= fun (_, kt4') ->
          assert_head_contents_equal "r1" (Store.Reference.Hash kt4) kt4';

          Store.Ref.write t r2 (Store.Reference.Hash kc2) >>= fun ()   ->
          Store.Ref.read  t r2      >>= fun (_, kc2') ->
          assert_head_contents_equal "r2" (Store.Reference.Hash kc2) kc2';

          Store.Ref.list t                 >!= fun rs   ->
            assert_refs_and_hashes_equal "refs" [r1, kt4; r2, kc2] rs;

            let commit = Store.Hash.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a" in
            Store.Ref.write t Store.Reference.head (Store.Reference.Hash commit) >>= fun () ->
            Store.Ref.read t Store.Reference.head >>= fun (_, value) ->
            Alcotest.(check head_contents) "head" (Store.Reference.Hash commit) value;
            Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Ref err))

  let test_search x () =
    let test () =
      let open Lwt_result in

      let ( >!= ) = Lwt.bind in

      create ~root () >>= fun t ->

      let check k path v =
        Search.find t k path >!= fun v' ->
          Alcotest.(check (option hash)) "search" (Some v) v';
          Lwt.return (Ok ())
      in

      check kt4 (`Path ["a";"b";"x"]) kv1 >>= fun () ->
      check kc2 (`Commit (`Path ["a";"b";"x"])) kv1 >>= fun () ->
      check kc2 (`Commit (`Path ["a"])) kt2 >>= fun () ->
      Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let filename_index_pack = Fpath.(v "../" / "data" / "pack.idx")
  let filename_pack = Fpath.(v "../" / "data" / "pack.pack")

  module RefIndexPack = Test_index_pack.Value(Store.Hash)

  let test_encoder_index_pack x () =
    let module Radix = Common.Radix in

    let read_file file =
      let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
      let ba = Lwt_bytes.map_file ~fd ~shared:false () in
      Unix.close fd;
      Cstruct.of_bigarray ba
    in

    let test () =
      let seq f = List.iter f RefIndexPack.values in
      let buf = Git.Buffer.create 0x8000 in
      let tmp = Cstruct.create 0x800 in
      let state = IndexEncoder.default seq RefIndexPack.hash in

      let rec go state = match IndexEncoder.eval tmp state with
        | `Flush state ->
          Git.Buffer.add buf (Cstruct.sub tmp 0 (IndexEncoder.used_out state));
          go (IndexEncoder.flush 0 (Cstruct.len tmp) state)
        | `End state ->
          if IndexEncoder.used_out state > 0
          then Git.Buffer.add buf (Cstruct.sub tmp 0 (IndexEncoder.used_out state));
          Lwt.return (Ok ())
        | `Error (_, err) -> Lwt.return (Error err)
      in

      Store.create ~root () >>= function
      | Error err -> Lwt.fail (Store err)
      | Ok t ->
        let open Lwt_result in

        go state >>= fun () ->
        let buf = Git.Buffer.unsafe_contents buf in
        let res = read_file (Fpath.to_string filename_index_pack) in

        assert_cstruct_data_equal "raw data" buf res;
        Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (IndexEncoder err))

  let test_decoder_index_pack x () =
    let module Radix = Common.Radix in

    let test () =
      Lwt_unix.openfile (Fpath.to_string filename_index_pack) [O_RDONLY] 0o644 >>= fun ic ->
      let src = Cstruct.create 0x8000 in

      let rec go acc state = match IndexDecoder.eval src state with
        | `Await state ->
          Lwt_bytes.read ic (Cstruct.to_bigarray src) 0 (Cstruct.len src) >>= fun len ->
          go acc (IndexDecoder.refill 0 len state)
        | `End _ -> Lwt.return (Ok acc)
        | `Hash (state, (hash, crc, off)) ->
          go (Radix.bind acc hash (crc, off)) state
        | `Error (_, err) -> Lwt.return (Error err)
      in

      Store.create ~root () >>= function
      | Error err -> Lwt.fail (Store err)
      | Ok t ->
        let open Lwt_result in

        go Radix.empty (IndexDecoder.make ()) >>= fun tree ->
        assert_index_pack_equal "reference index pack" tree
          (List.fold_left (fun a (h, v) -> Radix.bind a h v) Radix.empty RefIndexPack.values);
        Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (IndexDecoder err))

  module Graph = Map.Make(Int64)
  module Pack = Common.Radix

  let decode_pack_file filename_pack =
    Lwt_unix.openfile (Fpath.to_string filename_pack) [O_RDONLY] 0o644 >>= fun ic ->
    let src = Cstruct.create 0x8000 in
    let buf = Git.Buffer.create 0x8000 in

    let string_of_kind = function
      | PackDecoder.Commit -> "commit"
      | PackDecoder.Tree -> "tree"
      | PackDecoder.Blob -> "blob"
      | PackDecoder.Tag -> "tag"
      | _ -> assert false
    in

    let digest_with_header ctx state =
      let hdr =
        Fmt.strf "%s %d\000"
          (string_of_kind (PackDecoder.kind state))
          (PackDecoder.length state)
      in

      Store.Hash.Digest.feed ctx (Cstruct.of_string hdr)
    in

    let apply (src, kind) length hunks =
      let raw = Cstruct.create length in
      let ctx = Store.Hash.Digest.init () in

      let hdr =
        Fmt.strf "%s %d\000"
          (string_of_kind kind)
          length
      in

      Store.Hash.Digest.feed ctx (Cstruct.of_string hdr);

      List.fold_left (fun pos -> function
          | `Copy (off, len) ->
            let src = Cstruct.of_string (String.sub src off len) in
            Store.Hash.Digest.feed ctx src;
            Cstruct.blit src 0 raw pos len;
            pos + len
          | `Insert (off, len) ->
            let src = Cstruct.sub (Git.Buffer.unsafe_contents buf) off len in
            Store.Hash.Digest.feed ctx src;
            Cstruct.blit src 0 raw pos len;
            pos + len)
        0 hunks
      |> fun length' ->
      assert (length = length');

      let hash = Store.Hash.Digest.get ctx in
      hash, kind, Cstruct.to_string raw
    in

    let rec go ~pack ~graph ?current state = match PackDecoder.eval src state with
      | `Await state ->
        Lwt_bytes.read ic (Cstruct.to_bigarray src) 0 (Cstruct.len src) >>= fun len ->
        go ~pack ~graph ?current (PackDecoder.refill 0 len state)
      | `End (_, hash) -> Lwt.return (Ok (pack, graph, hash))
      | `Error (_, err) -> Lwt.return (Error err)
      | `Flush state ->
        let o, n = PackDecoder.output state in
        let current = match current with
          | Some (`Ctx ctx) ->
            Store.Hash.Digest.feed ctx (Cstruct.sub o 0 n);
            Some (`Ctx ctx)
          | None ->
            let ctx = Store.Hash.Digest.init () in
            digest_with_header ctx state;
            Store.Hash.Digest.feed ctx (Cstruct.sub o 0 n);
            Some (`Ctx ctx)
          | Some (`Hunks _) -> assert false
        in

        Git.Buffer.add buf (Cstruct.sub o 0 n);
        go ~pack ~graph ?current (PackDecoder.flush 0 (Cstruct.len o) state)
      | `Hunk (state, hunk) ->
        let current = match current, hunk with
          | Some (`Hunks hunks), PackDecoder.H.Copy (off, len) ->
            Some (`Hunks (`Copy (off, len) :: hunks))
          | Some (`Hunks hunks), PackDecoder.H.Insert raw ->
            let off, len = Git.Buffer.has buf, Cstruct.len raw in
            Git.Buffer.add buf raw;
            Some (`Hunks (`Insert (off, len) :: hunks))
          | None, PackDecoder.H.Copy (off, len) ->
            Some (`Hunks [ `Copy (off, len) ])
          | None, PackDecoder.H.Insert raw ->
            let off, len = Git.Buffer.has buf, Cstruct.len raw in
            Git.Buffer.add buf raw;
            Some (`Hunks [ `Insert (off, len) ])
          | Some (`Ctx _), _ -> assert false
        in

        go ~pack ~graph ?current (PackDecoder.continue state)
      | `Object state ->
        let pack, (off, kind, raw) = match PackDecoder.kind state, current with
          | (PackDecoder.Commit
            | PackDecoder.Tree
            | PackDecoder.Tag
            | PackDecoder.Blob) as kind,
            Some (`Ctx ctx) ->
            let hash = Store.Hash.Digest.get ctx in
            Pack.bind pack hash (PackDecoder.crc state, PackDecoder.offset state),
            (PackDecoder.offset state, kind, Git.Buffer.contents buf)
          | (PackDecoder.Commit
            | PackDecoder.Tree
            | PackDecoder.Tag
            | PackDecoder.Blob) as kind,
            None ->
            let ctx = Store.Hash.Digest.init () in
            digest_with_header ctx state;
            let hash = Store.Hash.Digest.get ctx in
            Pack.bind pack hash (PackDecoder.crc state, PackDecoder.offset state),
            (PackDecoder.offset state, kind, "")
          | PackDecoder.Hunk { PackDecoder.H.reference = PackDecoder.H.Hash src; target_length; _ },
            Some (`Hunks hunks) ->
            let hash, kind, raw =
              apply
                (Pack.lookup pack src |> function
                  | Some (_, off) -> Graph.find off graph
                  | None -> assert false)
                target_length (List.rev hunks) in
            Pack.bind pack hash (PackDecoder.crc state, PackDecoder.offset state),
            (PackDecoder.offset state, kind, raw)
          | PackDecoder.Hunk { PackDecoder.H.reference = PackDecoder.H.Offset rel; target_length; _ },
            Some (`Hunks hunks) ->
            let hash, kind, raw =
              apply
                (Graph.find Int64.(sub (PackDecoder.offset state) rel) graph)
                target_length (List.rev hunks) in
            Pack.bind pack hash (PackDecoder.crc state, PackDecoder.offset state),
            (PackDecoder.offset state, kind, raw)
          | _, _ -> assert false
        in
        let graph = Graph.add off (raw, kind) graph in
        Git.Buffer.clear buf;
        go ~pack ~graph (PackDecoder.next_object state)
    in

    let ztmp = Cstruct.create 0x8000 in
    let wtmp = Store.Inflate.window () in

    go ~pack:Pack.empty ~graph:Graph.empty (PackDecoder.default ztmp wtmp) >>= function
    | Ok (pack, graph, hash) ->
      Lwt_unix.close ic >>= fun () -> Lwt.return (Ok (pack, graph, hash))
    | Error err ->
      Lwt_unix.close ic >>= fun () -> Lwt.return (Error err)

  let test_decoder_pack x () =
    let test () =
      Store.create ~root () >>= function
      | Error err -> Lwt.fail (Store err)
      | Ok t ->
        let open Lwt_result in

        decode_pack_file filename_pack >>= fun (pack, _, _) ->
        assert_index_pack_equal "reference index pack" pack
          (List.fold_left (fun a (h, v) -> Radix.bind a h v) Radix.empty RefIndexPack.values);
        Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (PackDecoder err))

  let cstruct_copy cs =
    let ln = Cstruct.len cs in
    let rs = Cstruct.create ln in
    Cstruct.blit cs 0 rs 0 ln;
    rs

  let test_encoder_pack x () =
    let test () =
      let open Lwt_result in

      Store.create ~root () >>= fun t ->
      let open Lwt.Infix in

      Lwt_unix.openfile (Fpath.to_string filename_pack) [O_RDONLY] 0o644 >>= fun ic ->
      let tmp = Cstruct.create 0x8000 in

      let stream () =
        Lwt_bytes.read ic (Cstruct.to_bigarray tmp) 0 (Cstruct.len tmp) >>= function
        | 0 -> Lwt.return None
        | n -> Lwt.return (Some (Cstruct.sub tmp 0 n))
      in

      Store.Pack.from t stream >>= function
      | Error err ->
        Lwt.fail (Pack err)
      | Ok _ ->
        Store.contents t >>= function
        | Error err ->
          Lwt.fail (Store err)
        | Ok lst ->
          let snd (_, b) = Lwt.return b in
          Lwt_list.map_s snd lst >>= Store.Pack.make t >>= function
          | Error err ->
            Lwt.fail (Pack err)
          | Ok (stream, graph) ->
            let thread, u = Lwt.wait () in

            let rec cstruct_of_stream current =
              stream () >>= function
              | Some raw ->
                cstruct_of_stream (Cstruct.concat [current; cstruct_copy raw])
              | None -> Lwt_mvar.take graph >>= fun graph -> Lwt.wakeup u graph; Lwt.return current
            in

            cstruct_of_stream (Cstruct.create 0) >>= fun pack_raw -> thread >>= fun graph ->

            let module Mapper =
            struct
              type +'a io = 'a Lwt.t
              type fd = Cstruct.t
              type raw = Cstruct.t
              type path = unit
              type error = unit

              let pp_error = Fmt.nop
              let openfile () = Lwt.return (Ok pack_raw)
              let length raw = Lwt.return (Ok (Int64.of_int (Cstruct.len raw)))
              let map raw ?(pos = 0L) ~share:_ len =
                let pos = Int64.to_int pos in
                let len = min (Cstruct.len raw - pos) len in
                Lwt.return (Ok (Cstruct.sub raw pos len))
              let close _ = Lwt.return (Ok ())
            end in

            let module Decoder = Unpack.MakeDecoder(Store.Hash)(Mapper)(Store.Inflate) in
            let ztmp = Cstruct.create 0x8000 in
            let wtmp = Store.Inflate.window () in

            Decoder.make pack_raw
              (fun _ -> None)
              (fun hash ->
                 try let (crc, off) = Store.Pack.Graph.find hash graph in
                   Some (crc, off)
                 with _ -> None)
              (fun _ -> None)
              (fun hash -> Store.read_inflated t hash)
            >>= function
            | Ok state ->
              let fst (a, _) = Lwt.return a in
              Lwt_list.map_s fst lst >>=
              Lwt_list.iter_s
                (fun hash ->
                   Decoder.get_with_allocation state hash ztmp wtmp >>= function
                   | Ok _ -> Lwt.return ();
                   | Error _ -> assert false)
              >>= fun () -> Lwt.return (Ok t)
            | Error () -> assert false
            (* XXX(dinosaure): this error should never happen
               because [Mapper.map] returns [Ok] every times. *)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))
end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [ "Operations on blobs"       , speed, T.test_blobs x
  ; "Operations on trees"       , speed, T.test_trees x
  ; "Operations on commits"     , speed, T.test_commits x
  ; "Operations on tags"        , speed, T.test_tags x
  ; "Operations on references"  , speed, T.test_refs x
  ; "Search"                    , speed, T.test_search x
  ; "Index pack decoder"        , speed, T.test_decoder_index_pack x
  ; "Index pack encoder"        , speed, T.test_encoder_index_pack x
  ; "Pack decoder"              , `Slow, T.test_decoder_pack x
  ; "Pack encoder"              , `Slow, T.test_encoder_pack x ]
