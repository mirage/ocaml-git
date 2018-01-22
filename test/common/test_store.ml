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

let (>>?=) = Lwt_result.(>>=)
let (>|?=) = Lwt_result.(>|=)

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

module Make (Store : Git.S) = struct

  module Common = Make(Store)
  open Common

  module Search = Search.Make(Store)

  let reset t =
    Store.reset t >|= function
    | Ok ()   -> ()
    | Error e -> Alcotest.failf "reset failed: %a" Store.pp_error e

  let init_err = function
    | Ok x    -> Lwt.return x
    | Error e -> Alcotest.failf "error: %a" Store.pp_error e

  let check_err = function
    | Ok x    -> Lwt.return x
    | Error e -> Alcotest.failf "error: %a" Store.pp_error e

  let run test = Lwt_main.run (test ())

  let (!!) = Lazy.force

  let v0  = lazy
    (Store.Value.blob (Store.Value.Blob.of_cstruct (long_random_cstruct ())))
  let kv0 = lazy (Store.Value.digest !!v0)

  let v1  = lazy (Store.Value.blob (Store.Value.Blob.of_string "hoho"))
  let kv1 = lazy (Store.Value.digest !!v1)

  let v2  = lazy (Store.Value.blob (Store.Value.Blob.of_string ""))
  let kv2 = lazy (Store.Value.digest !!v2)

  (* Create a node containing t1 -w-> v1 *)
  let w = "a\042bbb\047"
  let t0 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Normal
                        ; name = w
                        ; node = !!kv1 } ]))
  let kt0 = lazy (Store.Value.digest !!t0)

  let t1 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Normal
                        ; name = "x"
                        ; node = !!kv1 } ]))
  let kt1 = lazy (Store.Value.digest !!t1)

  (* Create the tree t2 -b-> t1 -x-> v1 *)
  let t2 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Dir
                        ; name = "b"
                        ; node = !!kt1 } ]))
  let kt2 = lazy (Store.Value.digest !!t2)

  (* Create the tree t3 -a-> t2 -b-> t1 -x-> v1 *)
  let t3 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Dir
                        ; name = "a"
                        ; node = !!kt2 } ]))
  let kt3 = lazy (Store.Value.digest !!t3)

  (* Create the tree t4 -a-> t2 -b-> t1 -x-> v1
                       \-c-> v2 *)
  let t4 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Exec
                        ; name = "c"
                        ; node = !!kv2 }
                      ; { Store.Value.Tree.perm = `Dir
                        ; name = "a"
                        ; node = !!kt2 } ]))
  let kt4 = lazy (Store.Value.digest !!t4)

  let t5 = lazy (Store.Value.tree
                   (Store.Value.Tree.of_list
                      [ { Store.Value.Tree.perm = `Normal
                        ; name = long_random_string ()
                        ; node = !!kv2 }
                      ; { Store.Value.Tree.perm = `Dir
                        ; name = "a"
                        ; node = !!kt2 } ]))
  let kt5 = lazy (Store.Value.digest !!t5)

  let john_doe =
    { User.name  = "John Doe"
    ; email = "jon@doe.com"
    ; date  = 0L, None }

  (* c1 : t2 *)
  let c1 = lazy (
    Store.Value.commit
      (Store.Value.Commit.make
         ~author:john_doe
         ~committer:john_doe
         ~tree:!!kt2
         "hello r1"))
  let kc1 = lazy (Store.Value.digest !!c1)

  (* c1 -> c2 : t4 *)
  let c2 = lazy (
    Store.Value.commit
      (Store.Value.Commit.make
         ~author:john_doe
         ~committer:john_doe
         ~parents:[ !!kc1 ]
         ~tree:!!kt4
         "hello r1!"))
  let kc2 = lazy (Store.Value.digest !!c2)

  let c3 = lazy (
    let c2 = match !!c2 with Store.Value.Commit x -> x | _ -> assert false in
    Store.Value.(commit
                   (Commit.make
                      ~author:(Commit.author c2)
                      ~committer:(Commit.committer c2)
                      ~parents:(Commit.parents c2)
                      ~tree:!!kt5
                      (Commit.message c2))))
  let kc3 = lazy (Store.Value.digest !!c3)

  (* tag1: c1 *)
  let tag1 = lazy (
    Store.Value.(tag
                   (Tag.make
                      !!kc1 Tag.Commit ~tag:"foo" ~tagger:john_doe "Ho yeah!")))
  let ktag1 = lazy (Store.Value.digest !!tag1)

  (* tag2: c2 *)
  let tag2 = lazy (
    Store.Value.(tag
                   (Tag.make
                      !!kc2 Tag.Commit ~tag:"bar" ~tagger:john_doe "Haha!")))
  let ktag2 = lazy (Store.Value.digest !!tag2)

  (* r1: t4 *)
  let r1 = Store.Reference.of_string "refs/origin/head"

  (* r2: c2 *)
  let r2 = Store.Reference.of_string "refs/upstream/head"

  let check_write t name k v =
    Store.write t v >>= check_err >>= fun (k', _) ->
    assert_key_equal (name ^ "-key-1") k k';
    Store.read t k >>= check_err >>= fun v' ->
    assert_value_equal name v v';
    Store.write t v' >>= check_err >|= fun (k'', _) ->
    assert_key_equal (name ^ "-key-2") k k''

  let check_find t name k path e =
    Search.find t k path >|= fun k' ->
    assert_key_opt_equal (name ^ "-find") (Some e) k'

  let root = Fpath.v "test-git-store"

  let create ~root ?(index=false) () =
    Store.create ~root () >>= init_err >>= fun t ->
    reset t >>= fun () ->
    Lwt_list.iter_s (fun v ->
        Store.write t v >|= function
        | Error e -> Alcotest.failf "create: %a" Store.pp_error e
        | Ok _    -> ()
      ) (if not index then [
        !!v0; !!v1; !!v2;
        !!t0; !!t1; !!t2; !!t3; !!t4;
        !!c1; !!c2; !!c3;
      ] else [
         !!v1; !!v2;
         !!t1; !!t2; !!t4;
         !!c1; !!c2;
       ])
    >|= fun () -> t

  let is_ typ t k =
    Store.read t k >>= function
    | Error _ -> Lwt.return false
    | Ok v    ->
      Lwt.return (typ = Store.Value.kind v)

  let check_keys t name typ expected =
    Store.list t                     >>= fun ks ->
    Lwt_list.filter_s (is_ typ t) ks >|= fun ks ->
    assert_keys_equal name expected ks

  module IndexDecoder = Git.Index_pack.Decoder(Store.Hash)
  module IndexEncoder = Git.Index_pack.Encoder(Store.Hash)
  module PackDecoder  = Git.Unpack.MakeStreamDecoder(Store.Hash)(Store.Inflate)
  module PackEncoder  = Git.Pack.MakeStreamEncoder(Store.Hash)(Store.Deflate)

  let test_blobs () =
    let test () =
      create ~root ()               >>= fun t  ->
      check_write t "v1" !!kv1 !!v1 >>= fun () ->
      check_write t "v2" !!kv2 !!v2 >>= fun () ->
      check_keys t "blobs" `Blob [!!kv0; !!kv1; !!kv2]
    in
    run test

  let test_trees () =
    let test () =
      create ~root ()               >>= fun t  ->
      check_write t "t1" !!kt1 !!t1 >>= fun () ->
      check_write t "t2" !!kt2 !!t2 >>= fun () ->
      check_write t "t3" !!kt3 !!t3 >>= fun () ->
      check_write t "t4" !!kt4 !!t4 >>= fun () ->

      let p x = `Path x in
      check_find t "kt0:w"     !!kt0 (p [w])           !!kv1 >>= fun () ->
      check_find t "kt1:w"     !!kt1 (p ["x"])         !!kv1 >>= fun () ->
      check_find t "kt2:b"     !!kt2 (p ["b"])         !!kt1 >>= fun () ->
      check_find t "kt2:b/x"   !!kt2 (p ["b";"x"])     !!kv1 >>= fun () ->
      check_find t "kt3:a"     !!kt3 (p ["a"])         !!kt2 >>= fun () ->
      check_find t "kt3:a/b"   !!kt3 (p ["a";"b"])     !!kt1 >>= fun () ->
      check_find t "kt3:a/b/x" !!kt3 (p ["a";"b";"x"]) !!kv1 >>= fun () ->
      check_find t "kt4:c"     !!kt4 (p ["c"])         !!kv2 >>= fun () ->

      check_keys t "trees" `Tree [!!kt0; !!kt1; !!kt2; !!kt3; !!kt4]
    in
    run test

  module ValueIO = Git.Value.Raw(Store.Hash)(Store.Inflate)(Store.Deflate)

  let head_contents =
    let open Store.Reference in
    Alcotest.testable pp_head_contents equal_head_contents

  let hash = Alcotest.testable Store.Hash.pp Store.Hash.equal

  let test_commits () =
    let c =
      let root = Store.Hash.of_hex "3aadeb4d06f2a149e06350e4dab2c7eff117addc" in
      let thomas =
        { User.name = "Thomas Gazagnaire"
        ; email = "thomas@gazagnaire.org"
        ; date= (1435873834L, Some { User.sign = `Plus; hours = 1; minutes = 0 })}
      in
      let msg = "Initial commit" in
      Store.Value.Commit.make ~tree:root ~author:thomas ~committer:thomas msg
      |> Store.Value.commit
    in
    let test () =
      match ValueIO.to_raw c with
      | Error e -> Alcotest.failf "%a" ValueIO.EE.pp_error e
      | Ok raw  ->
        match ValueIO.of_raw_with_header (Cstruct.of_string raw) with
        | Error err -> Alcotest.failf "decoder: %a" Git.Error.Decoder.pp_error err
        | Ok c' ->
          assert_value_equal "commits: convert" c c';
          create ~root ()               >>= fun t   ->
          check_write t "c1" !!kc1 !!c1 >>= fun () ->
          check_write t "c2" !!kc2 !!c2 >>= fun () ->
          let p x = `Commit (`Path x) in
          check_find t "c1:b"     !!kc1 (p ["b"])          !!kt1 >>= fun () ->
          check_find t "c1:b/x"   !!kc1 (p ["b"; "x"])     !!kv1 >>= fun () ->
          check_find t "c2:a/b/x" !!kc2 (p ["a";"b"; "x"]) !!kv1 >>= fun () ->
          check_find t "c2:c"     !!kc2 (p ["c"])          !!kv2 >>= fun () ->
          check_keys t "commits" `Commit [!!kc1; !!kc2; !!kc3]
    in
    run test

  let test_tags () =
    let test () =
      create ~root ()                     >>= fun t   ->
      check_write t "tag1" !!ktag1 !!tag1 >>= fun () ->
      check_write t "tag2" !!ktag2 !!tag2 >>= fun () ->
      let p l x = `Tag (l, `Commit (`Path x)) in
      check_find t "tag1:b" !!ktag1 (p "foo" ["b"]) !!kt1 >>= fun () ->
      check_find t "tag2:a" !!ktag2 (p "bar" ["a"]) !!kt2 >>= fun () ->
      check_find t "tag2:c" !!ktag2 (p "bar" ["c"]) !!kv2 >>= fun () ->
      check_keys t "tags" `Tag [!!ktag1; !!ktag2]
    in
    run test

  let test_refs () =
    let test () =
      create ~root () >>= fun t ->
      Store.Ref.write t r1 (Store.Reference.Hash !!kt4)
      >>= check_err >>= fun () ->
      Store.Ref.read  t r1
      >>= check_err >>= fun (_, kt4') ->
      assert_head_contents_equal "r1" (Store.Reference.Hash !!kt4) kt4';

      Store.Ref.write t r2 (Store.Reference.Hash !!kc2)
      >>= check_err >>= fun ()   ->
      Store.Ref.read  t r2 >>= check_err >>= fun (_, kc2') ->
      assert_head_contents_equal "r2" (Store.Reference.Hash !!kc2) kc2';

      Store.Ref.list t >>= fun rs ->
      assert_refs_and_hashes_equal "refs" [r1, !!kt4; r2, !!kc2] rs;

      let commit = Store.Hash.of_hex "21930ccb5f7b97e80a068371cb554b1f5ce8e55a" in
      Store.Ref.write t Store.Reference.head (Store.Reference.Hash commit)
      >>= check_err >>= fun () ->
      Store.Ref.read t Store.Reference.head
      >>= check_err >|= fun (_, value) ->
      Alcotest.(check head_contents) "head" (Store.Reference.Hash commit) value
    in
    run test

  let test_search () =
    let test () =
      create ~root () >>= fun t ->
      let check k path v =
        Search.find t k path >|= fun v' ->
        Alcotest.(check (option hash)) "search" (Some v) v'
      in
      check !!kt4 (`Path ["a";"b";"x"]) !!kv1 >>= fun () ->
      check !!kc2 (`Commit (`Path ["a";"b";"x"])) !!kv1 >>= fun () ->
      check !!kc2 (`Commit (`Path ["a"])) !!kt2
    in
    run test

  let filename_index_pack = Fpath.(v "../" / "data" / "pack.idx")
  let filename_pack = Fpath.(v "../" / "data" / "pack.pack")

  module RefIndexPack = Test_index_pack.Value(Store.Hash)

  let test_encoder_index_pack () =
    let module Radix = Common.Radix in
    let read_file file =
      let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
      let ba = Lwt_bytes.map_file ~fd ~shared:false () in
      Unix.close fd;
      Cstruct.of_bigarray ba
    in
    let seq f = List.iter f RefIndexPack.values in
    let buf = Git.Buffer.create 0x8000 in
    let tmp = Cstruct.create 0x800 in
    let state = IndexEncoder.default seq RefIndexPack.hash in
    let rec go state = match IndexEncoder.eval tmp state with
      | `Error (_, err) -> Alcotest.failf "%a" IndexEncoder.pp_error err
      | `Flush state    ->
        Git.Buffer.add buf (Cstruct.sub tmp 0 (IndexEncoder.used_out state));
        go (IndexEncoder.flush 0 (Cstruct.len tmp) state)
      | `End state ->
        if IndexEncoder.used_out state > 0
        then Git.Buffer.add buf (Cstruct.sub tmp 0 (IndexEncoder.used_out state))
    in
    let test () =
      Store.create ~root () >>= init_err >>= fun t ->
      reset t >|= fun () ->
      go state;
      let buf = Git.Buffer.unsafe_contents buf in
      let res = read_file (Fpath.to_string filename_index_pack) in
      assert_cstruct_data_equal "raw data" buf res
    in
    run test

  let test_decoder_index_pack () =
    let module Radix = Common.Radix in
    let test () =
      Lwt_unix.openfile (Fpath.to_string filename_index_pack) [O_RDONLY] 0o644
      >>= fun ic ->
      let src = Cstruct.create 0x8000 in
      let rec go acc state = match IndexDecoder.eval src state with
        | `Error (_, e)            -> Alcotest.failf "%a" IndexDecoder.pp_error e
        | `Hash (s, (h, crc, off)) -> go (Radix.bind acc h (crc, off)) s
        | `End _                   -> Lwt.return acc
        | `Await state             ->
          Lwt_bytes.read ic (Cstruct.to_bigarray src) 0 (Cstruct.len src)
          >>= fun len -> go acc (IndexDecoder.refill 0 len state)
      in
      Store.create ~root () >>= init_err >>= fun t ->
      reset t >>= fun () ->
      go Radix.empty (IndexDecoder.make ()) >|= fun tree ->
      let tr = List.fold_left (fun a (h, v) ->
          Radix.bind a h v
        ) Radix.empty RefIndexPack.values
      in
      assert_index_pack_equal "reference index pack" tree tr
    in
    run test

  module Graph = Map.Make(Int64)
  module Pack = Common.Radix

  let decode_pack_file filename_pack =
    Lwt_unix.openfile (Fpath.to_string filename_pack) [O_RDONLY] 0o644 >>= fun ic ->
    let src = Cstruct.create 0x8000 in
    let buf = Git.Buffer.create 0x8000 in

    let string_of_kind = function
      | PackDecoder.Commit -> "commit"
      | PackDecoder.Tree   -> "tree"
      | PackDecoder.Blob   -> "blob"
      | PackDecoder.Tag    -> "tag"
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
      let hdr = Fmt.strf "%s %d\000" (string_of_kind kind) length in
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
      | `End (_, hash)  -> Lwt.return (Ok (pack, graph, hash))
      | `Error (_, err) -> Lwt.return (Error err)
      | `Flush state    ->
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
          | Some (`Hunks hunks), PackDecoder.HunkDecoder.Copy (off, len) ->
            Some (`Hunks (`Copy (off, len) :: hunks))
          | Some (`Hunks hunks), PackDecoder.HunkDecoder.Insert raw ->
            let off, len = Git.Buffer.has buf, Cstruct.len raw in
            Git.Buffer.add buf raw;
            Some (`Hunks (`Insert (off, len) :: hunks))
          | None, PackDecoder.HunkDecoder.Copy (off, len) ->
            Some (`Hunks [ `Copy (off, len) ])
          | None, PackDecoder.HunkDecoder.Insert raw ->
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
          | PackDecoder.Hunk { PackDecoder.HunkDecoder.reference = PackDecoder.HunkDecoder.Hash src; target_length; _ },
            Some (`Hunks hunks) ->
            let hash, kind, raw =
              apply
                (Pack.lookup pack src |> function
                  | Some (_, off) -> Graph.find off graph
                  | None -> assert false)
                target_length (List.rev hunks) in
            Pack.bind pack hash (PackDecoder.crc state, PackDecoder.offset state),
            (PackDecoder.offset state, kind, raw)
          | PackDecoder.Hunk { PackDecoder.HunkDecoder.reference = PackDecoder.HunkDecoder.Offset rel; target_length; _ },
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

    go ~pack:Pack.empty ~graph:Graph.empty (PackDecoder.default ztmp wtmp)
    >>= function
    | Ok x    -> Lwt_unix.close ic >|= fun () -> x
    | Error e ->
      Lwt_unix.close ic >|= fun () ->
      Alcotest.failf "%a" PackDecoder.pp_error e

  let test_decoder_pack () =
    let test () =
      Store.create ~root () >>= init_err >>= fun t ->
      reset t >>= fun () ->
      decode_pack_file filename_pack >|= fun (pack, _, _) ->
      let p = List.fold_left (fun a (h, v) ->
          Radix.bind a h v
        ) Radix.empty RefIndexPack.values
      in
      assert_index_pack_equal "reference index pack" pack p
    in
    run test

  let cstruct_copy cs =
    let ln = Cstruct.len cs in
    let rs = Cstruct.create ln in
    Cstruct.blit cs 0 rs 0 ln;
    rs

  let test_encoder_pack () =
    let test () =
      Store.create ~root () >>= init_err >>= fun t ->
      reset t >>= fun () ->
      Lwt_unix.openfile (Fpath.to_string filename_pack) [O_RDONLY] 0o644 >>= fun ic ->
      let tmp = Cstruct.create 0x8000 in
      let stream () =
        Lwt_bytes.read ic (Cstruct.to_bigarray tmp) 0 (Cstruct.len tmp) >|= function
        | 0 -> None
        | n -> Some (Cstruct.sub tmp 0 n)
      in
      Store.Pack.from t stream >>= check_err >>= fun _ ->
      Store.contents t >>= check_err >>= fun lst ->
      Store.Pack.make t (List.map snd lst) >>= check_err
      >>= fun (stream, graph) ->
      let thread, u = Lwt.wait () in
      let rec cstruct_of_stream current =
        stream () >>= function
        | Some raw -> cstruct_of_stream (Cstruct.concat [current; cstruct_copy raw])
        | None     -> Lwt_mvar.take graph >>= fun graph -> Lwt.wakeup u graph; Lwt.return current
      in
      cstruct_of_stream (Cstruct.create 0) >>= fun pack_raw ->
      thread >>= fun graph ->

      let module Mapper = struct
        type fd = Cstruct.t
        type error = unit
        let pp_error = Fmt.nop
        let openfile _ = Lwt.return (Ok pack_raw)
        let length raw = Lwt.return (Ok (Int64.of_int (Cstruct.len raw)))
        let map raw ?(pos = 0L) len =
          let pos = Int64.to_int pos in
          let len = min (Cstruct.len raw - pos) len in
          Lwt.return (Ok (Cstruct.sub raw pos len))
        let close _ = Lwt.return (Ok ())
      end in

      let module Decoder = Unpack.MakeRandomAccessPACK(Store.Hash)(Mapper)(Store.Inflate) in
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
        Lwt_list.iter_s (fun (hash, _) ->
            Decoder.get_with_result_allocation_from_hash state hash ztmp wtmp >|= function
            | Ok _    -> ()
            | Error e -> Alcotest.failf "%a" Decoder.pp_error e
          ) lst
      | Error () -> assert false
      (* XXX(dinosaure): this error should never happen
         because [Mapper.map] returns [Ok] every times. *)
    in
    run test

end

let suite name (module S: Git.S) =
  let module T = Make(S) in
  name,
  [ "Operations on blobs"       , `Quick, T.test_blobs
  ; "Operations on trees"       , `Quick, T.test_trees
  ; "Operations on commits"     , `Quick, T.test_commits
  ; "Operations on tags"        , `Quick, T.test_tags
  ; "Operations on references"  , `Quick, T.test_refs
  ; "Search"                    , `Quick, T.test_search
  ; "Index pack decoder"        , `Quick, T.test_decoder_index_pack
  ; "Index pack encoder"        , `Quick, T.test_encoder_index_pack
  ; "Pack decoder"              ,  `Slow, T.test_decoder_pack
  ; "Pack encoder"              ,  `Slow, T.test_encoder_pack ]
