(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

module Make
    (Store : Minimal.S
     with type Hash.Digest.buffer = Cstruct.t
      and type Hash.hex = string)
= struct
  module Sync = Git_tcp_unix.Make(Git_tcp_unix.Default)(Store)
  module HttpSync = Git_cohttp_lwt_unix.Make(Git_http.Default)(Store)

  exception Sync of Sync.error'

  module T = Test_store.Make(Store)
  include T

  let root = Store.Path.v "test-git-unix-store"

  let test_tcp_remote x () =
    let test () =
      let uri = Uri.of_string "git://localhost/" in

      create ~root () >>= function
      | Error err -> Lwt.return (Error err)
      | Ok t ->
        (* XXX(dinosaure): an empty repository has the HEAD reference
           points to refs/heads/master which one does not exists. If
           we want to know if a repository is empty, we need to check
           [Store.Reference.master]. *)
        Store.Ref.exists t Store.Reference.master >>= function
        | true ->
          Alcotest.fail "non-empty repository!"
        | false ->
          Sync.fetch_all t uri >>= function
          | Error err -> Lwt.fail (Sync err)
          | Ok () ->
            Sync.easy_update t ~reference:Store.Reference.master uri >>= function
            | Error err ->
              Lwt.fail (Sync err)
            | Ok [] -> Lwt.return (Ok t)
            | Ok (_ :: _) ->
              Alcotest.fail "de-synchronization of the git repository"
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let test_clone x () =
    let test () =
      let open Lwt_result in

      Store.create ~root () >>= fun t ->

      let clone_tcp ?(reference = Store.Reference.master) t uri =
        let open Lwt.Infix in

        Sync.easy_clone t ~reference uri >>= fun _ ->
        let open Lwt.Infix in

        Store.list t
        >>= Lwt_list.iter_s
          (fun hash ->
             Store.read_exn t hash >>= fun _ -> Lwt.return ())
        >>= fun () ->
        Store.Ref.read t Store.Reference.head >>= function
        | Error _ -> Alcotest.fail "empty clone!"
        | Ok (_, v) -> Store.reset t
          >>= function
          | Ok () -> Lwt.return ()
          | Error (`Store err) -> Lwt.fail (Store err)
          | Error (`Ref err) -> Lwt.fail (Ref err)
      in

      let clone_http ?(reference = Store.Reference.master) t uri =
        let open Lwt.Infix in

        HttpSync.easy_clone t ~reference uri >>= fun _ ->
        let open Lwt.Infix in

        Store.list t
        >>= Lwt_list.iter_s
          (fun hash ->
             Store.read_exn t hash >>= fun _ -> Lwt.return ())
        >>= fun () ->
        Store.Ref.read t Store.Reference.head >>= function
        | Error _ -> Alcotest.fail "empty clone!"
        | Ok _ -> Store.reset t
          >>= function
          | Ok () -> Lwt.return ()
          | Error (`Store err) -> Lwt.fail (Store err)
          | Error (`Ref err) -> Lwt.fail (Ref err)
      in

      let tcp_ocaml_git   = Uri.of_string "git://github.com/mirage/ocaml-git.git" in
      let http_ocaml_git  = Uri.of_string "http://github.com/mirage/ocaml-git.git" in
      let https_ocaml_git = Uri.of_string "https://github.com/mirage/ocaml-git.git" in

      let gh_pages  = Store.Reference.of_string "refs/heads/gh-pages" in

      let open Lwt.Infix in

      clone_tcp  t tcp_ocaml_git                      >>= fun () ->
      clone_tcp  t tcp_ocaml_git  ~reference:gh_pages >>= fun () ->
      clone_http t http_ocaml_git                     >>= fun () ->
      clone_http t https_ocaml_git                    >>= fun () ->
      clone_http t http_ocaml_git ~reference:gh_pages >>= fun () ->

      Lwt.return (Ok t)
    in
    run x (fun () -> let open Lwt.Infix in test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

(*
  let test_fetch x () =
    let test_one gri c0 c1 ?(update=false) diff =
      x.init () >>= fun () ->
      Store.create ~root () >>= fun t ->
      Store.list t >>= fun l ->
      Alcotest.(check (list sha1)) "empty" [] l;
      let fetch gri (branch, commit) =
        let b = Reference.of_raw ("refs/heads/" ^ branch) in
        let c = Hash_IO.of_hex commit in
>        Sync.clone t ~branch:(`Commit (Hash.to_commit c)) ~checkout:false gri
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
*)

end

(*
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
*)

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [ "TCP Remote operations"          , `Slow, T.test_tcp_remote x
  ; "TCP & HTTP Cloning remote repos", `Slow, T.test_clone x ]

module MemStore = Git.Mem.Make(Git_unix.Sha1)(Fpath)(Lwt_lock)(Git_unix.Inflate)(Git_unix.Deflate)(Cstruct_buffer)
module UnixStore = Git_unix.Store

let mem_backend =
  { name  = "Memory"
  ; store = (module MemStore)
  ; shell = false }

let unix_backend =
  { name  = "Unix Store"
  ; store = (module UnixStore)
  ; shell = true }

let () =
  Alcotest.run "git-unix"
    [ Test_store.suite (`Quick, mem_backend)
    ; Test_store.suite (`Quick, unix_backend)
    ; suite (`Slow, mem_backend)
    ; suite (`Slow, unix_backend) ]
