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

open Test_common

module Tcp (S : Test_store.S) = Test_sync.Make (struct
  module Sync = Git_unix.Sync (S)
  module M = Sync.Tcp
  module Store = S

  type error = M.error
  type endpoint = Git_unix.endpoint

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_unix.endpoint uri

  let fetch_all t ~references uri =
    let open Lwt.Infix in
    M.fetch_all t ~references uri
    >>= function
    | Error _ as err -> Lwt.return err | Ok _ -> Lwt.return (Ok ())

  let update t ~reference uri =
    M.update_and_create t
      ~references:(Store.Reference.Map.singleton reference [reference])
      uri
end)

module Http (Store : Test_store.S) = Test_sync.Make (struct
  module Sync = Git_unix.Sync (Store)
  module M = Sync.Http
  module Store = Store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_unix.endpoint uri

  let fetch_all t ~references uri =
    let open Lwt.Infix in
    M.fetch_all t ~references uri
    >>= function
    | Error _ as err -> Lwt.return err | Ok _ -> Lwt.return (Ok ())

  let update t ~reference uri =
    M.update_and_create t
      ~references:(Store.Reference.Map.singleton reference [reference])
      uri
end)

module Https (Store : Test_store.S) = Test_sync.Make (struct
  module Sync = Git_unix.Sync (Store)
  module M = Sync.Http
  module Store = Store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_unix.endpoint uri

  let fetch_all t ~references uri =
    let open Lwt.Infix in
    M.fetch_all t ~references uri
    >>= function
    | Error _ as err -> Lwt.return err | Ok _ -> Lwt.return (Ok ())

  let update t ~reference uri =
    M.update_and_create t
      ~references:(Store.Reference.Map.singleton reference [reference])
      uri
end)

module Mem_store = struct
  include Git.Mem.Store

  let v root = v root
  let u () =
    let open Lwt.Infix in
    v (Fpath.v ".") >>= function
    | Ok v -> Lwt.return v
    | Error _ -> assert false
end

(* XXX(dinosaure): we replace [move] to be a /forced move/: on [move a b], if
   [b] already exists, we delete it properly (Windows raises an error if [b]
   already exists). This case appear only on tests where we use
   [Store.Pack.from] on already existing PACK files. In this situation,
   [Store.Pack.from] wants to make a fresh new IDX file on [.git/objects/pack/]
   but it already exists because the PACK file comes from [.git/objects/pack/]
   too (and any PACK files on [.git/objects/pack/] have an IDX file - it's
   mandatory for Git).

   To avoid error on Windows, we tun [Git_unix.Fs] so. *)

module Fs = struct
  type error = Git_unix.Fs.error
  type t = Git_unix.Fs.t

  let pp_error = Git_unix.Fs.pp_error
  let is_dir = Git_unix.Fs.is_dir
  let is_file = Git_unix.Fs.is_file

  module File = struct
    include Git_unix.Fs.File

    let move t path_a path_b =
      let open Lwt.Infix in
      Lwt.try_bind
        (fun () -> Lwt_unix.stat (Fpath.to_string path_b))
        (fun _stat ->
          delete t path_b
          >>= function
          | Ok () -> move t path_a path_b | Error _ as err -> Lwt.return err )
        (fun _exn -> move t path_a path_b)
  end

  module Dir = Git_unix.Fs.Dir
  module Mapper = Git_unix.Fs.Mapper

  let has_global_watches = Git_unix.Fs.has_global_watches
  let has_global_checkout = Git_unix.Fs.has_global_checkout
end

module Fs_store = struct
  module I = Git.Inflate
  module D = Git.Deflate
  include Git.Store.Make (Digestif.SHA1) (Fs) (I) (D)

  let v ?dotgit ?compression ?buffer root =
    v ?dotgit ?compression ?buffer () root

  let v root = v root
end

module Thin = Test_thin.Make (struct
  module M = Git_unix.Sync (Fs_store)
  module Store = Fs_store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_unix.endpoint uri

  let fetch_all t ~references uri =
    let open Lwt.Infix in
    M.fetch_all t ~references uri
    >>= function
    | Error _ as err -> Lwt.return err | Ok _ -> Lwt.return (Ok ())

  let update t ~reference uri =
    M.update_and_create t
      ~references:(Store.Reference.Map.singleton reference [reference])
      uri
end)

module Tcp1 = Tcp (Mem_store)
module Tcp2 = Tcp (Fs_store)
module Tcp3 = Test_smart_regression.Make(Mem_store)
module Http1 = Http (Mem_store)
module Http2 = Https (Fs_store)
module Index = Test_index

let () =
  Mirage_crypto_rng_unix.initialize () ;
  verbose () ;
  Alcotest.run "git-unix"
    [ Test_store.suite "mem" (module Mem_store)
    ; Test_data.suite "mem" (module Test_data.Usual) (module Mem_store)
    ; Test_data.suite "mem" (module Test_data.Bomb) (module Mem_store)
    ; Test_store.suite "fs" (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Usual) (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Bomb) (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Udns) (module Fs_store)
    ; Index.suite Fpath.(v (Unix.getcwd ()) / "test-index")
    ; Test_smart.suite "smart (mem)" (module Mem_store)
    ; Test_smart.suite "smart (fs)" (module Fs_store)
      (* XXX(dinosaure): rev-list works only on an unix environment. Indeed,
         oracle (git) needs a well-formed git repository to work. However, the
         tested part belongs the core, so if it's work for git-unix, it should
         work to git-mem/git-mirage. *)
    ; Test_rev_list.suite "fs"
        (module Test_data.Usual)
        (module Fs_store)
        (module Test_rev_list.Usual (Fs_store))
    ; Tcp1.test_fetch "mem-local-tcp-sync" [Uri.of_string "git://localhost/"]
    ; Tcp1.test_clone "mem-remote-tcp-sync"
        [ Uri.of_string "git://github.com/mirage/ocaml-git.git", "master"
        ; Uri.of_string "git://github.com/mirage/ocaml-git.git", "gh-pages" ]
    ; Tcp2.test_fetch "fs-local-tcp-sync" [Uri.of_string "git://localhost/"]
    ; Tcp2.test_clone "fs-remote-tcp-sync"
        [ Uri.of_string "git://github.com/mirage/ocaml-git.git", "master"
        ; Uri.of_string "git://github.com/mirage/ocaml-git.git", "gh-pages" ]
    ; Http1.test_clone "mem-http-sync"
        [Uri.of_string "http://github.com/mirage/ocaml-git.git", "gh-pages"]
    ; Http2.test_clone "fs-https-sync"
        [Uri.of_string "https://github.com/mirage/ocaml-git.git", "gh-pages"]
    ; Thin.test_thin (Uri.of_string "https://github.com/mirage/decompress.git")
    ; Tcp3.tests ()
    ]
