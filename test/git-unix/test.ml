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

module TCP (S: Test_store.S) = Test_sync.Make(struct
    module M = Git_unix.Sync(S)
    module Store = S
    type error = M.error
    let pp_error = M.pp_error
    let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri

    let fetch_all t ~references uri =
      let open Lwt.Infix in

      M.fetch_all t ~references uri >>= function
      | Error _ as err -> Lwt.return err
      | Ok _ -> Lwt.return (Ok ())

    let update t ~reference uri = M.update_and_create t ~references:(Store.Reference.Map.singleton reference [ reference ]) uri
  end)

(* XXX(dinosaure): the divergence between the TCP API and the HTTP API
   will be update for an homogenization. *)

module HTTP (Store: Test_store.S) = Test_sync.Make(struct
    module M = Git_unix.HTTP(Store)
    module Store = Store
    type error = M.error
    let pp_error = M.pp_error
    let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri

    let fetch_all t ~references uri =
      let open Lwt.Infix in

      M.fetch_all t ~references uri >>= function
      | Error _ as err -> Lwt.return err
      | Ok _ -> Lwt.return (Ok ())

    let update t ~reference uri =
      M.update_and_create t
        ~references:(Store.Reference.Map.singleton reference [ reference ])
        uri
  end)

module HTTPS (Store: Test_store.S) = Test_sync.Make(struct
    module M = Git_unix.HTTP(Store)
    module Store = Store
    type error = M.error
    let pp_error = M.pp_error
    let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri

    let fetch_all t ~references uri =
      let open Lwt.Infix in

      M.fetch_all t ~references uri >>= function
      | Error _ as err -> Lwt.return err
      | Ok _ -> Lwt.return (Ok ())

    let update t ~reference uri =
      M.update_and_create t
        ~references:(Store.Reference.Map.singleton reference [ reference ])
        uri
  end)

module MemStore = struct
  include Git.Mem.Store(Digestif.SHA1)
  let create root = create ~root ()
end

module FsStore = struct
  include Git_unix.FS
  let create root = create ~root ()
end

module TCP1  = TCP(MemStore)
module TCP2  = TCP(FsStore)
module HTTP1 = HTTP(MemStore)
module HTTP2 = HTTPS(FsStore)

let () =
  verbose ();
  Alcotest.run "git-unix"
    [ Test_store.suite "mem" (module MemStore)
    ; Test_data.suite "mem" (module Test_data.Usual) (module MemStore)
    ; Test_data.suite "mem" (module Test_data.Bomb) (module MemStore)
    ; Test_store.suite "fs"  (module FsStore)
    ; Test_data.suite "fs"  (module Test_data.Usual) (module FsStore)
    ; Test_data.suite "fs"  (module Test_data.Bomb) (module FsStore)
    ; TCP1.test_fetch "mem-local-tcp-sync" ["git://localhost/"]
    ; TCP1.test_clone "mem-remote-tcp-sync" [
        "git://github.com/mirage/ocaml-git.git", "master";
        "git://github.com/mirage/ocaml-git.git", "gh-pages";
      ]
    ; TCP2.test_fetch "fs-local-tcp-sync" ["git://localhost/"]
    ; TCP2.test_clone "fs-remote-tcp-sync" [
        "git://github.com/mirage/ocaml-git.git", "master";
        "git://github.com/mirage/ocaml-git.git", "gh-pages";
      ]
    ; HTTP1.test_clone "mem-http-sync" [
        "http://github.com/mirage/ocaml-git.git", "gh-pages"
      ]
    ; HTTP2.test_clone "fs-https-sync" [
        "https://github.com/mirage/ocaml-git.git", "gh-pages"
      ]
    ]
