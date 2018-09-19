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
  module M = Git_unix.Sync (S)
  module Store = S

  type error = M.error
  type endpoint = Git.Gri.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri = Git.Gri.make

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
  module M = Git_unix.Http (Store)
  module Store = Store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = M.Endpoint.{uri; headers= M.Web.HTTP.Headers.empty}

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
  module M = Git_unix.Http (Store)
  module Store = Store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = M.Endpoint.{uri; headers= M.Web.HTTP.Headers.empty}

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

module Mem_store = struct include Git.Mem.Store

                          let v root = v root end

module Fs_store = struct include Git_unix.Store

                         let v root = v root end

module Thin = Test_thin.Make (struct
  module M = Git_unix.Http (Fs_store)
  module Store = Fs_store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = M.Endpoint.{uri; headers= M.Web.HTTP.Headers.empty}

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
module Http1 = Http (Mem_store)
module Http2 = Https (Fs_store)
module Index = Test_index

let () =
  verbose () ;
  Alcotest.run "git-unix"
    [ Test_store.suite "mem" (module Mem_store)
    ; Test_data.suite "mem" (module Test_data.Usual) (module Mem_store)
    ; Test_data.suite "mem" (module Test_data.Bomb) (module Mem_store)
    ; Test_store.suite "fs" (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Usual) (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Bomb) (module Fs_store)
    ; Test_data.suite "fs" (module Test_data.Udns) (module Fs_store)
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
    ]
