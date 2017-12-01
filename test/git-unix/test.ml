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
  module Sync = Git_unix_tcp.Make(Git_unix_tcp.Default)(Store)
  module HttpSync = Git_unix_http.Make(Git_http.Default)(Store)

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
        | Ok _ -> Store.reset t
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
end

let suite (_, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [ "TCP Remote operations"          , `Slow, T.test_tcp_remote x
  ; "TCP & HTTP Cloning remote repos", `Slow, T.test_clone x ]

module MemStore = Git.Mem.Make(Git_unix.SHA1)(Fpath)(Lwt_lock)(Git.Inflate)(Git.Deflate)
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
    ; suite (`Slow, { mem_backend with name = "Memory & Sync" })
    ; suite (`Slow, { unix_backend with name = "Unix & Sync" }) ]
