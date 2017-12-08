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

open Lwt.Infix
let (>>?=) = Lwt_result.bind

module type SYNC = sig
  (* common ground between Sync and Http.Sync *)
  module Store: Git.S
  type error

  val clone: Store.t -> reference:Store.Reference.t -> Uri.t ->
    (unit, error) result Lwt.t
  val fetch_all: Store.t -> Uri.t -> (unit, error) result Lwt.t
  val update: Store.t -> reference:Store.Reference.t -> Uri.t ->
    ((Store.Reference.t, Store.Reference.t * string) result list, error)
      result Lwt.t
  val kind: [`TCP | `HTTP | `HTTPS]
end

module Make (Sync: SYNC) = struct

  module Store = Sync.Store

  exception Sync of Sync.error

  module T = Test_store.Make(Store)
  include T

  let root = Fpath.v "test-git-store"

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
        Store.Ref.mem t Store.Reference.master >>= function
        | true ->
          Alcotest.fail "non-empty repository!"
        | false ->
          Sync.fetch_all t uri >>= function
          | Error err -> Lwt.fail (Sync err)
          | Ok () ->
            Sync.update t ~reference:Store.Reference.master uri >>= function
            | Error err ->
              Lwt.fail (Sync err)
            | Ok [] ->
              Lwt.return (Ok t)
            | Ok (_ :: _) ->
              Alcotest.fail "de-synchronization of the git repository"
    in
    run x (fun () -> test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let test_clone x () =
    let test () =
      Store.create ~root () >>?= fun t ->

      let clone_tcp ?(reference = Store.Reference.master) t uri =
        Sync.clone t ~reference uri >>= fun _ ->
        Store.list t >>=
        Lwt_list.iter_s (fun hash -> Store.read_exn t hash >>= fun _ -> Lwt.return ())
        >>= fun () ->
        Store.Ref.read t Store.Reference.head >>= function
        | Error _ -> Alcotest.fail "empty clone!"
        | Ok _ ->
          Store.reset t >>= function
          | Ok () -> Lwt.return ()
          | Error (`Store err) -> Lwt.fail (Store err)
          | Error (`Ref err) -> Lwt.fail (Ref err)
      in

      let url = match Sync.kind with
        | `TCP   -> Uri.of_string "git://github.com/mirage/ocaml-git.git"
        | `HTTP  -> Uri.of_string "http://github.com/mirage/ocaml-git.git"
        | `HTTPS -> Uri.of_string "https://github.com/mirage/ocaml-git.git"
      in
      let gh_pages  = Store.Reference.of_string "refs/heads/gh-pages" in
      clone_tcp  t url                      >>= fun () ->
      clone_tcp  t url  ~reference:gh_pages >>= fun () ->

      Lwt.return (Ok t)
    in
    run x (fun () -> test () >>= function
      | Ok t -> Lwt.return t
      | Error err -> Lwt.fail (Store err))

  let suite x =
    let (module S) = x.Test_common.store in
    x.name,
    [ "TCP Remote operations"          , `Slow, test_tcp_remote x
    ; "TCP & HTTP Cloning remote repos", `Slow, test_clone x ]

end
