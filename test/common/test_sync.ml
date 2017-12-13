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
end

module Make (Sync: SYNC) = struct

  module Store = Sync.Store

  exception Sync of Sync.error

  module T = Test_store.Make(Store)

  let root = Fpath.v "test-git-store"

  let run name tests: unit Alcotest.test =
    name, List.map (fun (msg, f) -> msg, `Slow, fun () -> T.run f) tests

  let reset t =
    Store.reset t >>= function
    | Ok ()              -> Lwt.return ()
    | Error (`Store err) -> Lwt.fail (T.Store err)
    | Error (`Ref err)   -> Lwt.fail (T.Ref err)

  let test_fetch name uris =
    let test uri =
      let uri = Uri.of_string uri in
      T.create ~root () >>= function
      | Error err -> Lwt.return (Error err)
      | Ok t      ->
        reset t >>= fun () ->
        (* XXX(dinosaure): an empty repository has the HEAD reference
           points to refs/heads/master which one does not exists. If
           we want to know if a repository is empty, we need to check
           [Store.Reference.master]. *)
        Store.Ref.mem t Store.Reference.master >>= function
        | true  -> Alcotest.fail "non-empty repository!"
        | false ->
          Sync.fetch_all t uri >>= function
          | Error err -> Lwt.fail (Sync err)
          | Ok ()     ->
            Sync.update t ~reference:Store.Reference.master uri >>= function
            | Error err   -> Lwt.fail (Sync err)
            | Ok []       -> Lwt.return (Ok t)
            | Ok (_ :: _) -> Alcotest.fail "de-synchronization of the git repository"
    in
    let tests =
      List.map (fun uri ->
        let msg = Fmt.strf "fetching %s" uri in
        msg, (fun () ->
            test uri >>= function
            | Ok t      -> Lwt.return t
            | Error err -> Lwt.fail (T.Store err))
        ) uris
    in
    run name tests

  let test_clone name uris =
    let test uri reference =
      let uri = Uri.of_string uri in
      Store.create ~root () >>?= fun t ->
      let clone_tcp t uri =
        Sync.clone t ~reference uri >>= fun _ ->
        Store.list t >>=
        Lwt_list.iter_s (fun hash -> Store.read_exn t hash >|= fun _ -> ())
        >>= fun () ->
        Store.Ref.read t Store.Reference.head >>= function
        | Error _ -> Alcotest.fail "empty clone!"
        | Ok _    -> reset t
      in
      clone_tcp t uri >|= fun () ->
      Ok t
    in
    let tests =
      List.map (fun (uri, reference) ->
          let reference = Store.Reference.Path.(heads / reference) in
          let msg =
            Fmt.strf "cloning %s (branch=%a)" uri Store.Reference.pp reference
          in
          msg, (fun () ->
              test uri reference >>= function
              | Ok t      -> Lwt.return t
              | Error err -> Lwt.fail (T.Store err))
        ) uris
    in
    run name tests

end
