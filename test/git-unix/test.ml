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

module TCP = Test_sync.Make(struct
    module M = Git_unix.Sync(Git_unix.FS)
    module Store = M.Store
    type error = M.error
    let clone t ~reference uri = M.clone t ~reference uri
    let fetch_all t uri = M.fetch_all t uri
    let update t ~reference uri = M.update t ~reference uri
    let kind = `TCP
  end)

(* XXX(dinosaure): the divergence between the TCP API and the HTTP API
   will be update for an homogenization. *)

module HTTP = Test_sync.Make(struct
    module M = Git_unix.HTTP(Git_unix.FS)
    module Store = M.Store
    type error = M.error
    let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri

    exception Jump of Store.Ref.error

    let fetch_all t uri =
      let open Lwt.Infix in

      M.fetch_all t ~references:Store.Reference.Map.empty uri >>= function
      | Error _ as err -> Lwt.return err
      | Ok (_, _, downloaded) ->
        Lwt.try_bind
          (fun () -> Lwt_list.iter_s
              (fun (remote_ref, new_hash) ->
                 Store.Ref.write t remote_ref (Store.Reference.Hash new_hash) >>= function
                 | Ok () -> Lwt.return ()
                 | Error err -> Lwt.fail (Jump err))
              (Store.Reference.Map.bindings downloaded))
          (fun () -> Lwt.return (Ok ()))
          (function Jump err -> Lwt. return (Error (`Ref err))
                  | err -> Lwt.fail err)

    let update t ~reference uri = M.update_and_create t ~references:(Store.Reference.Map.singleton reference [ reference ]) uri
    let kind = `HTTP
  end)

module HTTPS = Test_sync.Make(struct
    module M = Git_unix.HTTP(Git_unix.FS)
    module Store = M.Store
    type error = M.error
    let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri

    exception Jump of Store.Ref.error

    let fetch_all t uri =
      let open Lwt.Infix in

      M.fetch_all t ~references:Store.Reference.Map.empty uri >>= function
      | Error _ as err -> Lwt.return err
      | Ok (_, _, downloaded) ->
        Lwt.try_bind
          (fun () -> Lwt_list.iter_s
              (fun (remote_ref, new_hash) ->
                 Store.Ref.write t remote_ref (Store.Reference.Hash new_hash) >>= function
                 | Ok () -> Lwt.return ()
                 | Error err -> Lwt.fail (Jump err))
              (Store.Reference.Map.bindings downloaded))
          (fun () -> Lwt.return (Ok ()))
          (function Jump err -> Lwt. return (Error (`Ref err))
                  | err -> Lwt.fail err)

    let update t ~reference uri = M.update_and_create t ~references:(Store.Reference.Map.singleton reference [ reference ]) uri
    let kind = `HTTPS
  end)

module MemStore = Git.Mem.Store(Digestif.SHA1)
module UnixStore = Git_unix.FS

let mem_backend =
  { name  = "mem"
  ; store = (module MemStore)
  ; shell = false }

let unix_backend =
  { name  = "unix"
  ; store = (module UnixStore)
  ; shell = true }

let () =
  verbose ();
  Alcotest.run "git-unix"
    [ Test_store.suite (`Quick, mem_backend)
    ; Test_store.suite (`Quick, unix_backend)
    ; TCP.suite   { mem_backend  with name = "mem-tcp-sync"    }
    ; TCP.suite   { unix_backend with name = "unix-tcp-sync"   }
    ; HTTP.suite  { mem_backend  with name = "mem-http-sync"   }
    ; HTTPS.suite { unix_backend with name = "unix-https-sync" } ]
