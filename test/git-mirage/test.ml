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

module C = Conduit_mirage.With_tcp (Tcpip_stack_socket)
module R = Resolver_mirage.Make_with_stack (Mirage_crypto_rng) (Time) (Mclock) (Tcpip_stack_socket)

let run f =
  Lwt_main.run
    (Lwt.catch f (fun e -> Alcotest.failf "cannot connect: %a" Fmt.exn e))

let stack =
  run
  @@ fun () ->
  Tcpv4_socket.connect None
  >>= fun tcp ->
  Udpv4_socket.connect None
  >>= fun udp ->
  let interface = [Ipaddr.V4.of_string_exn "0.0.0.0"] in
  Tcpip_stack_socket.connect interface udp tcp

let conduit = run @@ fun () -> C.connect stack Conduit_mirage.empty

let resolver =
  let ns = None in
  let ns_port = None in
  R.R.init ?ns ?ns_port ~stack ()

module Store = struct include Git.Mem.Store

                      let v root = v root end

module TCP = Test_sync.Make (struct
  module Sync = Git_mirage.Sync (Store)
  module M = Sync.Tcp
  module Store = Store

  type endpoint = Git_mirage.endpoint
  type error = M.error

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_mirage.endpoint ~conduit ~resolver uri

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

let () =
  Mirage_crypto_rng_unix.initialize () ;
  Test_common.verbose () ;
  Alcotest.run "git-mirage"
    [ Test_store.suite "mirage" (module Store)
    ; Test_smart.suite "smart" (module Store)
    ; Test_data.suite "mirage" (module Test_data.Usual) (module Store)
    ; Test_data.suite "mirage" (module Test_data.Bomb) (module Store)
    ; TCP.test_fetch "tcp-sync-fetch" [Uri.of_string "git://localhost/"]
    ; TCP.test_clone "tcp-sync-clone"
        [ Uri.of_string "git://localhost/", "master"
        ; Uri.of_string "git://github.com/mirage/ocaml-git.git", "master" ] ]
