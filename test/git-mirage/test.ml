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

let protect_unix_exn = function
  | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
  | e -> Lwt.fail e

let ignore_enoent = function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
  | e -> Lwt.fail e

let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn
let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

let mkdir dirname =
  let rec aux dir =
    if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
    else
      let clear =
        if Sys.file_exists dir then safe Lwt_unix.unlink dir else Lwt.return ()
      in
      clear
      >>= fun () ->
      aux (Filename.dirname dir)
      >>= fun () -> protect (Lwt_unix.mkdir dir) 0o755
  in
  aux dirname

let command fmt =
  Printf.ksprintf
    (fun str ->
      Fmt.(pf stdout) "[exec] %s\n%!" str ;
      let i = Sys.command str in
      if i <> 0 then Fmt.(pf stderr) "[exec] error %d\n%!" i ;
      () )
    fmt

let rmdir dir =
  if Sys.os_type = "Win32" then command "cmd /d /v:off /c rd /s /q %S" dir
  else command "rm -rf %S" dir

module M = struct
  let root = "test-git-mirage-store"

  let init () =
    if Sys.file_exists root then rmdir root ;
    mkdir root >>= fun () -> mkdir Filename.(concat root "temp")

  include FS_unix

  let path p =
    if Sys.os_type <> "Win32" then p
    else
      let segs = Fpath.(segs (normalize (v p))) in
      String.concat "/" segs

  let read x p = read x (path p)
  let size x p = size x (path p)
  let create x p = create x (path p)
  let mkdir x p = mkdir x (path p)
  let destroy x p = destroy x (path p)
  let stat x p = stat x (path p)
  let listdir x p = listdir x (path p)
  let write x p = write x (path p)
  let connect () = FS_unix.connect root
end

module Gamma = struct end

module MirageStore = struct
  include Git_mirage.Store (M)

  let current_dir = Fpath.v M.root
  let v root = M.connect () >>= fun fs -> v ~current_dir fs root
end

module C = Conduit_mirage.With_tcp (Tcpip_stack_socket)
module R = Resolver_mirage.Make_with_stack (Time) (Tcpip_stack_socket)

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

module Store = struct
  include Git.Mem.Store

  let v root = v root
end

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
  Test_common.verbose () ;
  let () = Lwt_main.run (M.init ()) in
  Alcotest.run "git-mirage"
    [ Test_store.suite "mirage" (module MirageStore)
    ; Test_smart.suite "smart" (module MirageStore)
    ; Test_data.suite "mirage" (module Test_data.Usual) (module MirageStore)
    ; Test_data.suite "mirage" (module Test_data.Bomb) (module MirageStore)
    ; TCP.test_fetch "tcp-sync-fetch" [Uri.of_string "git://localhost/"]
    ; TCP.test_clone "tcp-sync-clone"
        [ Uri.of_string "git://localhost/", "master"
        ; Uri.of_string "git://github.com/mirage/ocaml-git.git", "master" ] ]
