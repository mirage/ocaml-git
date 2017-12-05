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

let () = Printexc.record_backtrace true

open Test_common

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
        if Sys.file_exists dir
        then safe Lwt_unix.unlink dir
        else Lwt.return ()
      in
      clear >>= fun () ->
      aux (Filename.dirname dir) >>= fun () ->
      protect (Lwt_unix.mkdir dir) 0o755;
    in
  aux dirname

let command fmt =
  Printf.ksprintf
    (fun str ->
      Fmt.(pf stdout) "[exec] %s\n%!" str;
      let i = Sys.command str in
      if i <> 0 then Fmt.(pf stderr) "[exec] error %d\n%!" i;
      ())
    fmt

let rmdir dir =
  if Sys.os_type = "Win32" then
    command "cmd /d /v:off /c rd /s /q %S" dir
  else
    command "rm -rf %S" dir

module M = struct
  let root = "test-git-mirage-store"

  let ( >>| ) x f =
    x >>= function
    | Ok x    -> f x
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" FS_unix.pp_write_error e

  let init () =
    if Sys.file_exists root then rmdir root;
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

module Gamma = struct
  type path = Fpath.t

  let temp = Fpath.(v M.root / "temp")
  let current = Fpath.v M.root
end

module Fs = Git_mirage.FS.Make(Gamma)(M)
module MirageStore = Git_mirage.Make(Git.Mem.Lock)(Fs)

let mirage_backend =
  { name  = "mirage"
  ; store = (module MirageStore)
  ; shell = true }


let () =
  verbose ();
  let () = Lwt_main.run (M.init ()) in
  Alcotest.run "git-mirage"
    [ Test_store.suite (`Quick, mirage_backend) ]
