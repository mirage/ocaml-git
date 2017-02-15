(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Test_common
open Git_mirage
open Result

(* FIXME: this should probably go somewhere else ... *)

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
    else (
      let clear =
        if Sys.file_exists dir then (
          safe Lwt_unix.unlink dir
        ) else
          Lwt.return_unit
      in
      clear >>= fun () ->
      aux (Filename.dirname dir) >>= fun () ->
      protect (Lwt_unix.mkdir dir) 0o755;
    ) in
  aux dirname

let command fmt =
  Printf.ksprintf (fun str ->
      Printf.printf "[exec] %s\n%!" str;
      let i = Sys.command str in
      if i <> 0 then Printf.printf "[exec] error %d\n%!" i;
      ()
    ) fmt

let rmdir dir =
    if Sys.os_type = "Win32" then
      command "cmd /d /v:off /c rd /s /q %S" dir
    else
      command "rm -rf %S" dir

module M = struct

  let root = "test-db"

  let (>>|) x f =
    x >>= function
    | Ok x    -> f x
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" FS_unix.pp_write_error e

  let init () =
    rmdir root;
    mkdir root

  include FS_unix

  let connect () = FS_unix.connect root

end

module S = FS(M)(SHA1_slow)(Git.Inflate.Make(Zlib))

let suite =
  {
    name  = "MIRAGE";
    init  = M.init;
    clean = unit;
    store = (module S);
    shell = true;
  }

let extra = [
  "SHA1-mirage", Test_store.array (module Git_mirage.SHA1_slow);
]

let () =
  Test_store.run ~extra "git-mirage" [
    `Quick, suite;
  ]
