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

let command fmt =
  Printf.ksprintf (fun str ->
      Printf.printf "[exec] %s\n" str;
      let _ = Sys.command str in
      ()
    ) fmt

module M = struct

  include FS_unix

  let root = "test-db"

  let (>>|) x f =
    x >>= function
    | Ok x    -> f x
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" FS_unix.pp_write_error e

  let connect () = connect root

  let init () =
    command "rm -rf %s" root;
    command "mkdir %s" root;
    connect ()  >>= fun t ->
    mkdir t "/" >>| fun () ->
    Lwt.return_unit

end

module S = FS(M)(SHA1_slow)(Git.Inflate.Make(Zlib))

let suite =
  {
    name  = "MIRAGE-FS";
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
