(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Test_store
open Git_mirage

let test_img = "test.img"

let command fmt =
  Printf.ksprintf (fun str ->
      Printf.printf "[x] %s\n" str;
      let _ = Sys.command str in
      ()
    ) fmt

module M = struct

  include FS_unix

  let (>>|) x f =
    x >>= function
    | `Ok x    -> f x
    | `Error e -> fail (Failure (string_of_error e))

  let connect () =
    connect "mirage-fs"

  let init () =
    command "rm -rf mirage-fs";
    connect ()  >>| fun t ->
    mkdir t "/" >>| fun () ->
    return_unit

end

module S = FS(M)

let suite =
  {
    name  = "MIR-FS-mirage";
    init  = M.init;
    clean = unit;
    store = (module S);
  }
