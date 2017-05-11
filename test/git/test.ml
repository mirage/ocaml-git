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

open Git
open Lwt.Infix
open Test_common

(* To avoid depending on git-unix *)
module Zlib = Git.Inflate.Make(Zlib)

module SHA1 = struct

  let cstruct buf =
    buf
    |> Nocrypto.Hash.SHA1.digest
    |> Cstruct.to_string
    |> fun x -> Git.Hash.of_raw x

  let string str =
    Cstruct.of_string str
    |> cstruct

  let length = Nocrypto.Hash.SHA1.digest_size

end

module Memory = Git.Mem.Make(SHA1)(Zlib)

let init () =
  Git.Value.Cache.clear ();
  Memory.clear_all ();
  Lwt.return_unit

let suite = {
  name  = "MEM";
  init  = init;
  clean = unit;
  store = (module Memory);
  shell = false;
}

let () =
  Test_store.run "git" [
    `Quick, suite;
  ]
