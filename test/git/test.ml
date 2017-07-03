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

open Test_common

let init () =
  Git.Value.Cache.clear ();
  Git.Mem.clear_all ();
  Lwt.return_unit

let suite = {
  name  = "MEM";
  init  = init;
  clean = unit;
  store = (module Git.Mem);
  shell = false;
}

let extra = [
  "SHA1-unix"  , Test_store.array (module Git.Hash.SHA1);
  "SHA256-unix", Test_store.array (module Git.Hash.SHA256);
]

let () =
  Test_store.run ~extra "git" [
    `Quick, suite;
  ]
