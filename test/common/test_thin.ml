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

module Make (Sync: Test_sync.SYNC) = struct
  module Store = Sync.Store
  module Test_store = Test_store.Make(Store)

  let root = Fpath.v "test-decompress"
  let thin = Fpath.(v ".." / "data" / "thin.pack")
  let uri = Uri.of_string "http://github.com/mirage/decompress.git"

  let run name tests: unit Alcotest.test =
    name, List.map (fun (msg, f) -> msg, `Slow, fun () -> Test_store.run f) tests

  let test_clone () =
    Test_store.create ~root () >>= fun t ->
    Store.Ref.mem t Store.Reference.master >>= function
    | true  -> Alcotest.fail "non-empty repository"
    | false ->
      Sync.clone t ~reference:Store.Reference.master uri >|= function
      | Error err -> Alcotest.failf "%a" Sync.pp_error err
      | Ok () -> ()

  let stream_of_filename filename =
    try
      let ic = open_in (Fpath.to_string filename) in
      let tp = Bytes.create 0x800 in
      let rs = Cstruct.create 0x800 in
      let stream () =
        match input ic tp 0 0x800 with
        | 0 -> Lwt.return_none (* XXX(dinosaure): behavior with a file: when we
                                  return 0, it's the end of the file. *)
        | ln -> Cstruct.blit_from_bytes tp 0 rs 0 ln;
          Lwt.return (Some (Cstruct.sub rs 0 ln))
        | exception End_of_file ->
          close_in ic;
          Lwt.return None in
      stream
    with exn -> Alcotest.failf "%s" (Printexc.to_string exn)

  let test_thin () =
    let stream = stream_of_filename thin in
    Store.v root >>= Test_store.check_err >>= fun t ->
    Store.Ref.mem t Store.Reference.master >>= function
    | false -> Alcotest.fail "empty repository"
    | true ->
      Store.Pack.from t stream >>= function
      | Ok (_hash, _n) -> Lwt.return ()
      | Error err -> Alcotest.failf "%a" Store.pp_error err

  let test_thin = run "thin" [ "clone", test_clone
                             ; "thin",  test_thin ]
end
