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

open Core_kernel.Std
open Git

module Bigstring = struct
  include Bigstring
  let compare t1 t2 =
    match Int.compare (Bigstring.length t1) (Bigstring.length t2) with
    | 0 -> String.compare (Bigstring.to_string t1) (Bigstring.to_string t2)
    | i -> i
  let equal t1 t2 =
    Int.equal (Bigstring.length t1) (Bigstring.length t2)
    && String.equal (Bigstring.to_string t1) (Bigstring.to_string t2)

  let pretty b =
    sprintf "%S" (to_string b)
end

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output stderr

let cmp_opt fn x y =
  match x, y with
  | Some x, Some y -> fn x y
  | None  , None   -> true
  | Some _, None
  | None  , Some _ -> false

let printer_opt fn = function
  | None   -> "<none>"
  | Some v -> fn v

let rec cmp_list fn x y =
  match x, y with
  | xh::xt, yh::yt -> fn xh yh && cmp_list fn xt yt
  | []    , []     -> true
  | _              -> false

let printer_list f = function
  | [] -> "[]"
  | l  -> Printf.sprintf "[ %s ]" (String.concat ~sep:", " (List.map ~f l))

let line msg =
  let line () = Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  Log.infof "ASSERT %s" msg;
  line ()

module Make (S: Store.S) = struct

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let mk equal compare pretty =
    let aux cmp printer msg =
      line msg;
      OUnit.assert_equal ~msg ~cmp ~printer in
    aux equal pretty,
    aux (cmp_opt equal) (printer_opt pretty),
    aux (cmp_list equal compare) (printer_list pretty)

  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk SHA1.equal SHA1.compare SHA1.to_hex

  let assert_value_equal, assert_value_opt_equal, assert_values_equal =
    mk Value.equal Value.compare Value.to_string

  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk Tag.equal Tag.compare Tag.to_string

  let assert_ref_equal, assert_ref_opt_equal, assert_refs_equal =
    mk Reference.equal Reference.compare Reference.to_string

  let assert_bigstring_equal, assert_bigstring_opt_equal, assert_bigstrings_equal =
    mk Bigstring.equal Bigstring.compare (fun b ->
        if Bigstring.length b < 40 then Bigstring.pretty b
        else sprintf "%S (%d)" (Bigstring.To_string.subo ~len:40 b) (Bigstring.length b)
      )

  let assert_pack_index_equal, assert_pack_index_opt_equal, assert_pack_indexes_equal =
    mk Pack_index.equal Pack_index.compare Pack_index.pretty

  let assert_pack_equal, assert_pack_opt_equal, assert_packs_equal =
    mk Pack.equal Pack.compare Pack.to_string

end
