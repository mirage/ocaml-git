(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let verbose () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output stderr

let quiet () =
  Log.set_log_level Log.ERROR;
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
  | l  -> Printf.sprintf "[ %s ]" (String.concat ", " (List.map f l))

module Make (S: Store.S) = struct

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let mk equal compare pretty =
    let aux (type a) cmp printer msg =
      let testable: a Alcotest.testable =
        let module M = struct
          type t = a
          let equal = cmp
          let pp fmt t = Format.pp_print_string fmt (printer t)
        end in
        (module M)
      in
      Alcotest.check testable msg
    in
    aux equal pretty,
    aux (cmp_opt equal) (printer_opt pretty),
    aux (cmp_list equal compare) (printer_list pretty)

  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk Hash.equal Hash.compare Hash.to_hex

  let assert_value_equal, assert_value_opt_equal, assert_values_equal =
    mk Value.equal Value.compare Value.pretty

  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk Tag.equal Tag.compare Tag.pretty

  let assert_ref_equal, assert_ref_opt_equal, assert_refs_equal =
    mk Reference.equal Reference.compare Reference.pretty

  let assert_cstruct_equal, assert_cstruct_opt_equal, assert_cstructs_equal =
    mk (=) compare Cstruct.debug

  let assert_pack_index_equal, assert_pack_index_opt_equal, assert_pack_indexes_equal =
    mk Pack_index.Raw.equal Pack_index.Raw.compare Pack_index.Raw.pretty

  let assert_pack_equal, assert_pack_opt_equal, assert_packs_equal =
    mk Pack.equal Pack.compare Pack.pretty

  let assert_index_equal, assert_index_opt_equal, assert_indexs_equal =
    mk Index.equal Index.compare Index.pretty

  let assert_raw_pack_equal, assert_raw_pack_opt_equal, assert_raw_packs_equal =
    mk Pack.Raw.equal Pack.Raw.compare Pack.Raw.pretty

end

let list_files kind dir =
  if Sys.file_exists dir then (
    let s = Lwt_unix.files_of_directory dir in
    let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
    let s = Lwt_stream.map (Filename.concat dir) s in
    let s = Lwt_stream.filter kind s in
    Lwt_stream.to_list s >>= fun l ->
    Lwt.return l
  ) else
    Lwt.return_nil

let directories dir =
  list_files (fun f -> try Sys.is_directory f with _ -> false) dir

let files dir =
  list_files (fun f -> try not (Sys.is_directory f) with _ -> false) dir

let rec_files dir =
  let rec aux accu dir =
    directories dir >>= fun ds ->
    files dir       >>= fun fs ->
    Lwt_list.fold_left_s aux (fs @ accu) ds in
  aux [] dir

let head_contents =
  let module M = struct
    type t = Reference.head_contents
    let equal = Reference.equal_head_contents
    let pp = Reference.pp_head_contents
    end
  in (module M: Alcotest.TESTABLE with type t = M.t)

let sha1 = (module Hash: Alcotest.TESTABLE with type t = Hash.t)

let sha1s =
  let module M = struct
    type t = Hash.Set.t
    let equal = Hash.Set.equal
    let pp fmt t = Format.pp_print_string fmt (Hash.Set.pretty t)
  end
  in (module M: Alcotest.TESTABLE with type t = M.t)
