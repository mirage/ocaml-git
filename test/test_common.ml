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

let pp ppf style h = Fmt.pf ppf "%a " Fmt.(styled style string) h

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("\r%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let verbose () = setup_log (Some Logs.Debug)
let quiet () = setup_log None

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

  let mk equal compare pp =
    let aux (type a) cmp pp msg =
      let testable: a Alcotest.testable =
        let module M = struct
          type t = a
          let equal = cmp
          let pp = pp
        end in
        (module M)
      in
      Alcotest.check testable msg
    in
    aux equal pp,
    aux (cmp_opt equal) Fmt.(option pp),
    aux (cmp_list equal compare) Fmt.(list pp)

  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk Hash.equal Hash.compare Hash.pp

  let assert_value_equal, assert_value_opt_equal, assert_values_equal =
    mk Value.equal Value.compare Value.pp

  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk Tag.equal Tag.compare Tag.pp

  let assert_ref_equal, assert_ref_opt_equal, assert_refs_equal =
    mk Reference.equal Reference.compare Reference.pp

  let assert_cstruct_equal, assert_cstruct_opt_equal, assert_cstructs_equal =
    mk (=) compare (Fmt.of_to_string Cstruct.debug)

  let assert_pack_index_equal, assert_pack_index_opt_equal, assert_pack_indexes_equal =
    mk Pack_index.Raw.equal Pack_index.Raw.compare Pack_index.Raw.pp

  let assert_pack_equal, assert_pack_opt_equal, assert_packs_equal =
    mk Pack.equal Pack.compare Pack.pp

  let assert_index_equal, assert_index_opt_equal, assert_indexs_equal =
    mk Index.equal Index.compare Index.pp

  let assert_raw_pack_equal, assert_raw_pack_opt_equal, assert_raw_packs_equal =
    mk Pack.Raw.equal Pack.Raw.compare Pack.Raw.pp

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
    let pp = Hash.Set.pp
  end
  in (module M: Alcotest.TESTABLE with type t = M.t)
