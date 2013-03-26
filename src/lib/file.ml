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

module Name = Abstract.String

module Dirname = struct
  include Abstract.String
  let basename d =
    let s = to_string d in
    Filename.basename s
end

let read filename =
  let ic = open_in_bin (Name.to_string filename) in
  let n = in_channel_length ic in
  let buf = String.create n in
  really_input ic buf 0 n;
  close_in ic;
  buf

let write filename contents =
  let oc = open_out_bin (Name.to_string filename) in
  output_string oc contents;
  close_out oc

let exists filename =
  Sys.file_exists (Name.to_string filename)

let dirname filename =
  Dirname.of_string (Filename.dirname (Name.to_string filename))

let basename filename =
  Filename.basename (Name.to_string filename)

let mkdir dirname =
  let rec aux dir =
    if Sys.file_exists dir then ()
    else (
      aux (Filename.dirname dir);
      Unix.mkdir dir 0o755
    ) in
  aux (Dirname.to_string dirname)

let list_files kind dir =
  let dir = Dirname.to_string dir in
  if Sys.file_exists dir then
    let d = Sys.readdir dir in
    let d = Array.to_list d in
    let d = List.rev_map (Filename.concat dir) d in
    let d = List.filter kind d in
    List.sort compare d
  else
    []

let directories dir =
  let l = list_files (fun f -> try Sys.is_directory f with _ -> false) dir in
  List.map Dirname.of_string l

let files dir =
  let l = list_files (fun f -> try not (Sys.is_directory f) with _ -> false) dir in
  List.map Name.of_string l

let rec_files dir =
  let rec aux accu dir =
    let d = directories dir in
    let f = files dir in
    List.fold_left aux (f @ accu) d in
  aux [] dir

module OP = struct

  let concat dir string =
    Filename.concat (Dirname.to_string dir) string

  let (/) dir string =
    Dirname.of_string (concat dir string)

  let (//) dir string =
    Name.of_string (concat dir string)

end
