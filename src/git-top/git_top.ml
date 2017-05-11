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

let printers = [
  "Git.SHA.pp";
  "Git.SHA.Blob.pp";
  "Git.SHA.Tree.pp";
  "Git.SHA.Commit.pp";
  "Git.Blob.pp";
  "Git.Tree.pp";
  "Git.Commit.pp";
  "Git.Value.pp";
  "Git.Index.pp";
  "Git.Index.pp_mode";
  "Git.Index.pp_stats";
  "Git.Index.pp_entry";
  "Git.Object_type.pp";
  "Git.Pack.pp";
  (*  "Git.Pack_index.pp_hum"; *)
  "Git.Packed_value.pp";
  "Git.Packed_refs.pp";
  "Git.Reference.pp";
  "Git.Reference.pp_head_contents";
  "Git.Tag.pp";
  "Git.User.pp";
]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing git-printers@."
