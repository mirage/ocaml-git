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

module Store = Git.Store.Make(Sha1)(Fpath)(Fs_lwt_unix.Lock)(Fs_lwt_unix.Fs)(Ocaml_inflate)(Ocaml_deflate)
module Graph = Git.Object_graph.Make(Store)

let printers =
  [ "Git.Crc32.pp"
  ; "Git.User.pp"
  ; "Git.Minienc.pp"
  ; "Git_top.Store.Value.pp"
  ; "Git_top.Store.Value.Commit.pp"
  ; "Git_top.Store.Value.Blob.pp"
  ; "Git_top.Store.Value.Tree.pp"
  ; "Git_top.Store.Value.Tag.pp"
  ; "Git_top.Store.Hash.pp"
  ; "Git_top.Store.Reference.pp"
  ; "Git_top.Store.Inflate.pp"
  (* ; "Backend.Store.pp" *)
  ; "Git_top.Store.Path.pp"
  ; "Git_top.Store.pp_error" ]

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

let () = Printexc.record_backtrace true
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_reporter (Logs_fmt.reporter ~dst:Fmt.stderr ~pp_header:Logs_fmt.pp_header ())
let () = Logs.set_level ~all:true (Some Logs.Debug)
