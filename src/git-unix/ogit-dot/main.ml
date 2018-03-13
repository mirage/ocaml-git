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

module Store = Git_unix.Store
module Graph = Git.Object_graph.Make(Store)

let pp_level ppf level =
  let style = match level with
    | Logs.App -> Logs_fmt.app_style
    | Logs.Debug -> Logs_fmt.debug_style
    | Logs.Error -> Logs_fmt.err_style
    | Logs.Info -> Logs_fmt.info_style
    | Logs.Warning -> Logs_fmt.warn_style
  in

  (Fmt.styled style (Fmt.const Fmt.string (match level with
      | Logs.App -> "APP"
      | Logs.Debug -> "DEBUG"
      | Logs.Error -> "ERROR"
      | Logs.Info -> "INFO"
      | Logs.Warning -> "WARNING"))) ppf ()

let pp_header ppf (level, title) =
  Fmt.pf ppf "[%a][%a]" pp_level level (Fmt.option ~none:Fmt.nop Fmt.string) title

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_src h _ k ppf fmt =
      let src = Logs.Src.name src in
      Format.kfprintf k ppf ("%a %a: @[" ^^ fmt ^^ "@]@.")
        pp_header (level, h) (Fmt.styled `Magenta Fmt.string) src
    in
    msgf @@ fun ?header ?tags fmt -> with_src header tags k ppf fmt
  in
  { Logs.report = report }

let () = Printexc.record_backtrace true
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let main ppf =
  let open Lwt_result in
  Store.create () >>= fun t ->
  Lwt.Infix.(Graph.to_dot t ppf >>= fun () -> Lwt.return (Ok ()))

let () =
  let filename =
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else "graph.dot"
  in
  let oc = open_out filename in
  let ppf = Format.formatter_of_out_channel oc in

  let () = match Lwt_main.run (main ppf) with
    | Ok () -> ()
    | Error err -> Fmt.pf Fmt.stderr "Error: %a.\n%!" Store.pp_error err
  in

  close_out oc
