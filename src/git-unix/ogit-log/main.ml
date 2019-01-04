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

open Git_unix
module Graph = Git.Object_graph.Make (Store)

let pp_level ppf level =
  let style =
    match level with
    | Logs.App -> Logs_fmt.app_style
    | Logs.Debug -> Logs_fmt.debug_style
    | Logs.Error -> Logs_fmt.err_style
    | Logs.Info -> Logs_fmt.info_style
    | Logs.Warning -> Logs_fmt.warn_style
  in
  (Fmt.styled style
     (Fmt.const Fmt.string
        ( match level with
        | Logs.App -> "APP"
        | Logs.Debug -> "DEBUG"
        | Logs.Error -> "ERROR"
        | Logs.Info -> "INFO"
        | Logs.Warning -> "WARNING" )))
    ppf ()

let pp_header ppf (level, title) =
  Fmt.pf ppf "[%a][%a]" pp_level level
    (Fmt.option ~none:Fmt.nop Fmt.string)
    title

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_src h _ k ppf fmt =
      let src = Logs.Src.name src in
      Format.kfprintf k ppf
        ("%a %a: @[" ^^ fmt ^^ "@]@.")
        pp_header (level, h)
        (Fmt.styled `Magenta Fmt.string)
        src
    in
    msgf @@ fun ?header ?tags fmt -> with_src header tags k ppf fmt
  in
  {Logs.report}

let () = Printexc.record_backtrace true
let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Error)
let root = Fpath.(v (Sys.getcwd ()))

let log_commit t h =
  let open Lwt_result in
  Store.read t h
  >>= fun v ->
    match v with
    | Commit c ->
      let msg = Store.Value.Commit.message c in
      let aut = Store.Value.Commit.author c in
      let date, tz_off = aut.date in
      let date = Ptime.of_float_s (Int64.to_float date) in
      let tz_off = match tz_off with
      | Some off -> (match off.sign with
        | `Plus -> off.hours * 3600 + off.minutes * 60
        | `Minus -> - (off.hours * 3600 + off.minutes * 60))
      | None -> 0 in
      Format.printf "\x1b[33mcommit %a\x1b[0m\n%!" Digestif.SHA1.pp h;
      Format.printf "Author: %s <%s>\n%!" aut.name aut.email;
      (match date with
      | Some date ->
        Format.printf "Date: %a\n%!" (Ptime.pp_human ~tz_offset_s:tz_off ()) date
      | None -> ());
      Format.printf "\n    %s\n\n%!" (String.trim msg);
      Lwt.return_ok ()
    | _ -> Lwt.return_ok ()

let main _ppf =
  let open Lwt_result in
  Store.v root
  >>= fun t -> Lwt.Infix.(
    Graph.of_commits t
    >>= fun g -> Lwt_list.map_s (log_commit t) (Graph.keys g)
    >>= fun _ -> Lwt.return_ok ())

let () =
  let ppf = Format.std_formatter in
  match Lwt_main.run (main ppf) with
  | Ok () -> ()
  | Error err -> Fmt.pf Fmt.stderr "Error: %a.\n%!" Store.pp_error err
