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

let () = Random.self_init ()

open Git_unix
module Sync_http = Http(Store)

module Option =
struct

  let map f = function
    | Some v -> Some (f v)
    | None -> None

  let map_default v f = function
    | Some v -> f v
    | None -> v

  let value_exn f = function
    | Some v -> v
    | None -> f ()

  let eq ?(none = false)~eq = function
    | Some x -> eq x
    | None -> none
end

let pad n x =
  if String.length x > n
  then x
  else x ^ String.make (n - String.length x) ' '

let pp_header ppf (level, header) =
  let level_style =
    match level with
    | Logs.App -> Logs_fmt.app_style
    | Logs.Debug -> Logs_fmt.debug_style
    | Logs.Warning -> Logs_fmt.warn_style
    | Logs.Error -> Logs_fmt.err_style
    | Logs.Info -> Logs_fmt.info_style
  in
  let level = Logs.level_to_string (Some level) in

  Fmt.pf ppf "[%a][%a]"
    (Fmt.styled level_style Fmt.string) level
    (Fmt.option Fmt.string) (Option.map (pad 10) header)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 10 (Fmt.strf "%+04.0fus" dt))
        pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_src_and_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stdout);
  let quiet = match style_renderer with Some _ -> true | None -> false in
  quiet, Fmt.stdout

type error =
  [ `Store of Store.error
  | `Sync of Sync_http.error ]

let store_err err = `Store err
let sync_err err = `Sync err

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Store.pp_error err
  | `Sync err -> Fmt.pf ppf "(`Sync %a)" Sync_http.pp_error err

let main show_tags show_heads repository =
  let root = Fpath.(v (Sys.getcwd ())) in

  let ( >>?= ) = Lwt_result.bind in
  let ( >>!= ) v f = Lwt_result.map_err f v in

  let https = Option.eq ~eq:((=) "https") (Uri.scheme repository) in

  Store.v ~root ()
  >>!= store_err
  >>?= fun git ->
  Sync_http.ls git ~https ?port:(Uri.port repository)
     (Option.value_exn
        (fun () -> raise (Failure "Invalid repository: no host."))
        (Uri.host repository))
     (Uri.path_and_query repository)
  >>!= sync_err
  >>?= fun { Sync_http.Common.refs; _ } ->
  let refs =
    List.filter (fun (_, reference, _) ->
        let path = Store.Reference.to_path reference in
        (List.exists ((=) "tags") (Fpath.segs path) && show_tags)
        || (List.exists ((=) "heads") (Fpath.segs path) && show_heads))
      refs
  in

  List.iter (fun (hash, reference, peeled) ->
      Fmt.(pf stdout) "%a      %a%s\n%!"
        Store.Hash.pp hash
        Store.Reference.pp reference
        (if peeled then "^{}" else ""))
    refs;

  Lwt.return (Ok ())

open Cmdliner

module Flag =
struct
  let uri =
    let parse str = Ok (Uri.of_string str) in
    let print = Uri.pp_hum in
    Arg.conv ~docv:"<uri>" (parse, print)

  let show =
    Arg.(value & vflag_all [ `Tags; `Heads; ]
           [ `Tags, info ["t"; "tags"] ~doc:"Limit to only refs/tags."
           ; `Heads, info ["h"; "heads"] ~doc:"Limit to only refs/heads." ])

  let repository =
    let doc = "" in
    Arg.(required & pos ~rev:true 0 (some uri) None & info [] ~docv:"<repository>" ~doc)
end

let setup_log =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main shows repository _ =
  let show_tags = List.exists ((=) `Tags) shows in
  let show_heads = List.exists ((=) `Heads) shows in
  match Lwt_main.run (main show_tags show_heads repository) with
  | Ok () -> `Ok ()
  | Error (#error as err) -> `Error (false, Fmt.strf "%a" pp_error err)

let command =
  let doc = "Clone a Git repository by the HTTP protocol." in
  let exits = Term.default_exits in
  Term.(ret (const main $ Flag.show $ Flag.repository $ setup_log)),
  Term.info "ogit-http-clone" ~version:"v0.1" ~doc ~exits

let () = Term.(exit @@ eval command)
