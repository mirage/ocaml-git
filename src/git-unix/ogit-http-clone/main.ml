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

module Sync_http = Git_unix.HTTP(Git_unix.FS)

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

  let is_some = function
    | Some _ -> true
    | None -> false

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

let setup_logs style_renderer level ppf =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ppf);
  let quiet = match style_renderer with Some _ -> true | None -> false in
  quiet, ppf

type error =
  [ `Store of Git_unix.FS.error
  | `Sync of Sync_http.error ]

let store_err err = `Store err
let sync_err err = `Sync err

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Git_unix.FS.pp_error err
  | `Sync err -> Fmt.pf ppf "(`Sync %a)" Sync_http.pp_error err

let main ppf progress origin branch repository directory =
  let name =
    Uri.path repository
    |> Astring.String.cuts ~empty:false ~sep:Fpath.dir_sep
    |> List.rev
    |> List.hd
    |> Fpath.v
    |> Fpath.rem_ext ~multi:true
    |> Fpath.basename
  in
  let root = Option.map_default Fpath.(v (Sys.getcwd ()) / name) Fpath.v directory in

  let ( >>?= ) = Lwt_result.bind in
  let ( >>!= ) v f = Lwt_result.map_err f v in

  let stdout =
    if progress
    then Some (fun raw -> Fmt.pf ppf "%s%!" (Cstruct.to_string raw); Lwt.return ())
    else None
  in

  let stderr =
    if progress
    then Some (fun raw -> Fmt.(pf stderr) "%s%!" (Cstruct.to_string raw); Lwt.return ())
    else None
  in

  let https = Option.eq ~eq:((=) "https") (Uri.scheme repository) in

  Git_unix.FS.create ~root ()
  >>!= store_err
  >>?= fun git ->
  Sync_http.clone_ext git ?stdout ?stderr ~https
    ?port:(Uri.port repository) ~reference:branch
    (Option.value_exn
       (fun () -> raise (Failure "Invalid repository: no host."))
       (Uri.host repository))
    (Uri.path_and_query repository)
  >>!= sync_err
  >>?= fun hash ->
  Git_unix.FS.Ref.write git
    branch (Git_unix.FS.Reference.Hash hash)
  >>!= store_err
  >>?= fun _ ->
  let branch_name = Fpath.base (Git_unix.FS.Reference.to_path branch) in

  Git_unix.FS.Ref.write git
    (Git_unix.FS.Reference.of_path Fpath.(v "remotes" / origin // branch_name))
    (Git_unix.FS.Reference.Hash hash)
  >>!= store_err
  >>?= fun _ ->
  Git_unix.FS.Ref.write git
    Git_unix.FS.Reference.head
    (Git_unix.FS.Reference.Ref branch)
  >>!= store_err
  >>?= fun _ -> Lwt.return (Ok ())

open Cmdliner

module Flag =
struct
  let output_value =
    let parse str = match str with
      | "stdout" -> Ok Fmt.stdout
      | "stderr" -> Ok Fmt.stderr
      | s -> Error (`Msg (Fmt.strf "%s is not an output." s))
    in
    let print ppf v = Fmt.pf ppf "%s"
        (if v == Fmt.stdout
         then "stdout"
         else "stderr")
    in
    Arg.conv ~docv:"<output>" (parse, print)

  let output =
    let doc =
      "Output of the progress status"
    in
    Arg.(value
         & opt output_value Fmt.stdout
         & info [ "output" ] ~doc ~docv:"<output>")

  let progress =
    let doc =
      "Progress status is reported on the standard error stream by default when it is \
       attached to a terminal, unless -q is specified. This flag forces progress status \
       even if the standard error stream is not directed to a terminal."
    in
    Arg.(value & flag & info ["progress"] ~doc)

  let origin =
    let doc =
      "Instead of using the remote name origin to keep track of the upstream repository, use \
       <name>."
    in
    Arg.(value & opt string "origin" & info ["o"; "origin"] ~doc ~docv:"<name>")

  let reference =
    let parse str = Ok (Git_unix.FS.Reference.of_string str) in
    let print = Git_unix.FS.Reference.pp in
    Arg.conv ~docv:"<name>" (parse, print)

  let branch =
    let doc =
      "Instead of pointing the newly created HEAD to the branch pointed to by the cloned \
       repository's HEAD, point to <name> branch instead. --branch can also take tags and \
       detaches the HEAD at that commit in the resulting repository."
    in
    Arg.(value
         & opt reference Git_unix.FS.Reference.master
         & info ["b"; "branch"] ~doc ~docv:"<name>")

  let uri =
    let parse str = Ok (Uri.of_string str) in
    let print = Uri.pp_hum in
    Arg.conv ~docv:"<uri>" (parse, print)

  let repository =
    let doc = "" in
    Arg.(required & pos ~rev:true 0 (some uri) None & info [] ~docv:"<repository>" ~doc)

  let directory =
    let doc = "" in
    Arg.(value & pos ~rev:true 1 (some string) None & info [] ~doc ~docv:"<directory>")
end

let setup_log =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level () $ Flag.output)

let main progress origin branch directory repository (quiet, ppf) =
  match Lwt_main.run (main ppf (not quiet && progress) origin branch directory repository) with
  | Ok () -> `Ok ()
  | Error (#error as err) -> `Error (false, Fmt.strf "%a" pp_error err)

let command =
  let doc = "Clone a Git repository by the HTTP protocol." in
  let exits = Term.default_exits in
  Term.(ret (const main $ Flag.progress $ Flag.origin $ Flag.branch $ Flag.repository $ Flag.directory $ setup_log)),
  Term.info "ogit-http-clone" ~version:"v0.1" ~doc ~exits

let () = Term.(exit @@ eval command)
