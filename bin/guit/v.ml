let () = Random.self_init ()

open Rresult
open Git_unix

let src = Logs.Src.create "guit-v" ~doc:"logs binary event"

module Log = (val Logs.src_log src : Logs.LOG)

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

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
    (Fmt.styled level_style Fmt.string)
    level (Fmt.option Fmt.string)
    (Option.map (pad 10) header)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 10 (Fmt.str "%+04.0fus" dt))
        pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_src_and_stamp header tags k fmt
  in
  { Logs.report }

let setup_logs style_renderer level =
  let quiet = Option.is_none level in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  quiet

let main quiet path =
  let root =
    match path with Some path -> path | None -> Fpath.v (Sys.getcwd ())
  in
  let open Lwt.Infix in
  Store.v root >>= function
  | Ok _t when not quiet ->
      Fmt.pr "%a ready to be used as a Git store.\n%!" Fpath.pp root;
      Lwt.return_ok ()
  | Ok _ -> Lwt.return_ok ()
  | Error err -> Lwt.return_error (R.msgf "%a" Store.pp_error err)

let main logs path =
  match Lwt_main.run (main logs path) with
  | Ok () -> Ok ()
  | Error (`Msg err) -> Error (Fmt.str "%s." err)

open Cmdliner

let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let directory =
  let doc = "Indicate path to repository root containing '.git' folder" in
  Arg.(
    value
    & opt (some fpath) None
    & info [ "r"; "root" ] ~doc ~docv:"<directory>")

let setup_logs =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_logs $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let command =
  let doc = "Make a Git repository." in
  let info = Cmd.info "v" ~doc in
  Cmd.v info Term.(const main $ setup_logs $ directory)

let () = exit @@ Cmd.eval_result command
