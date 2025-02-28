let () = Random.self_init ()

open Git_unix
module Sync = Sync (Store)

let src = Logs.Src.create "guit-upload-pack" ~doc:"logs binary event"

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
      let dt_us = 1e-3 *. Int64.to_float (Mtime_clock.elapsed_ns ()) in
      Fmt.kpf k ppf
        ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 10 (Fmt.str "%+04.0fus" dt_us))
        pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_src_and_stamp header tags k fmt
  in
  { Logs.report }

let setup_logs style_renderer level ppf =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ppf);
  let quiet = match level with Some _ -> false | None -> true in
  quiet, ppf

type error = [ `Store of Store.error | `Sync of Sync.error ]

let store_err err = `Store err
let sync_err err = `Sync err

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Store.pp_error err
  | `Sync err -> Fmt.pf ppf "(`Sync %a)" Sync.pp_error err

let main quiet (directory : string) : (unit, 'error) Lwt_result.t =
  let root =
    (match directory with "" -> Sys.getcwd () | _ -> directory) |> Fpath.v
  in
  let ( >>? ) = Lwt_result.bind in
  let ( >>?? ) = Lwt.bind in
  let ( >>! ) v f = Lwt_result.map_error f v in
  Store.v root >>! store_err >>? fun store ->
  let _push_stdout, _push_stderr =
    match quiet with
    | true -> ignore, ignore
    | false -> print_string, prerr_string
  in
  Git_unix.std_in_out_ctx () >>?? fun ctx ->
  Mimic.resolve ctx >>? fun flow ->
  Sync.upload_pack ~flow store >>?? Lwt.return_ok
(* >>! sync_err *)
(* >>? fun _ -> Lwt.return (Ok ()) *)

open Cmdliner

(* XXX(ulugbekna): We want ogit-fetch to have the following interface:
 * ogit-fetch [-r <path> | --root <path>] [--output <output_channel>]
 * [--progress] <repository> <refspec>... *)

let output =
  let converter =
    let parse str =
      match str with
      | "stdout" -> Ok Fmt.stdout
      | "stderr" -> Ok Fmt.stderr
      | s -> Error (`Msg (Fmt.str "%s is not an output." s))
    in
    let print ppf v =
      Fmt.pf ppf "%s" (if v == Fmt.stdout then "stdout" else "stderr")
    in
    Arg.conv ~docv:"<output>" (parse, print)
  in
  let doc =
    "Output of the progress status. Can take values 'stdout' (default) or \
     'stderr'."
  in
  Arg.(
    value & opt converter Fmt.stdout & info [ "output" ] ~doc ~docv:"<output>")

let directory =
  let doc = "Indicate path to repository root containing '.git' folder" in
  Arg.(value & opt string "" & info [ "r"; "root" ] ~doc ~docv:"<directory>")

(* XXX(ulugbekna): passed argument needs to be a URI of the repository *)
let repository =
  let endpoint =
    let parse = Smart_git.Endpoint.of_string in
    let print = Smart_git.Endpoint.pp in
    Arg.conv ~docv:"<uri>" (parse, print)
  in
  let doc = "URI leading to repository" in
  Arg.(
    required & pos 0 (some endpoint) None & info [] ~docv:"<repository>" ~doc)

let setup_logs =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_logs
    $ Fmt_cli.style_renderer ~docs ()
    $ Logs_cli.level ~docs ()
    $ output)

let main (quiet, _) directory =
  match Lwt_main.run (main quiet directory) with
  | Ok () -> Ok ()
  | Error (#error as err) -> Error (Fmt.str "%a." pp_error err)
  | Error _ -> Error "other"

let command =
  let doc = "Answer to a fetch." in
  let info = Cmd.info "upload-pack" ~doc in
  Cmd.v info Term.(const main $ setup_logs $ directory)

let () = exit @@ Cmd.eval_result command
