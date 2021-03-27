let () = Random.self_init ()

open Git_unix
module Sync = Sync (Store) (Git_cohttp_unix)

let src = Logs.Src.create "guit-fetch" ~doc:"logs binary event"

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

module Fifo = struct
  open Lwt.Infix

  let ( >>? ) = Lwt_result.bind

  type flow = Lwt_unix.file_descr * Lwt_unix.file_descr
  type endpoint = Fpath.t
  type error = [ `Error of Unix.error * string * string ]
  type write_error = [ `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf (`Error (err, f, v)) =
    Fmt.pf ppf "%s(%s) : %s" f v (Unix.error_message err)

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.pf ppf "Closed by peer"

  let read (ic, _) =
    let tmp = Bytes.create 0x1000 in
    let process () =
      Lwt_unix.read ic tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_ok `Eof
      | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn

  let write (_, oc) ({ Cstruct.len; _ } as cs) =
    let rec process buf off max =
      Lwt_unix.write oc buf off max >>= fun len ->
      if max - len = 0 then Lwt.return_ok ()
      else process buf (off + len) (max - len)
    in
    let buf = Cstruct.to_bytes cs in
    Lwt.catch (fun () -> process buf 0 len) @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn

  let rec writev fd = function
    | [] -> Lwt.return_ok ()
    | x :: r -> write fd x >>? fun () -> writev fd r

  let close (ic, oc) = Lwt_unix.close ic >>= fun () -> Lwt_unix.close oc

  let connect fpath =
    let process () =
      Lwt_unix.openfile (Fpath.to_string fpath ^ "-ic") Unix.[ O_RDONLY ] 0o644
      >>= fun ic ->
      Lwt_unix.openfile (Fpath.to_string fpath ^ "-oc") Unix.[ O_WRONLY ] 0o644
      >>= fun oc -> Lwt.return_ok (ic, oc)
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> raise exn
end

let fifo, _ = Mimic.register ~name:"fifo" (module Fifo)

let ctx =
  let open Mimic in
  let k0 scheme host =
    match scheme, host with
    | `Scheme "fifo", `Domain v -> (
        match Fpath.of_string (Domain_name.to_string v) with
        | Ok v -> Lwt.return_some v
        | Error _ -> Lwt.return_none)
    | `Scheme "fifo", `Name v -> (
        match Fpath.of_string v with
        | Ok v -> Lwt.return_some v
        | Error _ -> Lwt.return_none)
    | _ -> Lwt.return_none
  in
  Mimic.empty
  |> Mimic.fold fifo
       Fun.[ req Smart_git.git_scheme; req Smart_git.git_host ]
       ~k:k0

let main quiet (references : (Git.Reference.t * Git.Reference.t) list)
    (directory : string) (repository : Smart_git.Endpoint.t) :
    (unit, 'error) Lwt_result.t =
  let root =
    (match directory with "" -> Sys.getcwd () | _ -> directory) |> Fpath.v
  in
  let ( >>? ) = Lwt_result.bind in
  let ( >>! ) v f = Lwt_result.map_err f v in
  Store.v root >>! store_err >>? fun store ->
  let push_stdout, push_stderr =
    match quiet with
    | true -> ignore, ignore
    | false -> print_string, prerr_string
  in
  Sync.fetch ~push_stdout ~push_stderr ~ctx repository store (`Some references)
  >>! sync_err
  >>? fun _ -> Lwt.return (Ok ())

open Cmdliner

(* XXX(ulugbekna): We want ogit-fetch to have the following interface:
 * ogit-fetch [-r <path> | --root <path>] [--output <output_channel>]
 * [--progress] <repository> <refspec>... *)

let output =
  let conv' =
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
  Arg.(value & opt conv' Fmt.stdout & info [ "output" ] ~doc ~docv:"<output>")

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

(* XXX(ulugbekna): can be several references of form "remote_ref:local_ref"
 * or "remote_ref", where the latter means that the local_ref should
 * have the same name *)
let references =
  let reference =
    let parse str = Ok (Git.Reference.v str) in
    let print = Git.Reference.pp in
    Arg.conv ~docv:"<ref>" (parse, print)
  in
  let doc = "Reference to pull." in
  Arg.(
    non_empty
    & pos_right 0 (pair ~sep:':' reference reference) []
    & info ~doc ~docv:"<ref>" [])

let setup_logs =
  Term.(
    const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level () $ output)

let main (quiet, _) references directory repository =
  match Lwt_main.run (main quiet references directory repository) with
  | Ok () -> `Ok ()
  | Error (#error as err) -> `Error (false, Fmt.str "%a." pp_error err)

let command =
  let doc = "Fetch a Git repository." in
  let exits = Term.default_exits in
  ( Term.(ret (const main $ setup_logs $ references $ directory $ repository)),
    Term.info "fetch" ~doc ~exits )

let () = Term.(exit @@ eval command)
