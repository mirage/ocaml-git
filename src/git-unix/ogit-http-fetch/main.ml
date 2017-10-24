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

module Client =
struct
  type headers = Cohttp.Header.t
  type resp = Git_http.Web_cohttp_lwt.resp =
    { resp : Cohttp.Response.t
    ; body : Cohttp_lwt.Body.t }
  type body = unit -> (Cstruct.t * int * int) option Lwt.t
  type meth = Cohttp.Code.meth
  type uri = Uri.t

  type +'a io = 'a Lwt.t

  module Log =
  struct
    let src = Logs.Src.create "cohttp" ~doc:"logs cohttp event"
    include (val Logs.src_log src : Logs.LOG)
  end

  let call ?headers ?body meth uri =
    let open Lwt.Infix in

    let body = match body with
      | None -> None
      | Some stream ->
        Lwt_stream.from stream
        |> Lwt_stream.map (fun (buf, off, len) -> Cstruct.to_string (Cstruct.sub buf off len))
        |> fun stream -> Some (`Stream stream)
    in

    (* XXX(dinosaure): [~chunked:true] is mandatory, I don't want to
       explain why (I lost one day to find this bug) but believe me. *)
    Cohttp_lwt_unix.Client.call ?headers ?body ~chunked:false meth uri >>= fun ((resp, _) as v) ->
    if Cohttp.Code.is_redirection (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
    then begin
      let uri =
        Cohttp.Response.headers resp
        |> Cohttp.Header.to_list
        |> List.assoc "location"
        |> Uri.of_string
      in

      Log.info (fun l -> l ~header:"call" "Redirection to %a." Uri.pp_hum uri);

      Cohttp_lwt_unix.Client.call ?headers ?body ~chunked:false meth uri >>= fun (resp, body) ->
      Lwt.return { resp; body; }
    end else Lwt.return { resp; body = snd v; }
end

module Negociator = Git.Negociator.Make(Git_unix.Store)
module Sync_http = Git_http.Make(Git_http.Default)(Client)(Git_unix.Store)


module Log =
struct
  let src = Logs.Src.create "main" ~doc:"logs binary event"
  include (val Logs.src_log src : Logs.LOG)
end

let option_map f = function
  | Some v -> Some (f v)
  | None -> None

let option_map_default v f = function
  | Some v -> f v
  | None -> v

let option_value_exn f = function
  | Some v -> v
  | None -> f ()

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
    (Fmt.option Fmt.string) (option_map (pad 10) header)

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
  [ `Store of Git_unix.Store.error
  | `Reference of Git_unix.Store.Ref.error
  | `Sync of Sync_http.error ]

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Git_unix.Store.pp_error err
  | `Reference err -> Fmt.pf ppf "(`Reference %a)" Git_unix.Store.Ref.pp_error err
  | `Sync err -> Fmt.pf ppf "(`Sync %a)" Sync_http.pp_error err

let main ppf progress _ _ directory repository =
  let name =
    Uri.path repository
    |> Astring.String.cuts ~empty:false ~sep:Fpath.dir_sep
    |> List.rev
    |> List.hd
    |> Fpath.v
    |> Fpath.rem_ext ~multi:true
    |> Fpath.basename
  in
  let root = option_map_default Fpath.(v (Sys.getcwd ()) / name) Fpath.v directory in

  let open Lwt_result in

  let ( >!= ) v f = map_err f v in

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

  let https =
    match Uri.scheme repository with
    | Some "https" -> true
    | _ -> false
  in

  let want lst =
    try let (master, _, _) =
          List.find (function
              | (_, refname, false) -> Git_unix.Store.Reference.(equal master (of_string refname))
              | _ -> false) lst
      in Lwt.return [ master ]
    with Not_found -> Lwt.return []
  in

  Log.debug (fun l -> l ~header:"main" "root:%a, repository:%a.\n"
                Fpath.pp root Uri.pp_hum repository);

  (Git_unix.Store.create ~root () >!= fun err -> `Store err) >>= fun git ->
  (ok (Negociator.find_common git)) >>= fun (has, state, continue) ->
  let continue { Sync_http.Decoder.acks; shallow; unshallow } state = continue { Git.Negociator.acks; shallow; unshallow } state in
  (* structural typing god! *)

  (Sync_http.fetch git ?stdout ?stderr ~https ~negociate:(continue, state) ~has ~want ?port:(Uri.port repository)
     (option_value_exn
        (fun () -> raise (Failure "Invalid repository: no host."))
        (Uri.host repository))
     (Uri.path_and_query repository)
   >!= fun err -> `Sync err) >>= function
  | [ hash' ], n ->
    Log.debug (fun l -> l ~header:"main" "New version (%d object(s) added): %a." n Git_unix.Store.Hash.pp hash');
    (Git_unix.Store.Ref.write git ~locks:(Git_unix.Store.dotgit git) Git_unix.Store.Reference.master
       (Git_unix.Store.Reference.Hash hash')
     >!= fun err -> `Reference err)
  | [], 0 ->
    Log.debug (fun l -> l ~header:"main" "Git repository already updated.");
    Lwt.return (Ok ())
  | _, _ -> assert false

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
    let parse str = Ok (Git_unix.Store.Reference.of_string str) in
    let print = Git_unix.Store.Reference.pp in
    Arg.conv ~docv:"<name>" (parse, print)

  let branch =
    let doc =
      "Instead of pointing the newly created HEAD to the branch pointed to by the cloned \
       repository's HEAD, point to <name> branch instead. --branch can also take tags and \
       detaches the HEAD at that commit in the resulting repository."
    in
    Arg.(value
         & opt reference Git_unix.Store.Reference.master
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
  Term.(ret (const main $ Flag.progress $ Flag.origin $ Flag.branch $ Flag.directory $ Flag.repository $ setup_log)),
  Term.info "ogit-http-clone" ~version:"v0.1" ~doc ~exits

let () = Term.(exit @@ eval command)
