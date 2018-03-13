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
  [ `Store of Git_unix.Store.error
  | `Invalid_hash of Git_unix.Store.Hash.t ]

let store_err err = `Store err
let invalid_hash hash = `Invalid_hash hash

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Git_unix.Store.pp_error err
  | `Invalid_hash hash -> Fmt.pf ppf "(`Hash %a)" Git_unix.Store.Hash.pp hash

let pp_type ppf = function
  | Git_unix.Store.Value.Commit _ -> Fmt.string ppf "commit"
  | Git_unix.Store.Value.Tree _   -> Fmt.string ppf "tree"
  | Git_unix.Store.Value.Tag _    -> Fmt.string ppf "tag"
  | Git_unix.Store.Value.Blob _   -> Fmt.string ppf "blob"

let pp_size ppf value =
  Fmt.int64 ppf (Git_unix.Store.Value.F.length value)

let pp_exit = Fmt.nop

let pp_pretty_print ppf value =
  Git_unix.Store.Value.pp ppf value

let pp_inflate ppf raw =
  Git.Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len ppf raw

type rest =
  [ `Type | `Size | `Exit | `PrettyPrint ]

let main show hash =
  let root = Fpath.(v (Sys.getcwd ())) in

  let open Lwt_result in

  let ( >!= ) v f = map_err f v in

  Git_unix.Store.create ~root () >!= store_err >>= fun git ->

  match show with
  | `Inflate ->
    let open Lwt.Infix in

    (Git_unix.Store.read_inflated git hash >>= function
      | Some (_, raw) ->
        Fmt.(pf stdout) "%a%!" pp_inflate raw;
        Lwt.return (Ok ())
      | None ->
        Lwt.return (Error (invalid_hash hash)))
  | #rest as rest ->
    Git_unix.Store.read git hash >!= store_err >>= fun value ->
    let fmt = match rest with
      | `Type -> pp_type
      | `Size -> pp_size
      | `Exit -> pp_exit
      | `PrettyPrint -> pp_pretty_print
    in

    Fmt.(pf stdout) "%a\n%!" fmt value;
    Lwt.return (Ok ())

open Cmdliner

module Flag =
struct
  let show =
    Arg.(value & vflag `Inflate
        [ `Type, info [ "t" ] ~doc:"Instead of the content, show the object type identified by <object>."
        ; `Size, info [ "s" ] ~doc:"Instead of the content, show the object size identified by <object>."
        ; `Exit, info [ "e" ] ~doc:"Suppress all output; instead exit with zero status if <object> exists and is a valid object."
        ; `PrettyPrint, info [ "p" ] ~doc:"Pretty-print the contents of <object> base on its type." ])

  let hash =
    let parse x = try Ok (Git_unix.Store.Hash.of_hex x) with exn -> Error (`Msg (Printexc.to_string exn)) in
    let print = Git_unix.Store.Hash.pp in
    Arg.conv ~docv:"<object>" (parse, print)

  let value =
    Arg.(required & pos ~rev:true 0 (some hash) None & info [] ~docv:"<object>")
end

let setup_log =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main show hash _ =
  match Lwt_main.run (main show hash) with
  | Ok () -> `Ok ()
  | Error (#error as err) -> `Error (false, Fmt.strf "%a" pp_error err)

let command =
  let doc = "Provide content or type and size information for repository objects" in
  let exits = Term.default_exits in
  Term.(ret (const main $ Flag.show $ Flag.value $ setup_log)),
  Term.info "ogit-cat-file" ~version:"v0.1" ~doc ~exits

let () = Term.(exit @@ eval command)
