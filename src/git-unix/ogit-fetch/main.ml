let () = Random.self_init ()

open Git_unix
module Sync = Sync (Store) (Git_cohttp_unix)

let src = Logs.Src.create "ogit-fetch" ~doc:"logs binary event"

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
  let quiet = match style_renderer with Some _ -> true | None -> false in
  quiet, ppf

type error = [ `Store of Store.error | `Sync of Sync.error ]

let store_err err = `Store err
let sync_err err = `Sync err

let pp_error ppf = function
  | `Store err -> Fmt.pf ppf "(`Store %a)" Store.pp_error err
  | `Sync err -> Fmt.pf ppf "(`Sync %a)" Sync.pp_error err

module TCP = struct
  include Tcpip_stack_socket.V4V6.TCP

  type endpoint = Ipaddr.t * int

  let connect (ipaddr, port) =
    let open Lwt.Infix in
    connect ~ipv4_only:false ~ipv6_only:false Ipaddr.V4.Prefix.global None
    >>= fun t -> create_connection t (ipaddr, port)
end

module SSH = struct
  include Awa_mirage.Make (Tcpip_stack_socket.V4V6.TCP) (Mclock)

  type nonrec error = [ `TCP of TCP.error | `SSH of error ]

  let pp_error ppf = function
    | `TCP err -> TCP.pp_error ppf err
    | `SSH err -> pp_error ppf err

  type endpoint = {
    authenticator : Awa.Keys.authenticator option;
    user : string;
    path : string;
    key : Awa.Hostkey.priv;
    endpoint : TCP.endpoint;
  }

  let ( >>? ) = Lwt_result.bind

  open Lwt.Infix

  let read flow = read flow >|= Rresult.R.reword_error (fun err -> `SSH err)

  let connect { authenticator; user; path; key; endpoint } =
    let channel_request = Awa.Ssh.Exec (Fmt.str "git-upload-pack '%s'" path) in
    TCP.connect endpoint >|= Rresult.R.reword_error (fun err -> `TCP err)
    >>? fun flow ->
    client_of_flow ?authenticator ~user key channel_request flow
    >|= Rresult.R.reword_error (fun err -> `SSH err)
end

let tcp_value, tcp_protocol = Mimic.register ~name:"tcp" (module TCP)
let domain_name = Mimic.make ~name:"domain-namme"
let port = Mimic.make ~name:"port"
let ssh_value, ssh_protocol = Mimic.register ~name:"ssh" (module SSH)
let path = Mimic.make ~name:"path"
let seed = Mimic.make ~name:"ssh-seed"
let user = Mimic.make ~name:"user"
let authenticator = Mimic.make ~name:"ssh-authenticator"

let resolv ctx =
  let k domain_name port =
    match Unix.gethostbyname (Domain_name.to_string domain_name) with
    | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
        Lwt.return_some (Ipaddr_unix.of_inet_addr h_addr_list.(0), port)
    | _ | (exception _) -> Lwt.return_none
  in
  Mimic.fold tcp_value Mimic.Fun.[ req domain_name; dft port 9418 ] ~k ctx

let resolv_ssh ctx =
  let k authenticator sockaddr path user seed =
    let key = Awa.Keys.of_seed `Rsa seed in
    Lwt.return_some { SSH.authenticator; user; path; key; endpoint = sockaddr }
  in
  Mimic.fold ssh_value
    Mimic.Fun.[ opt authenticator; req tcp_value; req path; req user; req seed ]
    ~k ctx

let of_smart_git_endpoint edn ctx =
  match edn with
  | { Smart_git.Endpoint.scheme = `SSH v_user; path = v_path; host } ->
      ctx
      |> Mimic.add domain_name host
      |> Mimic.add path v_path
      |> Mimic.add user v_user
  | { Smart_git.Endpoint.path = v_path; host; _ } ->
      ctx |> Mimic.add domain_name host |> Mimic.add path v_path

let main (_ssh_seed : string)
    (references : (Git.Reference.t * Git.Reference.t) list) (directory : string)
    (repository : Smart_git.Endpoint.t) : (unit, 'error) Lwt_result.t =
  let repo_root =
    (match directory with "" -> Sys.getcwd () | _ -> directory) |> Fpath.v
  in
  let ( >>?= ) = Lwt_result.bind in
  let ( >>!= ) v f = Lwt_result.map_err f v in
  let ctx =
    Mimic.empty |> resolv |> resolv_ssh |> of_smart_git_endpoint repository
  in
  Store.v repo_root >>!= store_err >>?= fun store ->
  let push_stdout = print_endline in
  let push_stderr = prerr_endline in
  Sync.fetch ~push_stdout ~push_stderr ~ctx repository store (`Some references)
  >>!= sync_err
  >>?= fun _ -> Lwt.return (Ok ())

open Cmdliner

module Flag = struct
  (** We want ogit-fetch to have the following interface:
     ogit-fetch [-r <path> | --root <path>] [--output <output_channel>] [--progress] <repository> <refspec>... *)

  (* TODO polish code & CLI *)

  let output =
    let conv' =
      let parse str =
        match str with
        | "stdout" -> Ok Fmt.stdout
        | "stderr" -> Ok Fmt.stderr
        | s -> Error (`Msg (Fmt.str "%s is not an output." s))
      in
      let print ppf v =
        Fmt.pf ppf "%s" (if v = Fmt.stdout then "stdout" else "stderr")
      in
      Arg.conv ~docv:"<output>" (parse, print)
    in
    let doc =
      "Output of the progress status. Can take values 'stdout' (default) or \
       'stderr'."
    in
    Arg.(value & opt conv' Fmt.stdout & info [ "output" ] ~doc ~docv:"<output>")

  let progress =
    let doc =
      "Progress status is reported on the standard error stream by default \
       when it is attached to a terminal, unless -q is specified. This flag \
       forces progress status even if the standard error stream is not \
       directed to a terminal."
    in
    Arg.(value & flag & info [ "progress" ] ~doc)

  let directory =
    let doc = "indicate path to repository root containing '.git' folder" in
    Arg.(value & opt string "" & info [ "r"; "root" ] ~doc ~docv:"<directory>")

  let ssh_seed =
    let doc = "seed for SSH generated by awa_gen_key" in
    Arg.(value & opt string "" & info [ "s"; "seed" ] ~doc ~docv:"<ssh_seed>")

  (** passed argument needs to be a URI of the repository *)
  let repository =
    let endpoint =
      let parse = Smart_git.Endpoint.of_string in
      let print = Smart_git.Endpoint.pp in
      Arg.conv ~docv:"<uri>" (parse, print)
    in
    let doc = "URI leading to repository" in
    Arg.(
      required & pos 0 (some endpoint) None & info [] ~docv:"<repository>" ~doc)

  (** can be several references of form "remote_ref:local_ref" or "remote_ref", where the latter means that the local_ref should
  have the same name *)
  let references =
    let reference =
      let parse str = Ok (Git.Reference.v str) in
      let print = Git.Reference.pp in
      Arg.conv ~docv:"<ref>" (parse, print)
    in
    let doc = "" in
    Arg.(
      non_empty
      & pos_right 0 (pair ~sep:':' reference reference) []
      & info ~doc ~docv:"<ref>" [])
end

let setup_log =
  Term.(
    const setup_logs
    $ Fmt_cli.style_renderer ()
    $ Logs_cli.level ()
    $ Flag.output)

let main _ ssh_seed references directory repository _ =
  match Lwt_main.run (main ssh_seed references directory repository) with
  | Ok () -> `Ok ()
  | Error (#error as err) -> `Error (false, Fmt.str "%a" pp_error err)

let command =
  let doc = "Fetch a Git repository by the HTTP protocol." in
  let exits = Term.default_exits in
  ( Term.(
      ret
        (const main
        $ Flag.progress
        $ Flag.ssh_seed
        $ Flag.references
        $ Flag.directory
        $ Flag.repository
        $ setup_log)),
    Term.info "ogit-fetch" ~version:"v0.1" ~doc ~exits )

let () = Term.(exit @@ eval command)
