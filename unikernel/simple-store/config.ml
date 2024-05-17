(* mirage >= 4.5.0 & < 4.6.0 *)

open Mirage

let remote = runtime_arg ~pos:__POS__ "Unikernel.K.remote"
let port = runtime_arg ~pos:__POS__ "Unikernel.K.port"

type hash = Hash
type git = Git

let hash = typ Hash
let sha1 = impl ~packages:[ package "digestif" ] "Digestif.SHA1" hash
let git = typ Git

let git_impl path =
  let packages = [ package "git" ] in (* no bounds here, the git_client from mirage already emits bounds *)
  let runtime_args = match path with
    | None -> []
    | Some path -> [ path ] in
  let connect _ modname = function
    | [ _hash ] ->
        code ~pos:__POS__
          {ocaml|%s.v (Fpath.v ".") >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          modname modname
    | [ _hash ; key ] ->
        code ~pos:__POS__
          {ocaml|( match Option.map Fpath.of_string %s with
                 | Some (Ok path) -> %s.v path
                 | Some (Error (`Msg err)) -> failwith err
                 | None -> %s.v (Fpath.v ".") ) >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          key modname modname modname
    | _ -> assert false
  in
  impl ~packages ~runtime_args ~connect "Git.Mem.Make" (hash @-> git)

let minigit =
  main "Unikernel.Make"
    ~packages:[ package "ptime"
              ; package "hxd" ~sublibs:[ "core"; "string" ] ]
    ~runtime_args:[ remote ; port ]
    (stackv4v6 @-> git @-> git_client @-> job)

let git path hash = git_impl path $ hash

(* User space *)

let ssh_key =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"The private SSH key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_password =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
     let doc = Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
      Arg.(value & opt (some string) None doc)|}

let ssh_authenticator =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
     let doc = Arg.info ~doc:"SSH public key of the remote Git repository." ["ssh-authenticator"] in
      Arg.(value & opt (some string) None doc)|}

let https_authenticator =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
     let doc = Arg.info ~doc:"TLS authenticator of the remote Git repository." [ "https-authenticator" ] in
     Arg.(value & opt (some string) None doc)|}

let stack = generic_stackv4v6 default_network

let git_client =
  let dns = generic_dns_client stack in
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git)
      (git_http ~authenticator:https_authenticator tcp git))

let git     = git None sha1

let () =
  register "minigit"
    [ minigit $ stack $ git $ git_client ]
