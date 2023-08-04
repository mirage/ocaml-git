open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let port =
  let doc = Key.Arg.info ~doc:"The port where to listen." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

type hash = Hash
type git = Git

let hash = typ Hash
let sha1 = impl ~packages:[ package "digestif" ] "Digestif.SHA1" hash
let git = typ Git

let git_impl path =
  let packages = [ package "git" ] in (* no bounds here, the git_client from mirage already emits bounds *)
  let keys = match path with
    | None -> []
    | Some path -> [ Key.v path ] in
  let connect _ modname _ = match path with
    | None ->
        Fmt.str
          {ocaml|%s.v (Fpath.v ".") >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          modname modname
    | Some key ->
        Fmt.str
          {ocaml|( match Option.map Fpath.of_string %a with
                 | Some (Ok path) -> %s.v path
                 | Some (Error (`Msg err)) -> failwith err
                 | None -> %s.v (Fpath.v ".") ) >>= function
                 | Ok v -> Lwt.return v
                 | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          Key.serialize_call (Key.v key) modname modname modname in
  impl ~packages ~keys ~connect "Git.Mem.Make" (hash @-> git)

let minigit =
  foreign "Unikernel.Make"
    ~packages:[ package "ptime"
              ; package "hxd" ~sublibs:[ "core"; "string" ] ]
    ~keys:[ Key.v remote
          ; Key.v port ]
    (stackv4v6 @-> git @-> git_client @-> job)

let git path hash = git_impl path $ hash

(* User space *)

let ssh_key =
  let doc = Key.Arg.info ~doc:"The private SSH key." [ "ssh-key" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let https_authenticator =
  let doc = Key.Arg.info ~doc:"TLS authenticator of the remote Git repository." [ "https-authenticator" ] in
  Key.(create "https_authenticator" Arg.(opt (some string) None doc))

let stack = generic_stackv4v6 default_network

let git_client =
  let dns = generic_dns_client stack in
  let git = git_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~authenticator:ssh_authenticator tcp git)
      (git_http ~authenticator:https_authenticator tcp git))

let git     = git None sha1

let () =
  register "minigit"
    [ minigit $ stack $ git $ git_client ]
