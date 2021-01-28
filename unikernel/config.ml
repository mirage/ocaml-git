open Mirage

type mimic = Mimic

let mimic = typ Mimic

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = "ctx"
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf () =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname =
         function
         | [ stack ] ->
             Fmt.str "Lwt.return (%s.with_stack %s %s.ctx)" modname stack
               modname
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_tcp_impl stackv4 = mimic_tcp_conf () $ stackv4

let mimic_git_conf ~edn () =
  let packages = [ package "git-mirage" ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage.Make"
       method! packages = Key.pure packages
       method name = "git_ctx"
       method! connect _ modname _ =
         Fmt.str
           {|let ctx_git0 = %s.with_smart_git_endpoint (%a) %s.ctx in
             let ctx_git1 = %s.with_resolv ctx_git0 in
             Lwt.return ctx_git1|}
           modname Key.serialize_call edn modname
           modname
     end

let mimic_git_impl ~edn stackv4 mimic_tcp =
  mimic_git_conf ~edn () $ stackv4 $ mimic_tcp

let mimic_ssh_conf ~edn ~kind ~seed ~auth () =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let edn = Key.abstract edn in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; edn ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "ssh_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; git_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {| let ctx00 = Mimic.merge %s %s in
             let ctx01 = Option.fold ~none:ctx00 ~some:(fun v -> %s.%s v ctx00) %a in
             let ctx02 = Option.fold ~none:ctx01 ~some:(fun v -> %s.with_authenticator v ctx01) %a in
             let ctx03 = %s.with_resolv ctx02 in
             Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint (%a) ctx03)) |}
               tcp_ctx git_ctx modname with_key Key.serialize_call seed modname
               Key.serialize_call auth modname modname modname
               Key.serialize_call edn
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mimic_tcp mimic_git mclock =
  mimic_ssh_conf ~edn ~kind ~seed ~auth ()
  $ stackv4
  $ mimic_tcp
  $ mimic_git
  $ mclock

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf ~edn () =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; _; ctx ] ->
             Fmt.str
               "Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint %a %s))"
               modname modname Key.serialize_call edn ctx
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_dns_impl ~edn random mclock time stackv4 mimic_tcp =
  mimic_dns_conf ~edn () $ random $ mclock $ time $ stackv4 $ mimic_tcp

type hash = Hash

let hash = typ Hash

let sha1 =
  impl @@ object
       inherit base_configurable
       method ty = hash
       method module_name = "Digestif.SHA1"
       method! packages = Key.pure [ package "digestif" ]
       method name = "sha1"
     end

type git = Git

let git = typ Git

let git_conf ?path () =
  let keys =
    match path with Some path -> [ Key.abstract path ] | None -> []
  in
  impl @@ object
       inherit base_configurable
       method ty = hash @-> git
       method! keys = keys
       method module_name = "Git.Mem.Make"
       method! packages = Key.pure [ package "git"; package "digestif" ]
       method name = "git"
       method! connect _ modname _ =
         match path with
         | None ->
             Fmt.str
               {|%s.v (Fpath.v ".") >>= function
                  | Ok v -> Lwt.return v
                  | Error err -> Fmt.failwith "%%a" %s.pp_error err|}
               modname modname
         | Some key ->
             Fmt.str
               {|let res = match Option.map Fpath.of_string %a with
                    | Some (Ok path) -> %s.v path
                    | Some (Error (`Msg err)) -> failwith err
                    | None -> %s.v (Fpath.v ".") in
                  res >>= function
                  | Ok v -> Lwt.return v
                  | Error err -> Fmt.failwith "%%a" %s.pp_error err|}
               Key.serialize_call (Key.abstract key) modname modname modname
     end

let git_impl ?path hash = git_conf ?path () $ hash

(* User space *)

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc =
    Key.Arg.info ~doc:"SSH public key of the remote Git endpoint."
      [ "ssh-auth" ]
  in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let minigit =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote; Key.abstract ssh_seed; Key.abstract ssh_auth ]
    ~packages:[ package "cohttp-mirage"; package "git-cohttp-mirage" ]
    (git
    @-> time
    @-> console
    @-> resolver
    @-> conduit
    @-> mimic
    @-> job)

let mimic ~edn ~kind ~seed ~auth stackv4 random mclock time =
  let mtcp = mimic_tcp_impl stackv4 in
  let mgit = mimic_git_impl ~edn stackv4 mtcp in
  let mdns = mimic_dns_impl ~edn random mclock time stackv4 mtcp in
  let mssh = mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mtcp mgit mclock in
  merge mssh mdns

let stackv4 = generic_stackv4 default_network
let mclock = default_monotonic_clock
let time = default_time
let random = default_random
let git = git_impl sha1
let mimic = mimic stackv4 random mclock time
let console = default_console
let resolver = resolver_dns stackv4
let conduit = conduit_direct ~tls:true stackv4
let mimic = mimic ~edn:remote ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth

let () =
  register "minigit" ~packages:[]
    [ minigit $ git $ time $ console $ resolver $ conduit $ mimic ]
