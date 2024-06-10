open Lwt.Infix

type endpoint = {
  port : int;
  hostname : string;
  authenticator : Awa.Keys.authenticator option;
  user : string;
  credentials : [ `Password of string | `Pubkey of Awa.Hostkey.priv ];
  path : string;
  capabilities : [ `Rd | `Wr ];
}

let git_mirage_ssh_password = Mimic.make ~name:"git-mirage-ssh-password"
let git_mirage_ssh_key = Mimic.make ~name:"git-mirage-ssh-key"

let git_mirage_ssh_authenticator =
  Mimic.make ~name:"git-mirage-ssh-authenticator"

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t

  val with_optionnal_key :
    ?authenticator:string ->
    key:string option ->
    password:string option ->
    Mimic.ctx ->
    Mimic.ctx Lwt.t

  val ctx : Mimic.ctx
end

module Make
    (Mclock : Mirage_clock.MCLOCK)
    (TCP : Tcpip.Tcp.S)
    (Time : Mirage_time.S)
    (Happy_eyeballs : Mimic_happy_eyeballs.S with type flow = TCP.flow) : S =
struct
  module SSH = struct
    include Awa_mirage.Make (TCP) (Time) (Mclock)

    type nonrec endpoint = Happy_eyeballs.t * endpoint

    type nonrec write_error =
      [ `Write of write_error | `Connect of error | `Closed ]

    let pp_write_error ppf = function
      | `Connect err -> pp_error ppf err
      | `Write err -> pp_write_error ppf err
      | `Closed as err -> pp_write_error ppf err

    let write flow cs =
      write flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect (happy_eyeballs, edn) =
      let open Lwt.Infix in
      let channel_request =
        match edn.capabilities with
        | `Rd -> Awa.Ssh.Exec (Fmt.str "git-upload-pack '%s'" edn.path)
        | `Wr -> Awa.Ssh.Exec (Fmt.str "git-receive-pack '%s'" edn.path)
      in
      Happy_eyeballs.resolve happy_eyeballs edn.hostname [ edn.port ]
      >>= function
      | Error (`Msg err) -> Lwt.return_error (`Connect (`Msg err))
      | Ok ((_ipaddr, _port), flow) -> (
          client_of_flow ?authenticator:edn.authenticator ~user:edn.user
            edn.credentials channel_request flow
          >>= function
          | Error err -> Lwt.return_error (`Connect err)
          | Ok _ as v -> Lwt.return v)
  end

  let ssh_endpoint, _ssh_protocol = Mimic.register ~name:"ssh" (module SSH)

  let connect ctx =
    let edn = Mimic.make ~name:"ssh-endpoint" in
    let k0 happy_eyeballs edn = Lwt.return_some (happy_eyeballs, edn) in
    let k1 git_transmission git_scheme git_ssh_user git_hostname git_port
        git_path git_capabilities git_mirage_ssh_key git_mirage_ssh_password
        git_mirage_ssh_authenticator =
      match git_transmission, git_scheme with
      | `Exec, `SSH ->
          (* XXX(dinosaure): be sure that we don't want to initiate a wrong transmission protocol.
           * be sure that [k2] is called by [mimic]. *)
          let credentials =
            match git_mirage_ssh_key, git_mirage_ssh_password with
            | None, None | Some _, Some _ -> None
            | Some k, None -> Some (`Pubkey k)
            | None, Some p -> Some (`Password p)
          in
          Lwt.return
            (Option.map
               (fun credentials ->
                 {
                   port = git_port;
                   hostname = git_hostname;
                   authenticator = git_mirage_ssh_authenticator;
                   user = git_ssh_user;
                   credentials;
                   path = git_path;
                   capabilities = git_capabilities;
                 })
               credentials)
      | _ -> Lwt.return_none
    in
    let k2 git_scheme =
      match git_scheme with
      | `SSH -> Lwt.return_some `Exec
      | _ -> Lwt.return_none
    in
    let ctx =
      Mimic.fold ssh_endpoint
        Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs; req edn ]
        ~k:k0 ctx
    in
    let ctx =
      Mimic.fold edn
        Mimic.Fun.
          [
            req Smart_git.git_transmission;
            req Smart_git.git_scheme;
            req Smart_git.git_ssh_user;
            req Smart_git.git_hostname;
            dft Smart_git.git_port 22;
            req Smart_git.git_path;
            req Smart_git.git_capabilities;
            opt git_mirage_ssh_key;
            opt git_mirage_ssh_password;
            opt git_mirage_ssh_authenticator;
          ]
        ~k:k1 ctx
    in
    let ctx =
      Mimic.fold Smart_git.git_transmission
        Mimic.Fun.[ req Smart_git.git_scheme ]
        ~k:k2 ctx
    in
    Lwt.return ctx

  let with_optionnal_key ?authenticator ~key ~password ctx =
    let authenticator =
      Option.map Awa.Keys.authenticator_of_string authenticator
    in
    let key = Option.map Awa.Keys.of_string key in
    let ctx =
      match authenticator with
      | Some (Error err) ->
        print_endline ("[git-mirage-ssh] authenticator error: " ^ err);
        exit 64
      | Some (Ok authenticator) ->
          Mimic.add git_mirage_ssh_authenticator authenticator ctx
      | None -> ctx
    in
    match key, password with
    | Some (Error (`Msg err)), _ ->
      print_endline ("[git-mirage-ssh] ssh key error: " ^ err);
      exit 64
    | Some _, Some _ ->
      print_endline "[git-mirage-ssh] both key and password provided";
      exit 64
    | Some (Ok key), None ->
        let ctx = Mimic.add git_mirage_ssh_key key ctx in
        Lwt.return ctx
    | None, Some password ->
        let ctx = Mimic.add git_mirage_ssh_password password ctx in
        Lwt.return ctx
    | None, None -> Lwt.return ctx

  let ctx = Mimic.empty
end
