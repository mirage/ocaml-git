open Lwt.Infix

type 'stack endpoint = {
  stack : 'stack;
  ipaddr : Ipaddr.V4.t;
  port : int;
  authenticator : Awa.Keys.authenticator option;
  user : string;
  key : Awa.Hostkey.priv;
  path : string;
  capabilities : [ `Rd | `Wr ];
}

module Make
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_endpoint : (Stack.t * Ipaddr.V4.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
      val tcp_port : int Mimic.value
    end)
    (Mclock : Mirage_clock.MCLOCK) =
struct
  module SSH = struct
    include Awa_mirage.Make (Stack.TCPV4) (Mclock)

    type nonrec endpoint = Stack.t endpoint

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

    let connect edn =
      let open Lwt.Infix in
      let stack = Stack.tcpv4 edn.stack in
      let channel_request =
        match edn.capabilities with
        | `Rd -> Awa.Ssh.Exec (Fmt.str "git-upload-pack '%s'" edn.path)
        | `Wr -> Awa.Ssh.Exec (Fmt.str "git-receive-pack '%s'" edn.path)
      in
      Stack.TCPV4.create_connection stack (edn.ipaddr, edn.port) >>= function
      | Error err -> Lwt.return_error (`Connect (`Read err))
      | Ok flow -> (
          client_of_flow ?authenticator:edn.authenticator ~user:edn.user edn.key
            channel_request flow
          >>= function
          | Error err -> Lwt.return_error (`Connect err)
          | Ok _ as v -> Lwt.return v)
  end

  type endpoint = SSH.endpoint
  type flow = SSH.flow

  let ssh_endpoint, ssh_protocol =
    Mimic.register ~name:"mirage-ssh" (module SSH)

  let ssh_authenticator = Mimic.make ~name:"ssh-authenticator"
  let ssh_user = Mimic.make ~name:"ssh-user"
  let ssh_key = Mimic.make ~name:"ssh-key"
  let with_user v ctx = Mimic.add ssh_user v ctx

  let with_authenticator v ctx =
    match Awa.Keys.authenticator_of_string v with
    | Ok v -> Mimic.add ssh_authenticator v ctx
    | Error msg -> failwith msg

  let with_rsa_key v ctx =
    let v = Awa.Keys.of_seed `Rsa v in
    Mimic.add ssh_key v ctx

  let with_ed25519_key v ctx =
    let v = Awa.Keys.of_seed `Ed25519 v in
    Mimic.add ssh_key v ctx

  let with_resolv ctx =
    let k scheme (stack, ipaddr, port) ssh_authenticator ssh_user ssh_key
        git_path git_capabilities =
      match scheme with
      | `SSH ->
          Lwt.return_some
            {
              stack;
              ipaddr;
              port;
              authenticator = ssh_authenticator;
              user = ssh_user;
              key = ssh_key;
              path = git_path;
              capabilities = git_capabilities;
            }
      | _ -> Lwt.return_none
    in
    let ctx =
      Mimic.(
        fold ssh_endpoint
          Fun.
            [
              req Smart_git.git_scheme; req TCP.tcp_endpoint;
              opt ssh_authenticator; req ssh_user; req ssh_key;
              req Smart_git.git_path; req Smart_git.git_capabilities;
            ]
          ~k ctx)
    in
    let k scheme stack ipaddr port = k scheme (stack, ipaddr, port) in
    let ctx =
      Mimic.(
        fold ssh_endpoint
          Fun.
            [
              req Smart_git.git_scheme; req TCP.tcp_stack; req TCP.tcp_ipaddr;
              dft TCP.tcp_port 22; opt ssh_authenticator; req ssh_user;
              req ssh_key; req Smart_git.git_path;
              req Smart_git.git_capabilities;
            ]
          ~k ctx)
    in
    ctx

  let ctx = with_resolv Mimic.empty

  let with_smart_git_endpoint edn ctx =
    match Smart_git.Endpoint.of_string edn with
    | Ok { Smart_git.Endpoint.scheme = `SSH user; _ } -> with_user user ctx
    | _ -> ctx
end
