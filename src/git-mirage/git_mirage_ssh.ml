open Lwt.Infix

type 'stack endpoint = {
  stack : 'stack;
  ipaddr : Ipaddr.t;
  port : int;
  authenticator : Awa.Keys.authenticator option;
  user : string;
  key : Awa.Hostkey.priv;
  path : string;
  capabilities : [ `Rd | `Wr ];
}

module Make
    (Stack : Mirage_stack.V4V6) (TCP : sig
      val tcp_endpoint : (Stack.t * Ipaddr.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end)
    (Mclock : Mirage_clock.MCLOCK) =
struct
  module SSH = struct
    include Awa_mirage.Make (Stack.TCP) (Mclock)

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
      let stack = Stack.tcp edn.stack in
      let channel_request =
        match edn.capabilities with
        | `Rd -> Awa.Ssh.Exec (Fmt.str "git-upload-pack '%s'" edn.path)
        | `Wr -> Awa.Ssh.Exec (Fmt.str "git-receive-pack '%s'" edn.path)
      in
      Stack.TCP.create_connection stack (edn.ipaddr, edn.port) >>= function
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
  let ssh_key = Mimic.make ~name:"ssh-key"

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

  let ctx =
    let k0 scheme (stack, ipaddr, port) ssh_authenticator ssh_user ssh_key
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
    let k1 scheme stack ipaddr port = k0 scheme (stack, ipaddr, port) in
    let open Mimic in
    Mimic.empty
    |> fold ssh_endpoint
         Fun.
           [
             req Smart_git.git_scheme; req TCP.tcp_endpoint;
             opt ssh_authenticator; req Smart_git.git_ssh_user; req ssh_key;
             req Smart_git.git_path; req Smart_git.git_capabilities;
           ]
         ~k:k0
    |> fold ssh_endpoint
         Fun.
           [
             req Smart_git.git_scheme; req TCP.tcp_stack; req TCP.tcp_ipaddr;
             dft Smart_git.git_port 22; opt ssh_authenticator;
             req Smart_git.git_ssh_user; req ssh_key; req Smart_git.git_path;
             req Smart_git.git_capabilities;
           ]
         ~k:k1
end
