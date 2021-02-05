open Lwt.Infix

module Make (Stack : Mirage_stack.V4) = struct
  module TCP = struct
    include Stack.TCPV4

    type endpoint = Stack.t * Ipaddr.V4.t * int

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

    let connect : endpoint -> _ =
     fun (stack, ipaddr, port) ->
      let stack = Stack.tcpv4 stack in
      create_connection stack (ipaddr, port) >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Connect err)
  end

  let tcp_endpoint, tcp_protocol =
    Mimic.register ~name:"mirage-tcpip" (module TCP)

  let tcp_ipaddr = Mimic.make ~name:"tcp-ipaddr"
  let tcp_stack : Stack.t Mimic.value = Mimic.make ~name:"tcp-stack"
  let with_stack stack ctx = Mimic.add tcp_stack stack ctx

  let ctx =
    let k_git scheme stack ipaddr port =
      match scheme with
      | `Git -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none
    in
    let k_http scheme stack ipaddr port =
      match scheme with
      | `HTTP -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none
    in
    let open Mimic in
    Mimic.empty
    |> fold tcp_endpoint
         Fun.
           [
             req Smart_git.git_scheme; req tcp_stack; req tcp_ipaddr;
             dft Smart_git.git_port 9418;
           ]
         ~k:k_git
    |> fold tcp_endpoint
         Fun.
           [
             req Smart_git.git_scheme; req tcp_stack; req tcp_ipaddr;
             dft Smart_git.git_port 80;
           ]
         ~k:k_http
end
