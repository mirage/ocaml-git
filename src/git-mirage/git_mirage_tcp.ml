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
  let tcp_port = Mimic.make ~name:"tcp-port"
  let tcp_stack = Mimic.make ~name:"tcp-stack"
  let with_port v ctx = Mimic.add tcp_port v ctx
  let with_ipaddr v ctx = Mimic.add tcp_ipaddr v ctx
  let with_stack v ctx = Mimic.add tcp_stack v ctx

  let with_resolv ctx =
    let k stack ipaddr port = Lwt.return_some (stack, ipaddr, port) in
    Mimic.(
      fold tcp_endpoint
        Fun.[ req tcp_stack; req tcp_ipaddr; req tcp_port ]
        ~k ctx)

  let ctx = with_resolv Mimic.empty
end
