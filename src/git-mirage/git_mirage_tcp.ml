module Make (Stack : Mirage_stack.V4) = struct
  module TCP = struct
    include Stack.TCPV4

    type endpoint = Stack.t * Ipaddr.V4.t * int

    let connect : endpoint -> _ =
     fun (stack, ipaddr, port) ->
      let stack = Stack.tcpv4 stack in
      create_connection stack (ipaddr, port)
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
