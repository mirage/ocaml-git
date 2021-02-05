module Make (Stack : Mirage_stack.V4) : sig
  val tcp_endpoint : (Stack.t * Ipaddr.V4.t * int) Mimic.value

  val tcp_protocol :
    (Stack.t * Ipaddr.V4.t * int, Stack.TCPV4.flow) Mimic.protocol

  val tcp_ipaddr : Ipaddr.V4.t Mimic.value
  val tcp_stack : Stack.t Mimic.value
  val with_stack : Stack.t -> Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
end
