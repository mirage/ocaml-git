module Make (Stack : Mirage_stack.V4V6) : sig
  val tcp_endpoint : (Stack.t * Ipaddr.t * int) Mimic.value
  val tcp_protocol : (Stack.t * Ipaddr.t * int, Stack.TCP.flow) Mimic.protocol
  val tcp_ipaddr : Ipaddr.t Mimic.value
  val tcp_stack : Stack.t Mimic.value
  val with_stack : Stack.t -> Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
end
