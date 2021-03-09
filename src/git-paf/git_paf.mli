include Smart_git.HTTP

module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Mirage_stack.V4V6)
    (Paf : Paf_mirage.S with type stack = Stack.t) (TCP : sig
      val tcp_endpoint : (Stack.t * Ipaddr.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) : sig
  val ctx : Mimic.ctx
end
