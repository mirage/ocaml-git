module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
    end) : sig
  val dns_domain_name : [ `host ] Domain_name.t Mimic.value
  val with_domain_name : [ `host ] Domain_name.t -> Mimic.ctx -> Mimic.ctx
  val with_resolv : Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
  val with_smart_git_endpoint : string -> Mimic.ctx -> Mimic.ctx
end
