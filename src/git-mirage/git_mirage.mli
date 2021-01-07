module Make
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
      val tcp_port : int Mimic.value
      val tcp_endpoint : (Stack.t * Ipaddr.V4.t * int) Mimic.value
    end) : sig
  val git_path : string Mimic.value
  val git_capabilities : [ `Rd | `Wr ] Mimic.value
  val git_scheme : [ `Git | `SSH | `HTTP | `HTTPS ] Mimic.value
  val with_git_path : string -> Mimic.ctx -> Mimic.ctx
  val fetch : Mimic.ctx -> Mimic.ctx
  val push : Mimic.ctx -> Mimic.ctx
  val gri : Mimic.ctx -> Mimic.ctx
  val ssh : Mimic.ctx -> Mimic.ctx
  val http : Mimic.ctx -> Mimic.ctx
  val https : Mimic.ctx -> Mimic.ctx
  val with_resolv : Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
  val with_smart_git_endpoint : string -> Mimic.ctx -> Mimic.ctx
end
