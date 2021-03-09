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
    (Mclock : Mirage_clock.MCLOCK) : sig
  type nonrec endpoint = Stack.t endpoint
  type flow

  val ssh_endpoint : endpoint Mimic.value
  val ssh_protocol : (endpoint, flow) Mimic.protocol
  val ssh_authenticator : Awa.Keys.authenticator Mimic.value
  val ssh_key : Awa.Hostkey.priv Mimic.value
  val with_authenticator : string -> Mimic.ctx -> Mimic.ctx
  val with_rsa_key : string -> Mimic.ctx -> Mimic.ctx
  val with_ed25519_key : string -> Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
end
