type 'stack endpoint = {
  stack : 'stack;
  ipaddr : Ipaddr.V4.t;
  port : int;
  authenticator : Awa.Keys.authenticator option;
  user : string;
  key : Awa.Hostkey.priv;
  path : string;
  capabilities : [ `Rd | `Wr ];
}

module Make
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_endpoint : (Stack.t * Ipaddr.V4.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
      val tcp_port : int Mimic.value
    end) (Git : sig
      val git_path : string Mimic.value
      val git_capabilities : [ `Rd | `Wr ] Mimic.value
      val git_scheme : [ `Git | `SSH | `HTTP | `HTTPS ] Mimic.value
    end)
    (Mclock : Mirage_clock.MCLOCK) : sig
  type nonrec endpoint = Stack.t endpoint
  type flow

  val ssh_endpoint : endpoint Mimic.value
  val ssh_protocol : (endpoint, flow) Mimic.protocol
  val ssh_authenticator : Awa.Keys.authenticator Mimic.value
  val ssh_user : string Mimic.value
  val ssh_key : Awa.Hostkey.priv Mimic.value
  val with_user : string -> Mimic.ctx -> Mimic.ctx
  val with_authenticator : string -> Mimic.ctx -> Mimic.ctx
  val with_rsa_key : string -> Mimic.ctx -> Mimic.ctx
  val with_ed25519_key : string -> Mimic.ctx -> Mimic.ctx
  val with_resolv : Mimic.ctx -> Mimic.ctx
  val ctx : Mimic.ctx
  val with_smart_git_endpoint : string -> Mimic.ctx -> Mimic.ctx
end

module Destruct (SSH : sig
  type endpoint
  type flow

  val ssh_protocol : (endpoint, flow) Mimic.protocol
end) : sig
  val is : Mimic.flow -> bool
end
