type endpoint = {
  port : int;
  hostname : string;
  authenticator : Awa.Keys.authenticator option;
  user : string;
  key : Awa.Hostkey.priv;
  path : string;
  capabilities : [ `Rd | `Wr ];
}

val git_mirage_ssh_key : Awa.Hostkey.priv Mimic.value
val git_mirage_ssh_authenticator : Awa.Keys.authenticator Mimic.value

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t

  val with_optionnal_key :
    ?authenticator:string -> key:string option -> Mimic.ctx -> Mimic.ctx Lwt.t

  val ctx : Mimic.ctx
end

module Make
    (Mclock : Mirage_clock.MCLOCK)
    (TCP : Tcpip.Tcp.S)
    (Time : Mirage_time.S)
    (Happy_eyeballs : Mimic_happy_eyeballs.S with type flow = TCP.flow) : S
