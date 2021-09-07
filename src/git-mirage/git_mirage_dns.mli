module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4V6) (TCP : sig
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) : sig
  val with_dns :
    ?size:int ->
    ?nameservers:
      Dns.proto
      * [ `Plaintext of Ipaddr.t * int
        | `Tls of Tls.Config.client * Ipaddr.t * int ]
        list ->
    ?timeout:int64 ->
    Stack.t ->
    Mimic.ctx ->
    Mimic.ctx

  val ctx : Mimic.ctx
end
