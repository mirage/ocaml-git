module type S = sig
  type t
  type flow

  val happy_eyeballs : t Mimic.value

  val resolve :
    t ->
    string ->
    int list ->
    ((Ipaddr.t * int) * flow, [ `Msg of string ]) result Lwt.t
end

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6) : sig
  include S with type flow = Stack.TCP.flow

  module DNS : sig
    type t

    val create :
      ?size:int ->
      ?edns:[ `Auto | `Manual of Dns.Edns.t | `None ] ->
      ?nameservers:
        Dns.proto
        * [ `Plaintext of Ipaddr.t * int
          | `Tls of Tls.Config.client * Ipaddr.t * int ]
          list ->
      ?timeout:int64 ->
      Stack.t ->
      t

    val nameservers :
      t ->
      Dns.proto
      * [ `Plaintext of Ipaddr.t * int
        | `Tls of Tls.Config.client * Ipaddr.t * int ]
        list

    val getaddrinfo :
      t ->
      'r Dns.Rr_map.key ->
      'a Domain_name.t ->
      ('r, [> `Msg of string ]) result Lwt.t

    val gethostbyname :
      t ->
      [ `host ] Domain_name.t ->
      (Ipaddr.V4.t, [> `Msg of string ]) result Lwt.t

    val gethostbyname6 :
      t ->
      [ `host ] Domain_name.t ->
      (Ipaddr.V6.t, [> `Msg of string ]) result Lwt.t

    val get_resource_record :
      t ->
      'r Dns.Rr_map.key ->
      'a Domain_name.t ->
      ( 'r,
        [> `Msg of string
        | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
        | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ] )
      result
      Lwt.t
  end

  val connect :
    ?happy_eyeballs:Happy_eyeballs.t ->
    ?dns:DNS.t ->
    ?timer_interval:int64 ->
    Stack.t ->
    Mimic.ctx Lwt.t
end = struct
  module Happy_eyeballs =
    Happy_eyeballs_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)

  module DNS = Happy_eyeballs.DNS

  type t = Happy_eyeballs.t
  type flow = Stack.TCP.flow

  let happy_eyeballs = Mimic.make ~name:"happy-eyeballs"
  let resolve = Happy_eyeballs.connect

  (* XXX(dinosaure): [connect] is a function to fit with [functoria]. *)

  let connect ?happy_eyeballs:v ?dns ?timer_interval stack =
    let t =
      Happy_eyeballs.create ?happy_eyeballs:v ?dns ?timer_interval stack
    in
    Lwt.return (Mimic.add happy_eyeballs t Mimic.empty)
end

module TCPV4V6 (Stack : Tcpip.Stack.V4V6) : sig
  include
    Tcpip.Tcp.S
      with type t = Stack.TCP.t
       and type ipaddr = Ipaddr.t
       and type flow = Stack.TCP.flow

  val connect : Stack.t -> t Lwt.t
end = struct
  include Stack.TCP

  let connect stackv4v6 = Lwt.return (Stack.tcp stackv4v6)
end
