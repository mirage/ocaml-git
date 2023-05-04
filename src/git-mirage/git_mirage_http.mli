val git_mirage_http_headers : (string * string) list Mimic.value
val git_mirage_http_tls_config : Tls.Config.client Mimic.value

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t

  val with_optional_tls_config_and_headers :
    ?headers:(string * string) list ->
    ?authenticator:string ->
    Mimic.ctx ->
    Mimic.ctx Lwt.t

  val ctx : Mimic.ctx
end

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (TCP : Tcpip.Tcp.S)
    (Happy_eyeballs : Mimic_happy_eyeballs.S with type flow = TCP.flow) : S
