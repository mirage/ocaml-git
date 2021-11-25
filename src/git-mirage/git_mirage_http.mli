val git_mirage_http_headers : (string * string) list Mimic.value
val git_mirage_http_tls_config : Tls.Config.client Mimic.value

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t

  val with_optional_tls_config_and_headers :
    ?headers:(string * string) list ->
    ?tls_key_fingerprint:string ->
    ?tls_cert_fingerprint:string ->
    Mimic.ctx ->
    Mimic.ctx Lwt.t

  val ctx : Mimic.ctx
end

module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (TCP : Mirage_protocols.TCP)
    (Happy_eyeballs : Git_mirage_happy_eyeballs.S with type flow = TCP.flow) : S
