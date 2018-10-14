type endpoint =
  { uri: Uri.t
  ; conduit: Conduit_mirage.t
  ; resolver: Resolver_lwt.t
  ; headers: Cohttp.Header.t }

include Git.Tcp.NET with type endpoint := endpoint
