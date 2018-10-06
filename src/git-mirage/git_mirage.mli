module Net = Net

type endpoint = Net.endpoint = {
  uri     : Uri.t;
  conduit : Conduit_mirage.t;
  resolver: Resolver_lwt.t;
  headers : Cohttp.Header.t;
}

val endpoint:
  ?conduit:Conduit_mirage.t -> ?resolver:Resolver_lwt.t ->
  ?headers:Cohttp.Header.t -> Uri.t -> endpoint

module Sync (G : Git.S): sig

  module Tcp : Git.Sync.S with module Store := G
                           and type Endpoint.t = endpoint

  module Http: Git_http.Sync.S with module Store := G
                                and type Client.endpoint = endpoint

  include Git.Sync.S with module Store := G
                      and type Endpoint.t = endpoint
end
