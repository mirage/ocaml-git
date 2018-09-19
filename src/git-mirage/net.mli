module type CONDUIT = sig
  include Conduit_mirage.S

  val context : t
  val resolver : Resolver_lwt.t
end

module Make (C : CONDUIT) : Git.Tcp.NET with type endpoint = Git.Gri.t
