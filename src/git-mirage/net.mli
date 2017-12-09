module type CONDUIT = sig
  include Conduit_mirage.S
  val context: t
  val resolver: Resolver_lwt.t
end

module Make (C: CONDUIT): Git.Sync.NET
