module Client
    (P : Mirage_clock.PCLOCK)
    (R : Resolver_mirage.S)
    (S : Conduit_mirage.S) : sig
  include Smart_git.HTTP

  val ctx : ?authenticator:X509.Authenticator.t -> R.t -> S.t -> Mimic.ctx
end
