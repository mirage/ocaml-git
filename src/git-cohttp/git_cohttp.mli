module Make
    (Cohttp_client : Cohttp_lwt.S.Client with type ctx = Conduit.resolvers) :
  Smart_git.HTTP
