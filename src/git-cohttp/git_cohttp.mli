module Make
    (Cohttp_client : Cohttp_lwt.S.Client with type resolvers = Conduit.resolvers) :
  Smart_git.HTTP
