module Make
    (Digestif : Digestif.S)
    (Pack : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Index : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Conduit : Conduit.S
                 with type +'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (Store : Minimal.S with type hash = Digestif.t)
    (HTTP : Smart_git.HTTP) : sig
  type hash = Digestif.t

  type store = Store.t

  val fetch :
    resolvers:Conduit.resolvers ->
    Smart_git.endpoint ->
    store ->
    ?version:[> `V1 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `All | `Some of Reference.t list | `None ] ->
    src:Pack.uid ->
    dst:Pack.uid ->
    idx:Index.uid ->
    Pack.t ->
    Index.t ->
    ( [ `Pack of hash * (Reference.t * hash) list | `Empty ],
      [> `Msg of string | `Exn of exn | `Not_found | `Store of Store.error ] )
    result
    Lwt.t
end
