(** This interface describes the minimal I/O operations to a git repository. *)
module Make
    (N : Tcp.NET)
    (E : Sync.ENDPOINT with type t = N.endpoint)
    (G : Minimal.S) : Sync.S with module Store = G and module Endpoint = E
