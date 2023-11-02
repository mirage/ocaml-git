open Sigs

module V1 : sig
  type configuration = Neg.configuration

  val configuration :
    ?stateless:bool -> Smart.Capability.t list -> configuration
end

module Make
    (Scheduler : SCHED)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) : sig
  module V1 : sig
    val fetch :
      ?uses_git_transport:bool ->
      ?push_stdout:(string -> unit) ->
      ?push_stderr:(string -> unit) ->
      capabilities:Smart.Capability.t list ->
      ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
      ?want:[ `All | `Some of Ref.t list | `None ] ->
      host:string ->
      string ->
      Flow.t ->
      (Uid.t, Uid.t * int ref * int64, 'g) store ->
      (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) access ->
      V1.configuration ->
      (string * int * int -> unit IO.t) ->
      (Ref.t * Uid.t) list IO.t
  end

  module V2 : sig
    val get_server_capabilities :
      ?uses_git_transport:bool ->
      host:[ `host ] Domain_name.t ->
      path:string ->
      Wire_proto_v2.Context.t ->
      Flow.t ->
      Capability.t list IO.t

    val ls_refs_request :
      ?uses_git_transport:bool ->
      host:[ `host ] Domain_name.t ->
      path:string ->
      State.Context.t ->
      Flow.t ->
      Wire_proto_v2.Proto_vals_v2.Ls_refs.request ->
      Wire_proto_v2.Proto_vals_v2.Ls_refs.response IO.t
  end
end
