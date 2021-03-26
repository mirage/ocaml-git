open Sigs

type configuration

val configuration : ?stateless:bool -> unit -> configuration

module Make
    (Scheduler : SCHED)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) : sig
  val push :
    ?uses_git_transport:bool ->
    capabilities:Smart.Capability.t list ->
    [ `Create of Ref.t | `Delete of Ref.t | `Update of Ref.t * Ref.t ] list ->
    host:
      [ `Addr of Ipaddr.t
      | `Domain of [ `host ] Domain_name.t
      | `Name of string ] ->
    string ->
    Flow.t ->
    (Uid.t, Uid.t Pck.t, 'git) store ->
    (Uid.t, Ref.t, Uid.t Pck.t, 'git, Scheduler.t) access ->
    configuration ->
    (Uid.t list -> unit -> string option IO.t) ->
    unit IO.t
end
