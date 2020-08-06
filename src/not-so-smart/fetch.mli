open Sigs

type configuration = Neg.configuration

val configuration : ?stateless:bool -> Smart.Capability.t list -> configuration

module Make
    (Scheduler : SCHED)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) : sig
  val fetch_v1 :
    ?prelude:bool ->
    capabilities:Smart.Capability.t list ->
    ?want:[ `All | `Some of Ref.t list | `None ] ->
    host:[ `host ] Domain_name.t ->
    string ->
    Flow.t ->
    (Uid.t, Uid.t * int ref * int64, 'g) store ->
    (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) access ->
    configuration ->
    (string * int * int -> unit) ->
    (Ref.t * Uid.t) list IO.t
end
