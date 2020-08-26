open Sigs
module Scheduler : SCHED with type +'a s = 'a Lwt.t

val lwt : Scheduler.t scheduler
val lwt_io : (Conduit_lwt.flow, Conduit_lwt.error, Scheduler.t) flow
val lwt_fail : exn -> ('a, Scheduler.t) io
