open Sigs
module Scheduler : SCHED with type +'a s = 'a Lwt.t
module Flow : module type of Unixiz.Make (Mimic)

val lwt : Scheduler.t scheduler
val lwt_io : (Flow.t, Flow.error, Scheduler.t) flow
val lwt_fail : exn -> ('a, Scheduler.t) io
