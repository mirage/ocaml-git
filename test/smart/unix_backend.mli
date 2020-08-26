open Sigs
module Scheduler : SCHED with type +'a s = 'a

val unix : Scheduler.t scheduler
