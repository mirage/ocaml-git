module Scheduler = Sigs.Make_sched (struct
  type +'a t = 'a
end)

let unix =
  let open Scheduler in
  { Sigs.bind = (fun x f -> f (prj x)); Sigs.return = (fun x -> inj x) }
