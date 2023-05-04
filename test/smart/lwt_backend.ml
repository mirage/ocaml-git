module Scheduler = Hkt.Make_sched (struct
  type +'a t = 'a Lwt.t
end)

let lwt =
  let open Scheduler in
  let open Lwt.Infix in
  Sigs.
    {
      bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      return = (fun x -> inj (Lwt.return x));
    }

module Flow = Unixiz.Make (Mimic)

let lwt_io =
  let open Scheduler in
  Sigs.
    {
      recv = (fun flow raw -> inj (Flow.recv flow raw));
      send = (fun flow raw -> inj (Flow.send flow raw));
      pp_error = Flow.pp_error;
    }

let lwt_fail exn = Scheduler.inj (Lwt.fail exn)
