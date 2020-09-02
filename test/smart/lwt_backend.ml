module Scheduler = Sigs.Make_sched (struct type +'a t = 'a Lwt.t end)

let lwt =
  let open Scheduler in
  let open Lwt.Infix in
  {
    Sigs.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    Sigs.return = (fun x -> inj (Lwt.return x));
  }

let lwt_io =
  let open Scheduler in
  {
    Sigs.recv = (fun flow raw -> inj (Conduit_lwt.recv flow raw));
    Sigs.send = (fun flow raw -> inj (Conduit_lwt.send flow raw));
    Sigs.pp_error = Conduit_lwt.pp_error;
  }

let lwt_fail exn = Scheduler.inj (Lwt.fail exn)
