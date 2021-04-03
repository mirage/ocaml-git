module Lwt_scheduler = Carton.Make (struct type +'a t = 'a Lwt.t end)

type lwt = Lwt_scheduler.t

let prj x = Lwt_scheduler.prj x [@@inline]
let inj x = Lwt_scheduler.inj x [@@inline]

module Mutex = struct
  type 'a fiber = 'a Lwt.t
  type t = Lwt_mutex.t

  let create () = Lwt_mutex.create ()
  let lock t = Lwt_mutex.lock t
  let unlock t = Lwt_mutex.unlock t
end

module Condition = struct
  type 'a fiber = 'a Lwt.t
  type mutex = Mutex.t
  type t = unit Lwt_condition.t

  let create () = Lwt_condition.create ()
  let wait t mutex = Lwt_condition.wait ~mutex t
  let signal t = Lwt_condition.signal t ()
  let broadcast t = Lwt_condition.broadcast t ()
end

type 'a t = 'a Lwt.t

let bind x f = Lwt.bind x f
let return x = Lwt.return x
let parallel_map ~f lst = Lwt_list.map_p f lst
let parallel_iter ~f lst = Lwt_list.iter_p f lst

(* XXX(dinosaure): provide the opportunity to use
 * [Lwt_preemptive.detach]? *)
let detach f =
  let th, wk = Lwt.wait () in
  Lwt.async (fun () ->
      let res = f () in
      Lwt.wakeup_later wk res;
      Lwt.return_unit);
  th
