open Lwt.Infix

type 'a t = {
  r : Lwt_mutex.t;
  mutable b : int;
  g : Lwt_mutex.t;
  mutable shallow : 'a list;
}

(* XXX(dinosaure): simple Raynal's readers/writer lock. *)
(* TODO(dinosaure): use [libart] instead which can be lock-free. *)

let safely_get ~f t =
  Lwt_mutex.lock t.r >>= fun () ->
  t.b <- succ t.b;
  (if t.b = 1 then Lwt_mutex.lock t.g else Lwt.return ()) >>= fun () ->
  Lwt_mutex.unlock t.r;
  let res = f t.shallow in
  Lwt_mutex.lock t.r >>= fun () ->
  t.b <- pred t.b;
  if t.b = 0 then Lwt_mutex.unlock t.g;
  Lwt_mutex.unlock t.r;
  Lwt.return res

let identity x = x
let exists t ~equal uid = safely_get ~f:(List.exists (equal uid)) t
let get t = safely_get ~f:identity t

let append t uid =
  Lwt_mutex.lock t.g >>= fun () ->
  t.shallow <- uid :: t.shallow;
  Lwt_mutex.unlock t.g;
  Lwt.return_unit

let remove t ~equal uid =
  Lwt_mutex.lock t.g >>= fun () ->
  t.shallow <- List.filter (fun uid' -> not (equal uid uid')) t.shallow;
  Lwt_mutex.unlock t.g;
  Lwt.return_unit

let make shallow =
  { r = Lwt_mutex.create (); b = 0; g = Lwt_mutex.create (); shallow }
