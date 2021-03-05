type +'a t = ('a -> unit) -> unit

let return x k = k x
let bind t f k = t (fun x -> f x k)
let ( >>> ) a b k = a (fun () -> b k)
let ( >>= ) t f k = t (fun x -> f x k)
let ( >>| ) t f k = t (fun x -> k (f x))

let both a b =
  a >>= fun a ->
  b >>= fun b -> return (a, b)

module Ivar = struct
  type 'a state = Full of 'a | Empty of ('a -> unit) Queue.t
  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x =
    match t.state with
    | Full _ -> failwith "Ivar.fill"
    | Empty q ->
        t.state <- Full x;
        Queue.iter (fun f -> f x) q

  let read t k = match t.state with Full x -> k x | Empty q -> Queue.push k q
end

let fork f k =
  let ivar = Ivar.create () in
  f () (fun x -> Ivar.fill ivar x);
  k ivar

let fork_and_join f g =
  fork f >>= fun a ->
  fork g >>= fun b -> both (Ivar.read a) (Ivar.read b)

let fork_and_join_unit f g =
  fork f >>= fun a ->
  fork g >>= fun b -> Ivar.read a >>> Ivar.read b

type seq = { mutable prev : seq; mutable next : seq }
type prgn = Prgn : ('a -> unit) * 'a -> prgn

type node = {
  mutable node_prev : seq;
  mutable node_next : seq;
  prgn : prgn;
  mutable active : bool;
}

external node_of_seq : seq -> node = "%identity"
external seq_of_node : node -> seq = "%identity"

let is_empty seq = seq.next == seq

let remove node =
  if node.active then (
    node.active <- true;
    let seq = seq_of_node node in
    seq.prev.next <- seq.next;
    seq.next.prev <- seq.prev)

let pop seq =
  if is_empty seq then None
  else
    let res = node_of_seq seq.next in
    remove res;
    Some res.prgn

let add seq prgn =
  let node = { node_prev = seq.prev; node_next = seq; prgn; active = true } in
  seq.prev.next <- seq_of_node node;
  seq.prev <- seq_of_node node

let make_seq () =
  let rec seq = { prev = seq; next = seq } in
  seq

type pool = {
  seq : seq;
  work_mutex : Mutex.t;
  work_cond : Condition.t;
  working_cond : Condition.t;
  mutable working_cnt : int;
  mutable thread_cnt : int;
  mutable stop : bool;
}

let wait pool =
  Mutex.lock pool.work_mutex;
  let rec loop () =
    if
      ((not pool.stop) && (pool.working_cnt <> 0 || not (is_empty pool.seq)))
      || (pool.stop && pool.thread_cnt <> 0)
    then (
      Condition.wait pool.working_cond pool.work_mutex;
      loop ())
  in
  loop ();
  Mutex.unlock pool.work_mutex

let worker pool =
  let rec loop () =
    Mutex.lock pool.work_mutex;
    while is_empty pool.seq && not pool.stop do
      Condition.wait pool.work_cond pool.work_mutex
    done;
    if not pool.stop then (
      let res = pop pool.seq in
      pool.working_cnt <- pool.working_cnt + 1;
      Mutex.unlock pool.work_mutex;
      (try Option.iter (fun (Prgn (f, a)) -> f a) res with _exn -> ());
      Mutex.lock pool.work_mutex;
      pool.working_cnt <- pool.working_cnt - 1;
      if (not pool.stop) && pool.working_cnt = 0 && is_empty pool.seq then
        Condition.signal pool.working_cond;
      Mutex.unlock pool.work_mutex;
      loop ())
  in
  loop ();
  pool.thread_cnt <- pool.thread_cnt - 1;
  Condition.signal pool.working_cond;
  Mutex.unlock pool.work_mutex

let get_concurrency () =
  try
    let ic = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
    let close () = ignore (Unix.close_process_in ic) in
    let sc = Scanf.Scanning.from_channel ic in
    try
      Scanf.bscanf sc "%d" (fun n ->
          close ();
          n)
    with exn ->
      close ();
      raise exn
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ | End_of_file
  | Unix.Unix_error (_, _, _)
  ->
    1

let concurrency = ref (get_concurrency ())
let get_concurrency () = !concurrency
let set_concurrency v = concurrency := v

let make () =
  let pool =
    {
      working_cnt = 0;
      thread_cnt = !concurrency;
      stop = false;
      work_mutex = Mutex.create ();
      work_cond = Condition.create ();
      working_cond = Condition.create ();
      seq = make_seq ();
    }
  in
  for _ = 0 to !concurrency - 1 do
    ignore @@ Thread.create worker pool
  done;
  pool

let drop seq =
  let rec loop () = match pop seq with Some _ -> loop () | None -> () in
  loop ()

let reset pool =
  Mutex.lock pool.work_mutex;
  drop pool.seq;
  pool.stop <- true;
  Condition.broadcast pool.work_cond;
  Mutex.unlock pool.work_mutex;
  wait pool;
  pool.stop <- false;
  pool.thread_cnt <- !concurrency;
  for _ = 0 to !concurrency - 1 do
    ignore @@ Thread.create worker pool
  done

let pool = make ()

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x);
  wait pool;
  match !result with
  | Some x ->
      reset pool;
      x
  | None -> failwith "Fiber.run"

let detach prgn =
  let ivar = Ivar.create () in
  Mutex.lock pool.work_mutex;
  add pool.seq
    (Prgn
       ( (fun ivar ->
           let res = prgn () in
           Ivar.fill ivar res),
         ivar ));
  Condition.signal pool.work_cond;
  Mutex.unlock pool.work_mutex;
  Ivar.read ivar

module Mutex = struct
  type 'a fiber = 'a t
  type t = Mutex.t

  let create () = Mutex.create ()

  let lock t =
    fork (fun () ->
        Mutex.lock t;
        return ())
    >>= fun ivar -> Ivar.read ivar

  let unlock t = Mutex.unlock t
end

module Condition = struct
  type 'a fiber = 'a t
  type mutex = Mutex.t
  type t = Condition.t

  let create () = Condition.create ()

  let wait t mutex =
    fork (fun () ->
        Condition.wait t mutex;
        return ())
    >>= fun ivar -> Ivar.read ivar

  let signal t = Condition.signal t
  let broadcast t = Condition.broadcast t
end

let rec parallel_iter ~f lst =
  match lst with
  | [] -> return ()
  | x :: r ->
      fork (fun () -> f x) >>= fun ivar ->
      parallel_iter ~f r >>= fun () -> Ivar.read ivar

let rec parallel_map ~f lst =
  match lst with
  | [] -> return []
  | x :: r ->
      fork (fun () -> f x) >>= fun ivar ->
      parallel_map ~f r >>= fun r ->
      Ivar.read ivar >>= fun x -> return (x :: r)
