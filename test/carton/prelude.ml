type fd = { fd : Unix.file_descr; mx : int64 }

let unix_map : fd Carton.Dec.W.map =
 fun fd ~pos len ->
  let payload =
    let len = min Int64.(to_int (sub fd.mx pos)) len in
    Mmap.V1.map_file fd.fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray payload

module IO = struct
  type 'a t = ('a -> unit) -> unit

  let return x k = k x
  let bind t f k = t (fun x -> f x k)
  let ( >>= ) x f = bind x f

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

    let read t k =
      match t.state with Full x -> k x | Empty q -> Queue.push k q
  end

  module Future = struct let wait = Ivar.read end

  let fork f k =
    let ivar = Ivar.create () in
    f () (fun x -> Ivar.fill ivar x);
    k ivar

  type tpool = {
    seq : seq;
    work_mutex : Mutex.t;
    work_cond : Condition.t;
    working_cond : Condition.t;
    mutable working_cnt : int;
    mutable thread_cnt : int;
    mutable stop : bool;
  }

  and seq = { mutable prev : seq; mutable next : seq }

  let make_seq () =
    let rec seq = { prev = seq; next = seq } in
    seq

  type prgn = Prgn : ('a -> unit) * 'a -> prgn

  type node = {
    mutable node_prev : seq;
    mutable node_next : seq;
    prgn : prgn;
    mutable active : bool;
  }

  external node_of_seq : seq -> node = "%identity"
  external seq_of_node : node -> seq = "%identity"

  let is_empty tm = tm.seq.next == tm.seq

  let wait tm =
    Mutex.lock tm.work_mutex;
    let rec loop () =
      if
        ((not tm.stop) && (tm.working_cnt <> 0 || not (is_empty tm)))
        || (tm.stop && tm.thread_cnt <> 0)
      then (
        Condition.wait tm.working_cond tm.work_mutex;
        loop ())
    in
    loop ();
    Mutex.unlock tm.work_mutex

  let remove node =
    if node.active then (
      node.active <- false;
      let seq = seq_of_node node in
      seq.prev.next <- seq.next;
      seq.next.prev <- seq.prev)

  let pop tm =
    if is_empty tm then None
    else
      let res = node_of_seq tm.seq.next in
      remove res;
      Some res.prgn

  let worker tm =
    let rec loop () =
      Mutex.lock tm.work_mutex;
      while is_empty tm && not tm.stop do
        Condition.wait tm.work_cond tm.work_mutex
      done;
      if not tm.stop then (
        let res = pop tm in
        tm.working_cnt <- tm.working_cnt + 1;
        Mutex.unlock tm.work_mutex;
        (match res with Some (Prgn (f, a)) -> f a | None -> ());
        Mutex.lock tm.work_mutex;
        tm.working_cnt <- tm.working_cnt - 1;
        if (not tm.stop) && tm.working_cnt = 0 && is_empty tm then
          Condition.signal tm.working_cond;
        Mutex.unlock tm.work_mutex;
        loop ())
    in
    loop ();
    tm.thread_cnt <- tm.thread_cnt - 1;
    Condition.signal tm.working_cond;
    Mutex.unlock tm.work_mutex

  let concurrency = ref 4

  let make () =
    let tm =
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
      let _ = Thread.create worker tm in
      ()
    done;
    tm

  let drop tm =
    let rec loop () = match pop tm with Some _ -> loop () | None -> () in
    loop ()

  let reset tm =
    Mutex.lock tm.work_mutex;
    drop tm;
    tm.stop <- true;
    Condition.broadcast tm.work_cond;
    Mutex.unlock tm.work_mutex;
    wait tm;
    tm.stop <- false;
    tm.thread_cnt <- !concurrency;
    for _ = 0 to !concurrency - 1 do
      let _ = Thread.create worker tm in
      ()
    done

  let add tm prgn =
    Mutex.lock tm.work_mutex;
    let node =
      { node_prev = tm.seq.prev; node_next = tm.seq; prgn; active = true }
    in
    tm.seq.prev.next <- seq_of_node node;
    tm.seq.prev <- seq_of_node node;
    Condition.signal tm.work_cond;
    Mutex.unlock tm.work_mutex

  let tm = make ()

  let run fiber =
    let result = ref None in
    fiber (fun x -> result := Some x);
    wait tm;
    match !result with
    | Some x ->
        reset tm;
        x
    | None -> failwith "IO.run"

  module Mutex = struct
    type 'a fiber = 'a t

    let create () = Mutex.create ()

    let lock : Mutex.t -> unit t =
     fun t ->
      fork (fun () ->
          Mutex.lock t;
          return ())
      >>= fun future -> Future.wait future

    let unlock t = Mutex.unlock t

    type t = Mutex.t
  end

  module Condition = struct
    type 'a fiber = 'a t
    type mutex = Mutex.t
    type t = Condition.t

    let create () = Condition.create ()

    let wait mutex t =
      fork (fun () ->
          Condition.wait mutex t;
          return ())
      >>= fun future -> Future.wait future

    let signal t = Condition.signal t
    let broadcast t = Condition.broadcast t
  end

  let rec parallel_iter ~f = function
    | [] -> return ()
    | x :: r ->
        fork (fun () -> f x) >>= fun future ->
        parallel_iter ~f r >>= fun () -> Future.wait future

  let create_process tm prgn = add tm (Prgn (prgn, ()))

  let detach prgn =
    let ivar = Ivar.create () in
    create_process tm (fun () ->
        let res = prgn () in
        Ivar.fill ivar res);
    Ivar.read ivar

  let rec parallel_map ~f = function
    | [] -> return []
    | x :: r ->
        fork (fun () -> f x) >>= fun future ->
        parallel_map ~f r >>= fun r ->
        Future.wait future >>= fun x -> return (x :: r)
end

module Us = Carton.Make (struct type 'a t = 'a IO.t end)

let unix =
  let open IO in
  let open Us in
  {
    Carton.bind = (fun x f -> inj (bind (prj x) (fun x -> prj (f x))));
    Carton.return = (fun x -> inj (return x));
  }

let unix_read : (in_channel, Us.t) Carton.Dec.read =
 fun fd buf ~off ~len ->
  let n = input fd buf off len in
  Us.inj (IO.return n)
