open Sigs

module Log =
  (val let src = Logs.Src.create "find-common" in
       Logs.src_log src
      : Logs.LOG)

type configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

type 'uid hex = {
  to_hex : 'uid -> string;
  of_hex : string -> 'uid;
  compare : 'uid -> 'uid -> int;
}

(* Constants defined by the canoncial git implementation in C *)
let initial_flush = 16
let max_in_vain = 256
let large_flush = 16384
let pipe_safe_flush = 32

(** module type that defins common functions for a scheduler, e.g., Lwt or Async *)
module type Io_monad = sig
  type s

  val bind : ('a, s) io -> ('a -> ('b, s) io) -> ('b, s) io
  val map : ('a -> 'b) -> ('a, s) io -> ('b, s) io
  val ( >>= ) : ('a, s) io -> ('a -> ('b, s) io) -> ('b, s) io
  val ( >>| ) : ('a, s) io -> ('a -> 'b) -> ('b, s) io
  val return : 'a -> ('a, s) io

  val fold_left_s :
    f:('a -> 'b -> ('a, s) io) -> init:'a -> 'b list -> ('a, s) io
end

(** given ['s Sigs.scheduler], returns a module of type [Io_monad] that has
    infix operations, etc. This allows us to avoid repetitive redefinition of common
    functions. *)
let io_monad (type t) { bind; return } =
  (module struct
    type s = t

    let bind = bind
    let return = return
    let map f x = bind x (fun v -> return (f v))
    let ( >>= ) = bind
    let ( >>| ) x f = map f x

    let fold_left_s ~f ~init l =
      let rec go a = function
        | [] -> return a
        | x :: r -> bind (f a x) (fun a' -> go a' r)
      in
      go init l
  end : Io_monad
    with type s = t)

(* XXX(dinosaure): this part is really **ugly**! But we must follow the same
   behaviour of [git]. Instead to understand the synchronisation process of [git]
   with Smart.v1 and implement a state of the art synchronisation algorithm, I
   translated as is [fetch-pack.c:find_common] in OCaml. *)

let tips (type t) scheduler access store negotiator =
  let open (val io_monad scheduler : Io_monad with type s = t) in
  access.locals store >>= fun ref_lst ->
  fold_left_s ref_lst ~init:() ~f:(fun () reference ->
      access.deref store reference
      >>= Option.fold ~none:(return None) ~some:(fun uid ->
              access.get uid store)
      >>| Option.iter (fun obj -> Default.tip negotiator obj))

let consume_shallow_list (type t) scheduler io flow cfg deepen { of_hex; _ } ctx
    =
  let open (val io_monad scheduler : Io_monad with type s = t) in
  if cfg.stateless && Option.is_some deepen then
    Smart_flow.run scheduler raise io flow Smart.(recv ctx shallows)
    >>| fun shallows -> List.map (Smart.Shallow.map ~f:of_hex) shallows
  else return []

let handle_shallow (type t) scheduler io flow { of_hex; _ } access store ctx =
  let open (val io_monad scheduler : Io_monad with type s = t) in
  Smart_flow.run scheduler raise io flow Smart.(recv ctx shallows)
  >>= fun shallows ->
  let shallows = List.map (Smart.Shallow.map ~f:of_hex) shallows in
  fold_left_s shallows ~init:() ~f:(fun () -> function
    | Smart.Shallow.Shallow uid -> access.shallow store uid
    | Unshallow uid -> access.unshallow store uid)

let unsafe_write_have ctx hex =
  let packet = Fmt.str "have %s\n" hex in
  Smart.Unsafe.write ctx packet

let next_flush stateless count =
  if stateless then if count < large_flush then count lsl 1 else count * 11 / 10
  else if count < pipe_safe_flush then count lsl 1
  else count + pipe_safe_flush

let find_common (type t) scheduler io flow cfg
    ({ to_hex; of_hex; compare } as hex) access store negotiator ctx
    ?(deepen : [ `Depth of int | `Timestamp of int64 ] option) refs =
  let open (val io_monad scheduler : Io_monad with type s = t) in
  let { stateless; no_done; _ } = cfg in

  let fold acc remote_uid =
    access.get remote_uid store >>| function
    | Some _ -> acc
    | None -> (remote_uid, ref 0) :: acc
  in

  fold_left_s ~f:fold ~init:[] refs
  >>| List.sort_uniq (fun (a, _) (b, _) -> compare a b)
  >>= function
  | [] ->
      Log.debug (fun m -> m "Nothing to download.");
      Smart_flow.run scheduler raise io flow Smart.(send ctx flush ())
      >>= fun () -> return `Close
  | (uid, _) :: others as refs ->
      Log.debug (fun m -> m "We want %d commit(s)." (List.length refs));
      access.shallowed store >>= fun shallowed ->
      let shallowed = List.map to_hex shallowed in
      Smart_flow.run scheduler raise io flow
        Smart.(
          let uid = to_hex uid in
          let others = List.map (fun (uid, _) -> to_hex uid) others in
          let { Smart.Context.my_caps; _ } = Smart.Context.capabilities ctx in
          let deepen =
            (deepen
              :> [ `Depth of int | `Not of string | `Timestamp of int64 ] option)
          in
          send ctx send_want
            (Want.v ~capabilities:my_caps ~shallows:shallowed ?deepen
               (uid :: others)))
      >>= fun () ->
      (match deepen with
      | None -> return ()
      | Some _ -> handle_shallow scheduler io flow hex access store ctx)
      >>= fun () ->
      let in_vain = ref 0 in
      let count = ref 0 in
      let flush_at = ref initial_flush in
      let flushes = ref 0 in
      let got_continue = ref false in
      let got_ready = ref false in
      let retval = ref (-1) in
      (* TODO(dinosaure): handle [shallow] and [unshallow]. *)
      let rec go negotiator =
        Default.next scheduler ~parents:access.parents store negotiator
        >>= function
        | None ->
            Log.debug (fun m -> m "Stop the negotiation loop.");
            return ()
        | Some uid ->
            Log.debug (fun m -> m "[+] have %s." (to_hex uid));
            unsafe_write_have ctx (to_hex uid);
            (* completely unsafe! *)
            incr in_vain;
            incr count;
            Log.debug (fun m ->
                m "count: %d, in-vain: %d, flush-at: %d.\n%!" !count !in_vain
                  !flush_at);
            if !flush_at <= !count then (
              Smart_flow.run scheduler raise io flow Smart.(send ctx flush ())
              >>= fun () ->
              incr flushes;
              flush_at := next_flush stateless !count;
              if (not stateless) && !count = initial_flush then go negotiator
              else
                consume_shallow_list scheduler io flow cfg None hex ctx
                >>= fun _shallows ->
                let rec loop () =
                  Smart_flow.run scheduler raise io flow
                    Smart.(recv ctx recv_ack)
                  >>= function
                  | `NAK ->
                      Log.debug (fun m -> m "Receive NAK.");
                      return `Continue
                  | `ACK ack -> (
                      let ack = Smart.Negotiation.map ~f:of_hex ack in
                      match ack with
                      | ACK _ ->
                          flushes := 0;
                          cfg.multi_ack <- `None;
                          (* XXX(dinosaure): [multi_ack] supported by the client but it
                             is not supported by the server. TODO: use [Context.shared]. *)
                          retval := 0;
                          return `Done
                      | ACK_common uid | ACK_ready uid | ACK_continue uid -> (
                          access.get uid store >>= function
                          | None -> assert false
                          | Some obj ->
                              Default.ack scheduler ~parents:access.parents
                                store negotiator obj
                              >>= fun was_common ->
                              if
                                stateless
                                && Smart.Negotiation.is_common ack
                                && not was_common
                              then (
                                (* we need to replay the have for this object on the next RPC request so
                                   the peer kows it is in common with us. *)
                                Log.debug (fun m ->
                                    m "[+] have %s." (to_hex uid));
                                unsafe_write_have ctx (to_hex uid);
                                (* reset [in_vain] because an ack for this commit has not been seen. *)
                                in_vain := 0;
                                retval := 0;
                                got_continue := true;
                                loop ())
                              else if
                                (not stateless)
                                || not (Smart.Negotiation.is_common ack)
                              then (
                                in_vain := 0;
                                retval := 0;
                                got_continue := true;
                                if Smart.Negotiation.is_ready ack then
                                  got_ready := true;
                                loop ())
                              else (
                                retval := 0;
                                got_continue := true;
                                if Smart.Negotiation.is_ready ack then
                                  got_ready := true;
                                loop ())))
                in
                loop () >>= function
                | `Done -> return ()
                | `Continue ->
                    decr flushes;
                    if !got_continue && max_in_vain < !in_vain then return ()
                    else if !got_ready then return ()
                    else go negotiator)
            else go negotiator
      in
      go negotiator >>= fun () ->
      Log.debug (fun m ->
          m "Negotiation (got ready: %b, no-done: %b)." !got_ready no_done);
      (if (not !got_ready) || not no_done then
         Smart_flow.run scheduler raise io flow
           Smart.(send ctx negotiation_done ())
       else return ())
      >>= fun () ->
      if !retval <> 0 then (
        cfg.multi_ack <- `None;
        incr flushes);
      (if (not !got_ready) || not no_done then (
         Log.debug (fun m -> m "Negotiation is done!");
         Smart_flow.run scheduler raise io flow Smart.(recv ctx shallows)
         >>= fun _shallows -> return ())
       else return ())
      >>= fun () ->
      let rec go () =
        if !flushes > 0 || cfg.multi_ack = `Some || cfg.multi_ack = `Detailed
        then
          Smart_flow.run scheduler raise io flow Smart.(recv ctx recv_ack)
          >>= function
          | `NAK ->
              decr flushes;
              go ()
          | `ACK ack -> (
              let ack = Smart.Negotiation.map ~f:of_hex ack in
              match ack with
              | Smart.Negotiation.ACK _ -> return (`Continue 0)
              | ACK_common _ | ACK_continue _ | ACK_ready _ ->
                  cfg.multi_ack <- `Some;
                  go ())
        else if !count > 0 then return (`Continue !retval)
        else return (`Continue 0)
      in
      go ()
