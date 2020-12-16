open Sigs
open Stdlib

let ( <.> ) f g x = f (g x)
let src = Logs.Src.create "find-common"

module Log = (val Logs.src_log src : Logs.LOG)

let _initial_flush = 16
let _max_in_vain = 256
let _large_flush = 16384
let _pipe_safe_flush = 32

type ('a, 's) raise = exn -> ('a, 's) io

let io_buffer_size = 65536

let run :
    type f s.
    s scheduler ->
    ('a, s) raise ->
    (f, 'error, s) flow ->
    f ->
    ('res, [ `Protocol of Smart.error ]) Smart.t ->
    ('res, s) io =
 fun { bind; return } raise { recv; send; pp_error } flow fiber ->
  let ( >>= ) = bind in
  let tmp = Cstruct.create io_buffer_size in
  let failwithf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt in
  let rec go = function
    | Smart.Read { k; buffer; off; len; eof } -> (
        let max = min (Cstruct.len tmp) len in
        Log.debug (fun m -> m "Start to read %d byte(s)." max);
        recv flow (Cstruct.sub tmp 0 max) >>= function
        | Ok `End_of_flow ->
            Log.debug (fun m -> m "Got end of input.");
            go (eof ())
        | Ok (`Input len) ->
            Log.debug (fun m -> m "Got %d/%d byte(s)." len max);
            Cstruct.blit_to_bytes tmp 0 buffer off len;
            go (k len)
        | Error err ->
            Log.err (fun m -> m "Got an error: %a." pp_error err);
            failwithf "%a" pp_error err)
    | Smart.Write { k; buffer; off; len } ->
        let rec loop tmp =
          if Cstruct.len tmp = 0 then go (k len)
          else
            send flow tmp >>= function
            | Ok shift -> loop (Cstruct.shift tmp shift)
            | Error err -> failwithf "%a" pp_error err
        in
        Log.debug (fun m -> m "Write %d byte(s)." len);
        loop (Cstruct.of_string buffer ~off ~len)
    | Smart.Return v -> return v
    | Smart.Error (`Protocol err) ->
        Log.err (fun m -> m "Got a protocol error: %a." Smart.pp_error err);
        failwithf "%a" Smart.pp_error err
  in
  go fiber

(* XXX(dinosaure): this part is really **ugly**! But we must follow the same
   behaviour of [git]. Instead to understand the synchronisation process of [git]
   with Smart.v1 and implement a state of the art synchronisation algorithm, I
   translated as is [fetch-pack.c:find_common] in OCaml. *)

let unsafe_write_have ctx hex =
  let packet = Fmt.str "have %s\n" hex in
  Smart.Unsafe.write ctx packet

let next_flush stateless count =
  if stateless then
    if count < _large_flush then count lsl 1 else count * 11 / 10
  else if count < _pipe_safe_flush then count lsl 1
  else count + _pipe_safe_flush

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

let tips { bind; return } { get; deref; locals; _ } store negotiator =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in

  let rec go = function
    | [] -> return ()
    | reference :: others ->
        deref store reference
        >>= Option.fold ~none:(return None) ~some:(fun uid -> get uid store)
        >>| Option.iter (fun obj -> Default.tip negotiator obj)
        >>= fun () -> go others
  in
  locals store >>= go

let consume_shallow_list ({ bind; return } as scheduler) io flow cfg deepen
    { of_hex; _ } _access _store ctx =
  let ( >>= ) = bind in
  if cfg.stateless && Option.is_some deepen then
    run scheduler raise io flow Smart.(recv ctx shallows) >>= fun shallows ->
    let lst = List.map (Smart.Shallow.map ~f:of_hex) shallows in
    return lst
  else return []

let handle_shallow ({ bind; return } as scheduler) io flow { of_hex; _ } access
    store ctx =
  let ( >>= ) = bind in
  run scheduler raise io flow Smart.(recv ctx shallows) >>= fun shallows ->
  let lst = List.map (Smart.Shallow.map ~f:of_hex) shallows in
  let f = function
    | Smart.Shallow.Shallow uid -> access.shallow store uid
    | Smart.Shallow.Unshallow uid -> access.unshallow store uid
  in
  let rec go = function [] -> return () | h :: t -> f h >>= fun () -> go t in
  go lst

let find_common ({ bind; return } as scheduler) io flow
    ({ stateless; no_done; _ } as cfg) ({ to_hex; of_hex; compare } as hex)
    access store negotiator ctx
    ?(deepen : [ `Depth of int | `Timestamp of int64 ] option) refs =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in
  let fold_left_s ~f a l =
    let rec go a = function
      | [] -> return a
      | x :: r -> f a x >>= fun a -> go a r
    in
    go a l
  in
  let fold acc remote_uid =
    Log.debug (fun m -> m "<%s> exists locally?" (to_hex remote_uid));
    access.get remote_uid store >>= function
    | Some _ -> return acc
    | None -> return ((remote_uid, ref 0) :: acc)
  in
  fold_left_s ~f:fold [] refs
  >>| List.sort_uniq (fun (a, _) (b, _) -> compare a b)
  >>= function
  | [] ->
      Log.debug (fun m -> m "Nothing to download.");
      run scheduler raise io flow Smart.(send ctx flush ()) >>= fun () ->
      return `Close
  | uid :: others ->
      Log.debug (fun m ->
          m "We want %d commit(s)." (List.length (uid :: others)));
      access.shallowed store >>= fun shallowed ->
      let shallowed = List.map to_hex shallowed in
      run scheduler raise io flow
        Smart.(
          let uid = (to_hex <.> fst) uid in
          let others = List.map (to_hex <.> fst) others in
          let capabilities, _ = Smart.Context.capabilities ctx in
          let deepen =
            (deepen
              :> [ `Depth of int | `Not of string | `Timestamp of int64 ] option)
          in
          send ctx want
            (Want.want ~capabilities ~shallows:shallowed ?deepen uid ~others))
      >>= fun () ->
      (match deepen with
      | None -> return ()
      | Some _ -> handle_shallow scheduler io flow hex access store ctx)
      >>= fun () ->
      let in_vain = ref 0 in
      let count = ref 0 in
      let flush_at = ref _initial_flush in
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
              run scheduler raise io flow Smart.(send ctx flush ())
              >>= fun () ->
              incr flushes;
              flush_at := next_flush stateless !count;
              if (not stateless) && !count = _initial_flush then go negotiator
              else
                consume_shallow_list scheduler io flow cfg None hex access store
                  ctx
                >>= fun _shallows ->
                let rec loop () =
                  run scheduler raise io flow Smart.(recv ctx ack)
                  >>| Smart.Negotiation.map ~f:of_hex
                  >>= fun ack ->
                  match ack with
                  | Smart.Negotiation.NAK ->
                      Log.debug (fun m -> m "Receive NAK.");
                      return `Continue
                  | Smart.Negotiation.ACK _ ->
                      flushes := 0;
                      cfg.multi_ack <- `None;
                      (* XXX(dinosaure): [multi_ack] supported by the client but it
                         is not supported by the server. TODO: use [Context.shared]. *)
                      retval := 0;
                      return `Done
                  | Smart.Negotiation.ACK_common uid
                  | Smart.Negotiation.ACK_ready uid
                  | Smart.Negotiation.ACK_continue uid -> (
                      access.get uid store >>= function
                      | None -> assert false
                      | Some obj ->
                          Default.ack scheduler ~parents:access.parents store
                            negotiator obj
                          >>= fun was_common ->
                          if
                            stateless
                            && Smart.Negotiation.is_common ack
                            && not was_common
                          then (
                            (* we need to replay the have for this object on the next RPC request so
                               the peer kows it is in common with us. *)
                            Log.debug (fun m -> m "[+] have %s." (to_hex uid));
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
                            loop ()))
                in
                loop () >>= function
                | `Done -> return ()
                | `Continue ->
                    decr flushes;
                    if !got_continue && _max_in_vain < !in_vain then return ()
                    else if !got_ready then return ()
                    else go negotiator)
            else go negotiator
      in
      go negotiator >>= fun () ->
      Log.debug (fun m ->
          m "Negotiation (got ready: %b, no-done: %b)." !got_ready no_done);
      (if (not !got_ready) || not no_done then
       run scheduler raise io flow Smart.(send ctx negotiation_done ())
      else return ())
      >>= fun () ->
      if !retval <> 0 then (
        cfg.multi_ack <- `None;
        incr flushes);
      (if (not !got_ready) || not no_done then (
       Log.debug (fun m -> m "Negotiation is done!");
       run scheduler raise io flow Smart.(recv ctx shallows)
       >>= fun _shallows -> return ())
      else return ())
      >>= fun () ->
      let rec go () =
        if !flushes > 0 || cfg.multi_ack = `Some || cfg.multi_ack = `Detailed
        then (
          run scheduler raise io flow Smart.(recv ctx ack)
          >>| Smart.Negotiation.map ~f:of_hex
          >>= fun ack ->
          match ack with
          | Smart.Negotiation.ACK _ -> return (`Continue 0)
          | Smart.Negotiation.ACK_common _ | Smart.Negotiation.ACK_continue _
          | Smart.Negotiation.ACK_ready _ ->
              cfg.multi_ack <- `Some;
              go ()
          | Smart.Negotiation.NAK ->
              decr flushes;
              go ())
        else if !count > 0 then return (`Continue !retval)
        else return (`Continue 0)
      in
      go ()
