type configuration = Neg.configuration

let multi_ack capabilities =
  match
    ( List.exists (( = ) `Multi_ack) capabilities,
      List.exists (( = ) `Multi_ack_detailed) capabilities )
  with
  | true, true | false, true -> `Detailed
  | true, false -> `Some
  | false, false -> `None

let no_done = List.exists (( = ) `No_done)

let configuration ?(stateless = false) capabilities =
  {
    Neg.stateless;
    Neg.no_done = (if stateless then true else no_done capabilities);
    Neg.multi_ack = multi_ack capabilities;
  }

module S = Sigs

module Make
    (Scheduler : S.SCHED)
    (IO : S.IO with type 'a t = 'a Scheduler.s)
    (Flow : S.FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : S.UID)
    (Ref : S.REF) =
struct
  open Scheduler

  module Log = (val let src = Logs.Src.create "fetch" in
                    Logs.src_log src : Logs.LOG)

  let ( >>= ) x f = IO.bind x f
  let return x = IO.return x

  let sched =
    S.
      {
        bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
        return = (fun x -> inj (return x));
      }

  let fail exn =
    let fail = IO.fail exn in
    inj fail

  let io =
    S.
      {
        recv = (fun flow raw -> inj (Flow.recv flow raw));
        send = (fun flow raw -> inj (Flow.send flow raw));
        pp_error = Flow.pp_error;
      }

  let is_a_tag ref = List.exists (String.equal "tags") (Ref.segs ref)

  let references want have =
    match want with
    | `None -> [], []
    | `All ->
        List.fold_left
          (fun acc -> function
            | uid, ref, false when not (is_a_tag ref) -> (uid, ref) :: acc
            | _ -> acc)
          [] have
        |> List.split
    | `Some refs ->
        let fold acc (uid, ref, peeled) =
          if List.exists Ref.(equal ref) refs && not peeled then
            (uid, ref) :: acc
          else acc
        in
        List.fold_left fold [] have |> List.split

  let fetch_v1 ?(uses_git_transport = false) ?(push_stdout = ignore)
      ?(push_stderr = ignore) ~capabilities ?deepen ?want:(refs = `None) ~host
      path flow store access fetch_cfg pack =
    let client_caps =
      (* XXX(dinosaure): HTTP ([stateless]) enforces no-done capabilities. Otherwise, you never
         will receive the PACK file. *)
      if fetch_cfg.Neg.no_done && not (no_done capabilities) then
        `No_done :: capabilities
      else capabilities
    in
    let prelude ctx =
      let open Smart in
      let* () =
        if uses_git_transport then
          send ctx proto_request
            (Proto_request.upload_pack ~host ~version:1 path)
        else return ()
      in
      let* v = recv ctx advertised_refs in
      let v = Smart.Advertised_refs.map ~fuid:Uid.of_hex ~fref:Ref.v v in
      let uids, refs = references refs (Smart.Advertised_refs.refs v) in
      Smart.Context.replace_server_caps ctx
        (Smart.Advertised_refs.capabilities v);
      return (uids, refs)
    in
    let ctx = Smart.Context.make ~client_caps in
    let negotiator = Neg.make ~compare:Uid.compare in
    Neg.tips sched access store negotiator |> prj >>= fun () ->
    Smart_flow.run sched fail io flow (prelude ctx) |> prj
    >>= fun (uids, refs) ->
    let hex =
      { Neg.to_hex = Uid.to_hex; of_hex = Uid.of_hex; compare = Uid.compare }
    in
    Neg.find_common sched io flow fetch_cfg hex access store negotiator ctx
      ?deepen uids
    |> prj
    >>= function
    | `Close -> return []
    | `Continue res ->
        let recv_pack ctx =
          let open Smart in
          let side_band =
            Smart.Context.is_cap_shared ctx `Side_band
            || Smart.Context.is_cap_shared ctx `Side_band_64k
          in
          recv ctx (recv_pack ~push_stdout ~push_stderr side_band)
        in
        if res < 0 then Log.warn (fun m -> m "No common commits");
        let rec go () =
          Smart_flow.run sched fail io flow (recv_pack ctx) |> prj >>= function
          | `End_of_transmission -> return ()
          | `Payload (str, off, len) -> pack (str, off, len) >>= go
          | `Stdout -> go ()
          | `Stderr -> go ()
        in
        Log.debug (fun m -> m "Start to download PACK file.");
        go () >>= fun () -> return (List.combine refs uids)
end
