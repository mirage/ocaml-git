module V1 = struct
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
end

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

  let io_raise exn =
    let fail = IO.fail exn in
    inj fail

  let io =
    S.
      {
        recv = (fun flow raw -> inj (Flow.recv flow raw));
        send = (fun flow raw -> inj (Flow.send flow raw));
        pp_error = Flow.pp_error;
      }

  let references want have =
    match want with
    | `None -> [], []
    | `All ->
        List.fold_left
          (fun acc -> function
            | uid, ref, false -> (uid, ref) :: acc
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

  module V1 = struct
    let fetch ?(uses_git_transport = false) ?(push_stdout = ignore)
        ?(push_stderr = ignore) ~capabilities ?deepen ?want:(refs = `None) ~host
        path flow store access fetch_cfg push_pack =
      let client_caps =
        (* XXX(dinosaure): HTTP ([stateless]) enforces no-done capabilities. Otherwise, you never
           will receive the PACK file. *)
        if fetch_cfg.Neg.no_done && not (V1.no_done capabilities) then
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
        let server_caps = Smart.Advertised_refs.capabilities v in
        Smart.Context.replace_server_caps ctx server_caps;
        return (uids, refs)
      in

      let ctx = Smart.Context.make ~client_caps in

      State_flow.run sched io_raise Smart.pp_error io flow (prelude ctx) |> prj
      >>= fun (uids, refs) ->
      let hex =
        { Neg.to_hex = Uid.to_hex; of_hex = Uid.of_hex; compare = Uid.compare }
      in

      let negotiator = Neg.make ~compare:Uid.compare in
      Neg.tips sched access store negotiator |> prj >>= fun () ->
      Neg.find_common sched io flow fetch_cfg hex access store negotiator ctx
        ?deepen uids
      |> prj
      >>= function
      | `Close -> return []
      | `Continue res ->
          let recv_pack_state ctx =
            let open Smart in
            let side_band =
              Smart.Context.is_cap_shared ctx `Side_band
              || Smart.Context.is_cap_shared ctx `Side_band_64k
            in
            recv ctx (recv_pack ~side_band ~push_stdout ~push_stderr push_pack)
          in
          if res < 0 then Log.warn (fun m -> m "No common commits");
          let rec read_pack () =
            Log.debug (fun m -> m "Reading PACK file...");
            State_flow.run sched io_raise Smart.pp_error io flow
              (recv_pack_state ctx)
            |> prj
            >>= fun should_continue ->
            if should_continue then read_pack () else return ()
          in
          Log.debug (fun m -> m "Start to download PACK file.");
          read_pack () >>= fun () -> return (List.combine refs uids)
  end

  module V2 = struct
    let connect ?(uses_git_transport = false) ~host ~path ctx =
      let open Wire_proto_v2.Syntax in
      let return = Wire_proto_v2.return in
      let* () =
        if uses_git_transport then
          Wire_proto_v2.(
            send ctx Witness.Proto_request
              (Proto_vals_v2.Proto_request.upload_pack ~host ~version:2 path))
        else return ()
      in
      Wire_proto_v2.(recv ctx Witness.Capability_advertisement)

    let get_server_capabilities ?(uses_git_transport = false) ~host ~path ctx
        flow =
      let get_caps ctx =
        let open Wire_proto_v2.Syntax in
        let* caps = connect ~uses_git_transport ~host ~path ctx in
        let* () = Wire_proto_v2.send ctx Flush () in
        Wire_proto_v2.return caps
      in
      State_flow.run sched io_raise Wire_proto_v2.pp_error io flow
        (get_caps ctx)
      |> prj

    let ls_refs_request ?(uses_git_transport = false) ~host ~path ctx flow req =
      let ls_refs_resp =
        let open Wire_proto_v2.Syntax in
        let* caps = connect ~uses_git_transport ~host ~path ctx in
        (* TODO: how are server caps handled on the client side? *)
        let* () = Wire_proto_v2.send ctx Ls_refs_req (`Client_caps caps, req) in
        Wire_proto_v2.recv ctx Ls_refs_res
      in
      State_flow.run sched io_raise Wire_proto_v2.pp_error io flow ls_refs_resp
      |> prj
  end
end
