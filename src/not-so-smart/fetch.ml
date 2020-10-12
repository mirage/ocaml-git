open Sigs

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

module Make
    (Scheduler : SCHED)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Flow : FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : UID)
    (Ref : REF) =
struct
  open Scheduler

  let src = Logs.Src.create "fetch"

  module Log = (val Logs.src_log src : Logs.LOG)

  let ( >>= ) x f = IO.bind x f
  let return x = IO.return x

  let sched =
    {
      Sigs.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Sigs.return = (fun x -> inj (return x));
    }

  let fail exn =
    let fail = IO.fail exn in
    inj fail

  let io =
    {
      Sigs.recv = (fun flow raw -> inj (Flow.recv flow raw));
      Sigs.send = (fun flow raw -> inj (Flow.send flow raw));
      Sigs.pp_error = Flow.pp_error;
    }

  let references want have =
    match want with
    | `None -> [], []
    | `All ->
        List.fold_left
          (fun acc -> function uid, ref, false -> (uid, ref) :: acc | _ -> acc)
          [] have
        |> List.split
    | `Some refs ->
        let fold acc (uid, ref, peeled) =
          if List.exists Ref.(equal ref) refs && not peeled then
            (uid, ref) :: acc
          else acc
        in
        List.fold_left fold [] have |> List.split

  let fetch_v1 ?(prelude = true) ?(push_stdout = ignore) ?(push_stderr = ignore)
      ~capabilities ?deepen ?want:(refs = `None) ~host path flow store access
      fetch_cfg pack =
    let capabilities =
      (* XXX(dinosaure): HTTP ([stateless]) enforces no-done capabilities. Otherwise, you never
         will receive the PACK file. *)
      if fetch_cfg.Neg.no_done && not (no_done capabilities) then
        `No_done :: capabilities
      else capabilities
    in
    let prelude ctx =
      let open Smart in
      let* () =
        if prelude then
          send ctx proto_request
            (Proto_request.upload_pack ~host ~version:1 path)
        else return ()
      in
      let* v = recv ctx advertised_refs in
      let v = Smart.Advertised_refs.map ~fuid:Uid.of_hex ~fref:Ref.v v in
      let uids, refs = references refs (Smart.Advertised_refs.refs v) in
      update ctx (Smart.Advertised_refs.capabilities v);
      return (uids, refs)
    in
    let ctx = Smart.make capabilities in
    let negotiator = Neg.make ~compare:Uid.compare in
    Neg.tips sched access store negotiator |> prj >>= fun () ->
    Neg.run sched fail io flow (prelude ctx) |> prj >>= fun (uids, refs) ->
    let hex =
      {
        Neg.to_hex = Uid.to_hex;
        Neg.of_hex = Uid.of_hex;
        Neg.compare = Uid.compare;
      }
    in
    Neg.find_common sched io flow fetch_cfg hex access store negotiator ctx
      ?deepen uids
    |> prj
    >>= function
    | `Close -> return []
    | `Continue res ->
        let pack ctx =
          let open Smart in
          let side_band =
            Smart.shared `Side_band ctx || Smart.shared `Side_band_64k ctx
          in
          recv ctx
            (recv_pack ~side_band ~push_stdout ~push_stderr ~push_pack:pack)
        in
        if res < 0 then Log.warn (fun m -> m "No common commits");
        let rec go () =
          Log.debug (fun m -> m "Read PACK file.");
          Neg.run sched fail io flow (pack ctx) |> prj >>= fun continue ->
          if continue then go () else return ()
        in
        Log.debug (fun m -> m "Start to download PACK file.");
        go () >>= fun () -> return (List.combine refs uids)
end
