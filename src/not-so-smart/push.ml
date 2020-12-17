open Rresult

type configuration = { stateless : bool }

let configuration ?(stateless = true) () = { stateless }

module S = Sigs

module Make
    (Scheduler : S.SCHED)
    (IO : S.IO with type 'a t = 'a Scheduler.s)
    (Flow : S.FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : S.UID)
    (Ref : S.REF) =
struct
  let src = Logs.Src.create "push"

  module Log = (val Logs.src_log src : Logs.LOG)
  open Scheduler

  let ( >>= ) x f = IO.bind x f
  let return x = IO.return x
  let ( >>| ) x f = x >>= fun x -> return (f x)

  let sched =
    S.
      {
        bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
        return = (fun x -> inj (return x));
      }

  let fail exn = inj (IO.fail exn)

  let io =
    S.
      {
        recv = (fun flow raw -> inj (Flow.recv flow raw));
        send = (fun flow raw -> inj (Flow.send flow raw));
        pp_error = Flow.pp_error;
      }

  let push ?(prelude = true) ~capabilities:caps cmds ~host path flow store
      access push_cfg pack =
    let fiber ctx =
      let open Smart in
      let* () =
        if prelude then
          send ctx proto_request
            (Proto_request.receive_pack ~host ~version:1 path)
        else return ()
      in
      let* v = recv ctx advertised_refs in
      Context.update ctx (Smart.Advertised_refs.capabilities v);
      return (Smart.Advertised_refs.map ~fuid:Uid.of_hex ~fref:Ref.v v)
    in
    let ctx = Smart.Context.make caps in
    Neg.run sched fail io flow (fiber ctx) |> prj >>= fun advertised_refs ->
    Pck.commands sched
      ~capabilities:(Smart.Advertised_refs.capabilities advertised_refs)
      ~equal:Ref.equal ~deref:access.Sigs.deref store cmds
      (Smart.Advertised_refs.refs advertised_refs)
    |> prj
    >>= function
    | None ->
        Neg.run sched fail io flow Smart.(send ctx flush ()) |> prj
        >>= fun () -> return ()
    | Some cmds -> (
        Neg.run sched fail io flow
          Smart.(
            send ctx commands
              (Commands.map ~fuid:Uid.to_hex ~fref:Ref.to_string cmds))
        |> prj
        >>= fun () ->
        let exclude, sources =
          Pck.get_limits ~compare:Uid.compare
            (Smart.Advertised_refs.refs advertised_refs)
            (Smart.Commands.commands cmds)
        in
        Pck.get_uncommon_objects sched ~compare:Uid.compare access store
          ~exclude ~sources
        |> prj
        >>= fun uids ->
        Log.debug (fun m ->
            m "Prepare a pack of %d object(s)." (List.length uids));
        let stream = pack uids in
        let side_band =
          Smart.Context.is_cap_shared `Side_band ctx
          || Smart.Context.is_cap_shared `Side_band_64k ctx
        in
        let pack = Smart.send_pack ~stateless:push_cfg.stateless side_band in
        let rec go () =
          stream () >>= function
          | None ->
              let report_status =
                Smart.Context.is_cap_shared `Report_status ctx
              in
              Log.debug (fun m ->
                  m "report-status capability: %b." report_status);
              if report_status then
                Neg.run sched fail io flow Smart.(recv ctx status)
                |> prj
                >>| Smart.Status.map ~f:Ref.v
              else
                let cmds = List.map R.ok (Smart.Commands.commands cmds) in
                return (Smart.Status.v cmds)
          | Some payload ->
              Neg.run sched fail io flow Smart.(send ctx pack payload) |> prj
              >>= fun () -> go ()
        in
        go () >>= fun status ->
        match Smart.Status.to_result status with
        | Ok () ->
            Log.debug (fun m -> m "Push is done!");
            Log.info (fun m ->
                m "%a" Smart.Status.pp
                  (Smart.Status.map ~f:Ref.to_string status));
            return ()
        | Error err ->
            Log.err (fun m -> m "Push got an error: %s" err);
            return ())
end
