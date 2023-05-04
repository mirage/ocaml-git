open Rresult

type configuration = { stateless : bool }

let configuration ?(stateless = false) () = { stateless }

module S = Sigs

module Make
    (Scheduler : S.SCHED)
    (IO : S.IO with type 'a t = 'a Scheduler.s)
    (Flow : S.FLOW with type 'a fiber = 'a Scheduler.s)
    (Uid : S.UID)
    (Ref : S.REF) =
struct
  let src = Logs.Src.create "upload-pack"

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

  let upload_pack flow store access pack =
    let fiber ctx =
      let open Smart in
      let adv_ref = Advertised_refs.v1 ~capabilities:[] [] (* TODO *) in
      let* () = send ctx send_advertised_refs adv_ref in
      recv ctx recv_want
    in
    let ctx = Smart.Context.make ~client_caps in
    Smart_flow.run sched fail io flow (fiber ctx) |> prj >>= fun wants ->
    let rec go =
      let fiber ctx =
        let open Smart in
      let* h = recv ctx recv_have in
      let haves = Smart.a in
      return (w, h) Smart_flow.run sched fail io flow haves |> prj
      >>= fun haves -> _
    in
    go
    (*
        Not implemented: send shallow information
        Go:
           - recv have: string list * [`Flush | `Done]
           - send acks for each commit acknowledged *)
    >>=
    fun haves ->
    Pck.get_uncommon_objects sched ~compare:Uid.compare access store
      ~exclude:haves ~sources:wants
    |> prj
    >>= fun uids ->
    Log.debug (fun m -> m "Prepare a pack of %d object(s)." (List.length uids));
    let stream = pack uids in
    let side_band =
      Smart.Context.is_cap_shared ctx `Side_band
      || Smart.Context.is_cap_shared ctx `Side_band_64k
    in
    let pack = Smart.send_pack side_band in
    let rec go () =
      stream () >>= function
      | None -> return ()
      | Some payload ->
          Smart_flow.run sched fail io flow Smart.(send ctx pack payload) |> prj
          >>= fun () -> go ()
    in
    go ()
end
