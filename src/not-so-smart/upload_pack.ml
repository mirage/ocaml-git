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
  open Uid
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

  module Server_neg = struct
    (** Server-side common base negotiation. *)

    type 'uid t = {
      haves : 'uid list;
      last_common : 'uid option;
      has_common_base : bool;
    }

    let empty = { haves = []; last_common = None; has_common_base = false }

    let compute_has_common_base _store (_access : _ S.access) ~wants:_ _t =
      (* TODO: Compute whether all [wants] each have an ancestor in [t.haves]. *)
      false

    let mk_continue uid = Smart.Negotiation.mk_continue uid

    (** Returns the commits that should be [ACK]ed and update the state. *)
    let ack store (access : _ S.access) ~wants t new_haves =
      let rec loop t acc = function
        | [] -> return (t, List.rev acc)
        | hd :: tl -> (
            access.get hd store |> prj >>= function
            | Some _ ->
                let has_common_base =
                  t.has_common_base
                  || compute_has_common_base store access ~wants t
                in
                loop
                  { t with has_common_base; haves = hd :: t.haves }
                  (mk_continue hd :: acc) tl
            | None ->
                loop t
                  (if t.has_common_base then mk_continue hd :: acc else acc)
                  tl)
      in
      loop t [] new_haves

    (** Return the final [ACK] or [None] if the negotiation failed. *)
    let last_common t =
      match t.last_common with
      | Some uid -> Some (Smart.Negotiation.mk_ack uid)
      | None -> None
  end

  let upload_pack flow (access, _light_load, _heavy_load) store pack =
    let my_caps = [ `Multi_ack; `Side_band_64k; `Ofs_delta; `Thin_pack ] in

    access.S.locals store |> prj >>= fun refs ->
    let rec go refs acc head_acc =
      match refs with
      | [] -> (
          match head_acc with
          | None -> return acc
          | Some head -> return (head :: acc))
      | ref_ :: q -> (
          access.deref store ref_ |> prj >>= function
          | None -> go refs acc head_acc
          | Some uid -> (
              let ref_ = Ref.to_string ref_ in
              let uid = to_hex uid in
              let entry = uid, ref_, false in
              match ref_ with
              | "HEAD" -> go q acc (Some entry)
              | _ -> go q (entry :: acc) head_acc))
    in
    go refs [] None >>= fun refs ->
    let fiber ctx =
      let open Smart in
      let adv_ref = Advertised_refs.v1 ~capabilities:my_caps refs in
      let* () = send ctx send_advertised_refs adv_ref in
      recv ctx recv_want
    in
    let ctx = Smart.Context.make ~my_caps in
    Smart_flow.run sched fail io flow (fiber ctx) |> prj >>= fun wants ->
    (* TODO: Check that all the [wants] are in the store and each are the tip of a ref. *)
    Smart.Context.replace_their_caps ctx wants.Smart.Want.capabilities;

    let rec negotiate neg =
      Smart_flow.run sched fail io flow Smart.(recv ctx recv_have) |> prj
      >>= fun have ->
      let h, cmd =
        (Smart.Have.map ~f:of_hex have :> Uid.t list * [ `Done | `Flush ])
      in
      Server_neg.ack store access ~wants neg h >>= fun (neg, acks) ->
      let acks = List.map (Smart.Negotiation.map ~f:to_hex) acks in
      Smart_flow.run sched fail io flow Smart.(send ctx send_acks acks) |> prj
      >>= fun () ->
      match cmd with
      | `Done -> (
          match Server_neg.last_common neg with
          | Some ack ->
              let ack = Smart.Negotiation.map ~f:to_hex ack in
              Smart_flow.run sched fail io flow
                Smart.(send ctx send_acks [ ack ])
              |> prj
              >>= fun () -> return neg
          | None ->
              Smart_flow.run sched fail io flow Smart.(send ctx send_acks [])
              |> prj
              >>= fun () -> return neg)
      | `Flush -> negotiate neg
    in
    negotiate Server_neg.empty >>= fun neg ->
    let sources =
      let a, b = wants.wants in
      List.map of_hex (a :: b)
    in
    Pck.get_uncommon_objects sched ~compare:Uid.compare access store
      ~exclude:neg.haves ~sources
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
