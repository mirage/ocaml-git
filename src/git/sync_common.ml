module Make
    (G: Minimal.S)
= struct
  module Store = G
  module Revision = Revision.Make(Store)

  module Log =
  struct
    let src = Logs.Src.create "git.common.sync" ~doc:"logs git's common sync event"
    include (val Logs.src_log src: Logs.LOG)
  end

  open Lwt.Infix

  let packer ?(window = `Object 10) ?(depth = 50) git ~ofs_delta:_ remote commands =
    let commands' =
      (List.map (fun (hash, refname, _) -> `Delete (hash, refname)) remote)
      @ commands
    in

    (* XXX(dinosaure): we don't want to delete remote references but
       we want to exclude any commit already stored remotely. Se, we «
       delete » remote references from the result set. *)

    Lwt_list.fold_left_s (fun acc -> function
        | `Create _ -> Lwt.return acc
        | `Update (hash, _, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
        | `Delete (hash, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
      ) Store.Hash.Set.empty commands'
    >>= fun negative ->
    Lwt_list.fold_left_s (fun acc -> function
        | `Create (hash, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
        | `Update (_, hash, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
        | `Delete _ -> Lwt.return acc
      ) Store.Hash.Set.empty commands
    >|= (fun positive -> Revision.Range.E.diff positive negative)
    >>= fun elements ->
    Lwt_list.fold_left_s (fun acc commit ->
        Store.fold git
          (fun acc ?name:_ ~length:_ _ value -> Lwt.return (value :: acc))
          ~path:(Fpath.v "/") acc commit
      ) [] (Store.Hash.Set.elements elements)
    >>= fun entries -> Store.Pack.make git ~window ~depth entries

  let want_handler git choose remote_refs =
    (* XXX(dinosaure): in this /engine/, for each remote references,
       we took or not only if this reference is not /peeled/. Then,
       [choose] returns [true] or [false] if he wants to download the
       reference or not. Finally, we check if we don't have already
       the remote hash. and if it's the case, we don't download it. *)
    Lwt_list.filter_map_s (function
        | (remote_hash, remote_ref, false) ->
          (choose remote_ref >>= function
            | false ->
              Log.debug (fun l -> l ~header:"want_handler" "We missed the reference %a."
                            Store.Reference.pp remote_ref);
              Lwt.return None
            | true ->
              Lwt.return (Some (remote_ref, remote_hash)))
          >>= (function
              | None -> Lwt.return None
              | Some (remote_ref, remote_hash) ->
                Store.mem git remote_hash >>= function
                | true -> Lwt.return None
                | false -> Lwt.return (Some (remote_ref, remote_hash)))
        | _ -> Lwt.return None)
      remote_refs

  exception Jump of Store.Ref.error

  let update_and_create git ?locks ~references results =
    let results = List.fold_left
        (fun results (remote_ref, hash) -> Store.Reference.Map.add remote_ref hash results)
        Store.Reference.Map.empty results in
    let updated, missed = Store.Reference.Map.partition
        (fun remote_ref _ -> Store.Reference.Map.mem remote_ref results)
        references in
    let updated, downloaded = Store.Reference.Map.fold
        (fun remote_ref new_hash (updated', downloaded) ->
           try
             let local_refs = Store.Reference.Map.find remote_ref updated in
             List.fold_left (fun updated' local_ref -> Store.Reference.Map.add local_ref new_hash updated')
               updated' local_refs, downloaded
           with Not_found -> updated', Store.Reference.Map.add remote_ref new_hash downloaded)
        results Store.Reference.Map.(empty, empty) in
    Lwt.try_bind
      (fun () ->
         Lwt_list.iter_s
           (fun (local_ref, new_hash) ->
              Store.Ref.write git ?locks local_ref (Store.Reference.Hash new_hash)
              >>= function
              | Ok _ -> Lwt.return ()
              | Error err -> Lwt.fail (Jump err))
           (Store.Reference.Map.bindings updated))
      (fun () -> Lwt.return (Ok (updated, missed, downloaded)))
      (function
        | Jump err -> Lwt.return (Error (`Ref err))
        | exn -> Lwt.fail exn)

  let push_handler git references remote_refs =
    Store.Ref.list git >>= fun local_refs ->
    let local_refs = List.fold_left
        (fun local_refs (local_ref, local_hash) ->
           Store.Reference.Map.add local_ref local_hash local_refs)
        Store.Reference.Map.empty local_refs in

    Lwt_list.filter_map_p (function
        | (remote_hash, remote_ref, false) -> Lwt.return (Some (remote_ref, remote_hash))
        | _ -> Lwt.return None)
      remote_refs
    >>= fun remote_refs ->
    let actions =
      Store.Reference.Map.fold
        (fun local_ref local_hash actions ->
           try let remote_refs' = Store.Reference.Map.find local_ref references in
             List.fold_left (fun actions remote_ref ->
                 try let remote_hash = List.assoc remote_ref remote_refs in
                   `Update (remote_hash, local_hash, remote_ref) :: actions
                 with Not_found -> `Create (local_hash, remote_ref) :: actions)
               actions remote_refs'
            with Not_found -> actions)
        local_refs []
    in

    Lwt_list.filter_map_s
      (fun action -> match action with
        | `Update (remote_hash, local_hash, _) ->
          Store.mem git remote_hash >>= fun has_remote_hash ->
          Store.mem git local_hash >>= fun has_local_hash ->

          if has_remote_hash && has_local_hash
          then Lwt.return (Some action)
          else Lwt.return None
        | `Create (local_hash, _) ->
          Store.mem git local_hash >>= function
          | true -> Lwt.return (Some action)
          | false -> Lwt.return None)
      actions
end
