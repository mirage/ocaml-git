open Sigs
open Rresult

let src = Logs.Src.create "store"

module Log = (val Logs.src_log src : Logs.LOG)

type ('k, 'v) t = { tbl : ('k, 'v) Hashtbl.t; path : Fpath.t }

module Store = Hkt.Make_store (struct type nonrec ('k, 'v) t = ('k, 'v) t end)

type git = Store.t

let store_prj = Store.prj
let store_inj = Store.inj
let failwithf fmt = Fmt.kstr (fun err -> raise (Failure err)) fmt

let kind_of_object path uid =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_string ~trim:true
          (run_out ~err:err_null Cmd.(v "git" % "cat-file" % "-t" % uid)))
    ()
  |> R.join
  |> function
  | Ok ("commit", (_, `Exited 0)) -> R.ok (Some `Commit)
  | Ok ("tree", (_, `Exited 0)) -> R.ok (Some `Tree)
  | Ok ("tag", (_, `Exited 0)) -> R.ok (Some `Tag)
  | Ok ("blob", (_, `Exited 0)) -> R.ok (Some `Blob)
  | Ok _ -> R.ok None
  | Error err ->
      Log.err (fun m -> m "Got an error [kind_of_object]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let lightly_load { Sigs.return; _ } path uid =
  let uid = Uid.to_hex uid in
  let fiber =
    let open Bos in
    kind_of_object path uid >>= function
    | None -> failwithf "Object <%s> not found" uid
    | Some kind ->
        let kind =
          match kind with
          | `Commit -> `A
          | `Tree -> `B
          | `Blob -> `C
          | `Tag -> `D
        in
        OS.Dir.with_current path
          OS.Cmd.(
            fun () ->
              out_string ~trim:true
                (run_out ~err:err_null Cmd.(v "git" % "cat-file" % "-s" % uid)))
          ()
        |> R.join
        >>= fun (length, info) -> R.ok ((kind, int_of_string length), info)
  in
  match fiber with
  | Ok (v, (_, `Exited 0)) -> return v
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [lightly_load]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let heavily_load { Sigs.return; _ } path uid =
  let uid = Uid.to_hex uid in
  let fiber =
    let open Bos in
    kind_of_object path uid >>= function
    | None -> failwithf "Object <%s> not found" uid
    | Some kind ->
        let kind, str =
          match kind with
          | `Commit -> `A, "commit"
          | `Tree -> `B, "tree"
          | `Blob -> `C, "blob"
          | `Tag -> `D, "tag"
        in
        OS.Dir.with_current path
          OS.Cmd.(
            fun () ->
              out_string ~trim:false
                (run_out ~err:err_null Cmd.(v "git" % "cat-file" % str % uid)))
          ()
        |> R.join
        >>= fun (payload, info) -> R.ok ((kind, payload), info)
  in
  match fiber with
  | Ok ((kind, payload), (_, `Exited 0)) ->
      let payload =
        Bigstringaf.of_string payload ~off:0 ~len:(String.length payload)
      in
      return (Carton.Dec.v ~kind payload)
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [heavily_load]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let parents_of_commit path uid =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_lines ~trim:true
          (run_out ~err:err_null
             Cmd.(v "git" % "show" % "-s" % "--pretty=%P" % uid)))
    ()
  |> R.join
  |> function
  | Ok (uids, (_, `Exited 0)) -> R.ok uids
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [parents_of_commits]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let root_tree_of_commit path uid =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_string ~trim:true
          (run_out ~err:err_null
             Cmd.(v "git" % "show" % "-s" % "--pretty=%T" % uid)))
    ()
  |> R.join
  |> function
  | Ok (uid, (_, `Exited 0)) -> R.ok uid
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m ->
          m "Got an error [root_tree_of_commits]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let commit_of_tag path uid =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_string ~trim:true
          (run_out ~err:err_null
             Cmd.(v "git" % "rev-parse" % Fmt.str "%s^{commit}" uid)))
    ()
  |> R.join
  |> function
  | Ok (uid, (_, `Exited 0)) -> R.ok uid
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [commit_of_tag]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let preds_of_tree path uid =
  let uid_of_line line =
    match Astring.String.cuts ~sep:" " line with
    | _ :: _ :: uid_and_name -> (
        let uid_and_name = String.concat " " uid_and_name in
        match Astring.String.cut ~sep:"\t" uid_and_name with
        | Some (uid, _) -> R.ok uid
        | None -> R.error_msgf "Invalid line: %S" line)
    | _ -> R.error_msgf "Invalid line: %S" line
  in
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_lines ~trim:true
          (run_out ~err:err_null Cmd.(v "git" % "ls-tree" % uid)))
    ()
  |> R.join
  |> function
  | Ok (lines, (_, `Exited 0)) ->
      let uids = List.map uid_of_line lines in
      List.fold_left
        (fun a x ->
          a >>= fun a ->
          x >>= fun x -> R.ok (x :: a))
        (Ok []) uids
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [preds_of_tree]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let timestamp_of_commit path uid =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_string ~trim:true
          (run_out ~err:OS.Cmd.err_null
             Cmd.(v "git" % "show" % "-s" % "--pretty=%ct" % uid)))
    ()
  |> R.join
  |> function
  | Ok (ts, (_, `Exited 0)) -> R.ok (Int64.of_string ts)
  | Ok (_, (run_info, _)) ->
      Log.err
        Bos.(
          fun m ->
            m "Got an error while: %a" Cmd.pp (OS.Cmd.run_info_cmd run_info));
      failwithf "Object <%s> not found" uid
  | Error err ->
      Log.err (fun m -> m "Got an error [timestamp_of_commit]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let get_object_for_packer path (uid : Uid.t) =
  kind_of_object path (Uid.to_hex uid) >>= function
  | Some `Commit ->
      parents_of_commit path (Uid.to_hex uid) >>| List.map Uid.of_hex
      >>= fun preds ->
      root_tree_of_commit path (Uid.to_hex uid) >>| Uid.of_hex >>= fun root ->
      timestamp_of_commit path (Uid.to_hex uid) >>= fun ts ->
      R.ok (Some (Pck.make ~kind:Pck.commit { Pck.root; Pck.preds } ~ts uid))
  | Some `Tree ->
      preds_of_tree path (Uid.to_hex uid) >>| List.map Uid.of_hex
      >>= fun uids -> R.ok (Some (Pck.make ~kind:Pck.tree uids uid))
  | Some `Blob -> R.ok (Some (Pck.make ~kind:Pck.blob Pck.Leaf uid))
  | Some `Tag ->
      commit_of_tag path (Uid.to_hex uid) >>| Uid.of_hex >>= fun commit ->
      R.ok (Some (Pck.make ~kind:Pck.tag commit uid))
  | None -> R.ok None

let get_object_for_packer { return; _ } uid store =
  let { tbl; path } = Store.prj store in
  match Hashtbl.find tbl uid with
  | v -> return (Some v)
  | exception Not_found -> (
      match get_object_for_packer path uid with
      | Ok (Some v) ->
          Hashtbl.replace tbl uid v;
          return (Some v)
      | Ok None -> return None
      | Error err ->
          Log.warn (fun m ->
              m "Got an error [get_object_for_packer]: %a" R.pp_msg err);
          return None)

let get_commit_for_negotiation path (uid : Uid.t) =
  let open Bos in
  OS.Dir.with_current path
    OS.Cmd.(
      fun () ->
        out_string ~trim:true
          (run_out ~err:OS.Cmd.err_null
             Cmd.(v "git" % "show" % "-s" % "--pretty=%ct" % Uid.to_hex uid)))
    ()
  |> R.join
  >>= function
  | ts, (_, `Exited 0) ->
      let ts = Int64.of_string ts in
      let p = ref 0 in
      R.ok (Some (uid, p, ts))
  | _ -> R.ok None

let parents :
    type s.
    s scheduler ->
    Uid.t ->
    (Uid.t, Uid.t * int ref * int64, git) store ->
    ((Uid.t * int ref * int64) list, s) io =
 fun { Sigs.return; _ } uid store ->
  let { tbl; path } = Store.prj store in
  match parents_of_commit path (Uid.to_hex uid) with
  | Ok uids -> (
      let map uid =
        let uid = Uid.of_hex uid in
        match Hashtbl.find tbl uid with
        | obj -> obj
        | exception Not_found -> (
            match get_commit_for_negotiation path uid with
            | Ok (Some obj) ->
                Hashtbl.add tbl uid obj;
                obj
            | Ok None ->
                assert false
                (* XXX(dinosaure): impossible, [git] can not give to us unknown object. *)
            | Error err -> Stdlib.raise (Failure (Fmt.str "%a" R.pp_msg err)))
      in
      try
        let objs = List.map map uids in
        return objs
      with Failure err -> failwithf "%s" err)
  | Error err ->
      Log.err (fun m -> m "Got an error [parents]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let deref :
    type s. s scheduler -> (_, _, git) store -> Ref.t -> (Uid.t option, s) io =
 fun { Sigs.return; _ } store reference ->
  let { path; _ } = Store.prj store in
  let fiber =
    let open Bos in
    OS.Dir.with_current path
      OS.Cmd.(
        fun () ->
          out_string ~trim:true
            (run_out ~err:OS.Cmd.err_null
               Cmd.(v "git" % "show-ref" % "--hash" % (reference :> string))))
      ()
    |> R.join
  in
  match fiber with
  | Ok (uid, (_, `Exited 0)) -> return (Some (Uid.of_hex uid))
  | Ok _ -> return None
  | Error err ->
      Log.err (fun m -> m "Got an error [deref]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let locals :
    type s.
    s scheduler ->
    (Uid.t, Uid.t * int ref * int64, git) store ->
    (Ref.t list, s) io =
 fun { Sigs.return; _ } store ->
  let { path; _ } = Store.prj store in
  let fiber =
    let open Bos in
    OS.Dir.with_current path
      OS.Cmd.(
        fun () ->
          out_lines ~trim:true
            (run_out ~err:OS.Cmd.err_null Cmd.(v "git" % "show-ref")))
      ()
    |> R.join
  in
  match fiber with
  | Ok (refs, (_, `Exited 0)) ->
      let map line =
        match Astring.String.cut ~sep:" " line with
        | Some (_, reference) -> Ref.v reference
        | None -> Ref.v line
      in
      return (List.map map refs)
  | Ok _ -> return []
  | Error err ->
      Log.err (fun m -> m "Got an error [local]: %a" R.pp_msg err);
      failwithf "%a" R.pp_msg err

let get_commit_for_negotiation { Sigs.return; _ } uid store =
  let { tbl; path } = Store.prj store in
  match Hashtbl.find tbl uid with
  | v -> return (Some v)
  | exception Not_found -> (
      match get_commit_for_negotiation path uid with
      | Ok (Some obj) ->
          Hashtbl.replace tbl uid obj;
          return (Some obj)
      | Ok None -> return None
      | Error err ->
          Log.warn (fun m ->
              m "Got an error [get_commit_for_negotiation]: %a" R.pp_msg err);
          return None)

let safely_rd ~f path =
  Bos.OS.File.with_ic path @@ fun ic a ->
  Unix.lockf (Unix.descr_of_in_channel ic) Unix.F_RLOCK 0;
  f ic a

let safely_wr ~f path =
  Bos.OS.File.with_oc path @@ fun oc a ->
  Unix.lockf (Unix.descr_of_out_channel oc) Unix.F_LOCK 0;
  f oc a

let parse_shallow ic () =
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  let lst = Astring.String.fields ~empty:true (Bytes.unsafe_to_string rs) in
  List.map Uid.of_hex lst

let save_shallow oc lst =
  let ppf = Format.formatter_of_out_channel oc in
  let str = Fmt.str "%a" Fmt.(list ~sep:(any "\n") Uid.pp) lst in
  Log.debug (fun m -> m "Want to save: %S." str);
  Fmt.pf ppf "%a%!" Fmt.(list ~sep:(any "\n") Uid.pp) lst;
  Rresult.R.ok ()

let shallowed { Sigs.return; _ } store =
  let { path; _ } = Store.prj store in
  let shallow = Fpath.(path / ".git" / "shallow") in
  let fiber = safely_rd ~f:parse_shallow shallow () in
  match fiber with
  | Ok lst -> return lst
  | Error (`Msg err) ->
      Log.warn (fun m ->
          m "Got an error when we tried to get shallowed commits: %s" err);
      return []

let identity x = x
let always x _ = x

let shallow { Sigs.return; _ } store uid =
  let { path; _ } = Store.prj store in
  let shallow = Fpath.(path / ".git" / "shallow") in
  let fiber =
    let res = safely_rd ~f:parse_shallow shallow () in
    let shallowed = Result.fold ~ok:identity ~error:(always []) res in
    safely_wr ~f:save_shallow shallow (uid :: shallowed) |> Rresult.R.join
  in
  match fiber with
  | Ok () -> return ()
  | Error (`Msg err) ->
      Log.warn (fun m ->
          m "Got an error when we tried to shallow %a: %s" Uid.pp uid err);
      return ()

let access :
    type s.
    s scheduler -> (Uid.t, Ref.t, Uid.t * int ref * int64, git, s) access =
 fun scheduler ->
  {
    get = get_commit_for_negotiation scheduler;
    parents = parents scheduler;
    deref = deref scheduler;
    locals = locals scheduler;
    (* TODO(dinosaure): no [git] commands to shallow/unshallow commits. *)
    shallowed = shallowed scheduler;
    shallow = shallow scheduler;
    unshallow = (fun _store _uid -> scheduler.return ());
  }
