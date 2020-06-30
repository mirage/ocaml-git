let ( <.> ) f g x = f (g x)

let src = Logs.Src.create "git.sync"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Digestif : Digestif.S)
    (Pack : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Index : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Conduit : Conduit.S
                 with type +'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (Store : Minimal.S with type hash = Digestif.t)
    (HTTP : Smart_git.HTTP) =
struct
  type hash = Digestif.t

  type store = Store.t

  module Hash = Hash.Make (Digestif)
  module Scheduler = Sigs.Make_sched (Lwt)

  module Ministore = Sigs.Make_store (struct
    type ('k, 'v) t = Store.t * ('k, 'v) Hashtbl.t

    (* constraint 'k = Digestif.t *)
  end)

  open Lwt.Infix

  let get_commit_for_negotiation (t, hashtbl) hash =
    Log.debug (fun m -> m "Load commit %a." Hash.pp hash) ;
    match Hashtbl.find hashtbl hash with
    | v -> Lwt.return_some v
    | exception Not_found -> (
        (* XXX(dinosaure): given hash can not exist into [t],
         * in this call we try to see if remote hashes are available
         * locally. *)
        Store.read t hash
        >>= function
        | Ok (Value.Commit commit) ->
            let { User.date = ts, _; _ } = Store.Value.Commit.committer commit in
            let v = (hash, ref 0, ts) in
            Hashtbl.add hashtbl hash v ;
            Lwt.return_some v
        | Ok _ | Error _ -> Lwt.return_none)

  let parents_of_commit t hash =
    Log.debug (fun m -> m "Get parents of %a." Hash.pp hash) ;
    Store.read_exn t hash >>= function
    | Value.Commit commit -> Lwt.return (Store.Value.Commit.parents commit)
    | _ -> Lwt.return []

  let parents ((t, _hashtbl) as store) hash =
    parents_of_commit t hash >>= fun parents ->
    let fold acc hash =
      get_commit_for_negotiation store hash >>= function
      | Some v -> Lwt.return (v :: acc)
      | None -> Lwt.return acc in
    Lwt_list.fold_left_s fold [] parents

  let deref (t, _) refname =
    Log.debug (fun m -> m "Dereference %a." Reference.pp refname) ;
    Store.Ref.resolve t refname >>= function
    | Ok hash -> Lwt.return_some hash
    | Error _ -> Lwt.return_none

  let locals (t, _) =
    Log.debug (fun m -> m "Load locals references.") ;
    Store.Ref.list t >>= Lwt_list.map_p (Lwt.return <.> fst)

  let access =
    {
      Sigs.get =
        (fun uid t ->
          Scheduler.inj (get_commit_for_negotiation (Ministore.prj t) uid));
      Sigs.parents =
        (fun uid t -> Scheduler.inj (parents (Ministore.prj t) uid));
      Sigs.deref =
        (fun t refname -> Scheduler.inj (deref (Ministore.prj t) refname));
      Sigs.locals = (fun t -> Scheduler.inj (locals (Ministore.prj t)));
    }

  let lightly_load t hash =
    Store.read_exn t hash >>= fun v ->
    let kind =
      match v with
      | Value.Commit _ -> `A
      | Value.Tree _ -> `B
      | Value.Blob _ -> `C
      | Value.Tag _ -> `D in
    let length = Int64.to_int (Store.Value.length v) in
    Lwt.return (kind, length)

  let heavily_load t hash =
    Store.read_inflated t hash >>= function
    | Some (kind, { Cstruct.buffer; off; len }) ->
        let kind =
          match kind with
          | `Commit -> `A
          | `Tree -> `B
          | `Blob -> `C
          | `Tag -> `D in
        let raw = Bigstringaf.sub buffer ~off ~len in
        Lwt.return (Carton.Dec.v ~kind raw)
    | None -> Lwt.fail Not_found

  (* TODO *)

  include Smart_git.Make (Scheduler) (Pack) (Index) (Conduit) (HTTP) (Hash)
            (Reference)

  let fetch ~resolvers endpoint t ?version ?capabilities want ~src ~dst ~idx
      t_pck t_idx =
    let ministore = Ministore.inj (t, Hashtbl.create 0x100) in
    fetch ~resolvers
      (access, lightly_load t, heavily_load t)
      ministore endpoint ?version ?capabilities want t_pck t_idx ~src ~dst ~idx
end
