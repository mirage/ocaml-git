(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let ( <.> ) f g x = f (g x)
let src = Logs.Src.create "git.sync"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type hash
  type store
  type error = private [> `Msg of string | `Exn of exn | `Not_found ]

  val pp_error : error Fmt.t

  val fetch :
    ?push_stdout:(string -> unit) ->
    ?push_stderr:(string -> unit) ->
    resolvers:Conduit.resolvers ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 ] ->
    ?capabilities:Smart.Capability.t list ->
    ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
    [ `All | `Some of (Reference.t * Reference.t) list | `None ] ->
    ((hash * (Reference.t * hash) list) option, error) result Lwt.t

  val push :
    resolvers:Conduit.resolvers ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `Create of Reference.t
    | `Delete of Reference.t
    | `Update of Reference.t * Reference.t ]
    list ->
    (unit, error) result Lwt.t
end

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

  type error =
    [ `Msg of string | `Exn of exn | `Not_found | `Store of Store.error ]

  let pp_error ppf = function
    | `Msg err -> Fmt.string ppf err
    | `Exn exn -> Fmt.pf ppf "Exception: %s" (Printexc.to_string exn)
    | `Not_found -> Fmt.string ppf "Not found"
    | `Store err -> Fmt.pf ppf "Store error: %a" Store.pp_error err

  module Hash = Hash.Make (Digestif)
  module Scheduler = Hkt.Make_sched (Lwt)

  module Ministore = Hkt.Make_store (struct
    type ('k, 'v) t = Store.t * ('k, 'v) Hashtbl.t

    (* constraint 'k = Digestif.t *)
  end)

  open Lwt.Infix

  let get_commit_for_negotiation (t, hashtbl) hash =
    Log.debug (fun m -> m "Load commit %a." Hash.pp hash);
    match Hashtbl.find hashtbl hash with
    | v -> Lwt.return_some v
    | exception Not_found -> (
        (* XXX(dinosaure): given hash can not exist into [t],
         * in this call we try to see if remote hashes are available
         * locally. *)
        Store.read t hash
        >>= function
        | Ok (Value.Commit commit) ->
            let { User.date = ts, _; _ } =
              Store.Value.Commit.committer commit
            in
            let v = hash, ref 0, ts in
            Hashtbl.add hashtbl hash v;
            Lwt.return_some v
        | Ok _ | Error _ -> Lwt.return_none)

  let parents_of_commit t hash =
    Log.debug (fun m -> m "Get parents of %a." Hash.pp hash);
    Store.read_exn t hash >>= function
    | Value.Commit commit -> (
        Store.is_shallowed t hash >>= function
        | false -> Lwt.return (Store.Value.Commit.parents commit)
        | true -> Lwt.return [])
    | _ -> Lwt.return []

  let parents ((t, _hashtbl) as store) hash =
    parents_of_commit t hash >>= fun parents ->
    let fold acc hash =
      get_commit_for_negotiation store hash >>= function
      | Some v -> Lwt.return (v :: acc)
      | None -> Lwt.return acc
    in
    Lwt_list.fold_left_s fold [] parents

  let deref (t, _) refname =
    Log.debug (fun m -> m "Dereference %a." Reference.pp refname);
    Store.Ref.resolve t refname >>= function
    | Ok hash -> Lwt.return_some hash
    | Error _ -> Lwt.return_none

  let locals (t, _) =
    Log.debug (fun m -> m "Load locals references.");
    Store.Ref.list t >>= Lwt_list.map_p (Lwt.return <.> fst)

  let shallowed (t, _) =
    Log.debug (fun m -> m "Shallowed commits of the store.");
    Store.shallowed t

  let shallow (t, _) hash =
    Log.debug (fun m -> m "Shallow %a." Hash.pp hash);
    Store.shallow t hash

  let unshallow (t, _) hash =
    Log.debug (fun m -> m "Unshallow %a." Hash.pp hash);
    Store.unshallow t hash

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
      Sigs.shallowed = (fun t -> Scheduler.inj (shallowed (Ministore.prj t)));
      Sigs.shallow =
        (fun t uid -> Scheduler.inj (shallow (Ministore.prj t) uid));
      Sigs.unshallow =
        (fun t uid -> Scheduler.inj (unshallow (Ministore.prj t) uid));
    }

  let lightly_load t hash =
    Store.read_exn t hash >>= fun v ->
    let kind =
      match v with
      | Value.Commit _ -> `A
      | Value.Tree _ -> `B
      | Value.Blob _ -> `C
      | Value.Tag _ -> `D
    in
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
          | `Tag -> `D
        in
        let raw = Bigstringaf.sub buffer ~off ~len in
        Lwt.return (Carton.Dec.v ~kind raw)
    | None -> Lwt.fail Not_found

  include
    Smart_git.Make (Scheduler) (Pack) (Index) (Conduit) (HTTP) (Hash)
      (Reference)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> Lwt.return_error err

  let fetch ?(push_stdout = ignore) ?(push_stderr = ignore) ~resolvers endpoint
      t ?version ?capabilities ?deepen want ~src ~dst ~idx ~create_idx_stream
      ~create_pack_stream t_pck t_idx =
    let want, src_dst_mapping =
      match want with
      | (`All | `None) as x -> x, fun src -> [ src ]
      | `Some src_dst_refs ->
          let src_refs = List.map fst src_dst_refs in
          let src_dst_map =
            List.fold_left
              (fun src_dst_map (src_ref, dst_ref) ->
                try
                  let dst_refs = Reference.Map.find src_ref src_dst_map in
                  if List.exists (Reference.equal dst_ref) dst_refs then
                    src_dst_map
                  else
                    Reference.Map.add src_ref (dst_ref :: dst_refs) src_dst_map
                with Not_found ->
                  Reference.Map.add src_ref [ dst_ref ] src_dst_map)
              Reference.Map.empty src_dst_refs
          in
          let src_dst_mapping src_ref =
            Reference.Map.find_opt src_ref src_dst_map
            |> Option.value ~default:[ src_ref ]
          in
          `Some src_refs, src_dst_mapping
    in
    let ministore = Ministore.inj (t, Hashtbl.create 0x100) in
    fetch ~push_stdout ~push_stderr ~resolvers
      (access, lightly_load t, heavily_load t)
      ministore endpoint ?version ?capabilities ?deepen want t_pck t_idx ~src
      ~dst ~idx
    >>? function
    | `Empty -> Lwt.return_ok None
    | `Pack (uid, refs) ->
        Store.batch_write t uid ~pck:(create_pack_stream ())
          ~idx:(create_idx_stream ())
        >|= Rresult.R.reword_error (fun err -> `Store err)
        >>? fun () ->
        let update (src_ref, hash) =
          let write_dst_ref dst_ref =
            Store.Ref.write t dst_ref (Reference.Uid hash) >>= function
            | Ok v -> Lwt.return v
            | Error err ->
                Log.warn (fun m ->
                    m "Impossible to update %a to %a: %a." Reference.pp src_ref
                      Store.Hash.pp hash Store.pp_error err);
                Lwt.return_unit
          in
          let dst_refs = src_dst_mapping src_ref in
          Lwt_list.iter_p write_dst_ref dst_refs
        in
        Lwt_list.iter_p update refs >>= fun () ->
        Lwt.return_ok (Some (uid, refs))

  let get_object_for_packer t hash =
    Store.read t hash >>= function
    | Ok (Value.Blob _) ->
        Lwt.return_some (Pck.make ~kind:Pck.blob Pck.Leaf hash)
    | Ok (Value.Tree tree) ->
        let hashes = Tree.hashes tree in
        Lwt.return_some (Pck.make ~kind:Pck.tree hashes hash)
    | Ok (Value.Commit commit) ->
        (Store.is_shallowed t hash >|= function
         | true -> []
         | false -> Store.Value.Commit.parents commit)
        >>= fun preds ->
        let root = Store.Value.Commit.tree commit in
        let { User.date = ts, _; _ } = Store.Value.Commit.committer commit in
        Lwt.return_some
          (Pck.make ~kind:Pck.commit { Pck.root; Pck.preds } ~ts hash)
    | Ok (Value.Tag tag) ->
        let pred = Store.Value.Tag.obj tag in
        Lwt.return_some (Pck.make ~kind:Pck.tag pred hash)
    | Error _ -> Lwt.return_none

  let get_object_for_packer (t, hashtbl) hash =
    match Hashtbl.find hashtbl hash with
    | v -> Lwt.return_some v
    | exception Not_found -> (
        get_object_for_packer t hash >>= function
        | Some o as v ->
            Hashtbl.replace hashtbl hash o;
            Lwt.return v
        | None -> Lwt.return_none)

  let access =
    {
      Sigs.get =
        (fun uid t ->
          Scheduler.inj (get_object_for_packer (Ministore.prj t) uid));
      Sigs.parents = (fun _ _ -> assert false);
      Sigs.deref =
        (fun t refname -> Scheduler.inj (deref (Ministore.prj t) refname));
      Sigs.locals = (fun _ -> assert false);
      Sigs.shallowed = (fun _ -> assert false);
      Sigs.shallow = (fun _ -> assert false);
      Sigs.unshallow = (fun _ -> assert false);
    }

  let push ~resolvers endpoint t ?version ?capabilities cmds =
    let ministore = Ministore.inj (t, Hashtbl.create 0x100) in
    push ~resolvers
      (access, lightly_load t, heavily_load t)
      ministore endpoint ?version ?capabilities cmds
end
