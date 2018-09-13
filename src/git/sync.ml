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

let src = Logs.Src.create "git.sync" ~doc:"logs git's sync event"

module Log = (val Logs.src_log src : Logs.LOG)

module Default = struct
  let capabilities =
    [ `Multi_ack_detailed; `Thin_pack; `Side_band_64k; `Ofs_delta
    ; `Agent "git/2.0.0"; `Report_status; `No_done ]
end

module type ENDPOINT = sig
  type t

  val host : t -> string
  val path : t -> string
  val pp : t Fmt.t
end

module type S = sig
  module Store : Minimal.S
  module Endpoint : ENDPOINT

  type error

  val pp_error : error Fmt.t

  type command =
    [ `Create of Store.Hash.t * Store.Reference.t
    | `Delete of Store.Hash.t * Store.Reference.t
    | `Update of Store.Hash.t * Store.Hash.t * Store.Reference.t ]

  val pp_command : command Fmt.t

  val push :
       Store.t
    -> push:(   (Store.Hash.t * Store.Reference.t * bool) list
             -> (Store.Hash.t list * command list) Lwt.t)
    -> ?capabilities:Capability.t list
    -> Endpoint.t
    -> ( (Store.Reference.t, Store.Reference.t * string) result list
       , error )
       result
       Lwt.t

  val ls :
       Store.t
    -> ?capabilities:Capability.t list
    -> Endpoint.t
    -> ((Store.Hash.t * Store.Reference.t * bool) list, error) result Lwt.t

  type shallow_update =
    {shallow: Store.Hash.t list; unshallow: Store.Hash.t list}

  type acks =
    { shallow: Store.Hash.t list
    ; unshallow: Store.Hash.t list
    ; acks: (Store.Hash.t * [`Common | `Ready | `Continue | `ACK]) list }

  val fetch :
       Store.t
    -> ?shallow:Store.Hash.t list
    -> ?capabilities:Capability.t list
    -> notify:(shallow_update -> unit Lwt.t)
    -> negociate:(   acks
                  -> 'state
                  -> ([`Ready | `Done | `Again of Store.Hash.Set.t] * 'state)
                     Lwt.t)
                 * 'state
    -> have:Store.Hash.Set.t
    -> want:(   (Store.Hash.t * Store.Reference.t * bool) list
             -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[`Depth of int | `Timestamp of int64 | `Ref of Reference.t]
    -> Endpoint.t
    -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

  val fetch_some :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Endpoint.t
    -> ( Store.Hash.t Store.Reference.Map.t
         * Store.Reference.t list Store.Reference.Map.t
       , error )
       result
       Lwt.t

  val fetch_all :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Endpoint.t
    -> ( Store.Hash.t Store.Reference.Map.t
         * Store.Reference.t list Store.Reference.Map.t
         * Store.Hash.t Store.Reference.Map.t
       , error )
       result
       Lwt.t

  val fetch_one :
       Store.t
    -> ?capabilities:Capability.t list
    -> reference:Store.Reference.t * Store.Reference.t list
    -> Endpoint.t
    -> ( [`AlreadySync | `Sync of Store.Hash.t Store.Reference.Map.t]
       , error )
       result
       Lwt.t

  val clone :
       Store.t
    -> ?capabilities:Capability.t list
    -> reference:Store.Reference.t * Store.Reference.t
    -> Endpoint.t
    -> (unit, error) result Lwt.t

  val update_and_create :
       Store.t
    -> ?capabilities:Capability.t list
    -> references:Store.Reference.t list Store.Reference.Map.t
    -> Endpoint.t
    -> ( (Store.Reference.t, Store.Reference.t * string) result list
       , error )
       result
       Lwt.t
end

(* XXX(dinosaure): common module is a module (used by the tcp layer, http layer
   and ssh layer) about what we need to do locally when we push or fetch. This
   module needs only an implementation of the store and do some operations on
   it (update references, walk on branches, etc.). *)

module Common (G : Minimal.S) = struct
  module Store = G

  let src =
    Logs.Src.create "git.common.sync" ~doc:"logs git's common sync event"

  module Log = (val Logs.src_log src : Logs.LOG)

  type command =
    [ `Create of Store.Hash.t * Store.Reference.t
    | `Delete of Store.Hash.t * Store.Reference.t
    | `Update of Store.Hash.t * Store.Hash.t * Store.Reference.t ]

  let pp_command ppf = function
    | `Create (hash, r) ->
        Fmt.pf ppf "(`Create (%a, %a))" Store.Hash.pp hash Store.Reference.pp r
    | `Delete (hash, r) ->
        Fmt.pf ppf "(`Delete (%a, %a))" Store.Hash.pp hash Store.Reference.pp r
    | `Update (_of, _to, r) ->
        Fmt.pf ppf "(`Update (of:%a, to:%a, %a))" Store.Hash.pp _of
          Store.Hash.pp _to Store.Reference.pp r

  open Lwt.Infix

  module Node = struct
    type t = {value: Store.Value.t; mutable color: [`Black | `White]}

    let compare a b =
      match a.value, b.value with
      | Store.Value.Commit a, Store.Value.Commit b ->
          Store.Value.Commit.compare_by_date b a
      | a, b -> Store.Value.compare a b
  end

  module Pq = Psq.Make (Store.Hash) (Node)
  module Q = Encore.FQueue

  exception Store of Store.error

  let packer git exclude source =
    let store = Hashtbl.create 128 in
    let memoize get hash =
      try
        let ret = Hashtbl.find store hash in
        Lwt.return ret
      with Not_found -> (
        get hash
        >>= function
        | Ok value ->
            let node = {Node.value; color= `White} in
            Hashtbl.add store hash node ;
            Lwt.return node
        | Error err ->
            Log.err (fun l ->
                l "Got an error when we get the object: %a." Store.Hash.pp hash
            ) ;
            Lwt.fail (Store err) )
    in
    let preds = function
      | Store.Value.Commit commit ->
          Store.Value.Commit.tree commit :: Store.Value.Commit.parents commit
      | Store.Value.Tree tree ->
          List.map
            (fun {Store.Value.Tree.node; _} -> node)
            (Store.Value.Tree.to_list tree)
      | Store.Value.Tag tag -> [Store.Value.Tag.obj tag]
      | Store.Value.Blob _ -> []
    in
    let get = memoize (Store.read git) in
    let all_blacks pq =
      Pq.fold
        (fun _ -> function {Node.color= `Black; _} -> ( && ) true
          | _ -> ( && ) false )
        true pq
    in
    let propagate {Node.value; color} =
      let rec go q =
        match Q.shift q with
        | hash, q -> (
          try
            let node = Hashtbl.find store hash in
            node.Node.color <- color ;
            go (List.fold_left Q.push q (preds node.Node.value))
          with Not_found -> go q )
        | exception Q.Empty -> ()
      in
      go (Q.of_list (preds value))
    in
    let propagate_snapshot {Node.value; color} =
      let rec go q =
        match Q.shift q with
        | hash, q ->
            ( try
                let node = Hashtbl.find store hash in
                Lwt.return node
              with Not_found -> get hash )
            >>= fun node ->
            node.Node.color <- color ;
            go (List.fold_left Q.push q (preds node.Node.value))
        | exception Q.Empty -> Lwt.return ()
      in
      go (Q.of_list (preds value))
    in
    let rec garbage pq =
      if all_blacks pq then Lwt.return ()
      else
        match Pq.pop pq with
        | Some ((_, {Node.value; color= `Black}), pq) ->
            Lwt_list.fold_left_s
              (fun pq hash ->
                get hash
                >>= function
                | {Node.value= Store.Value.Tree _; _} as node ->
                    node.Node.color <- `Black ;
                    propagate_snapshot node >>= fun () -> Lwt.return pq
                | {Node.color= `White; _} as node ->
                    node.Node.color <- `Black ;
                    propagate node ;
                    Lwt.return (Pq.add hash node pq)
                | node -> Lwt.return (Pq.add hash node pq) )
              pq (preds value)
            >>= garbage
        | Some ((_, {Node.value; _}), pq) ->
            Lwt_list.fold_left_s
              (fun pq hash ->
                get hash >>= fun node -> Lwt.return (Pq.add hash node pq) )
              pq (preds value)
            >>= garbage
        | None -> Lwt.return ()
    in
    let collect () =
      Hashtbl.fold
        (fun hash -> function
          | {Node.color= `White; value} -> Store.Hash.Map.add hash value
          | _ -> fun acc -> acc )
        store Store.Hash.Map.empty
    in
    Lwt_list.map_s
      (fun hash ->
        get hash
        >>= function
        | {Node.value= Store.Value.Commit commit; _} as node ->
            get (Store.Value.Commit.tree commit)
            >>= fun node_root_tree ->
            propagate_snapshot node_root_tree
            >>= fun () -> Lwt.return (hash, node)
        | node -> Lwt.return (hash, node) )
      source
    >>= fun source ->
    Lwt_list.map_s
      (fun hash ->
        get hash
        >>= function
        | {Node.value= Store.Value.Commit commit; _} as node ->
            node.Node.color <- `Black ;
            get (Store.Value.Commit.tree commit)
            >>= fun node_root_tree ->
            node_root_tree.Node.color <- `Black ;
            propagate_snapshot node_root_tree
            >>= fun () -> Lwt.return (hash, node)
        | node -> Lwt.return (hash, node) )
      exclude
    >|= List.append source
    >|= Pq.of_list
    >>= fun pq -> garbage pq >|= collect

  let packer ?(window = `Object 10) ?(depth = 50) git ~ofs_delta:_ remote
      commands =
    let exclude =
      List.fold_left
        (fun exclude (hash, _, _) -> Store.Hash.Set.add hash exclude)
        Store.Hash.Set.empty remote
      |> fun exclude ->
      List.fold_left
        (fun exclude -> function
          | `Delete (hash, _) -> Store.Hash.Set.add hash exclude
          | `Update (hash, _, _) -> Store.Hash.Set.add hash exclude
          | `Create _ -> exclude )
        exclude commands
      |> Store.Hash.Set.elements
    in
    let source =
      List.fold_left
        (fun source -> function
          | `Update (_, hash, _) -> Store.Hash.Set.add hash source
          | `Create (hash, _) -> Store.Hash.Set.add hash source
          | `Delete _ -> source )
        Store.Hash.Set.empty commands
      |> Store.Hash.Set.elements
    in
    packer git exclude source
    >|= Store.Hash.Map.bindings
    >|= List.map snd
    >>= Store.Pack.make git ~window ~depth

  let want_handler git choose remote_refs =
    (* XXX(dinosaure): in this /engine/, for each remote references, we took or
       not only if this reference is not /peeled/. Then, [choose] returns
       [true] or [false] if he wants to download the reference or not. Finally,
       we check if we don't have already the remote hash. and if it's the case,
       we don't download it. *)
    Lwt_list.filter_map_s
      (function
        | remote_hash, remote_ref, false -> (
            choose remote_ref
            >>= (function
                  | false ->
                      Log.debug (fun l ->
                          l "We missed the reference %a." Store.Reference.pp
                            remote_ref ) ;
                      Lwt.return None
                  | true -> Lwt.return (Some (remote_ref, remote_hash)))
            >>= function
            | None -> Lwt.return None
            | Some (remote_ref, remote_hash) -> (
                Store.mem git remote_hash
                >>= function
                | true -> Lwt.return None
                | false -> Lwt.return (Some (remote_ref, remote_hash)) ) )
        | _ -> Lwt.return None)
      remote_refs

  exception Jump of Store.error

  let update_and_create git ~references results =
    let results =
      List.fold_left
        (fun results (remote_ref, hash) ->
          Store.Reference.Map.add remote_ref hash results )
        Store.Reference.Map.empty results
    in
    let updated, missed =
      Store.Reference.Map.partition
        (fun remote_ref _ -> Store.Reference.Map.mem remote_ref results)
        references
    in
    let updated, downloaded =
      Store.Reference.Map.fold
        (fun remote_ref new_hash (updated', downloaded) ->
          try
            let local_refs = Store.Reference.Map.find remote_ref updated in
            ( List.fold_left
                (fun updated' local_ref ->
                  Store.Reference.Map.add local_ref new_hash updated' )
                updated' local_refs
            , downloaded )
          with Not_found ->
            updated', Store.Reference.Map.add remote_ref new_hash downloaded )
        results
        Store.Reference.Map.(empty, empty)
    in
    Lwt.try_bind
      (fun () ->
        Lwt_list.iter_s
          (fun (local_ref, new_hash) ->
            Store.Ref.write git local_ref (Store.Reference.Hash new_hash)
            >>= function
            | Ok _ -> Lwt.return () | Error err -> Lwt.fail (Jump err) )
          (Store.Reference.Map.bindings updated) )
      (fun () -> Lwt.return (Ok (updated, missed, downloaded)))
      (function Jump err -> Lwt.return (Error err) | exn -> Lwt.fail exn)

  let push_handler git references remote_refs =
    Store.Ref.list git
    >>= fun local_refs ->
    let local_refs =
      List.fold_left
        (fun local_refs (local_ref, local_hash) ->
          Store.Reference.Map.add local_ref local_hash local_refs )
        Store.Reference.Map.empty local_refs
    in
    Lwt_list.filter_map_p
      (function
        | remote_hash, remote_ref, false ->
            Lwt.return (Some (remote_ref, remote_hash))
        | _ -> Lwt.return None)
      remote_refs
    >>= fun remote_refs ->
    let actions =
      Store.Reference.Map.fold
        (fun local_ref local_hash actions ->
          try
            let remote_refs' = Store.Reference.Map.find local_ref references in
            List.fold_left
              (fun actions remote_ref ->
                try
                  let remote_hash = List.assoc remote_ref remote_refs in
                  `Update (remote_hash, local_hash, remote_ref) :: actions
                with Not_found -> `Create (local_hash, remote_ref) :: actions
                )
              actions remote_refs'
          with Not_found -> actions )
        local_refs []
    in
    Lwt_list.filter_map_s
      (fun action ->
        match action with
        | `Update (remote_hash, local_hash, reference) ->
            Store.mem git remote_hash
            >>= fun has_remote_hash ->
            Store.mem git local_hash
            >>= fun has_local_hash ->
            Log.debug (fun l ->
                l "Check update command on %a for %a to %a (equal = %b)."
                  Store.Reference.pp reference Store.Hash.pp remote_hash
                  Store.Hash.pp local_hash
                  Store.Hash.(equal remote_hash local_hash) ) ;
            if
              has_remote_hash
              && has_local_hash
              && not (Store.Hash.equal remote_hash local_hash)
            then Lwt.return (Some (action :> command))
            else Lwt.return None
        | `Create (local_hash, _) -> (
            Store.mem git local_hash
            >>= function
            | true -> Lwt.return (Some (action :> command))
            | false -> Lwt.return None ) )
      actions
end
