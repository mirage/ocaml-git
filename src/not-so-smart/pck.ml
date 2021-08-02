open Sigs

let src = Logs.Src.create "pck"

module Log = (val Logs.src_log src : Logs.LOG)

type color = Black | White
type none = Leaf
type 'uid commit = { root : 'uid; preds : 'uid list }

type ('uid, 'preds) kind =
  | Commit : ('uid, 'uid commit) kind
  | Tree : ('uid, 'uid list) kind
  | Blob : ('uid, none) kind
  | Tag : ('uid, 'uid) kind

let commit = Commit
let tree = Tree
let blob = Blob
let tag = Tag

type 'uid t =
  | Node : {
      mutable color : color;
      ts : int64 option;
      uid : 'uid;
      kind : ('uid, 'preds) kind;
      preds : 'preds;
    }
      -> 'uid t

let make :
    type uid preds. kind:(uid, preds) kind -> preds -> ?ts:int64 -> uid -> uid t
    =
 fun ~kind preds ?ts uid -> Node { color = White; ts; uid; kind; preds }

let compare ~cmp (Node a) (Node b) =
  match a.ts, b.ts with
  | Some a, Some b -> Int64.compare b a
  | _ -> cmp a.uid b.uid

let preds : type uid. uid t -> uid list =
 fun (Node { kind; preds; _ }) ->
  match kind with
  | Commit -> preds.root :: preds.preds
  | Tree -> preds
  | Blob -> []
  | Tag -> [ preds ]

let memoize { bind; return } ~f =
  let ( >>= ) = bind in

  let tbl = Hashtbl.create 0x100 in
  ( tbl,
    fun key ->
      match Hashtbl.find tbl key with
      | value -> return value
      | exception Not_found ->
          f key >>= fun value ->
          Hashtbl.replace tbl key value;
          return value )

let commands { bind; return } ~capabilities ~equal:equal_reference ~deref store
    cmds have =
  let ( >>= ) = bind in
  let fold acc = function
    | `Create reference -> (
        deref store reference >>= function
        | Some uid -> return (Smart.Commands.create uid reference :: acc)
        | None -> return acc)
    | `Delete reference -> (
        match
          List.find_opt
            (fun (_, reference', peeled) ->
              equal_reference reference reference' && peeled = false)
            have
        with
        | Some (uid, _, _) -> return (Smart.Commands.delete uid reference :: acc)
        | None -> return acc)
    | `Update (local, remote) -> (
        deref store local >>= function
        | None -> return acc
        | Some uid_new -> (
            match
              List.find_opt
                (fun (_, reference', peeled) ->
                  equal_reference remote reference' && peeled = false)
                have
            with
            | Some (uid_old, _, _) ->
                return (Smart.Commands.update uid_old uid_new remote :: acc)
            | None -> return (Smart.Commands.create uid_new remote :: acc)))
  in
  let rec go a = function
    | [] -> return a
    | head :: tail -> fold a head >>= fun a -> go a tail
  in
  go [] cmds >>= function
  | [] -> return None
  | head :: tail ->
      return (Some (Smart.Commands.v ~capabilities ~others:tail head))

let get_limits :
    type uid ref.
    compare:(uid -> uid -> int) ->
    (uid * ref * bool) list ->
    (uid, ref) Smart.Commands.command list ->
    uid list * uid list =
 fun ~compare:compare_uid have cmds ->
  let module Set = Set.Make (struct
    type t = uid

    let compare = compare_uid
  end) in
  let exclude =
    let fold acc (uid, _, _) = Set.add uid acc in
    List.fold_left fold Set.empty have
  in
  let exclude =
    let fold acc = function
      | Smart.Commands.Create _ -> acc
      | Smart.Commands.Delete (uid, _) -> Set.add uid acc
      | Smart.Commands.Update (uid, _, _) -> Set.add uid acc
    in
    List.fold_left fold exclude cmds
  in
  let sources =
    let fold acc = function
      | Smart.Commands.Update (_, uid, _) -> Set.add uid acc
      | Smart.Commands.Create (uid, _) -> Set.add uid acc
      | Smart.Commands.Delete _ -> acc
    in
    List.fold_left fold Set.empty cmds
  in
  Set.elements exclude, Set.elements sources

let get_uncommon_objects :
    type uid g s.
    s scheduler ->
    compare:(uid -> uid -> int) ->
    (uid, 'ref, uid t, g, s) access ->
    (uid, uid t, g) store ->
    exclude:uid list ->
    sources:uid list ->
    (uid list, s) io =
 fun ({ bind; return } as scheduler) ~compare:compare_uid { get; _ } store
     ~exclude ~sources ->
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in
  let fold_left_s ~f a l =
    let rec go a = function
      | [] -> return a
      | x :: r -> f a x >>= fun a -> go a r
    in
    go a l
  in
  let map_p ~f l =
    let rec go = function
      | [] -> return []
      | x :: r ->
          f x >>= fun x ->
          go r >>= fun r -> return (x :: r)
    in
    go l
  in

  let tbl, get = memoize scheduler ~f:(fun uid -> get uid store) in

  let module K = struct
    type t = uid

    let compare = compare_uid
  end in
  let module P = struct
    type nonrec t = uid t

    let compare = compare ~cmp:compare_uid
  end in
  let module Psq = Psq.Make (K) (P) in
  let all_blacks psq =
    let fold _ (Node { color; _ }) acc = color = Black && acc in
    Psq.fold fold true psq
  in

  let propagate (Node { color; _ } as node) =
    let q = Queue.create () in
    let rec go () =
      match Queue.pop q with
      | uid -> (
          match Hashtbl.find tbl uid with
          | Some (Node value as node) ->
              value.color <- color;
              List.iter (fun uid -> Queue.push uid q) (preds node);
              go ()
          | None | (exception Not_found) -> go ())
      | exception Queue.Empty -> ()
    in
    List.iter (fun uid -> Queue.push uid q) (preds node);
    go ()
  in

  let propagate_snapshot (Node { color; _ } as node) =
    let q = Queue.create () in
    let rec go () =
      match Queue.pop q with
      | uid ->
          let tip = function
            | Some (Node value as node) ->
                value.color <- color;
                List.iter (fun uid -> Queue.add uid q) (preds node)
            | None -> ()
          in
          get uid >>| tip >>= fun () -> go ()
      | exception Queue.Empty -> return ()
    in
    List.iter (fun uid -> Queue.push uid q) (preds node);
    go ()
  in

  let rec garbage psq =
    if all_blacks psq then return ()
    else
      match Psq.pop psq with
      | Some ((_, (Node { color = Black; _ } as node)), psq) ->
          let fold psq uid =
            get uid >>= function
            | Some (Node ({ kind = Tree; _ } as value) as node) ->
                value.color <- Black;
                propagate_snapshot node >>= fun () -> return psq
            | Some (Node ({ color = White; _ } as value) as node) ->
                value.color <- Black;
                propagate node;
                let psq = Psq.add uid node psq in
                return psq
            | Some node ->
                let psq = Psq.add uid node psq in
                return psq
            | None -> return psq
          in
          fold_left_s ~f:fold psq (preds node) >>= garbage
      | Some ((_, node), psq) ->
          let fold psq uid =
            get uid >>= function
            | Some node -> return (Psq.add uid node psq)
            | None -> return psq
          in
          fold_left_s ~f:fold psq (preds node) >>= garbage
      | None -> return ()
  in

  let map_sources uid =
    get uid >>= function
    | Some (Node { kind = Commit; preds; _ } as node) ->
        let { root; _ } = preds in
        (get root >>= function
         | Some tree -> propagate_snapshot tree
         | None -> return ())
        >>= fun () -> return (Some (uid, node))
    | Some node -> return (Some (uid, node))
    | None -> return None
  in
  let map_exclude uid =
    get uid >>= function
    | Some (Node ({ kind = Commit; preds; _ } as value) as node) ->
        value.color <- Black;
        let { root; _ } = preds in
        (get root >>= function
         | Some (Node value as tree) ->
             value.color <- Black;
             propagate_snapshot tree
         | None -> return ())
        >>= fun () -> return (Some (uid, node))
    | Some (Node value as node) ->
        value.color <- Black;
        return (Some (uid, node))
    | None -> return None
  in
  map_p ~f:map_sources sources >>= fun sources ->
  map_p ~f:map_exclude exclude >>= fun exclude ->
  let fold acc = function
    | Some (key, value) -> Psq.add key value acc
    | None -> acc
  in
  let psq = List.fold_left fold Psq.empty (List.append exclude sources) in
  garbage psq >>= fun () ->
  let fold uid value acc =
    match value with Some (Node { color = White; _ }) -> uid :: acc | _ -> acc
  in
  return (Hashtbl.fold fold tbl [])
