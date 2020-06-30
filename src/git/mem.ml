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

let src = Logs.Src.create "git.mem" ~doc:"logs git's memory back-end"

module Log = (val Logs.src_log src : Logs.LOG)

let failuref fmt = Fmt.kstrf (fun err -> Failure err) fmt

(* XXX(dinosaure): a point about modules, functors and alias.
 * The choice was made to _defunctorize_ any types to avoid to
 * keep type equalities between resulted modules of application
 * of some functors.
 *
 * With this such design, we are able to use values at any points
 * of the code instead to get an error about incompatible types -
 * even if we know they are equals.
 *
 * However, [git] is designed to re-export modules with the same
 * name such as:
 * value.ml -> used into mem.ml, mem.ml must export [Value]
 *
 * To solve that, we do: [module Value = Value.Make (Hash) ]
 * However, we lost our module [value.ml]/[Value] with this. At this
 * stage, we can consider that it's fine but we must need to keep
 * an occurence to our [value.ml] to be able to _pattern-match_
 * on our [Value.t] where constructor are defined into [value.ml]
 * but they are not defined into [Value.Make (Hash)].
 *
 * For the second point, we decided to remove any new types produced
 * by functors and due to the fact that arities are not the same between
 * ['hash Value.t] and [Value.Make(Hash).t], at this stage, despite some
 * hacks (like on used about [Reference]), we must open our [value.ml]
 * at the beginning. *)

open Value
open Reference

module Make (Digestif : Digestif.S) = struct
  type hash = Digestif.t

  module Hash = Hash.Make (Digestif)
  module Value = Value.Make (Hash)

  module Reference = struct
    type hash = Digestif.t

    include (
      Reference :
        module type of Reference
          with type 'uid contents := 'uid Reference.contents)

    type contents = hash Reference.contents
  end

  type kind = [ `Commit | `Tree | `Blob | `Tag ]

  type error = [ `Not_found of Hash.t | `Reference_not_found of Reference.t ]

  let pp_error ppf = function
    | `Not_found hash -> Fmt.pf ppf "%a not found" Hash.pp hash
    | `Reference_not_found r -> Fmt.pf ppf "%a not found" Reference.pp r

  type t = {
    values : (Hash.t, Value.t Lazy.t) Hashtbl.t;
    inflated : (Hash.t, kind * Cstruct.t) Hashtbl.t;
    refs : (Reference.t, [ `H of Hash.t | `R of Reference.t ]) Hashtbl.t;
    root : Fpath.t;
    mutable head : Reference.contents option;
  }

  let root { root; _ } = root

  let v root =
    Lwt.return_ok
      {
        values = Hashtbl.create 1024;
        inflated = Hashtbl.create 1024;
        refs = Hashtbl.create 8;
        head = None;
        root;
      }

  let reset t =
    Log.info (fun l -> l "Reset memory store") ;
    Hashtbl.clear t.values ;
    Hashtbl.clear t.inflated ;
    Hashtbl.clear t.refs ;
    Lwt.return_ok ()

  let write t value =
    let hash = Value.digest value in
    Log.debug (fun m -> m "Store %a." Hash.pp hash) ;
    if Hashtbl.mem t.values hash
    then Lwt.return (Ok (hash, 0))
    else
      let kind =
        match value with
        | Commit _ -> `Commit
        | Blob _ -> `Blob
        | Tree _ -> `Tree
        | Tag _ -> `Tag in
      let raw = Value.to_raw value in
      Hashtbl.add t.values hash (lazy value) ;
      Hashtbl.add t.inflated hash (kind, Cstruct.of_string raw) ;
      Lwt.return_ok (hash, String.length raw)

  let digest kind raw =
    let len = Cstruct.len raw in
    let ctx = Hash.init () in
    let hdr =
      Fmt.strf "%s %d\000%!"
        (match kind with
        | `Commit -> "commit"
        | `Blob -> "blob"
        | `Tree -> "tree"
        | `Tag -> "tag")
        len in
    let ctx = Hash.feed_string ctx hdr in
    let ctx = Hash.feed_bigstring ctx (Cstruct.to_bigarray raw) in
    Hash.get ctx

  let write_inflated t ~kind inflated =
    let hash = digest kind inflated in
    if Hashtbl.mem t.values hash
    then Lwt.return hash
    else
      let value =
        lazy
          (match Value.of_raw ~kind inflated with
          | Error (`Msg err) ->
              let str = Fmt.strf "%s" err in
              raise (Failure str)
          | Ok value -> value) in
      Hashtbl.add t.inflated hash (kind, inflated) ;
      Hashtbl.add t.values hash value ;
      Lwt.return hash

  let read_inflated t h =
    try
      let value = Lazy.force (Hashtbl.find t.values h) in
      let kind =
        match value with
        | Commit _ -> `Commit
        | Blob _ -> `Blob
        | Tree _ -> `Tree
        | Tag _ -> `Tag in
      let raw = Value.to_raw_without_header value in
      Lwt.return_some (kind, Cstruct.of_string raw)
    with Not_found -> Lwt.return_none

  let read t h =
    try Ok (Lazy.force (Hashtbl.find t.values h))
    with Not_found -> Error (`Not_found h)

  let keys t = Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t = Lwt.return (keys t.values)

  let mem t h = Lwt.return (Hashtbl.mem t.values h)

  let size t h =
    let v =
      match read t h with
      | Ok (Blob v) -> Ok (Value.Blob.length v)
      | Ok (Commit _ | Tag _ | Tree _) | Error _ -> Error (`Not_found h) in
    Lwt.return v

  let read_exn t h =
    match read t h with
    | Error _ -> Lwt.fail (failuref "%a not found" Hash.pp h)
    | Ok v -> Lwt.return v

  let contents t =
    let hashes = keys t.values in
    let res =
      List.fold_left
        (fun acc h ->
          match read t h with Ok v -> (h, v) :: acc | Error _ -> acc)
        [] hashes in
    Lwt.return res

  let read t h = Lwt.return (read t h)

  module Traverse = Traverse_bfs.Make (struct
    module Hash = Hash
    module Value = Value

    type nonrec t = t

    let root { root; _ } = root

    let read_exn = read_exn
  end)

  let fold = Traverse.fold

  let iter = Traverse.iter

  type index = Bigstringaf.t

  type pack = Cstruct.t

  (* XXX(dinosaure): extraction of Git objects from a PACK file stored
     into a [Cstruct.t] is not scheduled by any blocking _syscall_. In this
     context, the best is to do the extraction without the [lwt] _monad_. *)
  module Sched = Carton.Make (struct
    type 'a t = 'a
  end)

  let sched =
    {
      Carton.bind = (fun x f -> f (Sched.prj x));
      Carton.return = (fun x -> Sched.inj x);
    }

  let batch_write t ~index ~pack =
    let map pack ~pos len =
      let pos = Int64.to_int pos in
      if pos > Cstruct.len pack
      then Sched.inj Bigstringaf.empty
      else
        let len = min (Cstruct.len pack - pos) len in
        let { Cstruct.buffer; off; _ } = pack in
        Sched.inj (Bigstringaf.sub buffer ~off:(off + pos) ~len) in
    let index =
      Carton.Dec.Idx.make index ~uid_ln:Hash.length ~uid_rw:Hash.to_raw_string
        ~uid_wr:Hash.of_raw_string in
    let findex uid =
      match Carton.Dec.Idx.find index uid with
      | Some (_, offset) -> offset
      | None -> raise Not_found in
    let z = De.bigstring_create De.io_buffer_size in
    let w = De.make_window ~bits:15 in
    let allocate _ = w in
    let pack =
      Carton.Dec.make pack ~z ~allocate ~uid_ln:Hash.length
        ~uid_rw:Hash.of_raw_string findex in
    let f ~uid ~offset ~crc:_ =
      if Hashtbl.mem t.values uid
      then
        ()
        (* XXX(dinosaure): an object from a PACK file can already
           exist in the store. *)
      else
        let weight =
          Sched.prj
            (Carton.Dec.weight_of_offset sched ~map pack ~weight:Carton.Dec.null
               offset) in
        let raw = Carton.Dec.make_raw ~weight in
        let res =
          Sched.prj (Carton.Dec.of_offset sched ~map pack raw ~cursor:offset)
        in
        let inflated =
          Cstruct.of_bigarray (Carton.Dec.raw res) ~off:0
            ~len:(Carton.Dec.len res) in
        let kind =
          match Carton.Dec.kind res with
          | `A -> `Commit
          | `B -> `Tree
          | `C -> `Blob
          | `D -> `Tag in
        let value =
          lazy
            (match Value.of_raw ~kind inflated with
            | Error (`Msg err) ->
                let str = Fmt.strf "%s" err in
                raise (Failure str)
            | Ok value -> value) in
        Hashtbl.add t.inflated uid (kind, inflated) ;
        Hashtbl.add t.values uid value in
    Carton.Dec.Idx.iter ~f index ;
    Lwt.return_ok ()

  module Ref = struct
    module Graph = Reference.Map

    let list t =
      Log.debug (fun l -> l "Ref.list") ;
      let graph, rest =
        Hashtbl.fold
          (fun k -> function `R ptr -> fun (a, r) -> (a, (k, ptr) :: r)
            | `H hash -> fun (a, r) -> (Graph.add k hash a, r))
          t.refs (Graph.empty, []) in
      let graph =
        List.fold_left
          (fun a (k, ptr) ->
            try
              let v = Graph.find ptr a in
              Graph.add k v a
            with Not_found -> a)
          graph rest in
      let r = Graph.fold (fun k v a -> (k, v) :: a) graph [] in
      Lwt.return r

    let mem t r =
      Log.debug (fun l -> l "Ref.mem %a" Reference.pp r) ;
      try
        let _ = Hashtbl.find t.refs r in
        Lwt.return true
      with Not_found -> Lwt.return false

    let rec resolve t r =
      Log.debug (fun l -> l "Ref.read %a" Reference.pp r) ;
      try
        match Hashtbl.find t.refs r with
        | `H s -> Lwt.return_ok s
        | `R r -> resolve t r
      with Not_found -> Lwt.return_error (`Reference_not_found r)

    let read t r =
      try
        match Hashtbl.find t.refs r with
        | `H hash -> Lwt.return_ok (Reference.uid hash)
        | `R refname -> Lwt.return_ok (Reference.ref refname)
      with Not_found -> Lwt.return_error (`Reference_not_found r)

    let remove t r =
      Log.debug (fun l -> l "Ref.remove %a" Reference.pp r) ;
      Hashtbl.remove t.refs r ;
      Lwt.return_ok ()

    let write t r value =
      Log.debug (fun l -> l "Ref.write %a" Reference.pp r) ;
      let head_contents =
        match value with Uid hash -> `H hash | Ref refname -> `R refname in
      Hashtbl.replace t.refs r head_contents ;
      Lwt.return_ok ()
  end

  let has_global_watches = false

  let has_global_checkout = false
end

module Store = Make (Digestif.SHA1)

module Sync
    (Digestif : Digestif.S)
    (Conduit : Conduit.S
                 with type +'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (Store : Minimal.S
               with type hash = Digestif.t
                and type pack = Cstruct.t
                and type index = Bigstringaf.t)
    (HTTP : Smart_git.HTTP) =
struct
  module Hash = Hash.Make (Digestif)

  module Index = struct
    type +'a fiber = 'a Lwt.t

    include Carton.Dec.Idx.M (Lwt) (Hash)

    let move _ ~src:_ ~dst:_ = assert false

    let map _ _ ~pos:_ _ = assert false
  end

  open Lwt.Infix
  include Sync.Make (Digestif) (Cstruct_append) (Index) (Conduit) (Store) (HTTP)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> Lwt.return_error err

  let fetch ~resolvers edn store ?version ?capabilities want =
    let t_idx = Carton.Dec.Idx.Device.device () in
    let t_pck = Cstruct_append.device () in
    let index = Carton.Dec.Idx.Device.create t_idx in
    let src = Cstruct_append.key t_pck in
    let dst = Cstruct_append.key t_pck in
    fetch ~resolvers edn store ?version ?capabilities want ~src ~dst ~idx:index
      t_pck t_idx
    >>? function
    | `Empty -> Lwt.return_ok None
    | `Pack (hash, refs) ->
        let index = Carton.Dec.Idx.Device.project t_idx index in
        let pack = Cstruct_append.project t_pck dst in
        Store.batch_write store ~index ~pack
        >|= Rresult.R.reword_error (fun err -> `Store err)
        >>? fun () ->
        let update (refname, hash) =
          Store.Ref.write store refname (Reference.Uid hash) >>= function
          | Ok v -> Lwt.return v
          | Error _err -> (* TODO *) Lwt.return () in
        Lwt_list.iter_p update refs >>= fun () ->
        Lwt.return_ok (Some (hash, refs))
end
