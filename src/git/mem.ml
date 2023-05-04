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

module Sched = Carton.Make (struct
  type 'a t = 'a
end)

type 'hash t = {
  values : ('hash, 'hash Value.t Lazy.t) Hashtbl.t;
  inflated : ('hash, [ `Commit | `Blob | `Tag | `Tree ] * Cstruct.t) Hashtbl.t;
  refs : (Reference.t, [ `H of 'hash | `R of Reference.t ]) Hashtbl.t;
  root : Fpath.t;
  dotgit : Fpath.t;
  shallows : 'hash Shallow.t;
  mutable head : 'hash Reference.contents option;
}

let batch_write :
    type uid pack index.
    uid t ->
    uid_ln:int ->
    uid_rw:(string -> uid) ->
    map:(pack -> pos:int64 -> int -> Bigstringaf.t) ->
    iter:(index -> f:(uid -> int64 -> unit) -> unit) ->
    pack ->
    index ->
    unit =
 fun store ~uid_ln ~uid_rw ~map ~iter pack index ->
  let tbl = Hashtbl.create 0x100 in
  let f uid offset = Hashtbl.add tbl uid offset in
  iter index ~f;

  let z = De.bigstring_create De.io_buffer_size in
  let w = De.make_window ~bits:15 in
  let allocate _ = w in
  let pack =
    Carton.Dec.make pack ~z ~allocate ~uid_ln ~uid_rw (Hashtbl.find tbl)
  in
  let f uid offset =
    let weight =
      Carton.Dec.weight_of_offset ~map pack ~weight:Carton.Dec.null offset
    in
    let raw = Carton.Dec.make_raw ~weight in
    Log.debug (fun m -> m "Unpack %08Lx from the given PACK file." offset);
    let res = Carton.Dec.of_offset ~map pack raw ~cursor:offset in
    let inflated =
      Cstruct.of_bigarray (Carton.Dec.raw res) ~off:0 ~len:(Carton.Dec.len res)
    in
    let kind =
      match Carton.Dec.kind res with
      | `A -> `Commit
      | `B -> `Tree
      | `C -> `Blob
      | `D -> `Tag
    in
    if not (Hashtbl.mem store.values uid) then
      Hashtbl.add store.inflated uid (kind, inflated)
  in
  iter index ~f

let failuref fmt = Fmt.kstr (fun err -> Failure err) fmt

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

module Make (Digestif : Digestif.S) = struct
  type hash = Digestif.t
  type nonrec t = hash t
  type git_store = t (* XXX(dinosaure): fix type alias. *)

  open Value
  open Reference
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

  type error =
    [ `Not_found of Hash.t
    | `Reference_not_found of Reference.t
    | `Cycle
    | `Msg of string ]

  let pp_error ppf = function
    | `Not_found hash -> Fmt.pf ppf "%a not found" Hash.pp hash
    | `Reference_not_found r -> Fmt.pf ppf "%a not found" Reference.pp r
    | `Cycle -> Fmt.pf ppf "Got a reference cycle"
    | `Msg err -> Fmt.string ppf err

  let root { root; _ } = root
  let dotgit { dotgit; _ } = dotgit

  let v ?dotgit root =
    let dotgit =
      match dotgit with Some v -> v | None -> Fpath.(root / ".git")
    in
    Lwt.return_ok
      {
        values = Hashtbl.create 1024;
        inflated = Hashtbl.create 1024;
        refs = Hashtbl.create 8;
        head = None;
        shallows = Shallow.make [];
        root;
        dotgit;
      }

  let reset t =
    Log.info (fun l -> l "Reset memory store.");
    Hashtbl.reset t.values;
    Hashtbl.reset t.inflated;
    Hashtbl.reset t.refs;
    t.head <- None;
    Log.debug (fun l -> l "Elements into refs: %d." (Hashtbl.length t.refs));
    Lwt.return_ok ()

  let write t value =
    Log.debug (fun m ->
        m "Write a new value into the store: %a." Value.pp value);
    let hash = Value.digest value in
    Log.debug (fun m -> m "Store %a." Hash.pp hash);
    if Hashtbl.mem t.values hash then Lwt.return (Ok (hash, 0))
    else (
      Hashtbl.add t.values hash (lazy value);
      Lwt.return_ok (hash, Int64.to_int (Value.length value)))

  let digest kind raw =
    let len = Cstruct.length raw in
    let ctx = Hash.init () in
    let hdr =
      Fmt.str "%s %d\000%!"
        (match kind with
        | `Commit -> "commit"
        | `Blob -> "blob"
        | `Tree -> "tree"
        | `Tag -> "tag")
        len
    in
    let ctx = Hash.feed_string ctx hdr in
    let ctx = Hash.feed_bigstring ctx (Cstruct.to_bigarray raw) in
    Hash.get ctx

  let write_inflated t ~kind inflated =
    Log.debug (fun m -> m "Write inflated Git object.");
    let hash = digest kind inflated in
    if Hashtbl.mem t.values hash then Lwt.return hash
    else
      let value =
        lazy
          (match Value.of_raw ~kind inflated with
          | Error (`Msg err) ->
              let str = Fmt.str "Value.of_raw(%a): %s" Hash.pp hash err in
              raise (Failure str)
          | Ok value -> value)
      in
      Hashtbl.add t.inflated hash (kind, inflated);
      Hashtbl.add t.values hash value;
      Lwt.return hash

  let read_inflated t h =
    try
      let value = Lazy.force (Hashtbl.find t.values h) in
      let kind =
        match value with
        | Commit _ -> `Commit
        | Blob _ -> `Blob
        | Tree _ -> `Tree
        | Tag _ -> `Tag
      in
      let raw = Value.to_raw_without_header value in
      Lwt.return_some (kind, Cstruct.of_string raw)
    with Not_found -> (
      try
        let kind, raw = Hashtbl.find t.inflated h in
        Lwt.return_some (kind, raw)
      with Not_found -> Lwt.return_none)

  let read t h =
    try Ok (Lazy.force (Hashtbl.find t.values h))
    with Not_found -> (
      try
        let kind, raw = Hashtbl.find t.inflated h in
        match Value.of_raw ~kind raw with
        | Ok v ->
            Hashtbl.add t.values h (lazy v);
            Ok v
        | Error (`Msg err) ->
            let str = Fmt.str "Value.of_raw(%a): %s" Hash.pp h err in
            raise (Failure str)
      with Not_found -> Error (`Not_found h))

  let keys t = Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t =
    Lwt.return (List.sort_uniq Hash.compare (keys t.values @ keys t.inflated))

  let mem t h = Lwt.return (Hashtbl.mem t.values h || Hashtbl.mem t.inflated h)

  let size t h =
    let v =
      match read t h with
      | Ok (Blob v) -> Ok (Value.Blob.length v)
      | Ok (Commit _ | Tag _ | Tree _) | Error _ ->
          (* TODO(dinosaure): shallow? *)
          Error (`Not_found h)
    in
    Lwt.return v

  let read_exn t h =
    match read t h with
    | Error _ -> Lwt.fail (failuref "%a not found" Hash.pp h)
    | Ok v -> Lwt.return v

  let read_opt t h =
    match read t h with
    | Error (`Not_found _) -> Lwt.return (Ok None)
    | Ok v -> Lwt.return (Ok (Some v))

  let contents t =
    let open Lwt.Infix in
    list t >>= fun hashes ->
    let res =
      List.fold_left
        (fun acc h ->
          match read t h with Ok v -> (h, v) :: acc | Error _ -> acc)
        [] hashes
    in
    Lwt.return res

  let read t h =
    match read t h with
    | Ok _ as v -> Lwt.return v
    | Error _ as err -> Lwt.return err

  let is_shallowed t hash = Shallow.exists t.shallows ~equal:Hash.equal hash
  let shallowed t = Shallow.get t.shallows
  let shallow t hash = Shallow.append t.shallows hash
  let unshallow t hash = Shallow.remove t.shallows ~equal:Hash.equal hash

  module Traverse = Traverse_bfs.Make (struct
    module Hash = Hash
    module Value = Value

    type nonrec t = git_store

    let root { root; _ } = root
    let read_exn = read_exn
    let is_shallowed = is_shallowed
  end)

  let fold = Traverse.fold
  let iter = Traverse.iter

  (* XXX(dinosaure): extraction of Git objects from a PACK file stored
     into a [Cstruct.t] is not scheduled by any blocking _syscall_. In this
     context, the best is to do the extraction without the [lwt] _monad_. *)
  let batch_write t _ ~pck ~idx =
    let open Lwt.Infix in
    let rec flat stream buf =
      stream () >>= function
      | Some str ->
          Buffer.add_string buf str;
          flat stream buf
      | None -> Lwt.return (Buffer.contents buf)
    in
    (* TODO(dinosaure): do first-pass instead to store all into a [string]. *)
    flat pck (Buffer.create 0x100) >>= fun pck_contents ->
    flat idx (Buffer.create 0x100) >>= fun idx_contents ->
    let index =
      Carton.Dec.Idx.make
        (Bigstringaf.of_string ~off:0
           ~len:(String.length idx_contents)
           idx_contents)
        ~uid_ln:Hash.length ~uid_rw:Hash.to_raw_string
        ~uid_wr:Hash.of_raw_string
    in
    let iter index ~f =
      let f ~uid ~offset ~crc:_ = f uid offset in
      Carton.Dec.Idx.iter ~f index
    in
    let map pck_contents ~pos len =
      let pos = Int64.to_int pos in
      let len = min (String.length pck_contents - pos) len in
      Bigstringaf.of_string ~off:pos ~len pck_contents
    in
    batch_write t ~uid_ln:Hash.length ~uid_rw:Hash.of_raw_string ~map ~iter
      pck_contents index;
    Lwt.return_ok ()

  module Ref = struct
    module Graph = Reference.Map

    let list t =
      Log.debug (fun l -> l "Ref.list.");
      let graph, rest =
        Hashtbl.fold
          (fun k -> function
            | `R ptr -> fun (a, r) -> a, (k, ptr) :: r
            | `H hash -> fun (a, r) -> Graph.add k hash a, r)
          t.refs (Graph.empty, [])
      in
      let graph =
        List.fold_left
          (fun a (k, ptr) ->
            try
              let v = Graph.find ptr a in
              Graph.add k v a
            with Not_found -> a)
          graph rest
      in
      let r = Graph.fold (fun k v a -> (k, v) :: a) graph [] in
      Lwt.return r

    let mem t r =
      Log.debug (fun l -> l "Ref.mem %a." Reference.pp r);
      try
        let _ = Hashtbl.find t.refs r in
        Lwt.return true
      with Not_found -> Lwt.return false

    exception Cycle

    let resolve t r =
      let rec go ~visited r =
        Log.debug (fun l -> l "Ref.resolve %a." Reference.pp r);
        try
          if List.exists (Reference.equal r) visited then raise Cycle;
          match Hashtbl.find t.refs r with
          | `H s ->
              Log.debug (fun l ->
                  l "Ref.resolve %a found: %a." Reference.pp r Hash.pp s);
              Lwt.return_ok s
          | `R r' ->
              let visited = r :: visited in
              go ~visited r'
        with
        | Not_found ->
            Log.err (fun l -> l "%a not found." Reference.pp r);
            Lwt.return_error (`Reference_not_found r)
        | Cycle ->
            Log.err (fun l -> l "Got a reference cycle");
            Lwt.return_error `Cycle
      in
      go ~visited:[] r

    let read t r =
      try
        match Hashtbl.find t.refs r with
        | `H hash -> Lwt.return_ok (Reference.uid hash)
        | `R refname -> Lwt.return_ok (Reference.ref refname)
      with Not_found -> Lwt.return_error (`Reference_not_found r)

    let remove t r =
      Log.debug (fun l -> l "Ref.remove %a." Reference.pp r);
      Hashtbl.remove t.refs r;
      Lwt.return_ok ()

    let write t r value =
      Log.debug (fun l -> l "Ref.write %a." Reference.pp r);
      let head_contents =
        match value with Uid hash -> `H hash | Ref refname -> `R refname
      in
      Hashtbl.replace t.refs r head_contents;
      Lwt.return_ok ()
  end

  let has_global_watches = false
  let has_global_checkout = false
end

module Store = Make (Digestif.SHA1)

module Sync (Git_store : Minimal.S) = struct
  let src = Logs.Src.create "git-mem.sync" ~doc:"logs git-mem's sync event"

  module Log = (val Logs.src_log src : Logs.LOG)
  module Idx = Carton.Dec.Idx.M (Lwt) (Git_store.Hash)

  module Index = struct
    type +'a fiber = 'a Lwt.t

    include (Idx : module type of Idx with type fd := Idx.fd)

    (* XXX(dinosaure): may be update [Carton.Dec.Idx.M], but it seems fine. *)

    type 'a rd = < rd : unit ; .. > as 'a
    type 'a wr = < wr : unit ; .. > as 'a

    type 'a mode =
      | Rd : < rd : unit > mode
      | Wr : < wr : unit > mode
      | RdWr : < rd : unit ; wr : unit > mode

    type 'm fd = Idx.fd

    let create :
        type a.
        ?trunc:bool -> mode:a mode -> t -> uid -> (a fd, error) result fiber =
     fun ?trunc:_ ~mode:_ t uid -> create t uid

    let move _ ~src:_ ~dst:_ = assert false
    let map _ _ ~pos:_ _ = assert false
  end

  include Sync.Make (Git_store.Hash) (Cstruct_append) (Index) (Git_store)

  let stream_of_cstruct ?(chunk = 0x1000) payload =
    let stream, emitter = Lwt_stream.create () in
    let fill () =
      let rec go pos =
        if pos = Cstruct.length payload then (
          emitter None;
          Lwt.return_unit)
        else
          let len = min chunk (Cstruct.length payload - pos) in
          let tmp = Bytes.create len in
          Cstruct.blit_to_bytes payload pos tmp 0 len;
          emitter (Some (Bytes.unsafe_to_string tmp));
          go (pos + len)
      in
      go 0
    in
    Lwt.async fill;
    fun () -> Lwt_stream.get stream

  let fetch ?(push_stdout = ignore) ?(push_stderr = ignore) ?threads ~ctx edn
      store ?version ?capabilities ?deepen want =
    let open Lwt.Infix in
    let t_idx = Carton.Dec.Idx.Device.device () in
    let t_pck = Cstruct_append.device () in
    let index = Carton.Dec.Idx.Device.create t_idx in
    let src = Cstruct_append.key t_pck in
    let dst = Cstruct_append.key t_pck in
    let create_idx_stream () =
      Carton.Dec.Idx.Device.project t_idx index
      |> Cstruct.of_bigarray
      |> stream_of_cstruct
    in
    let create_pack_stream () =
      let pack = Cstruct_append.project t_pck dst in
      stream_of_cstruct pack
    in
    fetch ~push_stdout ~push_stderr ?threads ~ctx edn store ?version
      ?capabilities ?deepen want ~src ~dst ~idx:index ~create_idx_stream
      ~create_pack_stream t_pck t_idx
    >>= fun res ->
    let _dst = Sys.opaque_identity dst in
    let _src = Sys.opaque_identity src in
    Lwt.return res
end
