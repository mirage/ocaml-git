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

open Lwt.Infix

let src = Logs.Src.create "git.store" ~doc:"logs git's store event"

module Log = (val Logs.src_log src : Logs.LOG)

module type Rs = sig
  type +'a fiber
  type t
  type error

  val pp_error : error Fmt.t
  val atomic_wr : t -> Reference.t -> string -> (unit, error) result fiber

  (* open / single_write / close *)
  val atomic_rd : t -> Reference.t -> (string, error) result fiber

  (* open / single_read / close *)
  val atomic_rm : t -> Reference.t -> (unit, error) result fiber

  (* unlink *)
  val list : t -> Reference.t list fiber
  val reset : t -> (unit, error) result fiber

  (* readdir / closedir *)
end

module type Mj = sig
  include Carton_git.STORE

  include
    Smart_git.APPEND
      with type t := t
       and type uid := uid
       and type 'a fd := 'a fd
       and type error := error
       and type +'a fiber := 'a fiber

  val reset : t -> (unit, error) result fiber
end

type ('uid, 'major_uid, 'major) major = {
  pck_major_uid_of_uid : 'major -> 'uid -> 'major_uid;
  idx_major_uid_of_uid : 'major -> 'uid -> 'major_uid;
  uid_of_major_uid : 'major_uid -> 'uid;
}

open Value
open Reference
open Loose

module Make
    (Digestif : Digestif.S)
    (Mn : Loose_git.STORE
            with type +'a fiber = 'a Lwt.t
             and type uid = Digestif.t)
    (Mj : Mj with type +'a fiber = 'a Lwt.t)
    (Rs : Rs with type +'a fiber = 'a) =
struct
  module Hash = Hash.Make (Digestif)
  module Value = Value.Make (Hash)
  module Caml_scheduler = Carton.Make (struct type 'a t = 'a end)

  module Reference = struct
    type hash = Hash.t

    include (
      Reference :
        module type of Reference
          with type 'uid contents := 'uid Reference.contents)

    type contents = hash Reference.contents
  end

  type hash = Hash.t

  module Loose = Loose_git.Make (Carton_lwt.Scheduler) (Lwt) (Mn) (Hash)
  module Pack = Carton_git.Make (Carton_lwt.Scheduler) (Lwt) (Mj) (Hash)

  let has_global_watches = true
  let has_global_checkout = true

  type t = {
    minor : Mn.t;
    major : Mj.t;
    major_uid : (hash, Mj.uid, Mj.t) major;
    packs : (Mj.uid, < rd : unit > Mj.fd, Hash.t) Carton_git.t;
    pools :
      ((< rd : unit > Mj.fd * int64)
      * (< rd : unit > Mj.fd * int64) Carton_git.buffers Lwt_pool.t)
      list;
    buffs : buffers Lwt_pool.t;
    rs : Rs.t;
    root : Fpath.t;
    dotgit : Fpath.t;
    shallows : hash Shallow.t;
    mutable refs : (Rs.t, Hash.t, Rs.error, Caml_scheduler.t) Reference.store;
  }

  let root { root; _ } = root
  let dotgit { dotgit; _ } = dotgit

  let v ~dotgit ~minor ~major ~major_uid ?(packed = []) ~refs root =
    Pack.make major ~uid_of_major_uid:major_uid.uid_of_major_uid
      ~idx_major_uid_of_uid:major_uid.idx_major_uid_of_uid
    >>= fun packs ->
    let fds = Pack.fds packs in
    let pools =
      let fold fd =
        ( fd,
          Lwt_pool.create 4 @@ fun () ->
          let z = Bigstringaf.create De.io_buffer_size in
          let w = De.make_window ~bits:15 in
          let allocate _ = w in
          let w = Carton.Dec.W.make fd in
          Lwt.return { Carton_git.z; allocate; w } )
      in
      List.map fold fds
    in
    let buffs =
      Lwt_pool.create 12 @@ fun () ->
      let buffers =
        {
          window = De.make_window ~bits:15;
          lz = De.Lz77.make_window ~bits:15;
          queue = De.Queue.create 0x1000;
          i = Bigstringaf.create De.io_buffer_size;
          o = Bigstringaf.create De.io_buffer_size;
          hdr = Cstruct.create 30;
        }
      in
      Lwt.return buffers
    in
    let rs = refs in
    Log.debug (fun m -> m "%d packed-refs added.\n%!" (List.length packed));
    let refs =
      {
        atomic_wr =
          (fun root refname str ->
            Caml_scheduler.inj (Rs.atomic_wr root refname str));
        atomic_rd =
          (fun root refname -> Caml_scheduler.inj (Rs.atomic_rd root refname));
        uid_of_hex = Hash.of_hex_opt;
        uid_to_hex = Hash.to_hex;
        packed;
      }
    in
    Lwt.return
      {
        minor;
        major;
        major_uid;
        packs;
        pools;
        buffs;
        rs;
        refs;
        root;
        dotgit;
        shallows = Shallow.make [];
      }

  type error =
    [ `Not_found of Hash.t
    | `Reference_not_found of Reference.t
    | `Cycle
    | `Minor of Mn.error
    | `Major of Mj.error
    | `Ref of Rs.error
    | `Contents
    | `Malformed
    | `Msg of string ]

  let pp_error ppf = function
    | `Not_found hash -> Fmt.pf ppf "%a not found" Hash.pp hash
    | `Reference_not_found refname ->
        Fmt.pf ppf "%a not found" Reference.pp refname
    | `Cycle -> Fmt.pf ppf "Got a reference cycle"
    | `Contents -> Fmt.pf ppf "contents retrieved an error"
    | `Malformed -> Fmt.pf ppf "Malformed Git object"
    | `Minor err -> Fmt.pf ppf "%a" Mn.pp_error err
    | `Major err -> Fmt.pf ppf "%a" Mj.pp_error err
    | `Ref err -> Fmt.pf ppf "%a" Rs.pp_error err
    | `Msg err -> Fmt.string ppf err

  let resources { pools; _ } fd = Lwt_pool.use (List.assoc fd pools)

  let read_inflated t hash =
    Log.debug (fun l -> l "Git.read %a" Hash.pp hash);
    Pack.get t.major ~resources:(resources t) t.packs hash >>= function
    | Ok v -> Lwt.return_some v
    | Error (`Msg _) -> Lwt.return_none
    | Error (`Not_found _) -> (
        Lwt_pool.use t.buffs @@ fun buffers ->
        Loose.atomic_get t.minor buffers hash >>= function
        | Ok v -> Lwt.return_some v
        | Error `Non_atomic -> (
            Loose.get t.minor buffers hash >>= function
            | Ok v -> Lwt.return_some v
            | Error _ -> Lwt.return_none))

  let read t hash =
    read_inflated t hash >>= function
    | None ->
        Log.err (fun m -> m "Object %a not found." Hash.pp hash);
        Lwt.return_error (`Not_found hash)
    | Some v ->
        let kind =
          match Carton.Dec.kind v with
          | `A -> `Commit
          | `B -> `Tree
          | `C -> `Blob
          | `D -> `Tag
        in
        let raw =
          Cstruct.of_bigarray (Carton.Dec.raw v) ~off:0 ~len:(Carton.Dec.len v)
        in
        Lwt.return (Value.of_raw ~kind raw)

  let read_exn t hash =
    read t hash >>= function
    | Ok v -> Lwt.return v
    | Error _ ->
        let err = Fmt.str "Git.Store.read_exn: %a not found" Hash.pp hash in
        Lwt.fail_invalid_arg err

  let stream_of_raw ?(chunk = De.io_buffer_size) raw =
    let len = Carton.Dec.len raw in
    let raw = Carton.Dec.raw raw in
    let raw = Bigstringaf.sub raw ~off:0 ~len in
    let pos = { contents = 0 } in
    let stream () =
      if !pos = len then Lwt.return_none
      else
        let len = min (len - !pos) chunk in
        let str = Bigstringaf.substring raw ~off:!pos ~len in
        pos := !pos + len;
        Lwt.return_some str
    in
    stream

  let write t v =
    let raw = Value.to_raw_without_header v in
    let len = String.length raw in
    let raw = Bigstringaf.of_string raw ~off:0 ~len in
    let kind =
      match v with Commit _ -> `A | Tree _ -> `B | Blob _ -> `C | Tag _ -> `D
    in
    let raw = Carton.Dec.v ~kind raw in
    Lwt_pool.use t.buffs @@ fun buffers ->
    Loose.atomic_add t.minor buffers raw >>= function
    | Ok v -> Lwt.return_ok v
    | Error (`Store err) -> Lwt.return_error (`Minor err)
    | Error `Non_atomic -> (
        let kind =
          match v with
          | Commit _ -> `Commit
          | Tree _ -> `Tree
          | Blob _ -> `Blob
          | Tag _ -> `Tag
        in
        let length = Int64.of_int (Carton.Dec.len raw) in
        let stream = stream_of_raw raw in
        Loose.add t.minor buffers (kind, length) stream >>= function
        | Ok _ as v -> Lwt.return v
        | Error (`Store err) -> Lwt.return_error (`Minor err))

  let read_inflated t hash =
    read_inflated t hash >>= function
    | Some v ->
        let kind =
          match Carton.Dec.kind v with
          | `A -> `Commit
          | `B -> `Tree
          | `C -> `Blob
          | `D -> `Tag
        in
        let raw =
          Cstruct.of_bigarray (Carton.Dec.raw v) ~off:0 ~len:(Carton.Dec.len v)
        in
        Lwt.return_some (kind, raw)
    | None -> Lwt.return_none

  let write_inflated t ~kind raw =
    let { Cstruct.buffer; off; len } = raw in
    let raw = Bigstringaf.sub buffer ~off ~len in
    let kind0 =
      match kind with `Commit -> `A | `Tree -> `B | `Blob -> `C | `Tag -> `D
    in
    let v = Carton.Dec.v ~kind:kind0 raw in
    Lwt_pool.use t.buffs @@ fun buffers ->
    Loose.atomic_add t.minor buffers v >>= function
    | Ok (hash, _) -> Lwt.return hash
    | Error (`Store err) ->
        Lwt.fail (Failure (Fmt.str "%a" pp_error (`Minor err)))
    | Error `Non_atomic -> (
        let consumed = Stdlib.ref false in
        let stream () =
          if !consumed then Lwt.return_none
          else (
            consumed := true;
            Lwt.return_some (Bigstringaf.to_string raw))
        in
        Loose.add t.minor buffers (kind, Int64.of_int len) stream >>= function
        | Ok (hash, _) -> Lwt.return hash
        | Error (`Store err) ->
            Lwt.fail (Failure (Fmt.str "%a" pp_error (`Minor err))))

  let mem t hash =
    if not (Pack.exists t.major t.packs hash) then Loose.exists t.minor hash
    else Lwt.return true

  let list t =
    let l0 = Pack.list t.major t.packs in
    Loose.list t.minor >|= List.rev_append l0

  let size t hash =
    if Pack.exists t.major t.packs hash then
      Pack.get t.major ~resources:(resources t) t.packs hash >>= function
      | Ok v -> Lwt.return_ok (Int64.of_int (Carton.Dec.len v))
      | Error _ as err -> Lwt.return err
    else
      Lwt_pool.use t.buffs @@ fun buffers ->
      Loose.size_and_kind t.minor buffers hash >>= function
      | Ok (size, _) -> Lwt.return_ok size
      | Error _ as err -> Lwt.return err

  let contents t =
    list t >>= fun hashes ->
    Lwt_list.map_p (fun hash -> read_exn t hash >|= fun v -> hash, v) hashes

  let is_shallowed t hash = Shallow.exists ~equal:Hash.equal t.shallows hash
  let shallowed t = Shallow.get t.shallows
  let shallow t hash = Shallow.append t.shallows hash
  let unshallow t hash = Shallow.remove t.shallows ~equal:Hash.equal hash

  module Traverse = Traverse_bfs.Make (struct
    module Hash = Hash
    module Value = Value

    type nonrec t = t

    let root { root; _ } = root
    let read_exn = read_exn
    let is_shallowed = is_shallowed
  end)

  let fold = Traverse.fold
  let iter = Traverse.iter

  let ( >>? ) x f =
    let open Lwt.Infix in
    x >>= function Ok x -> f x | Error err -> Lwt.return_error err

  let batch_write t hash ~pck ~idx =
    let mj_pck_uid = t.major_uid.pck_major_uid_of_uid t.major hash in
    let mj_idx_uid = t.major_uid.idx_major_uid_of_uid t.major hash in
    let rec save stream fd =
      stream () >>= function
      | Some str -> Mj.append t.major fd str >>= fun () -> save stream fd
      | None -> Mj.close t.major fd
    in
    Mj.create ~trunc:true ~mode:Mj.Wr t.major mj_pck_uid
    >>? save pck
    >>? (fun () ->
          Mj.create ~trunc:true ~mode:Mj.Wr t.major mj_idx_uid >>? save idx
          >>? fun () -> Pack.add t.major t.packs ~idx:mj_idx_uid mj_pck_uid)
    >|= Rresult.R.reword_error (fun err -> `Major err)

  module Ref = struct
    open Caml_scheduler

    let caml_scheduler =
      let open Caml_scheduler in
      { Carton.bind = (fun x f -> f (prj x)); Carton.return = (fun x -> inj x) }

    let src = Logs.Src.create "git.store.ref" ~doc:"logs git's reference event"

    module Log = (val Logs.src_log src : Logs.LOG)

    let list t =
      let lst = Rs.list t.rs in
      let fold acc refname =
        Log.debug (fun m -> m "Resolve %a." Reference.pp refname);
        let res = prj (Reference.resolve caml_scheduler t.rs t.refs refname) in
        match res with
        | Ok uid -> (refname, uid) :: acc
        | Error (`Not_found refname) ->
            Log.warn (fun m -> m "Reference %a not found." Reference.pp refname);
            acc
        | Error `Cycle ->
            Log.warn (fun m -> m "Got a cycle with %a." Reference.pp refname);
            acc
      in
      let res = List.fold_left fold [] lst in
      Lwt.return res

    let mem t refname =
      Log.debug (fun m -> m "Check reference %a." Reference.pp refname);
      let res = Rs.atomic_rd t.rs refname in
      match res with
      | Ok _ -> Lwt.return true
      | _ ->
          let res = Packed.exists refname t.refs.packed in
          Log.debug (fun m ->
              m "%a exists as packed-ref: %b" Reference.pp refname res);
          Lwt.return res

    let read t refname =
      Log.debug (fun m -> m "Read reference %a." Reference.pp refname);
      let res = prj (Reference.read caml_scheduler t.rs t.refs refname) in
      match res with
      | Ok _ as v -> Lwt.return v
      | Error (`Not_found refname) ->
          Log.err (fun m -> m "Reference %a not found." Reference.pp refname);
          Lwt.return_error (`Reference_not_found refname)

    let resolve t refname =
      Log.debug (fun m -> m "Resolve reference %a." Reference.pp refname);
      let res = prj (Reference.resolve caml_scheduler t.rs t.refs refname) in
      match res with
      | Ok _ as v -> Lwt.return v
      | Error (`Not_found refname) ->
          Lwt.return_error (`Reference_not_found refname)
      | Error `Cycle as err -> Lwt.return err

    let write t refname contents =
      let res =
        prj (Reference.write caml_scheduler t.rs t.refs refname contents)
      in
      match res with
      | Ok _ as v -> Lwt.return v
      | Error (`Store err) -> Lwt.return_error (`Ref err)

    let remove t refname =
      let res = Rs.atomic_rm t.rs refname in
      let res = Rresult.R.reword_error (fun err -> `Ref err) res in
      if Packed.exists refname t.refs.packed then (
        t.refs <- { t.refs with packed = Packed.remove refname t.refs.packed };
        Lwt.return res)
      else Lwt.return res
  end

  let reset t =
    Log.info (fun m -> m "Reset store %a." Fpath.pp t.root);
    Mn.reset t.minor >|= Rresult.R.reword_error (fun err -> `Minor err)
    >>? fun () ->
    Mj.reset t.major >|= Rresult.R.reword_error (fun err -> `Major err)
    >>? fun () ->
    let res = Rs.reset t.rs |> Rresult.R.reword_error (fun err -> `Ref err) in
    Lwt.return res
end
