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

  (* readdir / closedir *)

  type fd

  val input_line : fd -> string option fiber
end

module type Mj = sig
  include Carton_git.STORE

  include
    Smart_git.APPEND
      with type t := t
       and type uid := uid
       and type fd := fd
       and type error := error
       and type +'a fiber := 'a fiber
end

open Value
open Reference
open Loose

module Make
    (Digestif : Digestif.S)
    (Mn : Loose_git.STORE
            with type +'a fiber = 'a Lwt.t
             and type uid = Digestif.t)
    (Mj : Mj with type +'a fiber = 'a Lwt.t)
    (Rs : Rs with type +'a fiber = 'a Lwt.t) =
struct
  module Hash = Hash.Make (Digestif)
  module Value = Value.Make (Hash)

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

  (* TODO *)
  let has_global_watches = false

  let has_global_checkout = false

  type t = {
    minor : Mn.t;
    major : Mj.t;
    packs : (Mj.uid, Mj.fd, Hash.t) Carton_git.t;
    pools :
      ((Mj.fd * int64) * (Mj.fd * int64) Carton_git.buffers Lwt_pool.t) list;
    buffs : buffers Lwt_pool.t;
    rs : Rs.t;
    root : Fpath.t;
    mutable refs : (Rs.t, Hash.t, Rs.error, Carton_lwt.lwt) Reference.store;
  }

  let root { root; _ } = root

  let v ~minor ~major ~idx ?(packed = []) ~refs root =
    Pack.make major ~idx >>= fun packs ->
    let fds = Pack.fds packs in
    let pools =
      let fold fd =
        ( fd,
          Lwt_pool.create 4 @@ fun () ->
          let z = Bigstringaf.create De.io_buffer_size in
          let w = De.make_window ~bits:15 in
          let allocate _ = w in
          let w = Carton.Dec.W.make fd in
          Lwt.return { Carton_git.z; allocate; w } ) in
      List.map fold fds in
    let buffs =
      Lwt_pool.create 12 @@ fun () ->
      let buffers =
        {
          window = De.make_window ~bits:15;
          queue = De.Queue.create 0x1000;
          i = Bigstringaf.create De.io_buffer_size;
          o = Bigstringaf.create De.io_buffer_size;
          hdr = Cstruct.create 30;
        } in
      Lwt.return buffers in
    let rs = refs in
    let refs =
      {
        atomic_wr =
          (fun root refname str ->
            Carton_lwt.inj (Rs.atomic_wr root refname str));
        atomic_rd =
          (fun root refname -> Carton_lwt.inj (Rs.atomic_rd root refname));
        uid_of_hex = Hash.of_hex_opt;
        uid_to_hex = Hash.to_hex;
        packed;
      } in
    Lwt.return { minor; major; packs; pools; buffs; rs; refs; root }

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
    | `Cycle -> Fmt.pf ppf "Got a cycle"
    | `Contents -> Fmt.pf ppf "contents retrieved an error"
    | `Malformed -> Fmt.pf ppf "Malformed Git object"
    | `Minor err -> Fmt.pf ppf "%a" Mn.pp_error err
    | `Major err -> Fmt.pf ppf "%a" Mj.pp_error err
    | `Ref err -> Fmt.pf ppf "%a" Rs.pp_error err
    | `Msg err -> Fmt.string ppf err

  let resources { pools; _ } fd = Lwt_pool.use (List.assoc fd pools)

  let read_inflated t hash =
    Log.debug (fun l -> l "Git.read %a" Hash.pp hash) ;
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
    | None -> Lwt.return_error (`Not_found hash)
    | Some v ->
        let kind =
          match Carton.Dec.kind v with
          | `A -> `Commit
          | `B -> `Tree
          | `C -> `Blob
          | `D -> `Tag in
        let raw =
          Cstruct.of_bigarray (Carton.Dec.raw v) ~off:0 ~len:(Carton.Dec.len v)
        in
        Lwt.return (Value.of_raw ~kind raw)

  let read_exn t hash =
    read t hash >>= function
    | Ok v -> Lwt.return v
    | Error _ ->
        let err = Fmt.strf "Git.Store.read_exn: %a not found" Hash.pp hash in
        Lwt.fail_invalid_arg err

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
          | Tag _ -> `Tag in
        let length = Value.length v in
        let stream = Value.stream v in
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
          | `D -> `Tag in
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
        Lwt.fail (Failure (Fmt.strf "%a" pp_error (`Minor err)))
    | Error `Non_atomic -> (
        let consumed = Stdlib.ref false in
        let stream () =
          if !consumed
          then Lwt.return_none
          else (
            consumed := true ;
            Lwt.return_some (Bigstringaf.to_string raw)) in
        Loose.add t.minor buffers (kind, Int64.of_int len) stream >>= function
        | Ok (hash, _) -> Lwt.return hash
        | Error (`Store err) ->
            Lwt.fail (Failure (Fmt.strf "%a" pp_error (`Minor err))))

  let mem t hash =
    if not (Pack.exists t.major t.packs hash)
    then Loose.exists t.minor hash
    else Lwt.return true

  let list t =
    let l0 = Pack.list t.major t.packs in
    Loose.list t.minor >|= List.rev_append l0

  let size t hash =
    if Pack.exists t.major t.packs hash
    then
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
    Lwt_list.map_p (fun hash -> read_exn t hash >|= fun v -> (hash, v)) hashes

  let reset _ = Lwt.return_ok () (* TODO *)

  module Traverse = Traverse_bfs.Make (struct
    module Hash = Hash
    module Value = Value

    type nonrec t = t

    let root { root; _ } = root

    let read_exn = read_exn
  end)

  let fold = Traverse.fold

  let iter = Traverse.iter

  type index = Mj.uid

  type pack = Mj.uid

  let batch_write t ~index ~pack =
    Pack.add t.major t.packs ~idx:(fun _ -> index) pack
    >|= Rresult.R.reword_error (fun err -> `Major err)

  module Ref = struct
    open Carton_lwt.Scheduler

    let list t =
      Rs.list t.rs >>= fun lst ->
      let fold acc refname =
        prj (Reference.resolve Carton_lwt.lwt t.rs t.refs refname) >>= function
        | Ok uid -> Lwt.return ((refname, uid) :: acc)
        | Error (`Not_found refname) ->
            Log.warn (fun m -> m "Reference %a not found." Reference.pp refname) ;
            Lwt.return acc
        | Error `Cycle ->
            Log.warn (fun m -> m "Got a cycle with %a." Reference.pp refname) ;
            Lwt.return acc in
      Lwt_list.fold_left_s fold [] lst

    let mem t refname =
      Rs.atomic_rd t.rs refname >>= function
      | Ok _ -> Lwt.return true
      | _ -> Lwt.return (Packed.exists refname t.refs.packed)

    let read t refname =
      prj (Reference.read Carton_lwt.lwt t.rs t.refs refname) >>= function
      | Ok _ as v -> Lwt.return v
      | Error (`Not_found refname) ->
          Lwt.return_error (`Reference_not_found refname)

    let resolve t refname =
      prj (Reference.resolve Carton_lwt.lwt t.rs t.refs refname) >>= function
      | Ok _ as v -> Lwt.return v
      | Error (`Not_found refname) ->
          Lwt.return_error (`Reference_not_found refname)
      | Error `Cycle as err -> Lwt.return err

    let write t refname contents =
      prj (Reference.write Carton_lwt.lwt t.rs t.refs refname contents)
      >>= function
      | Ok _ as v -> Lwt.return v
      | Error (`Store err) -> Lwt.return_error (`Ref err)

    let remove t refname =
      Rs.atomic_rm t.rs refname >>= fun _ ->
      if Packed.exists refname t.refs.packed
      then (
        t.refs <- { t.refs with packed = Packed.remove refname t.refs.packed } ;
        Lwt.return_ok ())
      else Lwt.return_ok ()
  end
end
