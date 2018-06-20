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

[@@@warning "-32"]

open Lwt.Infix

let ( >>!= ) a f = Lwt_result.bind_lwt_err a f
let ( >>?= ) = Lwt_result.bind
let ( >>|= ) = Lwt_result.map
let ( >!= ) a f = Lwt_result.map_err f a

module Option =
struct
  let ( >>= ) v f = match v with Some v -> f v | None -> None
  let ( >|= ) v f = match v with Some v -> Some (f v) | None -> None
end

let src = Logs.Src.create "git.pack" ~doc:"Git pack engine"
module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig

  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS: S.FS

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := HDec
  module RPDec: Unpack.D
    with module Hash := Hash
     and type Mapper.fd = FS.Mapper.fd
     and type Mapper.error = FS.error
     and module Inflate := Inflate
     and module Hunk := HDec
     and module Pack := PDec

  module PEnc: Pack.P
    with module Hash = Hash
     and module Deflate = Deflate

  module IDec: Index_pack.LAZY
    with module Hash = Hash

  module IEnc: Index_pack.ENCODER
    with module Hash = Hash

  module PInfo: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec

  type t

  type ('mmu, 'location) r =
    { mmu         : 'mmu
    ; with_cstruct: 'mmu -> pack -> int -> (('location * Cstruct.t) -> unit Lwt.t) -> unit Lwt.t
    ; free        : 'mmu -> 'location -> unit Lwt.t }
  and pack = Pack of Hash.t | Unrecorded

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Error.Decoder.t
    | `Invalid_hash of Hash.t
    | `Delta of PEnc.Delta.error
    | `Not_found ]

  val pp_error: error Fmt.t

  val v: FS.t -> Fpath.t list -> t Lwt.t
  val lookup: t -> Hash.t -> (Hash.t * (Crc32.t * int64)) option
  val list: t -> Hash.t list
  val mem: t -> Hash.t -> bool

  val add:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Fpath.t
    -> [ `Normalized of PInfo.path ] PInfo.t
    -> (Hash.t * int, error) result Lwt.t

  val read:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> to_result:((RPDec.kind * Cstruct.t * int * RPDec.Ascendant.s) -> ('value, error) result Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Hash.t
    -> ('value, error) result Lwt.t

  val size:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> t
    -> Hash.t
    -> (int, error) result Lwt.t
end

module Make
    (Hash: S.HASH)
    (FS: S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
    (HDec: Unpack.H with module Hash := Hash)
    (PDec: Unpack.P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := HDec)
    (RPDec: Unpack.D with module Hash := Hash
                      and type Mapper.fd = FS.Mapper.fd
                      and type Mapper.error = FS.error
                      and module Inflate := Inflate
                      and module Hunk := HDec
                      and module Pack := PDec)
  (* : S with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS
       and module HDec := HDec
       and module PDec := PDec
       and module RPDec := RPDec *)
= struct

  module Hash = Hash
  module Inflate = Inflate
  module Deflate = Deflate
  module FS = Helper.FS(FS)

  module HDec = HDec
  module PDec = PDec
  module RPDec = RPDec
  module PInfo = Pack_info.Make(Hash)(Inflate)(HDec)(PDec)
  module PEnc = Pack.Stream(Hash)(Deflate)
  module IDec = Index_pack.Lazy(Hash)
  module IEnc = Index_pack.Encoder(Hash)

  (* The [Pack_engine] module implements operations on Git pack file
     and focus on keeping control on memory allocation as much as
     possible. The goal is to be able to read a pack file without
     having to load it fully in memory, and to keep control of memory
     allocation at runtime.

     In order to do this, a pack file can be in one of these five
     states:

     - exists: the PACK file already exists on the git repository

     - loaded: the PACK file is loaded: we promote exists to loaded
     when the user wants to read an object from this PACK file - but
     lookup, list and mem operations don't do this promotion.

     - normalized: when the PACK file comes from a stream (when you
     pull/fetch/clone)

     - resolved: when we resolved all delta-ified objects in the PACK
     file

     - total: when we ensure than the PACK file is thin or not *)

  type fs_error = FS.error Error.FS.t
  type inf_error = Inflate.error Error.Inf.t

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | inf_error
    | fs_error
    | Error.Decoder.t (* XXX(dinosaure): trick about [to_result] function. *)
    | `Invalid_hash of Hash.t
    | `Delta of PEnc.Delta.error
    | `Not_found ]

  type ('mmu, 'location) r =
    { mmu         : 'mmu
    ; with_cstruct: 'mmu -> pack -> int -> (('location * Cstruct.t) -> unit Lwt.t) -> unit Lwt.t
    ; free        : 'mmu -> 'location -> unit Lwt.t }
  and pack = Pack of Hash.t | Unrecorded

  let empty_cstruct = Cstruct.create 0

  module Exists =  struct

    (* Entry-point state for pack-files; Very few allocations are done
       in this state as only the IDX file is mmaped. Hence, only
       operations on the IDX file are allowed ([lookup], [mem]). *)

    type t =
      { index     : IDec.t
      ; hash_pack : Hash.t
      ; fd        : FS.Mapper.fd }

    let lookup { index; _ } hash = IDec.find index hash
    let mem { index; _ } hash = IDec.mem index hash
    let fold f { index; _ } a = IDec.fold index f a

    let v fs path =
      let err_idx_decoder err = `Idx_decoder err in

      let ( <.> ) f g = fun x -> f (g x) in
      let ( >>?= ) = Lwt_result.bind in
      let ( >>|= ) = Lwt_result.bind_result in
      (* ('a, 'e) result Lwt.t -> ('e -> (unit, 'e) result Lwt.t) -> ('a, 'e) result Lwt.t *)
      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      FS.Mapper.openfile fs path                        >|= Rresult.R.reword_error (Error.FS.err_open path)
      >>?= fun fd -> FS.Mapper.length fd                >|= Rresult.R.reword_error (Error.FS.err_length path)
      >>?= fun ln -> FS.Mapper.map fd (Int64.to_int ln) >|= Rresult.R.reword_error (Error.FS.err_map path)
      >>|= (Rresult.R.reword_error err_idx_decoder <.> IDec.make)
      >>?= fun id ->
       let hash_pack =
         let basename = Fpath.basename (Fpath.rem_ext path) in
         Scanf.sscanf basename "pack-%s" (fun x -> Hash.of_hex x) in
       Lwt.return_ok { index = id; hash_pack; fd; }
      >>!= fun er -> FS.Mapper.close fd                 >|= Rresult.R.reword_error (Error.FS.err_close path)
      >>?= fun () -> Lwt.return_error er
  end

  module Loaded =
  struct

    let src = Logs.Src.create "git.pack_engine.loaded" ~doc:"logs git's loaded pack event"
    module Log = (val Logs.src_log src: Logs.LOG)

    (* Pack files in the [loaded] state can only have been promoted
       from the [exist] state. That's the worst state regarding memory
       allocation.

       As for the [exist] state, [lookup] and [mem] are available. A
       new operation, [read] becomes available. Once an object is
       read, metadata regarding its size and checksum are memoized.

       Once the metadata for all objects is known, the pack can be
       promoted to the [normalized] state (note: the pack will be then
       immediately promoted to the [resolved] state as the pack is not
       thin). *)

    type t =
      { index : IDec.t
      ; pack  : RPDec.pack
      ; info  : [ `Pass ] PInfo.t
      ; fdi   : FS.Mapper.fd
      ; fdp   : FS.Mapper.fd
      ; cache_len : (int64, int) RPDec.Cache.t
      ; mutable thin : bool }

    let lookup { index; info; _ } hash =
      match Hashtbl.find info.PInfo.index hash with
      | (crc, abs_off, _) -> Some (crc, abs_off)
      | exception Not_found -> IDec.find index hash

    let mem { index; info; _ } hash =
      Hashtbl.mem info.PInfo.index hash || IDec.mem index hash

    let fold f { index; _ } a = IDec.fold index f a

    let rec object_to_delta ?(depth = 1) = function
      | RPDec.Ascendant.External { hash; raw; _ } ->
        PInfo.Unresolved { hash; length = Cstruct.len raw; }
      | RPDec.Ascendant.Root { RPDec.Base.offset; length; hash; _ } ->
        PInfo.Internal { abs_off = offset; length; hash; }
      | RPDec.Ascendant.Node { patch; source; } ->
        let inserts = List.fold_left (fun acc -> function
            | RPDec.Diff.Insert i -> RPDec.Diff.len i + acc
            | _ -> acc) 0 patch.RPDec.Patch.hunks in
        PInfo.Delta { hunks_descr = patch.RPDec.Patch.descr; inserts; depth; from = object_to_delta ~depth:(depth + 1) source; }

    let size { pack; _ } ~ztmp ~window:zwin hash =
      RPDec.Ascendant.length ~ztmp ~zwin pack (`Hash hash) >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    (* [read] is the main function available in the [loaded] state.

       ## Arguments

       [mmu]: memory management unit. Used every time a memory
       allocation is needed.

       [with_cstruct]: malloc

       [free]: free

       [to_result]: The only goal of this function is to take
       ownership on the given [Cstruct.t]. Usually, that buffer is
       allocated via [with_cstruct] and [to_result] is called before
       that function free it. Hence, usually [to_result] is simply a
       [cstruct_copy], but could also be [Value.to_result] which
       optimize copies in some cases.

       [ztmp]: zlib buffer

       [window]: zlib window

       [loaded]: the [loaded] state

       [hash]: object's hash

       ## Allocation

       [to_result] is the critical function regarding memory
       allocation management. The policy is let to the user. For
       instance, a LRU cache for managing pre-allocated buffers can be
       used, or a new buffer could be created on every invocation of
       that function.

       [free] will be called on the used buffer, just after
       [to_result].

       ## Mémoization

       Every reads updates a table keepking metadata information about
       objects.  One of the important metadata which are being tracked
       is to know if their are external references, e.g. if the pack
       file if thin or not (note: if the file was stored on disk,
       normally it shouldn't -- but ocaml-git supports it just fine. )
       *)
    let read
        (type mmu) (type location) (type value)
        ~(to_result:(RPDec.kind * Cstruct.t * int * RPDec.Ascendant.s) -> (value, error) result Lwt.t)
        ~ztmp ~window:zwin
        (r:(mmu, location) r)
        ({ pack; info; index; _ } as t) hash
      : [ `Error of error | `Promote of value | `Return of value ] Lwt.t =
      Log.debug (fun l ->
          l ~header:"read" "Try to extract object %a from PACK %a."
            Hash.pp hash Hash.pp t.info.PInfo.hash_pack);

      lookup t hash |> function
      | None -> Lwt.return (`Error (`Pack_decoder (RPDec.Invalid_hash hash)))
      | Some (_, abs_off) ->

        let cache = { RPDec.Cache.find = (fun _ -> None)
                    ; promote = (fun _ _ -> ()) } in

        RPDec.Ascendant.needed_from_absolute_offset ~ztmp ~zwin ~cache pack abs_off >>= function
        | Error err -> Lwt.return (`Error (`Pack_decoder err))
        | Ok needed ->
          let res = ref None in
          (r.with_cstruct r.mmu Unrecorded (needed * 3) @@ fun (loc_raw, raw) ->
           let base, rtmp = Cstruct.sub raw 0 needed, (Cstruct.sub raw needed needed, Cstruct.sub raw (needed * 2) needed) in
           let deliver () = r.free r.mmu loc_raw in
           let cache = { RPDec.Cache.find = (fun _ -> None)
                       ; promote = (fun _ _ -> ()) } in
           RPDec.Ascendant.get_from_absolute_offset ~ztmp ~zwin ~cache base pack abs_off
           >>= fun x -> res := Some (deliver, x, rtmp); Lwt.return_unit)

          >>= (fun () -> match !res with
              | None -> assert false
              | Some (deliver, res, rtmp) ->
                Rresult.R.reword_error (fun err -> `Pack_decoder err) res
                |> Lwt.return
                >>?= fun obj -> to_result (RPDec.Ascendant.reconstruct rtmp obj)
                >>?= fun res -> Lwt.return_ok (obj, res)
                >>= fun res -> deliver () >|= fun () -> res)

          (* XXX(dinosaure): at this moment, the user takes the ownership of
             [obj]. [rtmp] will be free-ed (with [deliver]) - and [base] too. *)

          >|= function
            | Ok (obj, value) ->
              let crc, abs_off, length = match obj with
                | RPDec.Ascendant.External _ -> assert false
                | RPDec.Ascendant.Root base ->
                  base.RPDec.Base.crc,
                  base.RPDec.Base.offset,
                  base.RPDec.Base.length
                | RPDec.Ascendant.Node { patch; _ } ->
                  patch.RPDec.Patch.crc,
                  patch.RPDec.Patch.offset,
                  patch.RPDec.Patch.descr.HDec.target_length in

              Hashtbl.replace info.PInfo.index hash (crc, abs_off, length);
              Hashtbl.replace info.PInfo.delta abs_off (object_to_delta obj);

              let () = match obj with
                | RPDec.Ascendant.External _ -> t.thin <- true
                | RPDec.Ascendant.Root _ -> ()
                | RPDec.Ascendant.Node { source; _ } ->
                  let rec go = function
                    | RPDec.Ascendant.Node { source; _ } -> go source
                    | RPDec.Ascendant.External _ -> t.thin <- true
                    | RPDec.Ascendant.Root _ -> () in
                  go source in

              if Hashtbl.length info.PInfo.delta = IDec.cardinal index
              then `Promote value
              else `Return value
            | Error err -> `Error err

    let pack_decoder ~read_and_exclude ~idx fs path =
      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      FS.Mapper.openfile fs path
      >|= Rresult.R.reword_error (Error.FS.err_open path)
      >>?= fun fd ->
      let fun_idx    = idx in
      let fun_last   = read_and_exclude in

      RPDec.make fd fun_idx fun_last
      >|= Rresult.R.reword_error (Error.FS.err_length path)
      >>?= fun pack -> Lwt.return_ok (fd, pack)
      >>!= fun er -> FS.Mapper.close fd >|= Rresult.R.reword_error (Error.FS.err_close path)
      >>?= fun () -> Lwt.return_error er

    (* [of_exists] promotes a pack file from [exist] to [loaded]. Only
       the IDX file. [read_and_exclude] reads objects in other pack
       files (excluding any references to the current one, to avoid
       infinite loops). *)
    let of_exists ~root ~read_and_exclude fs exists =
      let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.pack" (Hash.to_hex exists.Exists.hash_pack)) in
      let index = exists.Exists.index in
      let info = PInfo.v exists.Exists.hash_pack in

      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      pack_decoder ~read_and_exclude ~idx:(IDec.find exists.Exists.index) fs path
      >>?= fun (fd, pack) ->
      Lwt.return_ok { index
                    ; pack
                    ; info
                    ; fdi = exists.Exists.fd
                    ; fdp = fd
                    ; cache_len = RPDec.Ascendant.needed_cache 256
                    ; thin = false }
      >>!= fun er -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
      >>?= fun () -> Lwt.return_error er
  end

  module Normalized = struct

    let src = Logs.Src.create "git.pack_engine.normalized" ~doc:"logs git's pack normalization event"
    module Log = (val Logs.src_log src: Logs.LOG)

    (* [Normalized] pack files come from the network (clone, pull,
       fetch). In that state, some object references might come from a
       different pack file (e.g. if it is thin). *)

    type t =
      { pack  : RPDec.pack
      ; path  : Fpath.t
      ; info  : [ `Normalized of PInfo.path ] PInfo.t
      ; fd    : FS.Mapper.fd
      ; mutable thin : bool }

    let length_of_path path =
      let rec go acc = function
        | PInfo.Load _ -> acc
        | PInfo.Patch { hunks; src; _ } -> go (acc + hunks) src in
      go 0 path

    let digest (kind, raw) =
      let hdr = Fmt.strf "%s %d\000"
          (match kind with
               | `Commit -> "commit"
               | `Tree -> "tree"
               | `Tag -> "tag"
               | `Blob -> "blob")
          (Cstruct.len raw) in

      let ctx = Hash.Digest.init () in
      Hash.Digest.feed ctx (Cstruct.of_string hdr);
      Hash.Digest.feed ctx raw;
      Hash.Digest.get ctx

    let of_info ~read_and_exclude fs path_tmp info =
      let idx hash = match Hashtbl.find info.PInfo.index hash with
        | (crc, abs_off, _) -> Some (crc, abs_off)
        | exception Not_found -> None in

      Loaded.pack_decoder ~read_and_exclude ~idx fs path_tmp >>= function
      | Ok (fd, pack) ->
        Lwt.return_ok { pack; path = path_tmp; info; fd; thin = false }
      | Error _ as err -> Lwt.return err

    exception Fail of RPDec.error

    (* [second_pass] allows to analyse all compressed chains of
       objects and to discover if the pack file is thin or not.

       TODO: this code could me moved in [Resolved]. *)
    let second_pass
      (type mmu) (type location)
      ~ztmp
      ~window:zwin
      (r:(mmu, location) r)
      normalized =

      let cache_needed = { RPDec.Cache.find = (fun _ -> None)
                         ; promote = (fun _ _ -> ()) } in
      let cache_object = { RPDec.Cache.find = (fun _ -> None)
                         ; promote = (fun _ _ -> ()) } in

      let resolve abs_off =
        RPDec.Ascendant.needed_from_absolute_offset ~ztmp ~zwin ~cache:cache_needed normalized.pack abs_off >>= function
        | Error err -> Lwt.fail (Fail err)
        | Ok needed ->
          Log.debug (fun l -> l "Allocate %d byte(s) to extract %a:%Ld." (needed * 3) Hash.pp normalized.info.PInfo.hash_pack abs_off);

          r.with_cstruct r.mmu Unrecorded (needed * 3) @@ fun (loc_raw, raw) ->
          Log.debug (fun l -> l "Has %d byte(s)." (Cstruct.len raw));

          let rtmp = Cstruct.sub raw 0 needed, Cstruct.sub raw needed needed in
          let base = Cstruct.sub raw (needed * 2) needed in

          RPDec.Ascendant.get_from_absolute_offset ~ztmp ~zwin ~cache:cache_object base normalized.pack abs_off >>= function
          | Error err -> Lwt.fail (Fail err)
          | Ok obj ->

            Log.debug (fun l -> l "Patch of object: %a.\n" PInfo.pp_delta (Loaded.object_to_delta obj));

            let (kind, raw, _, metadata) = RPDec.Ascendant.reconstruct rtmp obj in
            let hash = digest (kind, raw) in
            let crc, abs_off = match metadata with
              | `Extern -> assert false
              (* XXX(dinosaure): impossible. An object is a patch or a base,
                 however, source of it can be extern. *)
              | `Patch metadata | `Base metadata ->
                metadata.RPDec.Ascendant.crc, metadata.RPDec.Ascendant.offset in

            Log.info (fun l ->
                l ~header:"second_pass" "Add object %a (length: %d, offset: %Ld)."
                  Hash.pp hash (Hashtbl.length normalized.info.PInfo.index) abs_off);

            Hashtbl.add normalized.info.PInfo.index hash (crc, abs_off, needed);
            Hashtbl.replace normalized.info.PInfo.delta abs_off (Loaded.object_to_delta obj);

            let () = match obj with
              | RPDec.Ascendant.External _ -> normalized.thin <- true
              | RPDec.Ascendant.Root _ -> ()
              | RPDec.Ascendant.Node { source; _ } ->
                let rec go = function
                  | RPDec.Ascendant.Node { source; _ } -> go source
                  | RPDec.Ascendant.External _ -> normalized.thin <- true
                  | RPDec.Ascendant.Root _ -> () in
                go source in

            r.free r.mmu loc_raw in

      Lwt.catch
        (fun () ->
           Lwt_list.iter_s
             resolve
             (Hashtbl.fold (fun k v a -> match v with
                  | PInfo.Delta _ | PInfo.Unresolved _ -> k :: a
                  | _ -> a) normalized.info.PInfo.delta []
              |> List.sort (fun ka kb -> Int64.compare ka kb))
           >>= fun () ->
           Log.debug (fun l ->
               l ~header:"second_pass" "Paths of delta-ification: %a."
                 Fmt.(Dump.hashtbl int64 PInfo.pp_delta) normalized.info.PInfo.delta);
           Lwt.return_ok ())
        (function Fail err -> Lwt.return_error (`Pack_decoder err)
                | exn -> Lwt.fail exn)
  end

  let with_buffer buff f =
    let c = ref None in
    buff (fun buf ->
        f buf >|= fun x ->
        c := Some x
      ) >|= fun () ->
    match !c with
    | Some x -> x
    | None   -> assert false

  module Resolved = struct

    let src = Logs.Src.create "git.pack_engine.resolved" ~doc:"logs git's resolved pack event"
    module Log = (val Logs.src_log src: Logs.LOG)

    (* In the [resolved] state, internal compressed objects are all
       known and the memory allocation needed to resolve external
       objects is bounded. All the delta chains are known, as well as
       the largest buffers needed to uncompress any object in the
       repository. *)

    type t =
      { pack       : RPDec.pack
      ; index      : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
      ; delta      : (int64, PInfo.delta) Hashtbl.t
      ; hash_pack  : Hash.t
      ; path_delta : PInfo.path
      ; fd         : FS.Mapper.fd
      ; buff       : (buffer -> unit Lwt.t) -> unit Lwt.t
      ; thin       : bool }
    and buffer =
      { htmp : Cstruct.t array
      ; rtmp : Cstruct.t * Cstruct.t
      ; base : Cstruct.t
      ; depth : int
      ; deliver : unit -> unit Lwt.t }

    let stream_of_path _ = assert false

    let mem { index; _ } hash = Hashtbl.mem index hash

    let fold f { index; _ } a = Hashtbl.fold (fun hash (crc, abs_off, _) a -> f hash (crc, abs_off) a) index a

    let depth_of_path path =
      let rec go acc = function
        | PInfo.Load _ -> acc
        | PInfo.Patch { src; _ } -> go (acc + 1) src in
      go 1 path

    let split_of_path cs path =
      let depth = depth_of_path path in
      let arr = Array.make depth empty_cstruct in
      let rec fill idx off = function
        | PInfo.Load _ -> ()
        | PInfo.Patch { hunks; src; _ } ->
          Array.unsafe_set arr idx (Cstruct.sub cs off hunks);
          fill (idx + 1) (off + hunks) src in
      fill 0 0 path; arr

    let list_of_path path =
      let rec go acc = function
        | PInfo.Load _ -> List.rev acc
        | PInfo.Patch { hunks; src; _ } ->
          go (hunks :: acc) src in
      go [] path

    module EIDX = struct
      module E = struct
        type state  = IEnc.t
        type result = unit
        type error  = IEnc.error
        type end' = [ `End of state ]
        type rest = [ `Flush of state | `Error of state * error ]
        let flush = IEnc.flush
        let used = IEnc.used_out
        let eval raw state = match IEnc.eval raw state with
          | #end' as v   -> let `End state = v in Lwt.return (`End (state, ()))
          | #rest as res -> Lwt.return res
      end
      include Helper.Encoder(E)(FS)
    end

    (* Save the IDX file is the pack file is not thin. *)
    let store_idx_file ~root fs sequence hash_pack =
      let file = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
      let encoder_idx = IEnc.default sequence hash_pack in
      let raw = Cstruct.create 0x8000 in (* XXX(dinosaure): as argument? *)
      let path = Fpath.(root / "objects" / "pack" / file) in
      EIDX.to_file fs path raw encoder_idx >|= function
      | Ok ()                  -> Ok ()
      | Error (`Encoder err)   -> Error (`Idx_encoder err)
      | Error #fs_error as err -> err

    let buffer
        (type mmu) (type location)
        (r:(mmu, location) r)
        hash_pack length_htmp length_raw path_delta : (buffer -> unit Lwt.t) -> unit Lwt.t =
      let ret = ref None in

      let make () =
        r.with_cstruct r.mmu (Pack hash_pack) (length_htmp + (length_raw * 3)) @@ fun (loc, buffer) ->

        Log.debug (fun l ->
          l ~header:"buffer" "Split hunks to: %a." Fmt.(Dump.list int) (list_of_path path_delta));

        let htmp = Cstruct.sub buffer 0 length_htmp in
        let htmp = split_of_path htmp path_delta in

        let depth = depth_of_path path_delta in

        let buffer = Cstruct.sub buffer length_htmp (length_raw * 3) in

        let rtmp = Cstruct.sub buffer 0 length_raw, Cstruct.sub buffer length_raw length_raw in
        let base = Cstruct.sub buffer (length_raw * 2) length_raw in

        let deliver () = r.free r.mmu loc in

        ret := Some { htmp; rtmp; base; deliver; depth; };
        Lwt.return_unit in

      (* XXX(dinosaure): bypass value restriction. *)
      let make () = make () >>= fun () -> match !ret with
        | Some ret -> Lwt.return ret
        | None -> assert false in

      let pool = Lwt_pool.create 4 make in
      Lwt_pool.use pool

    (* [of_normalized p] creates a [resolved] pack file from the
       normalized pack file [p]. This is done by applying
       {!Normalized.second_pass}. If the pack file is thin, we keep
       using the pack file in a temporary location; otherwise both the
       pack file and its associated IDX file are created in the
       repository and made available to other users. *)
    let of_normalized
        (type mmu) (type location)
        ~root
        ~read_and_exclude
        ~ztmp
        ~window
        fs
        (r:(mmu, location) r)
        normalized =
      Normalized.second_pass ~ztmp ~window r normalized >>= function
      | Error _ as err -> Lwt.return err
      | Ok () ->
        let info = PInfo.resolve ~length:(Hashtbl.length normalized.Normalized.info.PInfo.delta) normalized.Normalized.info in
        let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.pack" (Hash.to_hex info.PInfo.hash_pack)) in
        let `Resolved path_delta = info.PInfo.state in

        if normalized.Normalized.thin
        then
          let idx hash = match Hashtbl.find info.PInfo.index hash with
            | (crc, abs_off, _) -> Some (crc, abs_off)
            | exception Not_found -> None in

          Loaded.pack_decoder ~read_and_exclude ~idx fs normalized.Normalized.path
          >>?= fun (fd, pack) ->

          let length_hunks = Normalized.length_of_path path_delta in
          let length_objrw = Hashtbl.fold (fun _ (_, _, v) -> max v) info.PInfo.index 0 in
          let buff = buffer r info.PInfo.hash_pack length_hunks length_objrw path_delta in

          Lwt.return_ok { pack
                        ; index = info.PInfo.index
                        ; delta = info.PInfo.delta
                        ; hash_pack = info.PInfo.hash_pack
                        ; path_delta
                        ; fd
                        ; buff
                        ; thin = normalized.Normalized.thin }
        else
          let sequence f = Hashtbl.iter (fun k (crc, abs_off, _) -> f (k, (crc, abs_off))) info.PInfo.index in
          let idx hash = match Hashtbl.find info.PInfo.index hash with
            | (crc, abs_off, _) -> Some (crc, abs_off)
            | exception Not_found -> None in

          store_idx_file ~root fs sequence info.PInfo.hash_pack
          >>?= fun () -> FS.File.move fs normalized.Normalized.path path >|= Rresult.R.reword_error (Error.FS.err_move normalized.Normalized.path path)
          >>?= fun () -> FS.Mapper.close normalized.Normalized.fd >|= Rresult.R.reword_error (Error.FS.err_close normalized.Normalized.path)
          >>?= fun () -> Loaded.pack_decoder ~read_and_exclude ~idx fs path
          >>?= fun (fd, pack) ->

          let length_hunks = Normalized.length_of_path path_delta in
          let length_objrw = Hashtbl.fold (fun _ (_, _, v) -> max v) info.PInfo.index 0 in
          let buff = buffer r info.PInfo.hash_pack length_hunks length_objrw path_delta in

          Lwt.return_ok { pack
                        ; index = info.PInfo.index
                        ; delta = info.PInfo.delta
                        ; hash_pack = info.PInfo.hash_pack
                        ; path_delta
                        ; fd
                        ; buff
                        ; thin = normalized.Normalized.thin }

    let of_loaded
        (type mmu) (type location)
        (r:(mmu, location) r)
        loaded =
      Log.debug (fun l ->
          l ~header:"of_loaded" "Delta-ification path is complete: %a."
            Fmt.(Dump.hashtbl int64 PInfo.pp_delta) loaded.Loaded.info.PInfo.delta);

      let info = PInfo.normalize ~length:(Hashtbl.length loaded.Loaded.info.PInfo.delta) loaded.Loaded.info in
      let info = PInfo.resolve ~length:(Hashtbl.length loaded.Loaded.info.PInfo.delta) info in
      let `Resolved path_delta = info.PInfo.state in

      Log.debug (fun l ->
          l ~header:"of_loaded" "Approximation of delta-ification path is: %a."
            Fmt.(Dump.list int) (list_of_path path_delta));

      let length_hunks = Normalized.length_of_path path_delta in
      let length_objrw = Hashtbl.fold (fun _ (_, _, v) -> max v) info.PInfo.index 0 in
      let buff = buffer r info.PInfo.hash_pack length_hunks length_objrw path_delta in

      FS.Mapper.close loaded.Loaded.fdi >|= Rresult.R.reword_error Error.FS.err_sys_map >>?= fun () ->
      Lwt.return_ok { pack = loaded.Loaded.pack
                    ; index = info.PInfo.index
                    ; delta = info.PInfo.delta
                    ; hash_pack = info.PInfo.hash_pack
                    ; path_delta
                    ; fd = loaded.Loaded.fdp
                    ; buff
                    ; thin = loaded.Loaded.thin }

    (* [of_exists] allows to pass directly from the [exists] to the [resolved]
       state. The function is only valid on non-thin pack files. *)
    let of_exists
        (type mmu) (type location)
        ~root
        ~read_and_exclude
        ~ztmp
        ~window
        fs
        (r:(mmu, location) r)
        exists =
      let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.pack" (Hash.to_hex exists.Exists.hash_pack)) in
      let stream = stream_of_path path in
      let thin = ref false in
      let idx hash = match IDec.find exists.Exists.index hash with
        | Some _ as v -> v
        | none -> thin := true; none in

      PInfo.first_pass ~ztmp ~window ~idx stream >>= function
      | Ok info ->
        let info = PInfo.resolve ~length:(IDec.cardinal exists.Exists.index) info in
        let `Resolved path_delta = info.PInfo.state in

        let length_hunks = Normalized.length_of_path path_delta in
        let length_objrw = Hashtbl.fold (fun _ (_, _, v) -> max v) info.PInfo.index 0 in
        let buff = buffer r info.PInfo.hash_pack length_hunks length_objrw path_delta in
        let idx hash = match Hashtbl.find info.PInfo.index hash with
          | (crc, abs_off, _) -> Some (crc, abs_off)
          | exception Not_found -> None in

        let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

        Loaded.pack_decoder ~read_and_exclude ~idx fs path
        >>?= fun (fd, pack) -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
        >>?= fun () -> Lwt.return_ok { pack
                                     ; index = info.PInfo.index
                                     ; delta = info.PInfo.delta
                                     ; hash_pack = exists.Exists.hash_pack
                                     ; path_delta
                                     ; fd
                                     ; buff
                                     ; thin = !thin }
        >>!= fun er -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
        >>?= fun () -> Lwt.return_error er
      | Error err -> Lwt.return (Error (`Pack_info err))

    let lookup { index; _ } hash = match Hashtbl.find index hash with
      | (crc, abs_off, _) -> Some (crc, abs_off)
      | exception Not_found -> None

    let size ~ztmp ~window:zwin { pack; _ } hash =
      RPDec.Ascendant.length_from_hash ~ztmp ~zwin pack hash >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    let read (type value) ~ztmp ~window:zwin ~(to_result:(RPDec.kind * Cstruct.t * int * RPDec.Ascendant.s) -> (value, error) result Lwt.t) ({ pack; thin; _ } as t) hash
      : [ `Error of error | `Promote of value | `Return of value ] Lwt.t =
      with_buffer t.buff @@ fun { htmp; base; rtmp; deliver; _ } ->
      let cache = { RPDec.Cache.find = (fun _ -> None)
                  ; promote = (fun _ _ -> ()) } in
      RPDec.Ascendant.get_from_hash ~ztmp ~zwin ~cache ~htmp base pack hash >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)
      >>?= (fun x -> to_result (RPDec.Ascendant.reconstruct rtmp x)) >>= fun res -> deliver () >|= fun () -> match res with
      | Ok value ->
        if not thin then `Promote value else `Return value
      | Error err -> ` Error err
  end

  module Total =struct

    let src = Logs.Src.create "git.pack_engine.total" ~doc:"logs git's total pack event"
    module Log = (val Logs.src_log src: Logs.LOG)

    type t =
      { index      : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
      ; path_delta : PInfo.path
      ; hash_pack  : Hash.t
      ; pack       : RPDec.pack
      ; buff       : (Resolved.buffer -> unit Lwt.t) -> unit Lwt.t
      ; fd         : FS.Mapper.fd }

    let lookup { index; _ } hash = try Some (Hashtbl.find index hash) with Not_found -> None
    let mem { index; _ } hash = Hashtbl.mem index hash
    let fold f { index; _ } a = Hashtbl.fold (fun hash (crc, abs_off, _) a -> f hash (crc, abs_off) a) index a

    let size ~ztmp ~window:zwin { pack; _ } hash =
      RPDec.Ascendant.length_from_hash ~ztmp ~zwin pack hash >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    let read (type value) ~ztmp ~window:zwin ~(to_result:(RPDec.kind * Cstruct.t * int * RPDec.Ascendant.s) -> (value, error) result Lwt.t) ({ pack; _ } as t) hash
      : [ `Error of error | `Return of value ] Lwt.t =
      Log.debug (fun l ->
          l ~header:"read" "Try to extract object %a in PACK %a."
            Hash.pp hash Hash.pp t.hash_pack);

      let cache = { RPDec.Cache.find = (fun _ -> None)
                  ; promote = (fun _ _ -> ()) } in

      with_buffer t.buff @@ fun { htmp; rtmp; deliver; _ } ->
      RPDec.Ascendant.apply_from_hash ~ztmp ~zwin ~htmp ~cache rtmp pack hash >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)
      >>?= to_result >>= fun res -> deliver () >|= fun () -> match res with
      | Ok value -> `Return value
      | Error err -> ` Error err

    exception Leave of Hash.t

    module EPACK = struct
      module E = struct
        type state =
          { get : Hash.t -> Cstruct.t option Lwt.t
          ; pack: PEnc.t
          ; src : Cstruct.t option }
        type result =
          { tree: (Crc32.t * int64) PEnc.Map.t
          ; hash: Hash.t }
        type error = PEnc.error

        let option_get ~default = function Some a -> a | None -> default

        let rec eval dst state =
          match
            PEnc.eval (option_get ~default:empty_cstruct state.src) dst state.pack
          with
          | `End (pack, hash) ->
            Lwt.return (`End ({ state with pack; },
                              { tree = (PEnc.idx pack)
                              ; hash }))
          | `Error (pack, err) -> Lwt.return (`Error ({ state with pack; }, err))
          | `Flush pack        -> Lwt.return (`Flush { state with pack; })
          | `Await pack ->
            match state.src with
            | Some _ ->
              eval dst { state with pack = PEnc.finish pack ; src = None }
            | None   ->
              let hash = PEnc.expect pack in
              state.get hash >>= function
              | None -> Lwt.fail (Leave hash)
              | Some raw ->
                let state = {
                  state with pack = PEnc.refill 0 (Cstruct.len raw) pack;
                             src  = Some raw;
                } in
                eval dst state

        let flush off len ({ pack; _ } as state) =
          { state with pack = PEnc.flush off len pack }

        let used { pack; _ } = PEnc.used_out pack
      end
      include Helper.Encoder(E)(FS)
    end

    let random_string len =
      let gen () = match Random.int (26 + 26 + 10) with
        | n when n < 26 -> int_of_char 'a' + n
        | n when n < 26 + 26 -> int_of_char 'A' + n - 26
        | n -> int_of_char '0' + n - 26 - 26
      in
      let gen () = char_of_int (gen ()) in
      Bytes.create len |> fun raw ->
      for i = 0 to len - 1 do Bytes.set raw i (gen ()) done;
      Bytes.unsafe_to_string raw

    let store_pack_file ~fs fmt entries get =
      let ztmp = Cstruct.create 0x8000 in
      let file = fmt (random_string 10) in
      let state = PEnc.default ztmp entries in
      FS.Dir.temp fs >>= fun temp ->
      let path = Fpath.(temp / file) in
      let rawo = Cstruct.create 0x8000 in
      let state = { EPACK.E.get; src = None; pack = state } in
      Lwt.catch
        (fun () ->
           (* XXX(dinosaure): why is not atomic? *)
           EPACK.to_file fs ~atomic:false path rawo state >|= function
           | Ok { EPACK.E.tree; hash; } ->
             let index = Hashtbl.create (PEnc.Map.cardinal tree) in
             let paths = Hashtbl.create (PEnc.Map.cardinal tree) in
             List.iter (fun (entry, delta) ->
                 let (crc, abs_off) = PEnc.Map.find (PEnc.Entry.id entry) tree in
                 let path, needed = match delta.PEnc.Delta.delta with
                   | PEnc.Delta.Z ->
                     let length = Int64.to_int (PEnc.Entry.length entry) in
                     PInfo.Load length, length
                   | PEnc.Delta.S { src_length; _ } as delta ->
                     let rec go ~src_length:length k acc = function
                       | PEnc.Delta.S { length; src = { PEnc.Delta.delta }; hunks; src_length; _ } ->
                         let hunks = List.fold_left (fun a -> function Duff.Insert (_, l) -> a + l | _ -> a) 0 hunks in
                         go ~src_length (fun src -> PInfo.Patch { hunks; target = length; src }) (max acc length) delta
                       | PEnc.Delta.Z -> k (PInfo.Load (Int64.to_int length)), acc in
                     go ~src_length (fun x -> x) 0 delta in
                 Hashtbl.replace index hash (crc, abs_off, needed);
                 Hashtbl.replace paths abs_off path) entries;

             let rec merge abs_off path acc = match path, acc with
               | PInfo.Load a, PInfo.Load b -> PInfo.Load (max a b)
               | PInfo.Load x, PInfo.Patch { hunks; target; src; }
               | PInfo.Patch { hunks; target; src; }, Load x -> PInfo.Patch { hunks; target = max x target; src; }
               | PInfo.Patch { hunks = hunks_a
                       ; target = target_a
                       ; src = src_a },
                 Patch { hunks = hunks_b
                       ; target = target_b
                       ; src = src_b } ->
                 Patch { hunks = max hunks_a hunks_b; target = max target_a target_b; src = merge abs_off src_a src_b } in
             let path = Hashtbl.fold merge paths (Load 0) in

             Ok (Fpath.(temp / file)
                , (index, path)
                , hash)
           | Error #fs_error as err -> err
           | Error (`Encoder e) -> Error (`Pack_encoder e))
        (function
          | Leave hash -> Lwt.return (Error (`Invalid_hash hash))
          | exn        -> Lwt.fail exn (* XXX(dinosaure): should never happen. *))

    let filter_map f lst =
      List.fold_left (fun a v -> match f v with
          | Some v -> v :: a
          | None -> a) [] lst
      |> List.rev

    let third_pass ~root ~ztmp ~window:zwin ~read_inflated fs resolved =
      let deltas = filter_map (function PInfo.Delta { hunks_descr; _ } -> Some hunks_descr | _ -> None)
          (Hashtbl.fold (fun _ v a -> v :: a) resolved.Resolved.delta []) in
      let revidx = Hashtbl.create (Hashtbl.length resolved.Resolved.index) in
      Hashtbl.iter (fun hash (_, abs_off, _)-> Hashtbl.replace revidx abs_off hash) resolved.Resolved.index;

      let k2k = function
        | `Commit -> Pack.Kind.Commit
        | `Blob -> Pack.Kind.Blob
        | `Tree -> Pack.Kind.Tree
        | `Tag -> Pack.Kind.Tag in

      let make acc (_hash, (_, abs_off, _)) =
        with_buffer resolved.Resolved.buff @@ fun { htmp; base; rtmp; deliver; _ } ->

        let cache = { RPDec.Cache.find = (fun _ -> None)
                    ; promote = (fun _ _ -> ()) } in

        RPDec.Ascendant.get_from_absolute_offset ~ztmp ~zwin ~cache ~htmp base resolved.Resolved.pack abs_off >>= function
        | Error _ -> deliver () >|= fun () -> acc
        | Ok obj ->
          let delta = match obj with
            | RPDec.Ascendant.Root _ | RPDec.Ascendant.External _-> None
            | RPDec.Ascendant.Node { patch = { RPDec.Patch.descr = { HDec.reference = HDec.Hash hash; _  }; _ }; _ } ->
              Some (PEnc.Entry.From hash)
            | RPDec.Ascendant.Node { patch = { RPDec.Patch.descr = { HDec.reference = HDec.Offset abs_off; _  }; _ }; _ } ->
              try let hash = Hashtbl.find revidx abs_off in
                Some (PEnc.Entry.From hash)
              with Not_found -> None (* XXX(dinosaure): should not appear. *) in
          let (kind, raw, _depth, _metadata) = RPDec.Ascendant.reconstruct rtmp obj in
          let hash = Normalized.digest (kind, raw) in
          let kind = k2k kind in
          let length = Int64.of_int (Cstruct.len raw) in

          (* XXX(dinosaure): [hash] is already available. So we don't need to
             use [reconstruct]. However, we need to see if
             [get_from_absolute_offset] is faster than
             [get_from_absolute_offset] + [reconstruct] - specifically if this
             function is tail-rec or not.

             When we find a response about that, we can only use
             [get_from_absolute_offset]. *)

          deliver () >|= fun () -> PEnc.Entry.make hash ?delta kind length :: acc in
      let external_objects acc =
        let res = List.fold_left
            (fun acc hunks_descr ->
               match hunks_descr.HDec.reference with
               | HDec.Offset _ -> acc
               | HDec.Hash hash ->
                 if Hashtbl.mem resolved.Resolved.index hash
                 then acc
                 else
                   try ignore @@ List.find (Hash.equal hash) acc; acc
                   with Not_found -> hash :: acc)
            [] deltas in
        Lwt_list.fold_left_s
          (fun acc hash -> read_inflated hash >|= function
             | None -> acc
             | Some (kind, raw) -> PEnc.Entry.make hash (k2k kind) (Int64.of_int (Cstruct.len raw)) :: acc)
          acc res in
      let read_inflated hash =
        if Hashtbl.mem resolved.Resolved.index hash
        then
          with_buffer resolved.Resolved.buff @@ fun { htmp; base; deliver; _ } ->
          let length = Cstruct.len base in

          let rtmp = Cstruct.create (length * 2) in
          let rtmp = Cstruct.sub rtmp 0 length, Cstruct.sub rtmp length length in

          let cache = { RPDec.Cache.find = (fun _ -> None)
                      ; promote = (fun _ _ -> ()) } in

          RPDec.Ascendant.apply_from_hash ~ztmp ~zwin ~cache ~htmp rtmp resolved.Resolved.pack hash
          >>= fun obj -> deliver () >|= fun () -> match obj with
          | Error _ -> None
          | Ok (_, raw, _, _) -> (Some raw)
        else read_inflated hash >|= function
          | Some (_, raw) -> Some raw
          (* XXX(dinosaure): normalement, il devrait y avoir un [cstruct_copy]
             ici puisque [read_inflated] est externe à cette fonction - on y
             contrôle donc pas son allocation. Cependant, plus bas (dans [read])
             cette fonction est le [read_and_exclude] qui alloue bien les
             [Cstruct.t]. Donc dans ce cas, ce serait une allocation d'une
             allocation - ce qui est pas le plus top.

             Bref, attention si on change ce code. *)
          | None -> None in

      Hashtbl.fold (fun hash value acc -> (hash, value) :: acc) resolved.Resolved.index []
      |> Lwt_list.fold_left_s make []
      >>= external_objects
      >>= fun entries -> PEnc.Delta.deltas ~memory:false entries read_inflated (fun _ -> false) 10 50
      >>= function
      | Error err -> Lwt.return_error (`Delta err)
      | Ok entries ->
        store_pack_file ~fs (Fmt.strf "pack-%s.pack") entries read_inflated
        >>= function
        | Error _ as err -> Lwt.return err
        | Ok (path, (index, delta_path), hash_pack) ->
          let sequence f = Hashtbl.iter (fun hash (crc, abs_off, _) -> f (hash, (crc, abs_off))) index in
          Resolved.store_idx_file ~root fs sequence hash_pack >>= function
          | Error _ as err -> Lwt.return err
          | Ok () ->
            let file = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in
            let dst  = Fpath.(root / "objects" / "pack" / file) in
            FS.File.move fs path dst >|= Rresult.R.reword_error (Error.FS.err_move path dst)
            >>?= fun () -> FS.Mapper.close resolved.Resolved.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
            (* XXX(dinosaure): the temporary pack file from the
               [resolved] state is closed here. *)
            >>?= fun () -> Lwt.return_ok (dst, hash_pack, index, delta_path)

    let of_resolved
      (type mmu) (type location)
      ~root
      ~ztmp
      ~window
      ~read_inflated
      fs
      (r:(mmu, location) r)
      resolved =
      if resolved.Resolved.thin
      then third_pass ~root ~ztmp ~window ~read_inflated fs resolved >>= function
        | Error _ -> assert false
        | Ok (path, hash_pack, index, path_delta) ->

          let idx hash = match Hashtbl.find index hash with
            | (crc, abs_off, _) -> Some (crc, abs_off)
            | exception Not_found -> None in
          Loaded.pack_decoder ~read_and_exclude:(fun _ -> Lwt.return_none) ~idx fs path
          >>?= fun (fd, pack) ->

            let length_hunks = Normalized.length_of_path path_delta in
            let length_buffer = Hashtbl.fold (fun _ (_, _, v) -> max v) index 0 in
            let buff = Resolved.buffer r hash_pack length_hunks length_buffer path_delta in

            Lwt.return_ok { index; path_delta; hash_pack; pack; buff; fd; }
      else
        let extern _ = Lwt.return_none in

        Lwt.return_ok { pack       = RPDec.update_extern extern resolved.Resolved.pack
                      ; index      = resolved.Resolved.index
                      ; hash_pack  = resolved.Resolved.hash_pack
                      ; path_delta = resolved.Resolved.path_delta
                      ; fd         = resolved.Resolved.fd
                      ; buff       = resolved.Resolved.buff }
  end

  type state =
    | Exists     of Exists.t
    | Loaded     of Loaded.t
    | Resolved   of Resolved.t
    | Total      of Total.t

  type t = { packs : (Hash.t, state) Hashtbl.t }

  let pp_error ppf = function
    | `Pack_decoder err       -> RPDec.pp_error ppf err
    | `Pack_encoder err       -> PEnc.pp_error ppf err
    | `Pack_info err          -> PInfo.pp_error ppf err
    | `Idx_decoder err        -> IDec.pp_error ppf err
    | `Idx_encoder err        -> IEnc.pp_error ppf err
    | #inf_error as err       -> Error.Inf.pp_error Inflate.pp_error ppf err
    | #fs_error as err        -> Error.FS.pp_error FS.pp_error ppf err
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #Error.not_found as err -> Error.pp_not_found ppf err
    | `Delta err              -> PEnc.Delta.pp_error ppf err
    | `Invalid_hash hash ->
      Fmt.pf ppf "Unable to load %a, it does not exists in the store"
        Hash.pp hash

  (* XXX(dinosaure): this function could not be exposed. It used in a specific
     context, when a pack decoder requests an external object. This case appear
     only when the pack file is a /thin/-pack. So, we need to allocate some
     buffers to avoid a memory-explosion (if the object is delta-ified) and
     return a /fresh/ raw of the requested object. *)
  let strong_weight_read decoder _hash_pack hash =
    let ztmp = Cstruct.create 0x8000 in
    let zwin = Inflate.window () in

    let cache = { RPDec.Cache.find = (fun _ -> None)
                ; promote = (fun _ _ -> ()) } in

    (RPDec.Ascendant.needed_from_hash ~ztmp ~zwin ~cache decoder hash >>= function
      | Error _ -> Lwt.return_none
      | Ok needed ->
        let rtmp = Cstruct.create (needed * 2) in
        let rtmp = Cstruct.sub rtmp 0 needed, Cstruct.sub rtmp needed needed in

        let cache = { RPDec.Cache.find = (fun _ -> None)
                    ; promote = (fun _ _ -> ()) } in

        RPDec.Ascendant.apply_from_hash ~ztmp ~zwin ~cache rtmp decoder hash >>= function
        | Ok (kind, raw, _, _) -> Lwt.return_some (kind, raw)
        | Error _ -> Lwt.return_none)

  exception Catch of (RPDec.kind * Cstruct.t)

  (* XXX(dinosaure): [read_and_exclude] is called when a pack decoder wants to
     reconstruct an internal object and the source is external - by this way, we
     can consider the pack file as a /thin/ pack.

     So this is the biggest function which could allocate a lot. Obviously, in a
     real world, this function could not happen (Git takes care about pack file
     and stores only /non-thin/ pack files).

     However, from a [resolved]/[normalized]/[loaded]/[exists] information, we
     can not ensure than the pack file is not a /thin/-pack. So this function
     exists for the pack decoder when we want to retrieve an external object.

     Then, we exclude the pack itself to avoid a infinite recursion when the
     length of the redirection is 1. If it's upper, firstly, what? then, you
     could have an infinite loop. About this bug, we could change the API of the
     pack decoder and allow to inform a /close/ list to the function.

     NOTE: [exclude] is added for each call (we put the current hash of the PACK
     file), it's like a close list of already visited PACK files. This is to
     avoid an infinite recursion. It's common to have 2 times (or more) the same
     objet on multiple PACK files.

     So, a /thin/ PACK file A can expect an external object O which appear on a
     PACK file B which need an object M which could be appear inner A (and need
     O to be reconstructed). In other words, O needs itself to be reconstructed.
     This situation appear only if we did not exclude PACK file which we already
     try to get the O object.

     As I said, the object O can be appear 2 times (or more) in multiple PACK
     file. That means, object O can appear in an other PACK file. The goal is to
     get O from a different PACK file than A (may be a PACK file C) and this is
     the purpose of [exclude]. My brain is fuck up. *)
  let rec read_and_exclude ~root ~read_loose fs t exclude request =
    Lwt.catch
      (fun () -> Lwt_list.fold_left_s (fun acc (hash, pack) -> match acc with
           | Some _ -> assert false
           | None -> match pack with
             | Exists exists ->
               if Exists.mem exists request
               then
                 Loaded.of_exists ~root ~read_and_exclude:(read_and_exclude ~root ~read_loose fs t [ hash ]) fs exists >>= function
                 | Error err ->
                   Log.err (fun l -> l "Retrieve an error when we promote PACK %a to loaded state: %a."
                               Hash.pp exists.Exists.hash_pack pp_error err);
                   Lwt.return_none
                 | Ok loaded ->
                   Hashtbl.replace t.packs hash (Loaded loaded);
                   let exclude = hash :: exclude in
                   let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                   strong_weight_read (RPDec.update_extern read_and_exclude loaded.Loaded.pack) hash request >>= function
                   | Some value -> Lwt.fail (Catch value)
                   | None -> Lwt.return_none
               else Lwt.return_none
             | Loaded loaded ->
               if Loaded.mem loaded request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude loaded.Loaded.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none
             | Resolved resolved ->
               if Resolved.mem resolved request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude resolved.Resolved.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none
             | Total total ->
               if Total.mem total request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude total.Total.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none)
           None
           (Hashtbl.fold (fun hash pack acc ->
                if List.exists (Hash.equal hash) exclude
                then acc
                else (hash, pack) :: acc) t.packs []))
      (function
        | Catch (kind, raw) -> Lwt.return_some (kind, raw)
        | exn -> Lwt.fail exn)
    >>= function
    | None -> read_loose request
    | Some x -> Lwt.return_some x

  let v fs indexes =
    Lwt_list.map_p (Exists.v fs) indexes >|= fun exists ->
    let packs = Hashtbl.create 32 in
    List.iter (function Ok ({ Exists.hash_pack; _ } as exists) -> Hashtbl.replace packs hash_pack (Exists exists)
                      | Error err ->
                        Log.err (fun l -> l "Retrieve an error when we load IDX file: %a." pp_error err))
      exists;
    { packs }

  let add ~root ~read_loose ~ztmp ~window fs r t path_tmp info =
    let read_and_exclude = read_and_exclude ~root ~read_loose fs t [ info.PInfo.hash_pack ] in
    let read_inflated = read_and_exclude in

    Normalized.of_info ~read_and_exclude fs path_tmp info
    >>?= Resolved.of_normalized ~root ~read_and_exclude ~ztmp ~window fs r
    >>?= Total.of_resolved ~root ~ztmp ~window ~read_inflated fs r
    >>= function
    | Ok total ->
      Hashtbl.replace t.packs total.Total.hash_pack (Total total);
      Lwt.return_ok (total.Total.hash_pack, Hashtbl.length total.Total.index)
    | Error _ as err -> Lwt.return err

  exception Found of (Hash.t * (Crc32.t * int64))

  let lookup t hash =
    let jump0 hash = function
      | Some (crc, abs_off) -> raise (Found (hash, (crc, abs_off)))
      | None -> () in
    let jump1 hash = function
      | Some (crc, abs_off, _) -> raise (Found (hash, (crc, abs_off)))
      | None -> () in
    try
      Hashtbl.iter
        (fun hash_pack -> function
           | Exists exists -> Exists.lookup exists hash |> jump0 hash_pack
           | Loaded loaded -> Loaded.lookup loaded hash |> jump0 hash_pack
           | Resolved resolved -> Resolved.lookup resolved hash |> jump0 hash_pack
           | Total total -> Total.lookup total hash |> jump1 hash_pack)
        t.packs; None
    with Found v -> Some v

  let mem t hash =
    lookup t hash |> function
    | Some _ -> true
    | None   -> false

  let read ~root ~read_loose ~to_result ~ztmp ~window fs r t hash =
    let read_and_exclude hash_pack = read_and_exclude ~root ~read_loose fs t [ hash_pack ] in
    let read_inflated hash_pack = read_and_exclude hash_pack in

    let promote_loaded r (hash_pack, loaded) obj =
      Resolved.of_loaded r loaded >>= function
      | Ok resolved ->
        Log.debug (fun l -> l ~header:"promotion" "Promotion of %a from loaded to resolved." Hash.pp loaded.Loaded.info.PInfo.hash_pack);
        Hashtbl.replace t.packs hash_pack (Resolved resolved);
        Lwt.return_ok obj
      | Error _ as err -> Lwt.return err in

    let return_loaded r loaded = function
      | `Return ret -> Lwt.return_ok ret
      | `Error err -> Lwt.return_error err
      | `Promote obj -> promote_loaded r loaded obj in

    let promote_resolved fs r (hash_pack, resolved) obj =
      Total.of_resolved ~root ~ztmp ~window ~read_inflated:(read_inflated hash_pack) fs r resolved >>= function
      | Ok total ->
        Log.debug (fun l -> l ~header:"promotion" "Promotion of %a from resolved to total." Hash.pp resolved.Resolved.hash_pack);
        Hashtbl.replace t.packs hash_pack (Total total);
        Lwt.return_ok obj
      | Error _ as err -> Lwt.return err in

    let return_resolved fs r resolved = function
      | `Return ret -> Lwt.return_ok ret
      | `Error err -> Lwt.return_error err
      | `Promote obj -> promote_resolved fs r resolved obj in

    lookup t hash |> function
    | None -> Lwt.return_error `Not_found
    | Some (hash_pack, _) ->
      match Hashtbl.find t.packs hash_pack with
      | Loaded loaded ->
        Loaded.read ~to_result ~ztmp ~window r loaded hash
        >>= return_loaded r (hash_pack, loaded)
      | Exists exists ->
        (Loaded.of_exists ~root ~read_and_exclude:(read_and_exclude exists.Exists.hash_pack) fs exists >>= function
          | Error _ as err -> Lwt.return err
          | Ok loaded ->
            Hashtbl.replace t.packs hash_pack (Loaded loaded);
            Loaded.read ~to_result ~ztmp ~window r loaded hash
            >>= return_loaded r (hash_pack, loaded))
      | Resolved resolved ->
        Resolved.read ~ztmp ~window ~to_result resolved hash >>= return_resolved fs r (hash_pack, resolved)
      | Total total ->
        Total.read ~ztmp ~window ~to_result total hash >>= function
        | `Error err -> Lwt.return_error err
        | `Return obj -> Lwt.return_ok obj

  let size ~root ~read_loose ~ztmp ~window fs t hash =
    let read_and_exclude hash_pack = read_and_exclude ~root ~read_loose fs t [ hash_pack ] in

    lookup t hash |> function
    | None -> Lwt.return_error `Not_found
    | Some (hash_pack, _) ->
      match Hashtbl.find t.packs hash_pack with
      | Exists exists ->
        (Loaded.of_exists ~root ~read_and_exclude:(read_and_exclude exists.Exists.hash_pack) fs exists >>= function
          | Error _ as err -> Lwt.return err
          | Ok loaded ->
            Hashtbl.replace t.packs hash_pack (Loaded loaded);
            Loaded.size loaded ~ztmp ~window hash)
      | Loaded loaded -> Loaded.size loaded ~ztmp ~window hash
      | Resolved resolved -> Resolved.size resolved ~ztmp ~window hash
      | Total total -> Total.size total ~ztmp ~window hash

  let fold f t a =
    Hashtbl.fold (fun _ pack acc -> match pack with
        | Exists exists -> Exists.fold f exists acc
        | Loaded loaded -> Loaded.fold f loaded acc
        | Resolved resolved -> Resolved.fold f resolved acc
        | Total total -> Total.fold f total acc)
      t.packs a

  let list t = fold (fun hash _ acc ->
      if List.exists (Hash.equal hash) acc
      then acc else hash :: acc)
      t []
end
