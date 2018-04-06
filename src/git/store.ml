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
let ( >>!= ) x f = Lwt_result.map_err f x
let ( >>?= ) = Lwt_result.bind

let src = Logs.Src.create "git.store" ~doc:"logs git's store event"
module Log = (val Logs.src_log src : Logs.LOG)

(* XXX(samoht): duplicate of Loose.S? *)
module type LOOSE = sig

  type t
  type state
  type error

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  module Hash: S.HASH
  module FS : S.FS
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t

  val lookup: state -> Hash.t -> Hash.t option Lwt.t
  val mem: state -> Hash.t -> bool Lwt.t
  val list: state -> Hash.t list Lwt.t
  val read: state -> Hash.t -> (t, error) result Lwt.t
  val size: state -> Hash.t -> (int64, error) result Lwt.t
  val write: state -> t -> (Hash.t * int, error) result Lwt.t
  val write_inflated: state -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  val read_inflated: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  val read_inflated_wa: Cstruct.t -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  module D: S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = [ Error.Decoder.t | `Inflate of Inflate.error ]

  module E: S.ENCODER
    with type t = t
     and type init = int * t * int * Cstruct.t
     and type error = [ `Deflate of Deflate.error ]

end

(* XXX(samoht): duplicate of pack?.S ? *)
module type PACK = sig

  type t
  type value
  type state
  type error

  module Hash: S.HASH
  module FS: S.FS
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module HDec: Unpack.H with module Hash = Hash
  module PDec: Unpack.P
    with module Hash = Hash
     and module Inflate = Inflate
     and module Hunk := HDec
  module RPDec: Unpack.D
    with module Hash = Hash
     and module Inflate = Inflate
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

  val lookup: state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  val mem: state -> Hash.t -> bool Lwt.t
  val list: state -> Hash.t list Lwt.t
  val read: state -> Hash.t -> (t, error) result Lwt.t
  val size: state -> Hash.t -> (int, error) result Lwt.t

  type stream = unit -> Cstruct.t option Lwt.t

  module Graph : Map.S with type key = Hash.t

  val from: state -> stream -> (Hash.t * int, error) result Lwt.t
  val make: state
    -> ?window:[ `Object of int | `Memory of int ]
    -> ?depth:int
    -> value list
    -> (stream * (Crc32.t * int64) Graph.t Lwt_mvar.t, error) result Lwt.t
end

module type S = sig

  type t

  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS: S.FS

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t

  module Reference: Reference.IO
    with module Hash = Hash
     and module FS = FS

  module HDec: Unpack.H
    with module Hash = Hash

  module PDec: Unpack.P
    with module Hash = Hash
     and module Inflate = Inflate
     and module Hunk := HDec

  module RPDec: Unpack.D
    with module Hash = Hash
     and module Inflate = Inflate
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

  module Packed_refs: Packed_refs.S
    with module Hash = Hash
     and module FS = FS

  type error =
    [ `Delta             of PEnc.Delta.error
    | `Pack_decoder      of RPDec.error
    | `Pack_encoder      of PEnc.error
    | `Pack_info         of PInfo.error
    | `Idx_decoder       of IDec.error
    | `Idx_encoder       of IEnc.error
    | `Integrity         of string
    | `Invalid_hash      of Hash.t
    | `Invalid_reference of Reference.t
    | Error.Decoder.t
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Deflate.error Error.Def.t
    | Error.not_found ]

  val pp_error: error Fmt.t

  module Loose: LOOSE
    with type t = Value.t
     and type state = t
     and type error = error
     and module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module FS = FS

  module Pack: PACK
    with type t = RPDec.Object.t
     and type value = Value.t
     and type state = t
     and type error = error
     and module Hash = Hash
     and module FS = FS
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec
     and module RPDec := RPDec

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  type buffer

  val default_buffer: unit -> buffer
  val buffer:
    ?ztmp:Cstruct.t ->
    ?dtmp:Cstruct.t ->
    ?raw:Cstruct.t ->
    ?window:Inflate.window ->
    unit -> buffer
  val dotgit: t -> Fpath.t
  val root: t -> Fpath.t
  val compression : t -> int
  val mem: t -> Hash.t -> bool Lwt.t
  val list: t -> Hash.t list Lwt.t
  val read: t -> Hash.t -> (Value.t, error) result Lwt.t
  val read_exn: t -> Hash.t -> Value.t Lwt.t
  val write: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val size: t -> Hash.t -> (int64, error) result Lwt.t
  val read_inflated: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val write_inflated: t -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t

  val fold:
    t
    -> ('a -> ?name:Fpath.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t)
    -> path:Fpath.t
    -> 'a
    -> Hash.t
    -> 'a Lwt.t

  val iter: t -> (Hash.t -> Value.t -> unit Lwt.t) -> Hash.t -> unit Lwt.t

  module Ref: sig

    val mem: t -> Reference.t -> bool Lwt.t
    val graph: t -> (Hash.t Reference.Map.t, error) result Lwt.t
    val normalize: Hash.t Reference.Map.t -> Reference.head_contents -> (Hash.t, error) result Lwt.t
    val list: t -> (Reference.t * Hash.t) list Lwt.t
    val remove: t -> Reference.t -> (unit, error) result Lwt.t
    val read: t -> Reference.t -> (Reference.head_contents, error) result Lwt.t
    val resolve: t -> Reference.t -> (Hash.t, error) result Lwt.t
    val write: t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
  end

  val clear_caches: t -> unit Lwt.t
  val reset: t -> (unit, error) result Lwt.t

  val has_global_watches: bool
  val has_global_checkout: bool
end

module Option = struct
  let get ~default = function Some x -> x | None -> default
  let map f a = match a with Some a -> Some (f a) | None -> None
end

module Make (H: S.HASH) (FS: S.FS) (I: S.INFLATE) (D: S.DEFLATE) = struct

  module Hash = H
  module Inflate = I
  module Deflate = D
  module FS = FS

  let has_global_watches = FS.has_global_watches
  let has_global_checkout = FS.has_global_checkout

  module LooseImpl = Loose.Make(Hash)(FS)(Inflate)(Deflate)

  module HDec = Unpack.Hunk(Hash)
  module PDec = Unpack.Pack(Hash)(Inflate)(HDec)
  module RPDec = Unpack.Decoder(Hash)(struct
      type t = FS.t
      type error = FS.error
      include FS.Mapper
    end)(Inflate)(HDec)(PDec)

  module PackImpl
    : Pack_engine.S
      with module Hash = Hash
       and module FS = FS
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module HDec := HDec
       and module PDec := PDec
       and module RPDec := RPDec
    = Pack_engine.Make(H)(FS)(I)(D)(HDec)(PDec)(RPDec)

  module Value
    : Value.S
      with module Hash = H
       and module Inflate = I
       and module Deflate = D
       and module Blob = Blob.Make(Hash)
       and module Commit = Commit.Make(Hash)
       and module Tree = Tree.Make(Hash)
       and module Tag = Tag.Make(Hash)
       and type t = Value.Make(Hash)(Inflate)(Deflate).t
    = Value.Make(Hash)(Inflate)(Deflate)

  module PEnc = PackImpl.PEnc
  module IDec = PackImpl.IDec
  module IEnc = PackImpl.IEnc
  module PInfo = PackImpl.PInfo
  module Packed_refs = Packed_refs.Make(Hash)(FS)
  module Reference = Reference.IO(H)(FS)

  module DoubleHash = struct
    type t = Hash.t * Hash.t
    let hash = Hashtbl.hash
    let equal (a, b) (c, d) = Hash.equal a c && Hash.equal b d
  end

  module HashInt64 = struct
    type t = Hash.t * int64
    let hash = Hashtbl.hash
    let equal (a, b) (c, d) = Hash.equal a c && Int64.equal b d
  end

  (* XXX(dinosaure): need to limit the weight of [CacheObject] and
     [CacheValue] by the memory consumption of the data stored - and
     not by the number of theses data. Indeed, 5 commits is more
     cheaper than 1 blob sometimes. *)
  module CacheObject =
    Lru.M.Make
      (DoubleHash)
      (struct type t = RPDec.Object.t let weight _ = 1 end)

  module CacheValue =
    Lru.M.Make
      (Hash)
      (struct type t = Value.t let weight _ = 1 end)

  module CachePack =
    Lru.M.Make
      (Hash)
      (struct type t = PDec.t let weight _ = 1 end) (* fixed size *)

  module CacheIndex =
    Lru.M.Make
      (Hash)
      (* not fixed size by consider than it's ok. *)
      (struct type t = IDec.t let weight _ = 1 end)

  module CacheRevIndex =
    Lru.M.Make
      (HashInt64)
      (struct type t = Hash.t let weight _ = 1 end) (* fixed size *)

  type buffer =
    { window: Inflate.window
    ; raw   : Cstruct.t
    ; ztmp  : Cstruct.t
    ; dtmp  : Cstruct.t }

  type cache =
    { objects   : CacheObject.t
    ; values    : CacheValue.t
    ; packs     : CachePack.t
    ; indexes   : CacheIndex.t
    ; revindexes: CacheRevIndex.t }

  type t =
    { fs          : FS.t
    ; dotgit      : Fpath.t
    ; root        : Fpath.t
    ; compression : int
    ; cache       : cache
    ; buffer      : (buffer -> unit Lwt.t) -> unit Lwt.t
    ; packed      : (Reference.t, Hash.t) Hashtbl.t
    ; engine      : PackImpl.t }

  type error =
    [ PackImpl.error
    | `Delta of PEnc.Delta.error
    | Error.Decoder.t
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Deflate.error Error.Def.t
    | `Invalid_reference of Reference.t ]

  let pp_error ppf = function
    | #LooseImpl.error as err -> Fmt.pf ppf "%a" LooseImpl.pp_error err
    | #PackImpl.error as err -> Fmt.pf ppf "%a" PackImpl.pp_error err
    | `Delta err -> Fmt.pf ppf "(`Delta %a)" PEnc.Delta.pp_error err
    | _ -> assert false

  let lift_error err = (err :> error)

  let new_buffer = function
    | Some b -> b
    | None   -> Cstruct.create (4 * 1024)

  let new_window = function
    | Some w -> w
    | None   -> Inflate.window ()

  let buffer ?ztmp ?dtmp ?raw ?window () =
    let window = new_window window in
    let ztmp = new_buffer ztmp in
    let dtmp = new_buffer dtmp in
    let raw = new_buffer raw in
    { window; ztmp; dtmp; raw }

  let with_buffer t f =
    let c = ref None in
    t.buffer (fun buf ->
        f buf >|= fun x ->
        c := Some x
      ) >|= fun () ->
    match !c with
    | Some x -> x
    | None   -> assert false

  module Loose = struct

    module Hash = Hash
    module FS = FS
    module Inflate = Inflate
    module Deflate = Deflate
    module Value = Value

    type state = t
    type t = LooseImpl.t
    type nonrec error = error
    type kind = LooseImpl.kind

    let read t path =
      with_buffer t @@ fun { ztmp; dtmp; raw; window } ->
      LooseImpl.read ~fs:t.fs ~root:t.dotgit ~window ~ztmp ~dtmp ~raw path
      >>!= lift_error

    let size t path =
      with_buffer t @@ fun { ztmp; dtmp; raw; window } ->
      LooseImpl.size ~fs:t.fs ~root:t.dotgit ~window ~ztmp ~dtmp ~raw path
      >>!= lift_error

    let write t value =
      with_buffer t @@ fun { ztmp; raw; _ } ->
      LooseImpl.write ~fs:t.fs ~root:t.dotgit ~ztmp ~raw ~level:t.compression value
      >>!= lift_error

    let mem t = LooseImpl.mem ~fs:t.fs ~root:t.dotgit
    let list t = LooseImpl.list ~fs:t.fs ~root:t.dotgit

    let lookup t hash =
      LooseImpl.mem ~fs:t.fs ~root:t.dotgit hash >|= function
      | true  -> Some hash
      | false -> None

    let read_inflated t hash =
      with_buffer t @@ fun { ztmp; dtmp; raw; window } ->
      LooseImpl.inflate ~fs:t.fs ~root:t.dotgit ~window ~ztmp ~dtmp ~raw hash
      >>!= lift_error

    let read_inflated_wa result t hash =
      with_buffer t @@ fun { ztmp; dtmp; raw; window } ->
      LooseImpl.inflate_wa ~fs:t.fs ~root:t.dotgit ~window ~ztmp ~dtmp ~raw
        ~result hash
      >>!= lift_error

    let write_inflated t ~kind value =
      with_buffer t @@ fun { raw; _ } ->
      LooseImpl.write_inflated
        ~fs:t.fs ~root:t.dotgit ~level:t.compression ~raw ~kind value
      >>= function
      | Ok hash -> Lwt.return hash
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" LooseImpl.pp_error e

    module D = Value.D
    module E = Value.E
  end

  module Pack = struct

    module Hash = Hash
    module FS = Helper.FS(FS)
    module Inflate = Inflate
    module Deflate = Deflate

    module HDec = HDec
    module PDec = PDec
    module PInfo = PackImpl.PInfo
    module IEnc = PackImpl.IEnc
    module IDec = PackImpl.IDec
    module PEnc = PackImpl.PEnc
    module RPDec = RPDec

    type state = t

    type t = RPDec.Object.t
    type value = Value.t
    type nonrec error = error

    let lookup t hash =
      PackImpl.lookup t.engine hash

    let mem t hash =
      PackImpl.mem t.engine hash

    let list t =
      PackImpl.list t.engine

    let read t hash =
      mem t hash >>= function
      | false -> Lwt.return (Error `Not_found)
      | true ->
        with_buffer t @@ fun { ztmp; window; _ } ->
        Log.debug (fun l -> l "Git object %a found in a PACK file." Hash.pp hash);
        PackImpl.read ~root:t.dotgit ~ztmp ~window t.engine hash
        >>!= lift_error

    let size t hash =
      mem t hash >>= function
      | false -> Lwt.return (Error `Not_found)
      | true ->
        with_buffer t @@ fun { ztmp; window; _ } ->
        PackImpl.size ~root:t.dotgit ~ztmp ~window t.engine hash >>!= lift_error

    type stream = unit -> Cstruct.t option Lwt.t

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

    let to_temp_file fs fmt stream =
      let filename_of_pack = fmt (random_string 10) in
      FS.Dir.temp fs >>= fun temp_dir ->
      let path = Fpath.(temp_dir / filename_of_pack) in
      FS.with_open_w fs path @@ fun fd ->
      Log.debug (fun l -> l "Saving the pack stream to %a" Fpath.pp path);
      let rec go ?chunk ~call () =
        Lwt_stream.peek stream >>= function
        | None ->
          Log.debug (fun l ->l "Pack stream saved to %a" Fpath.pp path);
          Lwt.return (Ok path)
        | Some raw ->
          Log.debug (fun l -> l "Chunk received (length=%d)" (Cstruct.len raw));
          let off, len = match chunk with
            | Some (off, len) -> off, len
            | None            -> 0, Cstruct.len raw
          in
          FS.File.write raw ~off ~len fd >>= function
          | Error err -> Lwt.return Error.(v @@ FS.err_write path err)
          | Ok 0 when len <> 0 ->
            if call <> 50 (* XXX(dinosaure): as argument? *)
            then go ?chunk ~call:(call + 1) ()
            else Lwt.return Error.(v @@ FS.err_stack path)
          | Ok n when n = len ->
            Log.debug (fun l -> l "Consuming a chunk of the pack stream.");
            Lwt_stream.junk stream >>= fun () ->
            go ~call:0 ()
          | Ok n -> go ~chunk:(off + n, len - n) ~call:0 ()
      in
      go ~call:0 ()

    let extern git hash =
      read git hash >>= function
      | Ok o ->
        Lwt.return (Some (o.RPDec.Object.kind, o.RPDec.Object.raw))
      | Error _ -> Loose.lookup git hash >>= function
        | None -> Lwt.return None
        | Some _ ->
          Loose.read_inflated git hash >|= function
          | Error #Loose.error -> None
          | Ok v               -> Some v

    module GC =
      Collector.Make(struct
        module Hash = Hash
        module Value = Value
        module Deflate = Deflate
        module PEnc = PEnc

        type nonrec t = state
        type nonrec error = error
        type kind = RPDec.kind

        let pp_error = pp_error
        let read_inflated = extern
        let contents _ = assert false
      end)

    module Graph = GC.Graph

    let make = GC.make_stream

    let cstruct_copy cs =
      let ln = Cstruct.len cs in
      let rs = Cstruct.create ln in
      Cstruct.blit cs 0 rs 0 ln;
      rs

    let canonicalize git path_pack decoder_pack fdp ~htmp ~rtmp delta info =
      let k2k = function
        | `Commit -> Pack.Kind.Commit
        | `Blob -> Pack.Kind.Blob
        | `Tree -> Pack.Kind.Tree
        | `Tag -> Pack.Kind.Tag
      in
      let make acc (hash, (_, offset)) =
        with_buffer git (fun { ztmp; window; _ } ->
            RPDec.get_from_offset
              ~htmp:htmp decoder_pack offset rtmp ztmp window
          ) >|= function
        | Error err ->
          Log.err (fun l ->
              l ~header:"from" "Retrieve an error when we try to \
                                resolve the object at the offset %Ld \
                                in the temporary pack file %a: %a."
                offset Fpath.pp path_pack RPDec.pp_error err);
          acc
        | Ok obj ->
          let open RPDec.Object in
          let delta = match obj.from with
            | External hash        -> Some (PEnc.Entry.From hash)
            | Direct _             -> None
            | Offset { offset; _ } ->
              try
                let (_, hash) =
                  PInfo.Graph.find offset info.PInfo.graph
                in
                Option.map (fun hash -> PEnc.Entry.From hash) hash
              with Not_found ->
                None
          in

          PEnc.Entry.make hash ?delta
            (k2k obj.RPDec.Object.kind) obj.RPDec.Object.length
          :: acc
      in

      let external_ressources acc =
        let res = List.fold_left
          (fun acc (_, hunks_descr) ->
             let open PInfo in

             match hunks_descr.HDec.reference with
             | HDec.Hash hash when not (Map.mem hash info.tree) ->
               (try List.find (Hash.equal hash) acc |> fun _ -> acc
                with Not_found -> hash :: acc)
             | _ -> acc)
          [] delta
        in
        Lwt_list.fold_left_s
          (fun acc hash -> extern git hash >|= function
             | None -> acc
             | Some (kind, raw) ->
               let entry = PEnc.Entry.make hash (k2k kind) (Int64.of_int (Cstruct.len raw)) in
               entry :: acc)
          acc res
      in

      let get hash =
        if PInfo.Map.mem hash info.PInfo.tree
        then
          with_buffer git @@ fun { ztmp; window; _ } ->
          RPDec.get_with_result_allocation_from_hash
            ~htmp:htmp
            decoder_pack
            hash
            ztmp window >|= function
          | Error _ -> None
          | Ok obj -> (Some obj.RPDec.Object.raw)
        else extern git hash >|= function
          | Some (_, raw) -> Some (cstruct_copy raw)
          | None          -> None
      in

      let tag _ = false in

      PInfo.Map.bindings info.PInfo.tree
      |> Lwt_list.fold_left_s make []
      >>= external_ressources
      >>= fun entries -> PEnc.Delta.deltas ~memory:false entries get tag 10 50
      >>= function
      | Error err -> Lwt.return (Error (`Delta err))
      | Ok entries ->
        PackImpl.save_pack_file ~fs:git.fs
          (Fmt.strf "pack-%s.pack")
          entries
          (fun hash ->
             if PInfo.Map.mem hash info.PInfo.tree
             then
               with_buffer git @@ fun { ztmp; window; _ } ->
               RPDec.get_with_result_allocation_from_hash
                 ~htmp:htmp
                 decoder_pack
                 hash
                 ztmp window
               >|= function
               | Error _ -> None
               | Ok obj -> (Some (obj.RPDec.Object.raw))
             else extern git hash >|= function
               | Some (_, raw) -> Some raw
               | None -> None)
        >>!= lift_error
        >>= function
        | Error _ as err -> Lwt.return err
        | Ok (path, sequence, hash_pack) ->
          (PackImpl.save_idx_file
             ~fs:git.fs ~root:git.dotgit sequence hash_pack >>!= lift_error)
          >>= function
          | Error _ as err -> Lwt.return err
          | Ok () ->
            let filename_pack = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in
            let dst = Fpath.(git.dotgit / "objects" / "pack" / filename_pack) in
            (FS.File.move git.fs path dst >|= function
              | Error err -> Error.(v @@ FS.err_move path dst err)
              | Ok ()     -> Ok (hash_pack, List.length entries))
            >>= fun ret ->
            FS.Mapper.close fdp >|= function
            | Error sys_err ->
              Log.err (fun l ->
                  l "Impossible to close the pack file %a, ignoring: %a."
                          Fpath.pp path_pack FS.Mapper.pp_error sys_err);
              ret
            | Ok () -> ret

    let from git stream =
      let stream0, stream1 =
        let stream', push' = Lwt_stream.create () in

        Lwt_stream.from
          (fun () -> stream () >>= function
             | Some raw ->
               Log.debug (fun l ->
                   l "Dispatch a chunk of the PACK stream (length: %d)."
                     (Cstruct.len raw));
               push' (Some (cstruct_copy raw));
               Lwt.return (Some raw)
             | None ->
               Log.debug (fun l ->
                   l "Dispatch end of the PACK stream.");
               push' None;
               Lwt.return None),
        stream'
      in

      let info = PInfo.v (Hash.of_hex (String.make (Hash.Digest.length * 2) '0')) in

      (with_buffer git @@ fun { ztmp; window; _ } ->
       PInfo.from_stream ~ztmp ~window info (fun () -> Lwt_stream.get stream0)
       >>!= (fun sys_err -> `Pack_info sys_err))
      >>?= fun info ->
      to_temp_file git.fs (Fmt.strf "pack-%s.pack") stream1 >>?= fun path ->

      let module Graph = PInfo.Graph in

      FS.Mapper.openfile git.fs path >>= function
      | Error err ->
        Lwt.return Error.(v @@ FS.err_open path err)
      | Ok fdp ->
        let `Partial { PInfo.Partial.hash = hash_pack;
                       PInfo.Partial.delta; } = info.PInfo.state
        in

        let htmp =
          let raw = Cstruct.create (info.PInfo.max_length_insert_hunks * (info.PInfo.max_depth + 1)) in
          Array.init
            (info.PInfo.max_depth + 1)
            (fun i -> Cstruct.sub raw (i * info.PInfo.max_length_insert_hunks) info.PInfo.max_length_insert_hunks)
        in

        let rtmp =
          Cstruct.create info.PInfo.max_length_object,
          Cstruct.create info.PInfo.max_length_object,
          info.PInfo.max_length_object
        in

        RPDec.make fdp
          (fun _ -> None)
          (fun hash ->
            try Some (PInfo.Map.find hash info.PInfo.tree)
            with Not_found -> None)
          (* XXX(dinosaure): this function will be updated. *)
          (fun _ -> None)
          (fun hash -> extern git hash)
        >>= function
        | Error err ->
          Lwt.return Error.(v @@ FS.err_length path err)
        | Ok decoder ->
          let hash_of_object obj =
            let ctx = Hash.Digest.init () in
            let hdr = Fmt.strf "%s %Ld\000"
                (match obj.RPDec.Object.kind with
                 | `Commit -> "commit"
                 | `Blob   -> "blob"
                 | `Tree   -> "tree"
                 | `Tag    -> "tag")
                obj.RPDec.Object.length
            in

            Hash.Digest.feed ctx (Cstruct.of_string hdr);
            Hash.Digest.feed ctx obj.RPDec.Object.raw;
            Hash.Digest.get ctx
          in

          let crc obj = match obj.RPDec.Object.from with
            | RPDec.Object.Offset { crc; _ } -> crc
            | RPDec.Object.External _ ->
              raise (Invalid_argument "Try to get the CRC-32 checksum from an external ressource.")
            | RPDec.Object.Direct { crc; _ } -> crc
          in

          Lwt_list.fold_left_s
            (fun ((decoder, tree, graph) as acc) (offset, hunks_descr) ->
               with_buffer git (fun { ztmp; window; _ } ->
               RPDec.get_from_offset
                 ~htmp:htmp
                 decoder
                 offset
                 rtmp ztmp window) >>= function
               | Ok obj ->
                 let hash = hash_of_object obj in
                 let crc = crc obj in
                 let tree = PInfo.Map.add hash (crc, offset) tree in

                 let graph =
                   let open PInfo in

                   let depth_source, _ = match hunks_descr.HDec.reference with
                     | HDec.Offset rel_off ->
                       (try Graph.find Int64.(sub offset rel_off) graph
                        with Not_found -> 0, None)
                     | HDec.Hash hash_source ->
                       try let _, abs_off = Map.find hash_source tree in
                           Graph.find abs_off graph
                       with Not_found -> 0, None
                   in

                   Graph.add offset (depth_source + 1, Some hash) graph
                 in

                 Lwt.return
                   (RPDec.update_idx (fun key ->
                        try Some (PInfo.Map.find key tree)
                        with Not_found -> None) decoder,
                    tree, graph)
               | Error err ->
                 Log.err (fun l -> l ~header:"from" "Retrieve an error when we try to \
                                                     resolve the object at the offset %Ld \
                                                     in the temporary pack file %a: %a."
                             offset Fpath.pp path RPDec.pp_error err);
                 Lwt.return acc)
            (decoder, info.PInfo.tree, info.PInfo.graph) delta
          >>= fun (decoder, tree', graph') ->

          let is_total =
            PInfo.Graph.for_all
              (fun _ -> function (_, Some _) -> true | (_, None) -> false)
              graph'
          in

          if is_total
          then
            Lwt_list.for_all_p
              (fun (_, hunks_descr) ->
                 let open PInfo in

                 match hunks_descr.HDec.reference with
                 | HDec.Offset _ -> Lwt.return true
                 | HDec.Hash hash ->
                   Lwt.return (Map.mem hash tree'))
              delta
            >>= fun is_not_thin ->
            if is_not_thin
            then begin
              let info =
                { info with PInfo.tree = tree'
                          ; PInfo.graph = graph'
                          ; PInfo.state =
                              `Full { PInfo.Full.thin = not is_not_thin
                                    ; PInfo.Full.hash = hash_pack } }
              in

              (FS.Mapper.close fdp >>!= Error.FS.err_close path)
              >>?= fun () ->
              (PackImpl.add_total ~root:git.dotgit git.engine path info >>!= lift_error)
            end else
              canonicalize git path decoder fdp ~htmp ~rtmp delta info
              >>?= fun (hash, count) ->
              (PackImpl.add_exists ~root:git.dotgit git.engine hash >>!= lift_error)
              >>?= fun () -> Lwt.return (Ok (hash, count))
          else
            Fmt.kstrf (fun x ->  Lwt.return (Error (`Integrity x)))
              "Impossible to get all informations from the file: %a."
              Hash.pp hash_pack
  end

  type kind =
    [ `Commit
    | `Blob
    | `Tree
    | `Tag ]

  let read state hash =
    Log.debug (fun l -> l "read %a" Hash.pp hash);
    Pack.read state hash >>= function
    | Ok o ->
      (match o.RPDec.Object.kind with
       | `Commit ->
         Value.Commit.D.to_result o.RPDec.Object.raw
         |> Rresult.R.map (fun v -> Value.Commit v)
         |> Rresult.R.reword_error (fun err -> (err :> error))
       | `Tree ->
         Value.Tree.D.to_result o.RPDec.Object.raw
         |> Rresult.R.map (fun v -> Value.Tree v)
         |> Rresult.R.reword_error (fun err -> (err :> error))
       | `Tag ->
         Value.Tag.D.to_result o.RPDec.Object.raw
         |> Rresult.R.map (fun v -> Value.Tag v)
         |> Rresult.R.reword_error (fun err -> (err :> error))
       | `Blob ->
         Value.Blob.D.to_result o.RPDec.Object.raw
         |> Rresult.R.map (fun v -> Value.Blob v)
         |> fun blob -> Rresult.R.(ok (get_ok blob)))
      |> Lwt.return
    | Error err -> Loose.lookup state hash >>= function
      | None -> Lwt.return (Error err)
      | Some _ -> Loose.read state hash >|= function
        | Error e -> Error (e :> error)
        | Ok v    -> Ok v

  let read_exn t hash =
    read t hash >>= function
    | Ok v    -> Lwt.return v
    | Error _ ->
      let err = Fmt.strf "Git.Store.read_exn: %a not found" Hash.pp hash in
      Lwt.fail_invalid_arg err

  let write state hash =
    Loose.write state hash >|= function
    | Error e -> Error (e :> error)
    | Ok v    -> Ok v

  let read_inflated state hash =
    Pack.read state hash >>= function
    | Ok o -> Lwt.return (Some (o.RPDec.Object.kind, o.RPDec.Object.raw))
    | Error _ -> Loose.lookup state hash >>= function
      | None -> Lwt.return None
      | Some _ ->
        Loose.read_inflated state hash >|= function
        | Error _ -> None
        | Ok v    -> Some v

  let write_inflated t ~kind value =
    Loose.write_inflated t ~kind value

  let indexes ~fs git =
    FS.Dir.contents fs ~rel:false Fpath.(git / "objects" / "pack") >>= function
    | Error err -> Lwt.return Error.(v @@ FS.err_sys_dir err)
    | Ok lst    ->
      let idx =
        List.fold_left (fun acc path ->
            if Fpath.has_ext "idx" path then path :: acc else acc
          ) [] lst
      in
      PackImpl.v fs idx >|= fun v ->
      Ok v

  let lookup state hash =
    Pack.lookup state hash >>= function
    | Some (hash_pack, (_, offset)) ->
      Lwt.return (`Pack_decoder (hash_pack, offset))
    | None ->
      Loose.lookup state hash >|= function
      | Some _ -> `Loose
      | None   -> `Not_found

  let mem state hash =
    lookup state hash >|= function
    | `Not_found -> false
    | _ -> true

  let list state =
    Loose.list state
    >>= fun looses -> Pack.list state
    >|= fun packed -> List.append looses packed

  let size state hash =
    Pack.size state hash >>= function
    | Ok v -> Lwt.return (Ok (Int64.of_int v))
    | Error err ->
      Loose.mem state hash >>= function
      | false -> Lwt.return (Error (err :> error))
      | true ->
        Loose.size state hash >|=
        Rresult.R.reword_error (fun x -> (x :> error))

  exception Leave of error

  let contents state =
    list state
    >>= fun lst ->
    Lwt.try_bind (fun () ->
        Lwt_list.map_s (fun hash ->
            Log.debug (fun l ->
                l ~header:"contents" "Try to read the object: %a." Hash.pp hash);
            read state hash
            >>= function
            | Ok v -> Lwt.return (hash, v)
            | Error err ->
              Log.err (fun l ->
                  l ~header:"contents" "Retrieve an error: %a." pp_error err);
              Lwt.fail (Leave err))
          lst)
      (fun lst -> Lwt.return (Ok lst))
      (function
        | Leave err -> Lwt.return (Error err)
        | exn       -> Lwt.fail exn)

  module T =
    Traverse_bfs.Make(struct
      module Hash = Hash
      module Value = Value

      type nonrec t = t
      type nonrec error = error

      let pp_error = pp_error
      let read = read
    end)

  let fold = T.fold
  let iter = T.iter

  module Ref = struct

    let contents ~fs top =
      let rec lookup acc dir =
        FS.Dir.contents fs ~rel:true Fpath.(top // dir) >>?= fun l ->
        Lwt_list.filter_p (fun x ->
            FS.is_dir fs Fpath.(top // dir // x) >|= function
            | Ok v    -> v
            | Error _ -> false
          ) l
        >>= fun dirs ->
        Lwt_list.filter_p (fun x ->
            FS.is_file fs Fpath.(top // dir // x) >|= function
            | Ok v    -> v
            | Error _ -> false
          ) l
        >>= fun files ->
        let files = List.map (fun file -> Fpath.append dir file) files in
        Lwt_list.fold_left_s (function
            | Ok acc      -> fun x -> lookup acc Fpath.(dir // x)
            | Error _ as e -> fun _ -> Lwt.return e
          ) (Ok acc) dirs >>?= fun acc -> Lwt.return (Ok (acc @ files))
      in
      lookup [] (Fpath.v ".")

    module Graph = Reference.Map

    (* XXX(dinosaure): this function does not return any {!Error} value. *)
    let graph t =
      Log.debug (fun l -> l "graph_p");
      contents ~fs:t.fs Fpath.(t.dotgit / "refs") >>= function
      | Error err -> Lwt.return Error.(v @@ FS.err_sys_dir err)
      | Ok files  ->
        Log.debug (fun l ->
            let pp_files = Fmt.hvbox (Fmt.list Fpath.pp) in
            l "contents files: %a." pp_files files);
        Lwt_list.fold_left_s (fun acc abs_ref ->
            (* XXX(dinosaure): we already normalize the reference
               (which is absolute). so we consider than the root as
               [/]. *)
            let r = Reference.of_path abs_ref in
            with_buffer t @@ fun { dtmp; raw; _ } ->
            Reference.read ~fs:t.fs ~root:t.dotgit r ~dtmp ~raw >|= function
            | Ok v -> v :: acc
            | Error err ->
              Log.err (fun l ->
                  l "Error while reading %a: %a."
                    Reference.pp r Reference.pp_error err);
              acc)
          [] (Reference.(to_path head) :: files)
        >>= fun lst ->
        let partial, graph =
          List.fold_left (fun (rest, graph) -> function
              | r, Reference.Hash hash -> (rest, Graph.add r hash graph)
              | r, Reference.Ref link ->
                Log.debug (fun l ->
                    l "adding ref %a -> %a as a partial value."
                      Reference.pp r Reference.pp link);
                (r, link) :: rest, graph
            ) ([], Graph.empty) lst
        in
        (with_buffer t (fun { dtmp; raw; _ } ->
             Packed_refs.read ~fs:t.fs ~root:t.dotgit ~dtmp ~raw
           ) >>= function
         | Error _ as err -> Lwt.return err
         | Ok packed_refs ->
           Hashtbl.reset t.packed;
           List.iter (function
               | `Peeled _            -> ()
               | `Ref (refname, hash) ->
                 Hashtbl.add t.packed (Reference.of_string refname) hash
             ) packed_refs;
           Lwt.return (Ok packed_refs)
        ) >|= function
        | Ok packed_refs ->
          let graph =
            List.fold_left (fun graph -> function
                | `Peeled _      -> graph
                | `Ref (r, hash) -> Graph.add (Reference.of_string r) hash graph
              ) graph packed_refs
          in
          let graph =
            List.fold_left (fun graph (refname, link) ->
                Log.debug (fun l ->
                    l "Resolving the reference %a -> %a."
                      Reference.pp refname Reference.pp link);
                try
                  let hash = Graph.find link graph in
                  Graph.add refname hash graph
                with Not_found -> graph
              ) graph partial
          in
          Ok graph
        | Error #Packed_refs.error ->
          let graph =
            List.fold_left (fun graph (refname, link) ->
                Log.debug (fun l ->
                    l "Resolving the reference %a -> %a."
                      Reference.pp refname Reference.pp link);
                try
                  let hash = Graph.find link graph in
                  Graph.add refname hash graph
                with Not_found -> graph
              ) graph partial
          in
          Ok graph

    let normalize graph = function
      | Reference.Hash hash   -> Lwt.return (Ok hash)
      | Reference.Ref refname ->
        try Lwt.return (Ok (Graph.find refname graph))
        with Not_found -> Lwt.return (Error (`Invalid_reference refname))

    let list t =
      graph t >|= function
      | Ok graph ->
        Graph.fold (fun refname hash acc -> (refname, hash) :: acc) graph []
        |> List.stable_sort (fun (a, _) (b, _) -> Reference.compare a b)
      | Error _ -> []

    let mem t reference =
      let in_packed_refs () = Hashtbl.mem t.packed reference in
      Reference.mem ~fs:t.fs ~root:t.dotgit reference >|= function
      | true  -> true
      | false -> in_packed_refs ()

    let remove t reference =
      (with_buffer t (fun { dtmp; raw; _ } ->
           Packed_refs.read ~fs:t.fs ~root:t.dotgit ~dtmp ~raw
         ) >>= function
       | Error _ -> Lwt.return None
       | Ok packed_refs ->
         Lwt_list.exists_p
           (function
             | `Peeled _ -> Lwt.return false
             | `Ref (refname, _) ->
               Lwt.return Reference.(equal (of_string refname) reference))
           packed_refs
         >>= function
         | false -> Lwt.return None
         | true ->
           Lwt_list.fold_left_s
             (fun acc -> function
                | `Peeled hash -> Lwt.return (`Peeled hash :: acc)
                | `Ref (refname, hash) when not Reference.(equal (of_string refname) reference) ->
                  Lwt.return (`Ref (refname, hash) :: acc)
                | _ -> Lwt.return acc)
             [] packed_refs
           >|= fun packed_refs' -> Some packed_refs')
      >>= (function
          | None -> Lwt.return (Ok ())
          | Some packed_refs' ->
            with_buffer t (fun { raw; _ } ->
                Packed_refs.write ~fs:t.fs ~root:t.dotgit ~raw packed_refs'
              ) >|= function
            | Ok ()   -> Ok ()
            | Error e -> Error (e :> error))
      >>= function
      | Error _ as err -> Lwt.return err
      | Ok () ->
        Reference.remove ~fs:t.fs ~root:t.dotgit reference >|= function
        | Ok ()   -> Ok ()
        | Error e -> Error (e :> error)

    let read t reference =
      FS.is_file t.fs Fpath.(t.dotgit // Reference.to_path reference)
      >>= function
      | Ok true ->
        (with_buffer t @@ fun { dtmp; raw; _ } ->
         Reference.read ~fs:t.fs ~root:t.dotgit ~dtmp ~raw reference >|= function
         | Ok (_, v) -> Ok v
         | Error e   -> Error (e :> error))
      | Ok false | Error _ ->
        let v =
          if Hashtbl.mem t.packed reference
          then
            try Ok (Reference.Hash (Hashtbl.find t.packed reference))
            with Not_found -> Error `Not_found
          else Error `Not_found
        in
        Lwt.return v

    let resolve t reference =
      graph t >>= function
      | Error _ as err -> Lwt.return err
      | Ok graph ->
         try Lwt.return (Ok (Graph.find reference graph))
         with Not_found -> Lwt.return (Error `Not_found)

    let write t reference value =
      with_buffer t (fun { raw; _ } ->
          Reference.write ~fs:t.fs ~root:t.dotgit ~raw reference value
        ) >>= function
      | Error (#Reference.error as err) -> Lwt.return (Error (err : error))
      | Ok () ->
        match Hashtbl.mem t.packed reference with
        | false -> Lwt.return (Ok ())
        | true ->
          with_buffer t (fun { dtmp; raw; _ } ->
              Packed_refs.read ~fs:t.fs ~root:t.dotgit ~dtmp ~raw
            ) >>= function
          | Error _ as err -> Lwt.return err
          | Ok packed_refs ->
            Lwt_list.fold_left_s
              (fun acc -> function
                 | `Peeled _ as v -> Lwt.return (v :: acc)
                 | `Ref (refname, hash) when not Reference.(equal (of_string refname) reference) -> Lwt.return (`Ref (refname, hash) :: acc)
                 | _ -> Lwt.return acc)
              [] packed_refs
            >>= fun packed_refs' ->
            Hashtbl.reset t.packed;
            List.iter (function
                |`Ref (refname, hash) ->
                  Hashtbl.add t.packed (Reference.of_string refname) hash
                | `Peeled _ -> ()
              ) packed_refs';
            with_buffer t (fun { raw; _ } ->
                Packed_refs.write ~fs:t.fs ~root:t.dotgit ~raw packed_refs'
              ) >|= function
            | Ok ()   -> Ok ()
            | Error e -> Error (e : error)

  end

  let cache ?(indexes = 5) ?(packs = 5) ?(objects = 5) ?(values = 5) ?(revindexes = 5) () =
    { indexes    = CacheIndex.create indexes
    ; packs      = CachePack.create packs
    ; objects    = CacheObject.create objects
    ; values     = CacheValue.create values
    ; revindexes = CacheRevIndex.create revindexes }

  let default_buffer () =
    let raw = Cstruct.create (0x8000 * 2) in
    let buf = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout (2 * 0x8000) in

    { window = Inflate.window ()
    ; ztmp = Cstruct.sub raw 0 0x8000
    ; dtmp = Cstruct.of_bigarray ~off:0 ~len:0x8000 buf
    (* XXX(dinosaure): bon ici, c'est une note compliqué, j'ai mis 2
       jours à fixer le bug. Donc je l'explique en français, c'est
       plus simple.

       En gros, [Helper.MakeDecoder] utilise ce buffer comme buffer
       interne pour gérer les alterations. Ce qui ce passe, c'est que
       dans la fonction [refill], il s'agit de compléter à partir d'un
       [input] (typiquement [zl]) le buffer interne. C'est finalement
       un __mauvais__ jeu entre un [Cstruct.t] et un [Bigarray].

       Il s'agit de connaître la véritable taille du [Bigarray] et de
       rajouter avec [blit] le contenu de l'[input] si la taille du
       [Bigarray] (et pas du [Cstruct]) est suffisante.

       Avant, cette modification, [zl], [de] et [io] partagaient le
       même [Bigarray] découpé (avec [Cstruct]) en 3. Donc, dans le
       [MakeDecoder], [refill] considérait (pour des gros fichiers
       faisant plus de 0x8000 bytes) que après [de], nous avions
       encore de la place - et dans ce cas, nous avions [io].

       Ainsi, on [blit]ait [zl] dans [de+sizeof(de) == io], et
       finalement, on se retrouvait à essayer de décompresser ce que
       nous avions décompressé. (YOLO).

       Donc, on considère maintenant [de] comme un [Cstruct.t] et un
       [Bigarray] physiquement différent pour éviter ce problème.
       Cependant, il faudrait continuer à introspecter car j'ai
       l'intuition que pour un fichier plus gros que [2 * 0x8000], on
       devrait avoir un problème. Donc TODO. *)
    ; raw = Cstruct.sub raw 0x8000 0x8000 }

  let sanitize_head git =
    Ref.mem git Reference.head >>= function
    | true  -> Lwt.return (Ok ())
    | false -> Ref.write git Reference.head Reference.(Ref master)

  let sanitize_filesystem fs root dotgit =
    FS.Dir.create fs root
    >>?= (fun _ -> FS.Dir.create fs dotgit)
    >>?= (fun _ -> FS.Dir.create fs Fpath.(dotgit / "objects"))
    >>?= (fun _ -> FS.Dir.create fs Fpath.(dotgit / "objects" / "pack"))
    >>?= (fun _ -> FS.Dir.create fs Fpath.(dotgit / "objects" / "info"))
    >>?= (fun _ -> FS.Dir.create fs Fpath.(dotgit / "refs"))
    >>?= (fun _ -> Lwt.return (Ok ()))
    >>!= Error.FS.err_sys_dir

  let create ?root ?dotgit ?(compression = 4) ?buffer fs =
    let buffer = match buffer with
      | Some f -> f
      | None   ->
        let p = Lwt_pool.create 4 (fun () -> Lwt.return (default_buffer ())) in
        Lwt_pool.use p
    in
    let resolve_paths () = match root, dotgit with
     | Some root, Some dotgit -> Lwt.return (Ok (root, dotgit))
     | None, _ | _, None ->
       (FS.Dir.current fs >>!= Error.FS.err_sys_dir) >>?= fun current ->
       let root = Option.get ~default:current root in
       let dotgit  = Option.get ~default:Fpath.(root / ".git") dotgit in
       Lwt.return (Ok (root, dotgit))
    in
    resolve_paths () >>?= fun (root, dotgit) ->
    sanitize_filesystem fs root dotgit >>?= fun () ->
    indexes ~fs dotgit >>?= fun engine ->
    let git =
      { fs
      ; dotgit
      ; root
      ; compression
      ; engine
      ; packed = Hashtbl.create 64
      ; cache = cache ()
      ; buffer = buffer }
    in
    sanitize_head git >>?= fun () ->
    Lwt.return (Ok git)

  let clear_caches t =
    CacheIndex.drop_lru t.cache.indexes;
    CacheRevIndex.drop_lru t.cache.revindexes;
    CachePack.drop_lru t.cache.packs;
    CacheValue.drop_lru t.cache.values;
    CacheObject.drop_lru t.cache.objects;
    Lwt.return ()

  let reset t =
    Log.info (fun l -> l ~header:"reset" "Start to reset the Git repository");
    (FS.Dir.delete t.fs Fpath.(t.dotgit / "objects")
     >>?= fun () -> FS.Dir.create t.fs Fpath.(t.dotgit / "objects")
     >>?= fun _ -> FS.Dir.create t.fs Fpath.(t.dotgit / "objects" / "info")
     >>?= fun _ -> FS.Dir.create t.fs Fpath.(t.dotgit / "objects" / "pack")
     >>?= fun _ -> FS.Dir.delete t.fs Fpath.(t.dotgit / "refs")
     >>?= fun () -> FS.Dir.create t.fs Fpath.(t.dotgit / "refs" / "heads")
     >>?= fun _  -> FS.Dir.create t.fs Fpath.(t.dotgit / "refs" / "tags")
    ) >>!= Error.FS.err_sys_dir
    >>?= fun _ -> Ref.write t Reference.head Reference.(Ref master)
     (* XXX(dinosaure): an empty git repository has HEAD which points
        to a non-existing refs/heads/master. *)

  let dotgit      { dotgit; _ }      = dotgit
  let root        { root; _ }        = root
  let compression { compression; _ } = compression

end
