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

module type LOOSE =
sig
  type t
  type state

  module Hash : S.HASH
  module Path : S.PATH
  module FileSystem : S.FS with type path = Path.t
  module Inflate : S.INFLATE
  module Deflate : S.DEFLATE

  module Value
    : Value.S with module Hash = Hash
               and module Inflate = Inflate
               and module Deflate = Deflate

  type error = [ `SystemFile of FileSystem.File.error
               | `SystemDirectory of FileSystem.Dir.error
               | Value.D.error
               | Value.E.error ]

  val pp_error : error Fmt.t
  val lookup_p : state -> Hash.t -> Hash.t option Lwt.t
  val lookup : state -> Hash.t -> Hash.t option Lwt.t
  val exists : state -> Hash.t -> bool Lwt.t
  val list : state -> Hash.t list Lwt.t
  val read_p : ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> window:Inflate.window -> state -> Hash.t -> (t, error) result Lwt.t
  val read_s : state -> Hash.t -> (t, error) result Lwt.t
  val read : state -> Hash.t -> (t, error) result Lwt.t
  val size_p : ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> window:Inflate.window -> state -> Hash.t -> (int64, error) result Lwt.t
  val size_s : state -> Hash.t -> (int64, error) result Lwt.t
  val size : state -> Hash.t -> (int64, error) result Lwt.t
  val write_p : ztmp:Cstruct.t -> raw:Cstruct.t -> state -> t -> (Hash.t * int, error) result Lwt.t
  val write_s : state -> t -> (Hash.t * int, error) result Lwt.t
  val write : state -> t -> (Hash.t * int, error) result Lwt.t
  val raw_p : window:Inflate.window -> ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  val raw_s : state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  val raw : state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  val raw_wa : window:Inflate.window -> ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> result:Cstruct.t -> state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  val raw_was : Cstruct.t -> state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t

  module D : S.DECODER with type t = t
                        and type raw = Cstruct.t
                        and type init = Inflate.window * Cstruct.t * Cstruct.t
                        and type error = [ `Decoder of string | `Inflate of Inflate.error ]

  module E : S.ENCODER with type t = t
                        and type raw = Cstruct.t
                        and type init = int * t * int * Cstruct.t
                        and type error = [ `Deflate of Deflate.error ]
end

module type PACK =
sig
  type t
  type state

  module Hash : S.HASH
  module Path : S.PATH
  module FileSystem  : S.FS with type path = Path.t
  module Inflate : S.INFLATE
  module IDXDecoder : Index_pack.LAZY
  module PACKDecoder
    : Unpack.DECODER with module Hash = Hash
                      and module Inflate = Inflate

  type error = [ `SystemFile of FileSystem.File.error
               | `SystemDirectory of FileSystem.Dir.error
               | `SystemMapper of FileSystem.Mapper.error
               | `IndexDecoder of IDXDecoder.error
               | `PackDecoder of PACKDecoder.error
               | `Not_found ]

  val pp_error : error Fmt.t
  val lookup_p : state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  val lookup : state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  val exists : state -> Hash.t -> bool Lwt.t
  val list : state -> Hash.t list Lwt.t
  val read_p : ztmp:Cstruct.t -> window:Inflate.window -> state -> Hash.t -> (t, error) result Lwt.t
  val read_s : state -> Hash.t -> (t, error) result Lwt.t
  val read : state -> Hash.t -> (t, error) result Lwt.t
  val read_wa : ?htmp:Cstruct.t array -> ztmp:Cstruct.t -> window:Inflate.window -> result:Cstruct.t * Cstruct.t -> state -> Hash.t -> (t, error) result Lwt.t
  val read_was : ?htmp:Cstruct.t array -> (Cstruct.t * Cstruct.t) -> state -> Hash.t -> (t, error) result Lwt.t
  val size_p : ztmp:Cstruct.t -> window:Inflate.window -> state -> Hash.t -> (int, error) result Lwt.t
  val size_s : state -> Hash.t -> (int, error) result Lwt.t
  val size : state -> Hash.t -> (int, error) result Lwt.t
end

module type S =
sig
  type t

  (* XXX(dinosaure): Functorized module. *)
  module Hash
    : S.HASH
  module Path
    : S.PATH
  module Inflate
    : S.INFLATE
  module Deflate
    : S.DEFLATE
  module Lock
    : S.LOCK
  module FileSystem
    : S.FS with type path = Path.t
            and type File.lock = Lock.elt

  module Value
    : Value.S with module Hash = Hash
               and module Inflate = Inflate
               and module Deflate = Deflate
  module Reference
    : Reference.IO with module Hash = Hash
                    and module Path = Path
                    and module Lock = Lock
                    and module FileSystem = FileSystem

  module IDXDecoder
    : Index_pack.LAZY with module Hash = Hash
  module PACKDecoder
    : Unpack.DECODER with module Hash = Hash
                      and module Inflate = Inflate
  module PACKEncoder
    : Pack.ENCODER with module Hash = Hash
                    and module Deflate = Deflate

  module Loose
    : LOOSE with type t = Value.t
             and type state = t
             and module Hash = Hash
             and module Path = Path
             and module Inflate = Inflate
             and module Deflate = Deflate
             and module FileSystem = FileSystem

  module Pack
    : PACK with type t = PACKDecoder.Object.t
            and type state = t
            and module Hash = Hash
            and module Path = Path
            and module FileSystem = FileSystem
            and module Inflate = Inflate
            and module PACKDecoder = PACKDecoder
            and module IDXDecoder = IDXDecoder

  type error =
    [ Loose.error
    | Pack.error ]

  val pp_error : error Fmt.t
  val create : ?root:Path.t -> ?dotgit:Path.t -> ?compression:int -> unit -> (t, error) result Lwt.t
  val dotgit : t -> Path.t
  val root : t -> Path.t
  val compression : t -> int
  val exists : t -> Hash.t -> bool Lwt.t
  val list : t -> Hash.t list Lwt.t
  val read_p : ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> window:Inflate.window -> t -> Hash.t -> (Value.t, error) result Lwt.t
  val read_s : t -> Hash.t -> (Value.t, error) result Lwt.t
  val read : t -> Hash.t -> (Value.t, error) result Lwt.t
  val read_exn : t -> Hash.t -> Value.t Lwt.t
  val write_p : ztmp:Cstruct.t -> raw:Cstruct.t -> t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val write_s : t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val write : t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val size_p : ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> window:Inflate.window -> t -> Hash.t -> (int64, error) result Lwt.t
  val size_s : t -> Hash.t -> (int64, error) result Lwt.t
  val size : t -> Hash.t -> (int64, error) result Lwt.t
  val raw_p : ztmp:Cstruct.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> window:Inflate.window -> t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  val raw_s : t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  val raw : t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
  val buffer_window : t -> Inflate.window
  val buffer_zl : t -> Cstruct.t
  val buffer_de : t -> Cstruct.t
  val buffer_io : t -> Cstruct.t
  val indexes : t -> Hash.t list
  val fold : t -> ('a -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t) -> path:Path.t -> 'a -> Hash.t -> 'a Lwt.t

  module Ref :
  sig
    module Packed_refs : Packed_refs.S with module Hash = Hash
                                        and module Path = Path
                                        and module FileSystem = FileSystem

    type nonrec error =
      [ Packed_refs.error
      | error
      | `Invalid_reference of Reference.t ]

    val pp_error : error Fmt.t
    val graph_p : t -> dtmp:Cstruct.t -> raw:Cstruct.t -> (Hash.t Reference.Map.t, error) result Lwt.t
    val graph : t -> (Hash.t Reference.Map.t, error) result Lwt.t
    val normalize : Hash.t Reference.Map.t -> Reference.head_contents -> (Hash.t, error) result Lwt.t
    val list_p : t -> dtmp:Cstruct.t -> raw:Cstruct.t -> (Reference.t * Hash.t) list Lwt.t
    val list_s : t -> (Reference.t * Hash.t) list Lwt.t
    val list : t -> (Reference.t * Hash.t) list Lwt.t
    val remove_p : t -> dtmp:Cstruct.t -> raw:Cstruct.t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    val remove_s : t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    val remove : t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    val read_p : t -> dtmp:Cstruct.t -> raw:Cstruct.t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val read_s : t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val read : t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val write_p : t -> ?locks:Lock.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val write_s : t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val write : t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val test_and_set : t -> ?locks:Lock.t -> Reference.t -> test:Reference.head_contents option -> set:Reference.head_contents option -> (bool, error) result Lwt.t
  end
end

module Option =
struct
  let get ~default = function Some x -> x | None -> default
  let map f = function Some v -> Some (f v) | None -> None
end

module Make
    (H : S.HASH with type Digest.buffer = Cstruct.t
                 and type hex = string)
    (P : S.PATH)
    (L : S.LOCK with type key = P.t
                 and type +'a io = 'a Lwt.t)
    (FS : S.FS with type path = P.t
                and type File.raw = Cstruct.t
                and type File.lock = L.elt
                and type Mapper.raw = Cstruct.t
                and type +'a io = 'a Lwt.t)
    (I : S.INFLATE)
    (D : S.DEFLATE)
  : S with module Hash = H
       and module Path = P
       and module Lock = L
       and module FileSystem = FS
       and module Inflate = I
       and module Deflate = D
= struct
  module Hash = H
  module Path = P
  module Inflate = I
  module Deflate = D
  module Lock = L
  module FileSystem = FS

  module LooseImpl
    : Loose.S with module Hash = Hash
               and module Path = Path
               and module FileSystem = FileSystem
               and module Inflate = Inflate
               and module Deflate = Deflate
   = Loose.Make(H)(P)(FS)(I)(D)

  module Value
    : Value.S with module Hash = Hash
               and module Inflate = Inflate
               and module Deflate = Deflate
               and module Blob = LooseImpl.Blob
               and module Tree = LooseImpl.Tree
               and module Tag = LooseImpl.Tag
               and module Commit = LooseImpl.Commit
               and type t = LooseImpl.t
    = LooseImpl

  module PACKDecoder = Unpack.MakeDecoder(H)(FS.Mapper)(I)
  module PACKEncoder = Pack.MakePACKEncoder(H)(D)
  module Reference = Reference.IO(H)(P)(L)(FS)
  module IDXDecoder = Index_pack.Lazy(H)

  module DoubleHash =
  struct
    type t = Hash.t * Hash.t

    let hash = Hashtbl.hash

    let equal (a, b) (c, d) =
      Hash.equal a c && Hash.equal b d
  end

  module HashInt64 =
  struct
    type t = Hash.t * int64

    let hash = Hashtbl.hash

    let equal (a, b) (c, d) =
      Hash.equal a c && Int64.equal b d
  end

  (* XXX(dinosaure): need to limit the weight of [CacheObject] and [CacheValue]
     by the memory consumption of the data stored - and not by the number of
     theses data. Indeed, 5 commits is more cheaper than 1 blob sometimes. *)
  module CacheObject   = Lru.M.Make(DoubleHash)(struct type t = PACKDecoder.Object.t let weight _ = 1 end)
  module CacheValue    = Lru.M.Make(Hash)(struct type t = Value.t let weight _ = 1 end)
  module CachePack     = Lru.M.Make(Hash)(struct type t = PACKDecoder.t let weight _ = 1 end) (* fixed size *)
  module CacheIndex    = Lru.M.Make(Hash)(struct type t = IDXDecoder.t let weight _ = 1 end) (* not fixed size by consider than it's ok. *)
  module CacheRevIndex = Lru.M.Make(HashInt64)(struct type t = Hash.t let weight _ = 1 end) (* fixed size *)

  type cache =
    { objects     : CacheObject.t
    ; values      : CacheValue.t
    ; packs       : CachePack.t
    ; indexes     : CacheIndex.t
    ; revindexes  : CacheRevIndex.t }
  and buffer =
    { window      : Inflate.window
    ; io          : Cstruct.t
    ; zl          : Cstruct.t
    ; de          : Cstruct.t }
  and t =
    { dotgit      : Path.t
    ; root        : Path.t
    ; compression : int
    ; idxs        : (Hash.t * Path.t) list
    ; cache       : cache
    ; buffer      : buffer }

  module Loose =
  struct
    module Hash = Hash
    module Path = Path
    module FileSystem = FileSystem
    module Inflate = Inflate
    module Deflate = Deflate
    module Value = Value

    type state = t
    type t = LooseImpl.t

    type error = LooseImpl.error

    let read_p ~ztmp ~dtmp ~raw ~window t =
      LooseImpl.read ~root:t.dotgit ~window ~ztmp ~dtmp ~raw
    let size_p ~ztmp ~dtmp ~raw ~window t =
      LooseImpl.size ~root:t.dotgit ~window ~ztmp ~dtmp ~raw
    let write_p ~ztmp ~raw t value =
      LooseImpl.write ~root:t.dotgit ~ztmp ~raw ~level:t.compression value

    let pp_error = LooseImpl.pp_error

    let exists t =
      LooseImpl.exists
        ~root:t.dotgit
    let read_s t =
      LooseImpl.read
        ~root:t.dotgit
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~dtmp:t.buffer.de
        ~raw:t.buffer.io
    let read = read_s
    let size_s t =
      LooseImpl.size
        ~root:t.dotgit
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~dtmp:t.buffer.de
        ~raw:t.buffer.io
    let size = size_s
    let list t =
      LooseImpl.list
        ~root:t.dotgit
    let write_s t value =
      write_p ~ztmp:t.buffer.zl ~raw:t.buffer.io t value
    let write = write_s

    let lookup_p state hash =
      let open Lwt.Infix in

      Lwt.try_bind
        (fun () ->
          list state
          >>= function
          | (_ :: _) as lst ->
            Lwt_list.find_s (fun x -> Hash.equal hash x |> Lwt.return) lst
            >|= fun x -> (Some x)
          | [] -> Lwt.return None)
        Lwt.return
        (fun exn -> Lwt.return None)

    let lookup = lookup_p

    let raw_p ~window ~ztmp ~dtmp ~raw t hash =
      LooseImpl.inflate ~root:t.dotgit ~window ~ztmp ~dtmp ~raw hash

    let raw_s t hash =
      raw_p
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~dtmp:t.buffer.de
        ~raw:t.buffer.io
        t hash

    let raw = raw_s

    let raw_wa ~window ~ztmp ~dtmp ~raw ~result t hash =
      LooseImpl.inflate_wa ~root:t.dotgit ~window ~ztmp ~dtmp ~raw ~result hash

    let raw_was result t hash =
      raw_wa
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~raw:t.buffer.io
        ~dtmp:t.buffer.de
        ~result
        t hash

    module D : S.DECODER with type t = t
                               and type raw = Cstruct.t
                               and type init = Inflate.window * Cstruct.t * Cstruct.t
                               and type error = Value.D.error
      = Value.D

    module E : S.ENCODER with type t = t
                               and type raw = Cstruct.t
                               and type init = int * t * int * Cstruct.t
                               and type error = Value.E.error
      = Value.E
  end

  module Pack =
  struct
    type state = t

    module Path = Path
    module FileSystem = FileSystem
    module Inflate = Inflate
    module Hash = Hash
    module IDXDecoder = IDXDecoder
    module PACKDecoder = PACKDecoder

    type error = [ `IndexDecoder of IDXDecoder.error
                 | `PackDecoder of PACKDecoder.error
                 | `Not_found
                 | `SystemFile of FileSystem.File.error
                 | `SystemDirectory of FileSystem.Dir.error
                 | `SystemMapper of FileSystem.Mapper.error ]

    let pp_error ppf = function
      | `PackDecoder err ->
        Helper.ppe ~name:"`PackDecoder" (Fmt.hvbox PACKDecoder.pp_error) ppf err
      | `IndexDecoder err ->
        Helper.ppe ~name:"`IndexDecoder" (Fmt.hvbox IDXDecoder.pp_error) ppf err
      | `Not_found -> Fmt.string ppf "`Not_found"
      | `SystemFile sys_err -> Helper.ppe ~name:"`SystemFile" FileSystem.File.pp_error ppf sys_err
      | `SystemDirectory sys_err -> Helper.ppe ~name:"`SystemDirectory" FileSystem.Dir.pp_error ppf sys_err
      | `SystemMapper sys_err -> Helper.ppe ~name:"`SystemMapper" FileSystem.Mapper.pp_error ppf sys_err

    type t = PACKDecoder.Object.t

    let load_idx state hash_idx path =
      let open Lwt.Infix in

      FileSystem.Mapper.openfile path
      >>= (function Ok fd -> FileSystem.Mapper.length fd
          >>= (function Ok length -> FileSystem.Mapper.map ~share:false fd (Int64.to_int length)
                      | Error err -> Lwt.return (Error err))
                  | Error err -> Lwt.return (Error err))
      >|= (function
          | Error sys_err -> Error (`SystemMapper sys_err)
          | Ok map ->
            IDXDecoder.make map
            |> function
            | Ok idx -> Ok idx
            | Error err -> Error (`IndexDecoder err))

    let lookup_p state hash =
      let open Lwt.Infix in

      Lwt_list.fold_left_s
        (function
          | Some value -> fun x -> Lwt.return (Some value)
          | None -> fun (hash_idx, path) ->
            match CacheIndex.find hash_idx state.cache.indexes with
            | Some idx ->
              Lwt.return (IDXDecoder.find idx hash |> Option.map (fun v -> (hash_idx, v)))
            | None ->
              load_idx state hash_idx path
              >|= function
              | Ok idx ->
                CacheIndex.add hash_idx idx state.cache.indexes; (* XXX(dinosaure): thread-safe? *)
                IDXDecoder.find idx hash |> Option.map (fun v -> (hash_idx, v))
              | Error err -> None)
        None state.idxs

    let lookup = lookup_p

    let exists state hash =
      let open Lwt.Infix in
      lookup state hash >|= function Some _ -> true | None -> false

    let path_idx state hash_idx =
      let buf = Buffer.create (5 + Hash.Digest.length * 2) in
      let ppf = Fmt.with_buffer buf in

      Fmt.pf ppf "pack-%a%!" Hash.pp hash_idx;
      let filename_pack = Buffer.contents buf in

      Path.((state.dotgit / "objects" / "pack" / filename_pack) + "pack")

    type info =
      { max_length : int
      ; max_depth  : int }

    module Graph = Map.Make(Int64)

    let info_from_pack state ~raw ~ztmp ~window hash_idx =
      let path = path_idx state hash_idx in

      let open Lwt.Infix in

      FileSystem.File.open_r ~mode:0o644 path >>= function
      | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
      | Ok fd ->
        let state = PACKDecoder.P.default ztmp window in

        let rec go length graph t = match PACKDecoder.P.eval raw t with
          | `Flush t ->
            let o, n = PACKDecoder.P.output t in
            go length graph (PACKDecoder.P.flush 0 (Cstruct.len o) t)
          | `Hunk (t, _) ->
            go length graph (PACKDecoder.P.continue t)
          | `Error (err, t) ->
            Lwt.return (Error (`PackDecoder err))
          | `Await t ->
            FileSystem.File.read raw ~off:0 ~len:(Cstruct.len raw) fd >>= (function
                | Ok n ->
                  let t = PACKDecoder.P.refill 0 n t in
                  go length graph t
                | Error sys_err -> Lwt.return (Error (`SystemFile sys_err)))
          | `End (t, hash) ->
            assert (Hash.equal hash hash_idx);

            Lwt.return (Ok { max_depth = Graph.fold (fun _ -> max) graph 0; max_length = length })
          | `Object t ->
            match PACKDecoder.P.kind t with
            | PACKDecoder.P.Commit
            | PACKDecoder.P.Tree
            | PACKDecoder.P.Tag
            | PACKDecoder.P.Blob ->
              go (max (PACKDecoder.P.length t) length) graph (PACKDecoder.P.next_object t)
            | PACKDecoder.P.Hunk ({ PACKDecoder.P.H.reference = PACKDecoder.P.H.Offset rel_off; _ } as hunks) ->
              let off = PACKDecoder.P.offset t in
              let length =
                max length
                @@ max (PACKDecoder.P.length t)
                @@ max hunks.PACKDecoder.P.H.target_length hunks.PACKDecoder.P.H.source_length
              in
              let graph =
                let depth_base =
                  try Graph.find Int64.(sub off rel_off) graph
                  with Not_found -> 0
                in

                Graph.add off (depth_base + 1) graph
              in

              go length graph (PACKDecoder.P.next_object t)
            | PACKDecoder.P.Hunk hunks ->
              let length =
                max length
                @@ max (PACKDecoder.P.length t)
                @@ max hunks.PACKDecoder.P.H.target_length hunks.PACKDecoder.P.H.source_length
              in
              go length graph (PACKDecoder.P.next_object t)
        in

        go 0 Graph.empty state

    let rec load_pack state hash_idx path_idx =
      let open Lwt.Infix in
      let path_pack = Path.set_ext "pack" path_idx in

      (match CacheIndex.find hash_idx state.cache.indexes with
       | Some idx -> Lwt.return (Ok idx)
       | None -> load_idx state hash_idx path_idx >|= function
         | Ok idx -> CacheIndex.add hash_idx idx state.cache.indexes; Ok idx
         | Error err -> Error err)
      >>= function
      | Error err -> Lwt.return (Error err)
      | Ok idx ->
        FileSystem.Mapper.openfile path_pack
        >>= function
        | Error sys_err -> Lwt.return (Error (`SystemMapper sys_err))
        | Ok fd -> PACKDecoder.make fd
                     (fun hash -> CacheObject.find (hash_idx, hash) state.cache.objects)
                     (IDXDecoder.find idx)
                     (fun offset -> CacheRevIndex.find (hash_idx, offset) state.cache.revindexes)
                     (extern state) >>= function
          | Ok pack -> CachePack.add hash_idx pack state.cache.packs; Lwt.return (Ok pack)
          | Error sys_err -> Lwt.return (Error (`SystemMapper sys_err))
    and extern state hash =
      (* XXX(dinosaure): [extern] is only used for the source to reconstruct an object. *)
      let open Lwt.Infix in

      lookup state hash >>= (function
          | None -> Lwt.return `Not_found
          | Some (hash_idx, (crc32, absolute_offset)) ->
            CacheRevIndex.add (hash_idx, absolute_offset) hash state.cache.revindexes;
            match CachePack.find hash_idx state.cache.packs with
            | None -> load_pack state hash_idx (path_idx state hash_idx) >>= (function
                | Ok pack ->
                  CachePack.add hash_idx pack state.cache.packs;
                  PACKDecoder.get_with_allocation pack hash state.buffer.zl state.buffer.window >>= (function
                      | Ok o ->
                        CacheObject.add (hash_idx, hash) o state.cache.objects;
                        (* XXX(dinosaure): need to check if it's safe to save
                           this object. Check if [object.raw] is not a shared
                           buffer. *)
                        Lwt.return (`Object (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
                      | Error err -> Lwt.return (`Error (`PackDecoder err)))
                | Error err -> Lwt.return (`Error err))
            | Some pack -> PACKDecoder.get_with_allocation pack hash state.buffer.zl state.buffer.window >>= (function
                | Ok o ->
                  CacheObject.add (hash_idx, hash) o state.cache.objects;
                  (* XXX(dinosaure): same as below. *)
                  Lwt.return (`Object (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
                | Error err -> Lwt.return (`Error (`PackDecoder err))))
      >>= function
      | `Object o ->
        Lwt.return (Some o)
      | (`Not_found | `Error _) as pack_result ->
        Loose.lookup state hash >>= function
        | Some _ ->
          Loose.raw_s state hash
          >|= (function
              | Ok v -> Some v
              | Error err -> None)
        | None -> match pack_result with
          | `Not_found | `Error _ -> Lwt.return None

    (* XXX(dinosaure): consider than [extern] is total. That means, when we
       request a git object, if [extern] returns [None], this object does not exist
       as loosed object or packed object. *)

    let list state =
      let open Lwt.Infix in

      Lwt_list.fold_left_s
        (fun acc (hash_idx, path) -> match CacheIndex.find hash_idx state.cache.indexes with
           | Some idx -> IDXDecoder.fold idx (fun hash _ acc -> hash :: acc) [] |> Lwt.return
           | None -> load_idx state hash_idx path
             >|= function
             | Ok idx ->
               CacheIndex.add hash_idx idx state.cache.indexes;
               IDXDecoder.fold idx (fun hash _ acc -> hash :: acc) []
             | Error err -> acc)
        [] state.idxs

    let read_p ~ztmp ~window state hash =
      let open Lwt.Infix in

      lookup state hash >>= function
      | None -> Lwt.return (Error `Not_found)
      | Some (hash_idx, _) ->
        match CachePack.find hash_idx state.cache.packs with
        | Some pack ->
          PACKDecoder.get_with_allocation pack hash ztmp window
          >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
        | None -> load_pack state hash_idx (path_idx state hash_idx) >>= function
          | Ok pack ->
            PACKDecoder.get_with_allocation pack hash ztmp window
            >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
          | Error err -> Lwt.return (Error err)

    let read_s state hash =
      read_p ~ztmp:state.buffer.zl ~window:state.buffer.window state hash

    let read = read_s

    let read_wa ?htmp ~ztmp ~window ~result state hash =
      let open Lwt.Infix in

      lookup state hash >>= function
      | None -> Lwt.return (Error `Not_found)
      | Some (hash_idx, _) ->
        let (a, b) = result in

        match CachePack.find hash_idx state.cache.packs with
        | Some pack ->
          PACKDecoder.optimized_get ~limit:true ?h_tmp:htmp pack hash (a, b, 0) ztmp window
          >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
        | None -> load_pack state hash_idx (path_idx state hash_idx) >>= function
          | Ok pack ->
            PACKDecoder.optimized_get ~limit:true ?h_tmp:htmp pack hash (a, b, 0) ztmp window
            >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
          | Error err -> Lwt.return (Error err)

    let read_was ?htmp result t hash =
      read_wa
        ?htmp
        ~ztmp:t.buffer.zl
        ~window:t.buffer.window
        ~result
        t hash

    let size_p ~ztmp ~window state hash =
      let open Lwt.Infix in

      lookup state hash >>= function
      | None -> Lwt.return (Error `Not_found)
      | Some (hash_idx, _) ->
        match CachePack.find hash_idx state.cache.packs with
        | Some pack ->
          PACKDecoder.length pack hash ztmp window
          >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
        | None -> load_pack state hash_idx (path_idx state hash_idx) >>= function
          | Ok pack ->
            PACKDecoder.length pack hash ztmp window
            >|= Rresult.R.reword_error (fun err -> `PackDecoder err)
          | Error err -> Lwt.return (Error err)

    let size_s state hash =
      size_p
        ~ztmp:state.buffer.zl
        ~window:state.buffer.window
        state hash

    let size = size_s

    let entry_of_value hash ?preferred ?delta ?path:name value =
      let kind = match value with
        | Value.Commit _ -> Pack.Kind.Commit
        | Value.Tree _   -> Pack.Kind.Tree
        | Value.Tag _    -> Pack.Kind.Tag
        | Value.Blob _   -> Pack.Kind.Blob
      in

      PACKEncoder.Entry.make ?preferred ?delta hash ?name kind (Value.F.length value)
  end

  type error = [ Loose.error | Pack.error ]

  let pp_error ppf = function
    | #Loose.error as err -> Fmt.pf ppf "%a" Loose.pp_error err
    | #Pack.error as err -> Fmt.pf ppf "%a" Pack.pp_error err

  let read_p ~ztmp ~dtmp ~raw ~window state hash =
    let open Lwt.Infix in

    Pack.read_p ~ztmp ~window state hash >>= function
    | Ok o ->
      (match o.PACKDecoder.Object.kind with
       | `Commit ->
         Value.Commit.D.to_result o.PACKDecoder.Object.raw
         |> Rresult.R.map (fun v -> Value.Commit v)
       | `Blob ->
         Value.Blob.D.to_result o.PACKDecoder.Object.raw
         |> Rresult.R.map (fun v -> Value.Blob v)
       | `Tree ->
         Value.Tree.D.to_result o.PACKDecoder.Object.raw
         |> Rresult.R.map (fun v -> Value.Tree v)
       | `Tag ->
         Value.Tag.D.to_result o.PACKDecoder.Object.raw
         |> Rresult.R.map (fun v -> Value.Tag v))
      |> (function
          | Error (`Decoder err) -> Lwt.return (Error (`Decoder err))
          | Ok v -> Lwt.return (Ok v))
    | Error (#Pack.error as err) -> Loose.lookup state hash >>= function
      | None -> Lwt.return (Error err)
      | Some _ -> Loose.read_p ~window ~ztmp ~dtmp ~raw state hash >>= function
        | Error (#Loose.error as err) -> Lwt.return (Error err)
        | Ok v -> Lwt.return (Ok v)

  let read_s t hash =
    read_p
      ~ztmp:t.buffer.zl
      ~dtmp:t.buffer.de
      ~raw:t.buffer.io
      ~window:t.buffer.window
      t hash

  let read = read_s

  let read_exn t hash =
    let open Lwt.Infix in

    read t hash >>= function
    | Error _ ->
      let err = Fmt.strf "Git.Store.read_exn: %a not found" Hash.pp hash in
      Lwt.fail (Invalid_argument err)
    | Ok v -> Lwt.return v

  let write_p ~ztmp ~raw state hash =
    let open Lwt.Infix in
    Loose.write_p ~ztmp ~raw state hash >|= function
    | Error (#LooseImpl.error as err) -> Error (err :> error)
    | Ok v -> Ok v

  let write_s state hash =
    let open Lwt.Infix in
    Loose.write_s state hash >|= function
    | Error (#LooseImpl.error as err) -> Error (err :> error)
    | Ok v -> Ok v

  let write = write_s

  let raw_p ~ztmp ~dtmp ~raw ~window state hash =
    let open Lwt.Infix in

    Pack.read_p ~ztmp ~window state hash >>= function
    | Ok o ->
      Lwt.return (Some (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
    | Error err -> Loose.lookup state hash >>= function
      | None -> Lwt.return None
      | Some _ -> Loose.raw_p ~window ~ztmp ~dtmp ~raw state hash >>= function
        | Error #Loose.error -> Lwt.return None
        | Ok v -> Lwt.return (Some v)

  let raw_s t hash =
    raw_p
      ~ztmp:t.buffer.zl
      ~dtmp:t.buffer.de
      ~raw:t.buffer.io
      ~window:t.buffer.window
      t hash

  let raw = raw_s

  let raw_wa ?htmp ~ztmp ~dtmp ~raw ~window ~result state hash =
    let open Lwt.Infix in

    Pack.read_wa ~ztmp ~window ~result state hash >>= function
    | Ok o ->
      Lwt.return (Some (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
    | Error #Pack.error -> Loose.lookup state hash >>= function
      | None -> Lwt.return None
      | Some _ ->
        let (result, _) = result in
        Loose.raw_wa ~window ~ztmp ~dtmp ~raw ~result state hash >>= function
        | Error #Loose.error -> Lwt.return None
        | Ok v -> Lwt.return (Some v)

  let raw_was ?htmp result t hash =
    raw_wa
      ?htmp
      ~ztmp:t.buffer.zl
      ~dtmp:t.buffer.de
      ~raw:t.buffer.io
      ~window:t.buffer.window
      ~result
      t hash

  let indexes git =
    let hash path =
      let basename = Path.basename (Path.rem_ext path) in
      Scanf.sscanf basename "pack-%s" (fun x -> Hash.of_hex x)
    in

    let open Lwt.Infix in

    FileSystem.Dir.contents ~dotfiles:false ~rel:false Path.(git / "objects" / "pack")
    >>= function
    | Ok lst ->
      Lwt_list.fold_left_s
        (fun acc path ->
           if Path.has_ext "idx" path
           then
             try let hash = hash path in Lwt.return ((hash, path) :: acc)
             with exn -> Lwt.return acc
           else Lwt.return acc)
        [] lst
      >>= fun v -> Lwt.return (Ok v)
    | Error err -> Lwt.return (Error err)

  let lookup_p state hash =
    let open Lwt.Infix in

    Pack.lookup state hash
    >>= function
    | Some (hash_pack, (absolute_offset, crc32)) -> Lwt.return (`PackDecoder (hash_pack, absolute_offset))
    | None -> Loose.lookup state hash >>= function
      | Some _ -> Lwt.return `Loose
      | None -> Lwt.return `Not_found

  let lookup = lookup_p

  let exists state hash =
    let open Lwt.Infix in

    lookup state hash >|= function
    | `Not_found -> false
    | _ -> true

  let list state =
    let open Lwt.Infix in

    Loose.list state
    >>= fun looses -> Pack.list state
    >|= fun packed -> List.append looses packed

  let size_p ~ztmp ~dtmp ~raw ~window state hash =
    let open Lwt.Infix in
    Pack.size_p ~ztmp ~window state hash >>= function
    | Ok v -> Lwt.return (Ok (Int64.of_int v))
    | Error (#Pack.error as err) ->
      Loose.exists state hash >>= function
      | true -> Loose.size_p ~ztmp ~dtmp ~raw ~window state hash >|= Rresult.R.reword_error (fun x -> (x :> error))
      | false -> Lwt.return (Error (err :> error))

  let size_s state hash =
    size_p
      ~ztmp:state.buffer.zl
      ~dtmp:state.buffer.de
      ~raw:state.buffer.io
      ~window:state.buffer.window
      state hash

  let size = size_s

  exception Leave of error

  let contents state =
    let open Lwt.Infix in

    list state
    >>= fun lst ->
    Lwt.try_bind
      (fun () -> Lwt_list.map_s
          (fun hash -> read state hash
            >|= function
            | Ok v -> (hash, v)
            | Error err -> raise (Leave err))
            lst)
        (fun lst -> Lwt.return (Ok lst))
        (function Leave err -> Lwt.return (Error err))

  let normalize entries state =
    let cache = Hashtbl.create (List.length entries) in

    let open Lwt.Infix in

    contents state
    >>= (function
        | Ok lst ->
          Lwt_list.iter_s
            (function
              | (_, Value.Tree tree) ->
                Lwt_list.iter_s (fun { Value.Tree.name; node; _ } -> Hashtbl.add cache node name; Lwt.return ()) tree
              | _ -> Lwt.return ())
            lst
        | Error _ -> Lwt.return ())
    (* XXX(dinosaure): I unsound the error because is not mandatory to compute
       all name for each entry. It's just better to make an optimized PACK file. *)
    >>= fun () ->
    Lwt_list.map_p
      (fun entry ->
         try let name = Hashtbl.find cache (PACKEncoder.Entry.id entry) in
           Lwt.return (PACKEncoder.Entry.name entry name)
         with Not_found -> Lwt.return entry)
      entries

  let delta entries tagger ?(depth = 50) ?(window = `Object 10) state =
    let open Lwt.Infix in

    let memory, window = match window with `Object w -> false, w | `Memory w -> true, w  in
    let read hash = raw_s state hash >|= function Some (kind, raw) -> Some raw | None -> None in

    PACKEncoder.Delta.deltas ~memory entries read tagger depth window

  (* XXX(dinosaure): see Irmin implementation. *)
  let fold t (f : ('acc -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'acc Lwt.t)) ~path acc hash =
    let names = Hashtbl.create 0x100 in

    let open Lwt.Infix in

    let rec walk close rest queue acc =
      match rest with
      | [] ->
        (match Queue.pop queue with
         | rest -> walk close rest queue acc
         | exception Queue.Empty -> Lwt.return acc)
      | hash :: rest ->
        if Hash.Set.exists ((=) hash) close
        then walk close rest queue acc
        else
          let close' = Hash.Set.add hash close in

          read t hash >>= function
          | Ok (Value.Commit commit as value) ->
            let rest' = rest @ Value.Commit.parents commit in
            Queue.add [ Value.Commit.tree commit ] queue;
            f acc ~length:(Value.Commit.F.length commit) hash value >>= fun acc' ->
            walk close' rest' queue acc'
          | Ok (Value.Tree tree as value) ->
            let path = try Hashtbl.find names hash with Not_found -> path in
            Lwt_list.iter_s (fun { Value.Tree.name; node; _ } -> Hashtbl.add names node Path.(path / name); Lwt.return ()) tree >>= fun () ->
            let rest' = rest @ List.map (fun { Value.Tree.node; _ } -> node) tree in
            f acc ~name:path ~length:(Value.Tree.F.length tree) hash value >>= fun acc' ->
            walk close' rest' queue acc'
          | Ok (Value.Blob blob as value) ->
            let path = try Hashtbl.find names hash with Not_found -> path in
            f acc ~name:path ~length:(Value.Blob.F.length blob) hash value >>= fun acc' ->
            walk close' rest queue acc'
          | Ok (Value.Tag tag as value) ->
            let rest' = rest @ [ Value.Tag.obj tag ] in
            f acc ~length:(Value.Tag.F.length tag) hash value >>= fun acc' ->
            walk close' rest' queue acc'
          | Error _ ->
            walk close' rest queue acc
    in

    walk Hash.Set.empty [ hash ] (Queue.create ()) acc

  module Ref =
  struct
    module Packed_refs = Packed_refs.Make(Hash)(Path)(FileSystem)
    (* XXX(dinosaure): we need to check the packed references when we write and remove. *)

    let pp_error ppf = function
      | #Packed_refs.error as err -> Fmt.pf ppf "%a" Packed_refs.pp_error err
      | #error as err -> Fmt.pf ppf "%a" pp_error err
      | `Invalid_reference err ->
        Helper.ppe ~name:"`Invalid_reference" Reference.pp ppf err

    type nonrec error =
      [ Packed_refs.error
      | error
      | `Invalid_reference of Reference.t ]


    let ( >>== ) v f =
      let open Lwt.Infix in

      v >>= function
      | Ok v -> f v
      | Error _ as e -> Lwt.return e

    let contents dir =
      let open Lwt.Infix in

      let rec lookup acc dir =
        FileSystem.Dir.contents dir
        >>== fun l ->
        Lwt_list.filter_p (fun x -> FileSystem.is_dir x >|= function Ok v -> v | Error _ -> false) l >>= fun dirs ->
        Lwt_list.filter_p (fun x -> FileSystem.is_file x >|= function Ok v -> v | Error _ -> false) l >>= fun files ->

        Lwt_list.fold_left_s
          (function Ok acc -> fun x -> lookup acc x
                  | Error _ as e -> fun x -> Lwt.return e)
          (Ok acc) dirs >>== fun acc -> Lwt.return (Ok (acc @ files))
      in

      lookup [] dir

    module Graph = Reference.Map

    (* XXX(dinosaure): this function does not return any {!Error} value. *)
    let graph_p t ~dtmp ~raw =
      let open Lwt.Infix in

      contents Path.(t.dotgit / "refs") >>= function
      | Error sys_err -> Lwt.return (Error (`SystemDirectory sys_err))
      | Ok files ->
        Lwt_list.fold_left_s
          (fun acc abs_ref ->
             (* XXX(dinosaure): we already normalize the reference (which is
                absolute). so we consider than the root as [/]. *)
             Reference.read ~root:(Path.v "/") (Reference.of_path abs_ref) ~dtmp ~raw
             >|= function
             | Ok v -> v :: acc
             | Error _ -> acc)
          [] (Path.(t.dotgit / "HEAD") :: files)
        >>= fun lst -> Lwt_list.fold_left_s
            (fun (rest, graph) -> function
               | refname, Reference.Hash hash -> Lwt.return (rest, Graph.add refname hash graph)
               | refname, Reference.Ref link -> Lwt.return ((refname, link) :: rest, graph))
            ([], Graph.empty) lst
        >>= fun (partial, graph) ->
        Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
        | Ok packed_refs ->
          Lwt_list.fold_left_s
            (fun graph -> function
               | `Peeled tagged -> Lwt.return graph
               | `Ref (refname, hash) -> Lwt.return (Graph.add (Reference.of_string refname) hash graph))
            graph packed_refs
          >>= fun graph -> Lwt_list.fold_left_s
            (fun graph (refname, link) ->
               try let hash = Graph.find link graph in Lwt.return (Graph.add refname hash graph)
               with Not_found -> Lwt.return graph)
            graph partial
          >|= fun graph -> Ok graph
        | Error #Packed_refs.error ->
          Lwt_list.fold_left_s
            (fun graph (refname, link) ->
               try let hash = Graph.find link graph in Lwt.return (Graph.add refname hash graph)
               with Not_found -> Lwt.return graph)
            graph partial
          >|= fun graph -> Ok graph

    let graph t = graph_p t ~dtmp:t.buffer.de ~raw:t.buffer.io

    let normalize graph = function
      | Reference.Hash hash -> Lwt.return (Ok hash)
      | Reference.Ref refname ->
        try Lwt.return (Ok (Graph.find refname graph))
        with Not_found -> Lwt.return (Error (`Invalid_reference refname))

    let list_p t ~dtmp ~raw =
      let open Lwt.Infix in

      graph_p t ~dtmp ~raw >>= function
      | Ok graph ->
        Graph.fold (fun refname hash acc -> (refname, hash) :: acc) graph []
        |> List.stable_sort (fun (a, _) (b, _) -> Reference.compare a b)
        |> fun lst -> Lwt.return lst
      | Error _ -> Lwt.return []

    let list_s t = list_p t ~dtmp:t.buffer.de ~raw:t.buffer.io

    let list = list_s

    let remove_p t ~dtmp ~raw ?locks reference =
      let open Lwt.Infix in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks Path.(t.dotgit / "references"))
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      (Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
        | Error _ -> Lwt.return None
        | Ok packed_refs ->
          Lwt_list.exists_p
            (function
              | `Peeled hash -> Lwt.return false
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
            Packed_refs.write ~root:t.dotgit ~raw packed_refs' >>= function
            | Ok () -> Lwt.return (Ok ())
            | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error)))
      >>= function
      | Error _ as err -> Lwt.return err
      | Ok () ->
        Reference.remove ~root:t.dotgit reference >>= function
        | Ok () -> Lwt.return (Ok ())
        | Error (#Reference.error as err) -> Lwt.return (Error (err : error))

    let remove_s t ?locks reference =
      remove_p t ?locks ~dtmp:t.buffer.de ~raw:t.buffer.io reference

    let remove = remove_s

    let read_p t ~dtmp ~raw reference =
      let open Lwt.Infix in

      FileSystem.is_file Path.(t.dotgit // (Reference.to_path reference)) >>= function
      | Ok true ->
        (Reference.read ~root:t.dotgit ~dtmp ~raw reference >|= function
         | Ok _ as v -> v
         | Error (#Reference.error as err) -> Error (err : error))
      | Ok false | Error _ ->
        Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
        | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))
        | Ok lst ->
          Lwt.catch
            (fun () -> Lwt_list.find_s
                (function `Peeled _ -> Lwt.return false
                        | `Ref (refname, hash) -> Lwt.return Reference.(equal (of_string refname) reference))
                lst >|= function `Ref (_, hash) -> Ok (reference, Reference.Hash hash)
                               | `Peeled _ -> assert false)
            (function exn -> Lwt.return (Error `Not_found))

    let read_s t reference =
      read_p t ~dtmp:t.buffer.de ~raw:t.buffer.io reference

    let read = read_s

    let write_p t ?locks ~dtmp ~raw reference value =
      let open Lwt.Infix in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks Path.(t.dotgit / "references"))
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      Reference.write ~root:t.dotgit ~raw reference value >>= function
      | Error (#Reference.error as err) -> Lwt.return (Error (err : error))
      | Ok () ->
        Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
        | Error _ -> Lwt.return (Ok ())
        | Ok packed_refs ->
          Lwt_list.exists_s (function `Peeled _ -> Lwt.return false
                                  | `Ref (refname, hash) -> Lwt.return Reference.(equal (of_string refname) reference))
            packed_refs
          >>= function
          | false -> Lwt.return (Ok ())
          | true ->
            Lwt_list.fold_left_s
              (fun acc -> function
                 | `Peeled _ as v -> Lwt.return (v :: acc)
                 | `Ref (refname, hash) when not Reference.(equal (of_string refname) reference) -> Lwt.return (`Ref (refname, hash) :: acc)
                 | _ -> Lwt.return acc)
              [] packed_refs
            >>= fun packed_refs' ->
            Packed_refs.write ~root:t.dotgit ~raw packed_refs' >>= function
            | Ok () -> Lwt.return (Ok ())
            | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))

    let write_s t ?locks reference value =
      write_p t ?locks ~dtmp:t.buffer.de ~raw:t.buffer.io reference value

    let write = write_s

    let unpack_reference t ~dtmp ~raw ?locks reference =
      let open Lwt.Infix in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks Path.(t.dotgit / "references"))
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
      | Error _ -> Lwt.return (Ok ())
      | Ok packed_refs ->
        Lwt_list.exists_s (function
            | `Peeled _ -> Lwt.return false
            | `Ref (refname, hash) -> Lwt.return Reference.(equal (of_string refname) reference))
          packed_refs >>= function
        | false -> Lwt.return (Ok ())
        | true ->
          Lwt_list.fold_left_s (fun (pi, acc) -> function
              | `Peeled hash -> Lwt.return (pi, `Peeled hash :: acc)
              | `Ref (refname, hash) when not Reference.(equal reference (of_string refname)) ->
                Lwt.return (pi, `Ref (refname, hash) :: acc)
              | `Ref (refname, hash) -> Lwt.return (Some (reference, hash), acc))
            (None, []) packed_refs
          >>= function
          | None, _ -> assert false
          (* XXX(dinosaure): we prove than reference is in packed_refs, so it's
             a mandatory to return a [Some v]. *)
          | (Some (_, hash), packed_refs') -> Packed_refs.write ~root:t.dotgit ~raw packed_refs
            >>= function
            | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))
            | Ok () -> Reference.write ~root:t.dotgit ~raw reference (Reference.Hash hash) >|= function
              | Ok () -> Ok ()
              | Error (#Reference.error as err) -> Error (err : error)

    let test_and_set t ?locks reference ~test ~set =
      let open Lwt.Infix in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks Path.(t.dotgit / "references"))
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      unpack_reference t ~dtmp:t.buffer.de ~raw:t.buffer.io reference >>= fun _ ->
      Reference.test_and_set ~root:t.dotgit reference ~test ~set >|= function
      | Error (#Reference.error as err) -> Error (err : error)
      | Ok _ as v -> v
  end

  let cache ?(indexes = 5) ?(packs = 5) ?(objects = 5) ?(values = 5) ?(revindexes = 5) () =
    { indexes    = CacheIndex.create indexes
    ; packs      = CachePack.create packs
    ; objects    = CacheObject.create objects
    ; values     = CacheValue.create values
    ; revindexes = CacheRevIndex.create revindexes }

  let buffer () =
    let raw = Cstruct.create (0x8000 * 3) in

    { window = Inflate.window ()
    ; zl = Cstruct.sub raw 0 0x8000
    ; de = Cstruct.sub raw 0x8000 0x8000
    ; io = Cstruct.sub raw (2 * 0x8000) 0x8000 }

  let sanitize_filesystem root dotgit =
    let open Lwt.Infix in

    let ( >>== ) v f = v >>= function
      | Ok v -> f v
      | Error _ as err -> Lwt.return err
    in

    FileSystem.Dir.create ~path:true root
    >>== fun _ ->
    FileSystem.Dir.create ~path:true dotgit
    >>== fun _ ->
    FileSystem.Dir.create ~path:true Path.(dotgit / "objects")
    >>== fun _ ->
    FileSystem.Dir.create ~path:true Path.(dotgit / "objects" / "pack")
    >>== fun _ -> Lwt.return (Ok ())

  let create ?root ?dotgit ?(compression = 4) () =
    let open Lwt.Infix in

    let ( >>== ) v f = v >>= function
      | Ok v -> f v
      | Error _ as err -> Lwt.return err
    in

    (match root, dotgit with
     | None, _ | _, None ->
       (FileSystem.Dir.current ()
        >>= function
        | Ok current ->
          let root = Option.get ~default:current root in
          let dotgit  = Option.get ~default:Path.(root / ".git") dotgit in

          sanitize_filesystem root dotgit
          >>== fun () -> indexes dotgit
          >>== fun idxs ->
          Lwt.return (Ok { dotgit
                         ; root
                         ; compression
                         ; idxs
                         ; cache = cache ()
                         ; buffer = buffer () })
        | Error sys_err -> Lwt.return (Error sys_err))
     | Some root, Some dotgit ->
       sanitize_filesystem root dotgit
       >>== fun () -> indexes dotgit
       >>== fun idxs ->
       Lwt.return (Ok { dotgit
                      ; root
                      ; compression
                      ; idxs
                      ; cache = cache ()
                      ; buffer = buffer () }))
    >>= function
    | Ok t -> Lwt.return (Ok t)
    | Error sys_err -> Lwt.return (Error (`SystemDirectory sys_err))

  let dotgit      { dotgit; _ }      = dotgit
  let root        { root; _ }        = root
  let compression { compression; _ } = compression

  let buffer_window { buffer; _ } = buffer.window
  let buffer_zl { buffer; _ } = buffer.zl
  let buffer_de { buffer; _ } = buffer.de
  let buffer_io { buffer; _ } = buffer.io

  (* XXX(dinosaure): clash of name with [indexes] below. *)
  let indexes t = List.map fst t.idxs
end
