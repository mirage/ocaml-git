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
let ( >!= ) = Lwt_result.bind_lwt_err
let ( >?= ) = Lwt_result.bind

module type LOOSE = sig

  type t
  type state

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

  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | Value.D.error
    | Value.E.error ]

  val pp_error: error Fmt.t
  val lookup_p: state -> Hash.t -> Hash.t option Lwt.t
  val lookup: state -> Hash.t -> Hash.t option Lwt.t
  val mem: state -> Hash.t -> bool Lwt.t
  val list: state -> Hash.t list Lwt.t
  val read_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (t, error) result Lwt.t
  val read_s: state -> Hash.t -> (t, error) result Lwt.t
  val read: state -> Hash.t -> (t, error) result Lwt.t
  val size_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (int64, error) result Lwt.t
  val size_s: state -> Hash.t -> (int64, error) result Lwt.t
  val size: state -> Hash.t -> (int64, error) result Lwt.t
  val write_p:
       ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> state -> t -> (Hash.t * int, error) result Lwt.t
  val write_s: state -> t -> (Hash.t * int, error) result Lwt.t
  val write: state -> t -> (Hash.t * int, error) result Lwt.t
  val write_inflated: state -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  val raw_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> window:Inflate.window
    -> raw:Cstruct.t
    -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  val raw_s: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  val raw: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  val raw_wa:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> window:Inflate.window
    -> raw:Cstruct.t
    -> result:Cstruct.t
    -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  val raw_was: Cstruct.t -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  module D: S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = [ `Decoder of string | `Inflate of Inflate.error ]

  module E: S.ENCODER
    with type t = t
     and type init = int * t * int * Cstruct.t
     and type error = [ `Deflate of Deflate.error ]
end

module type PACK = sig

  type t
  type value
  type state

  module Hash: S.HASH
  module FS: S.FS
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module PACKDecoder: Unpack.DECODER
    with module Hash = Hash
     and module Inflate = Inflate
  module PACKEncoder: Pack.ENCODER
    with module Hash = Hash
     and module Deflate = Deflate
  module IDXDecoder: Index_pack.LAZY
    with module Hash = Hash
  module IDXEncoder: Index_pack.ENCODER
    with module Hash = Hash
  module Pack_info: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate

  type error =
    [ `PackDecoder of PACKDecoder.error
    | `PackEncoder of PACKEncoder.error
    | `PackInfo of Pack_info.error
    | `IdxDecoder of IDXDecoder.error
    | `IdxEncoder of IDXEncoder.error
    | `SystemFile of FS.File.error
    | `SystemMapper of FS.Mapper.error
    | `SystemDir of FS.Dir.error
    | `Invalid_hash of Hash.t
    | `Delta of PACKEncoder.Delta.error
    | `SystemIO of string
    | `Integrity of string
    | `Not_found ]

  val pp_error: error Fmt.t
  val lookup: state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  val mem: state -> Hash.t -> bool Lwt.t
  val list: state -> Hash.t list Lwt.t
  val read_p:
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (t, error) result Lwt.t
  val read_s: state -> Hash.t -> (t, error) result Lwt.t
  val read: state -> Hash.t -> (t, error) result Lwt.t
  val size_p:
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (int, error) result Lwt.t
  val size_s: state -> Hash.t -> (int, error) result Lwt.t
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
  module Lock: S.LOCK

  module FS: S.FS with type File.lock = Lock.elt

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
     and module Lock = Lock
     and module FS = FS

  module PACKDecoder: Unpack.DECODER
    with module Hash = Hash
     and module Inflate = Inflate

  module PACKEncoder: Pack.ENCODER
    with module Hash = Hash
     and module Deflate = Deflate

  module Loose: LOOSE
    with type t = Value.t
     and type state = t
     and module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module FS = FS

  module Pack: PACK
    with type t = PACKDecoder.Object.t
     and type value = Value.t
     and type state = t
     and module Hash = Hash
     and module FS = FS
     and module Inflate = Inflate

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  type error =
    [ Loose.error
    | Pack.error ]

  val pp_error: error Fmt.t
  val create:
       ?root:Fpath.t
    -> ?dotgit:Fpath.t
    -> ?compression:int
    -> unit -> (t, error) result Lwt.t
  val dotgit: t -> Fpath.t
  val root: t -> Fpath.t
  val compression : t -> int
  val mem: t -> Hash.t -> bool Lwt.t
  val list: t -> Hash.t list Lwt.t
  val read_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (Value.t, error) result Lwt.t
  val read_s: t -> Hash.t -> (Value.t, error) result Lwt.t
  val read: t -> Hash.t -> (Value.t, error) result Lwt.t
  val read_exn: t -> Hash.t -> Value.t Lwt.t
  val write_p:
       ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val write_s: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val write: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  val size_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (int64, error) result Lwt.t
  val size_s: t -> Hash.t -> (int64, error) result Lwt.t
  val size: t -> Hash.t -> (int64, error) result Lwt.t
  val raw_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val raw_s: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val raw: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val read_inflated: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val write_inflated: t -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t
  val buffer_window: t -> Inflate.window
  val buffer_zl: t -> Cstruct.t
  val buffer_de: t -> Cstruct.t
  val buffer_io: t -> Cstruct.t

  val fold:
       t
    -> ('a -> ?name:Fpath.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t)
    -> path:Fpath.t
    -> 'a
    -> Hash.t
    -> 'a Lwt.t

  module Ref: sig

    module Packed_refs: Packed_refs.S
      with module Hash = Hash
       and module FS = FS

    type nonrec error =
      [ Packed_refs.error
      | error
      | `Invalid_reference of Reference.t ]

    val pp_error: error Fmt.t
    val mem: t -> Reference.t -> bool Lwt.t
    val graph_p:
         dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> ?locks:Lock.t
      -> t -> (Hash.t Reference.Map.t, error) result Lwt.t
    val graph: ?locks:Lock.t -> t -> (Hash.t Reference.Map.t, error) result Lwt.t
    val normalize: Hash.t Reference.Map.t -> Reference.head_contents -> (Hash.t, error) result Lwt.t
    val list_p:
         dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> ?locks:Lock.t
      -> t -> (Reference.t * Hash.t) list Lwt.t
    val list_s: ?locks:Lock.t -> t -> (Reference.t * Hash.t) list Lwt.t
    val list: ?locks:Lock.t -> t -> (Reference.t * Hash.t) list Lwt.t
    val remove_p:
         dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> ?locks:Lock.t
      -> t -> Reference.t -> (unit, error) result Lwt.t
    val remove_s: t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    val remove: t -> ?locks:Lock.t -> Reference.t -> (unit, error) result Lwt.t
    val read_p:
         dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> ?locks:Lock.t
      -> t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val read_s: ?locks:Lock.t -> t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val read: ?locks:Lock.t -> t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    val write_p:
         ?locks:Lock.t
      -> dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val write_s: t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val write: t -> ?locks:Lock.t -> Reference.t -> Reference.head_contents -> (unit, error) result Lwt.t
    val test_and_set:
        t
     -> ?locks:Lock.t
     -> Reference.t
     -> test:Reference.head_contents option
     -> set:Reference.head_contents option
     -> (bool, error) result Lwt.t
  end

  val clear_caches: ?locks:Lock.t -> t -> unit Lwt.t
  val reset: ?locks:Lock.t -> t -> (unit, [ `Store of error | `Ref of Ref.error ]) result Lwt.t
end

module Option =
struct
  let get ~default = function Some x -> x | None -> default
  let map f a = match a with Some a -> Some (f a) | None -> None
end

module Make
    (H: S.HASH)
    (L: S.LOCK)
    (FS: S.FS with type File.lock = L.elt)
    (I: S.INFLATE)
    (D: S.DEFLATE)
  : S with module Hash = H
       and module Lock = L
       and module FS = FS
       and module Inflate = I
       and module Deflate = D
= struct
  module Hash = H
  module Inflate = I
  module Deflate = D
  module Lock = L
  module FS = FS

  module LooseImpl: Loose.S
    with module Hash = H
     and module Inflate = I
     and module Deflate = D
     and module FS = FS
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t
    = Loose.Make(Hash)(FS)(Inflate)(Deflate)

  module PackImpl
    : Pack_engine.S
      with module Hash = Hash
       and module FS = FS
       and module Inflate = Inflate
       and module Deflate = Deflate
    = Pack_engine.Make(H)(FS)(I)(D)

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

  module PACKDecoder = PackImpl.PACKDecoder
  module PACKEncoder = PackImpl.PACKEncoder
  module IDXDecoder = PackImpl.IDXDecoder

  module Reference = Reference.IO(H)(L)(FS)

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

  (* XXX(dinosaure): need to limit the weight of [CacheObject] and
     [CacheValue] by the memory consumption of the data stored - and
     not by the number of theses data. Indeed, 5 commits is more
     cheaper than 1 blob sometimes. *)
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
    { dotgit      : Fpath.t
    ; root        : Fpath.t
    ; compression : int
    ; cache       : cache
    ; buffer      : buffer
    ; packed      : (Reference.t, Hash.t) Hashtbl.t
    ; engine      : PackImpl.t }

  module Loose = struct
    module Hash = Hash
    module FS = FS
    module Inflate = Inflate
    module Deflate = Deflate
    module Value = Value

    type state = t
    type t = LooseImpl.t

    type kind = LooseImpl.kind

    type error = LooseImpl.error

    let pp_error = LooseImpl.pp_error

    let read_p ~ztmp ~dtmp ~raw ~window t =
      LooseImpl.read ~root:t.dotgit ~window ~ztmp ~dtmp ~raw
    let size_p ~ztmp ~dtmp ~raw ~window t =
      LooseImpl.size ~root:t.dotgit ~window ~ztmp ~dtmp ~raw
    let write_p ~ztmp ~raw t value =
      LooseImpl.write ~root:t.dotgit ~ztmp ~raw ~level:t.compression value

    let mem t =
      LooseImpl.mem
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

    let lookup_p t hash =
      LooseImpl.mem ~root:t.dotgit hash >|= function
      | true -> Some hash
      | false -> None

    let lookup = lookup_p

    let raw_p ~ztmp ~dtmp ~window ~raw t hash =
      LooseImpl.inflate ~root:t.dotgit ~window ~ztmp ~dtmp ~raw hash

    let raw_s t hash =
      raw_p
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~dtmp:t.buffer.de
        ~raw:t.buffer.io
        t hash

    let raw = raw_s

    let raw_wa ~ztmp ~dtmp ~window ~raw ~result t hash =
      LooseImpl.inflate_wa ~root:t.dotgit ~window ~ztmp ~dtmp ~raw ~result hash

    let raw_was result t hash =
      raw_wa
        ~window:t.buffer.window
        ~ztmp:t.buffer.zl
        ~dtmp:t.buffer.de
        ~raw:t.buffer.io
        ~result
        t hash

    let write_inflated t ~kind value =
      LooseImpl.write_inflated
        ~root:t.dotgit
        ~level:t.compression
        ~raw:t.buffer.io
        ~kind
        value >>= function
      | Ok hash -> Lwt.return hash
      | Error (#LooseImpl.error as err) ->
        Lwt.fail (Failure (Fmt.strf "%a" LooseImpl.pp_error err))

    module D: S.DECODER
        with type t = t
         and type init = Inflate.window * Cstruct.t * Cstruct.t
         and type error = Value.D.error
      = Value.D

    module E: S.ENCODER
        with type t = t
         and type init = int * t * int * Cstruct.t
         and type error = Value.E.error
      = Value.E
  end

  module Pack = struct

    module Hash = Hash
    module FS = FS
    module Inflate = Inflate
    module Deflate = Deflate

    module Pack_info = PackImpl.Pack_info
    module IDXEncoder = PackImpl.IDXEncoder
    module IDXDecoder = PackImpl.IDXDecoder
    module PACKEncoder = PackImpl.PACKEncoder
    module PACKDecoder = PackImpl.PACKDecoder

    module Log =
    struct
      let src = Logs.Src.create "git.store.pack" ~doc:"logs git's store event (pack)"
      include (val Logs.src_log src : Logs.LOG)
    end

    type state = t

    type error =
      [ `SystemIO of string
      | `Delta of PACKEncoder.Delta.error
      | PackImpl.error ]

    let pp_error ppf = function
      | `SystemIO err -> Fmt.pf ppf "(`SystemIO %s)" err
      | `Delta err -> Fmt.pf ppf "(`Delta %a)" PACKEncoder.Delta.pp_error err
      | #PackImpl.error as err -> PackImpl.pp_error ppf err

    type t = PACKDecoder.Object.t
    type value = Value.t

    let lookup t hash =
      PackImpl.lookup t.engine hash

    let mem t hash =
      PackImpl.mem t.engine hash

    let list t =
      PackImpl.list t.engine

    let read_p ~ztmp ~window t hash =
      mem t hash >>= function
      | false -> Lwt.return (Error `Not_found)
      | true ->
        Log.debug (fun l -> l "Git object %a found in a PACK file."
                      Hash.pp hash);

        PackImpl.read ~root:t.dotgit ~ztmp ~window t.engine hash
        >!= (fun err -> Lwt.return (err :> error))

    let read_s t hash =
      read_p ~ztmp:t.buffer.zl ~window:t.buffer.window t hash

    let read = read_s

    let size_p ~ztmp ~window t hash =
      mem t hash >>= function
      | false -> Lwt.return (Error `Not_found)
      | true ->
        PackImpl.size ~root:t.dotgit ~ztmp ~window t.engine hash
        >!= (fun err -> Lwt.return (err :> error))

    let size_s t hash =
      size_p
        ~ztmp:t.buffer.zl
        ~window:t.buffer.window
        t hash

    let size = size_s

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

    let to_temp_file fmt stream =
      let filename_of_pack = fmt (random_string 10) in
      FS.Dir.temp () >>= fun temp_dir ->
      FS.File.open_w ~mode:0o644 Fpath.(temp_dir / filename_of_pack) >>= function
      | Error err -> Lwt.return (Error (`SystemFile err))
      | Ok fd ->
        Log.debug (fun l -> l ~header:"to_temp_file" "Save the pack stream to the file %a."
                      Fpath.pp Fpath.(temp_dir / filename_of_pack));

        let rec go ?chunk ~call () = Lwt_stream.peek stream >>= function
          | None ->
            Log.debug (fun l -> l ~header:"to_temp_file" "Pack stream saved to the file %a."
                          Fpath.pp Fpath.(temp_dir / filename_of_pack));

            (FS.File.close fd >>= function
              | Ok () -> Lwt.return (Ok Fpath.(temp_dir / filename_of_pack))
              | Error err ->
                Log.err (fun l -> l ~header:"to_temp_file" "Cannot close the file: %s." filename_of_pack);
                Lwt.return (Error (`SystemFile err)))
          | Some raw ->
            Log.debug (fun l -> l ~header:"to_temp_file" "Receive a chunk of the pack stream (length: %d)."
                          (Cstruct.len raw));

            let off, len = match chunk with
              | Some (off, len) -> off, len
              | None -> 0, Cstruct.len raw
            in

            FS.File.write raw ~off ~len fd >>= function
            | Ok 0 when len <> 0 ->
              if call = 50 (* XXX(dinosaure): as argument? *)
              then
                let err = Fmt.strf "Impossible to store the file: %s." filename_of_pack in

                FS.File.close fd >>= function
                | Ok () -> Lwt.return (Error (`SystemIO err))
                | Error _ ->
                  Log.err (fun l -> l ~header:"to_temp_file" "Cannot close the file: %s." filename_of_pack);
                  Lwt.return (Error (`SystemIO err))
              else go ?chunk ~call:(call + 1) ()
            | Ok n when n = len ->
              Log.debug (fun l -> l ~header:"to_temp_file" "Consume current chunk of the pack stream.");
              Lwt_stream.junk stream >>= fun () -> go ~call:0 ()
            | Ok n ->
              let chunk = (off + n, len - n) in
              go ~chunk ~call:0 ()
            | Error err ->
              FS.File.close fd >>= function
              | Ok () -> Lwt.return (Error (`SystemFile err))
              | Error _ ->
                Log.err (fun l -> l ~header:"to_temp_file" "Cannot close the file: %s." filename_of_pack);
                Lwt.return (Error (`SystemFile err))
        in

        go ~call:0 ()

    let extern git hash =
      read_p
        ~ztmp:git.buffer.zl
        ~window:git.buffer.window
        git hash >>= function
      | Ok o ->
        Lwt.return (Some (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
      | Error _ -> Loose.lookup git hash >>= function
        | None -> Lwt.return None
        | Some _ ->
          Loose.raw_p
            ~window:git.buffer.window
            ~ztmp:git.buffer.zl
            ~dtmp:git.buffer.de
            ~raw:git.buffer.io
            git hash >>= function
          | Error #Loose.error -> Lwt.return None
          | Ok v -> Lwt.return (Some v)

    module GC =
      Gc.Make(struct
        module Hash = Hash
        module Value = Value
        module Deflate = Deflate
        module PACKEncoder = PACKEncoder

        type nonrec t = state
        type nonrec error = error
        type kind = PACKDecoder.kind

        let pp_error = pp_error
        let read_inflated = extern
        let contents _ = assert false
      end)

    module Graph = GC.Graph

    let make = GC.make_stream

    let canonicalize git path_pack decoder_pack fdp ~htmp ~rtmp ~ztmp ~window delta info =
      let k2k = function
        | `Commit -> Pack.Kind.Commit
        | `Blob -> Pack.Kind.Blob
        | `Tree -> Pack.Kind.Tree
        | `Tag -> Pack.Kind.Tag
      in
      let make acc (hash, (_, offset)) =
        PACKDecoder.optimized_get' ~h_tmp:htmp decoder_pack offset rtmp ztmp window >>= function
        | Error err ->
          Log.err (fun l -> l ~header:"from" "Retrieve an error when we try to \
                                              resolve the object at the offset %Ld \
                                              in the temporary pack file %a: %a."
                      offset Fpath.pp path_pack PACKDecoder.pp_error err);
          Lwt.return acc
        | Ok obj ->
          let delta = match obj.PACKDecoder.Object.from with
            | PACKDecoder.Object.External hash -> Some (PACKEncoder.Entry.From hash)
            | PACKDecoder.Object.Direct _ -> None
            | PACKDecoder.Object.Offset { offset; _ } ->
              try let (_, hash) = Pack_info.Graph.find offset info.Pack_info.graph in
                Option.map (fun hash -> PACKEncoder.Entry.From hash) hash
              with Not_found -> None
          in

          Lwt.return (PACKEncoder.Entry.make hash ?delta (k2k obj.PACKDecoder.Object.kind) obj.PACKDecoder.Object.length :: acc)
      in

      let external_ressources acc =
        List.fold_left
          (fun acc (_, hunks_descr) ->
             let open Pack_info in

             match hunks_descr.PACKDecoder.H.reference with
             | PACKDecoder.H.Hash hash when not (Radix.mem info.tree hash) ->
               (try List.find (Hash.equal hash) acc |> fun _ -> acc
                with Not_found -> hash :: acc)
             | _ -> acc)
          [] delta
        |> Lwt_list.fold_left_s
          (fun acc hash -> extern git hash >>= function
             | None ->
               Lwt.return acc
             | Some (kind, raw) ->
               let entry = PACKEncoder.Entry.make hash (k2k kind) (Int64.of_int (Cstruct.len raw)) in
               Lwt.return (entry :: acc))
          acc
      in

      let get hash =
        if Pack_info.Radix.mem info.Pack_info.tree hash
        then PACKDecoder.get_with_allocation
            ~h_tmp:htmp
            decoder_pack
            hash
            ztmp window >>= function
          | Error _ ->
            Lwt.return None
          | Ok obj -> Lwt.return (Some obj.PACKDecoder.Object.raw)
        else extern git hash >|= function
          | Some (_, raw) -> Some raw
          | None -> None
      in

      let tag _ = false in

      Pack_info.Radix.to_list info.Pack_info.tree
      |> Lwt_list.fold_left_s make []
      >>= external_ressources
      >>= fun entries -> PACKEncoder.Delta.deltas ~memory:false entries get tag 10 50
      >>= function
      | Error err -> Lwt.return (Error (`Delta err))
      | Ok entries ->
        PackImpl.save_pack_file
          (Fmt.strf "pack-%s.pack")
          entries
          (fun hash ->
             if Pack_info.Radix.mem info.Pack_info.tree hash
             then PACKDecoder.get_with_allocation
                 ~h_tmp:htmp
                 decoder_pack
                 hash
                 ztmp window >>= function
               | Error _ ->
                 Lwt.return None
               | Ok obj ->
                 Lwt.return (Some (obj.PACKDecoder.Object.raw))
             else extern git hash >|= function
               | Some (_, raw) -> Some raw
               | None -> None)
          >>= function
          | Error err -> Lwt.return (Error (err :> error))
          | Ok (path, sequence, hash_pack) ->
            PackImpl.save_idx_file ~root:git.dotgit sequence hash_pack >>= function
            | Error err -> Lwt.return (Error (err :> error))
            | Ok () ->
              let filename_pack = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in

              (FS.File.move path Fpath.(git.dotgit / "objects" / "pack" / filename_pack) >>= function
              | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
              | Ok () -> Lwt.return (Ok (hash_pack, List.length entries)))
              >>= fun ret ->
              FS.Mapper.close fdp >>= function
              | Error sys_err ->
                Log.err (fun l -> l ~header:"canonicalize" "Impossible to close the pack file %a: %a."
                            Fpath.pp path_pack FS.Mapper.pp_error sys_err);
                Lwt.return ret
              | Ok () -> Lwt.return ret

    let from git stream =
      let ztmp = Cstruct.create 0x8000 in
      let window = Inflate.window () in

      let stream0, stream1 =
        let stream', push' = Lwt_stream.create () in

        let cstruct_copy cs =
          let ln = Cstruct.len cs in
          let rs = Cstruct.create ln in
          Cstruct.blit cs 0 rs 0 ln;
          rs
        in

        Lwt_stream.from
          (fun () -> stream () >>= function
             | Some raw ->
               Log.debug (fun l -> l ~header:"from" "Dispatch a chunk of the PACK stream (length: %d)."
                             (Cstruct.len raw));
               push' (Some (cstruct_copy raw));
               Lwt.return (Some raw)
             | None ->
               Log.debug (fun l -> l ~header:"from" "Dispatch end of the PACK stream.");
               push' None;
               Lwt.return None),
        stream'
      in

      let info = Pack_info.v (Hash.of_hex (String.make (Hash.Digest.length * 2) '0')) in

      (Pack_info.from_stream ~ztmp ~window info (fun () -> Lwt_stream.get stream0)
       >!= (fun sys_err -> Lwt.return (`PackInfo sys_err))) >?= fun info ->
      to_temp_file (Fmt.strf "pack-%s.pack") stream1 >?= fun path ->

      let module Graph = Pack_info.Graph in

      FS.Mapper.openfile path >>= function
      | Error err -> Lwt.return (Error (`SystemMapper err))
      | Ok fdp ->
        let `Partial { Pack_info.Partial.hash = hash_pack;
                       Pack_info.Partial.delta; } = info.Pack_info.state
        in

        let htmp =
          let raw = Cstruct.create (info.Pack_info.max_length_insert_hunks * (info.Pack_info.max_depth + 1)) in
          Array.init
            (info.Pack_info.max_depth + 1)
            (fun i -> Cstruct.sub raw (i * info.Pack_info.max_length_insert_hunks) info.Pack_info.max_length_insert_hunks)
        in

        let rtmp =
          Cstruct.create info.Pack_info.max_length_object,
          Cstruct.create info.Pack_info.max_length_object,
          info.Pack_info.max_length_object
        in

        PACKDecoder.make fdp
          (fun _ -> None)
          (fun hash -> Pack_info.Radix.lookup info.Pack_info.tree hash)
          (* XXX(dinosaure): this function will be updated. *)
          (fun _ -> None)
          (fun hash -> extern git hash)
        >>= function
        | Error err ->
          Lwt.return (Error (`SystemMapper err))
        | Ok decoder ->
          let hash_of_object obj =
            let ctx = Hash.Digest.init () in
            let hdr = Fmt.strf "%s %Ld\000"
                (match obj.PACKDecoder.Object.kind with
                 | `Commit -> "commit"
                 | `Blob   -> "blob"
                 | `Tree   -> "tree"
                 | `Tag    -> "tag")
                obj.PACKDecoder.Object.length
            in

            Hash.Digest.feed ctx (Cstruct.of_string hdr);
            Hash.Digest.feed ctx obj.PACKDecoder.Object.raw;
            Hash.Digest.get ctx
          in

          let crc obj = match obj.PACKDecoder.Object.from with
            | PACKDecoder.Object.Offset { crc; _ } -> crc
            | PACKDecoder.Object.External _ ->
              raise (Invalid_argument "Try to get the CRC-32 checksum from an external ressource.")
            | PACKDecoder.Object.Direct { crc; _ } -> crc
          in

          Lwt_list.fold_left_s
            (fun ((decoder, tree, graph) as acc) (offset, hunks_descr) ->
               PACKDecoder.optimized_get'
                 ~h_tmp:htmp
                 decoder
                 offset
                 rtmp ztmp window >>= function
               | Ok obj ->
                 let hash = hash_of_object obj in
                 let crc = crc obj in
                 let tree = Pack_info.Radix.bind tree hash (crc, offset) in

                 let graph =
                   let open Pack_info in

                   let depth_source, _ = match hunks_descr.PACKDecoder.H.reference with
                     | PACKDecoder.H.Offset rel_off ->
                       (try Graph.find Int64.(sub offset rel_off) graph
                        with Not_found -> 0, None)
                     | PACKDecoder.H.Hash hash_source ->
                       try match Radix.lookup tree hash_source with
                         | Some (_, abs_off) -> Graph.find abs_off graph
                         | None -> 0, None
                       with Not_found -> 0, None
                   in

                   Graph.add offset (depth_source + 1, Some hash) graph
                 in

                 Lwt.return
                   (PACKDecoder.update_idx (Pack_info.Radix.lookup tree) decoder,
                    tree, graph)
               | Error err ->
                 Log.err (fun l -> l ~header:"from" "Retrieve an error when we try to \
                                                     resolve the object at the offset %Ld \
                                                     in the temporary pack file %a: %a."
                             offset Fpath.pp path PACKDecoder.pp_error err);
                 Lwt.return acc)
            (decoder, info.Pack_info.tree, info.Pack_info.graph) delta
          >>= fun (decoder, tree', graph') ->

          let is_total =
            Pack_info.Graph.for_all
              (fun _ -> function (_, Some _) -> true | (_, None) -> false)
              graph'
          in

          if is_total
          then
            Lwt_list.for_all_p
              (fun (_, hunks_descr) ->
                 let open Pack_info in

                 match hunks_descr.PACKDecoder.H.reference with
                 | PACKDecoder.H.Offset _ -> Lwt.return true
                 | PACKDecoder.H.Hash hash ->
                   Lwt.return (Radix.mem tree' hash))
              delta
            >>= fun is_not_thin ->
            if is_not_thin
            then begin
              let info =
                { info with Pack_info.tree = tree'
                          ; Pack_info.graph = graph'
                          ; Pack_info.state =
                              `Full { Pack_info.Full.thin = not is_not_thin
                                    ; Pack_info.Full.hash = hash_pack } }
              in

              (FS.Mapper.close fdp
               >!= fun sys_err -> Lwt.return (`SystemMapper sys_err))
              >?= fun () -> PackImpl.add_total ~root:git.dotgit git.engine path info
                            >!= fun err -> Lwt.return (err :> error)
            end else
              canonicalize git path decoder fdp ~htmp ~rtmp ~ztmp ~window delta info
              >?= fun (hash, count) ->
              (PackImpl.add_exists ~root:git.dotgit git.engine hash
               >!= (fun err -> Lwt.return (err :> error)))
              >?= fun () -> Lwt.return (Ok (hash, count))
          else Lwt.return
              (Error (`Integrity (Fmt.strf "Impossible to get all informations from the file: %a."
                                    Hash.pp hash_pack)))
  end

  module Log =
  struct
    let src = Logs.Src.create "git.store" ~doc:"logs git's store event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type error = [ Loose.error | Pack.error ]

  type kind =
    [ `Commit
    | `Blob
    | `Tree
    | `Tag ]

  let pp_error ppf = function
    | #Loose.error as err -> Fmt.pf ppf "%a" Loose.pp_error err
    | #Pack.error as err -> Fmt.pf ppf "%a" Pack.pp_error err

  let read_p ~ztmp ~dtmp ~raw ~window state hash =
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
    Log.debug (fun l -> l ~header:"read_s" "Request to read %a in the current Git repository." Hash.pp hash);

    read_p
      ~ztmp:t.buffer.zl
      ~dtmp:t.buffer.de
      ~raw:t.buffer.io
      ~window:t.buffer.window
      t hash

  let read =
    Log.debug (fun l -> l ~header:"read" "Use the alias of read_s");
    read_s

  let read_exn t hash =
    read t hash >>= function
    | Error _ ->
      let err = Fmt.strf "Git.Store.read_exn: %a not found" Hash.pp hash in
      Lwt.fail (Invalid_argument err)
    | Ok v -> Lwt.return v

  let write_p ~ztmp ~raw state hash =
    Loose.write_p ~ztmp ~raw state hash >|= function
    | Error (#LooseImpl.error as err) -> Error (err :> error)
    | Ok v -> Ok v

  let write_s state hash =
    Loose.write_s state hash >|= function
    | Error (#LooseImpl.error as err) -> Error (err :> error)
    | Ok v -> Ok v

  let write = write_s

  let raw_p ~ztmp ~dtmp ~raw ~window state hash =
    Pack.read_p ~ztmp ~window state hash >>= function
    | Ok o ->
      Lwt.return (Some (o.PACKDecoder.Object.kind, o.PACKDecoder.Object.raw))
    | Error _ -> Loose.lookup state hash >>= function
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

  let read_inflated t hash =
    raw_s t hash

  let write_inflated t ~kind value =
    Loose.write_inflated t ~kind value

  let indexes git =
    FS.Dir.contents ~dotfiles:false ~rel:false Fpath.(git / "objects" / "pack")[@warning "-44"]
    >>= function
    | Ok lst ->
      Lwt_list.fold_left_s
        (fun acc path ->
           if Fpath.has_ext "idx" path
           then Lwt.return (path :: acc)
           else Lwt.return acc)
        [] lst
      >>= PackImpl.v >|= fun v -> Ok v
    | Error err -> Lwt.return (Error err)

  let lookup_p state hash =
    Pack.lookup state hash
    >>= function
    | Some (hash_pack, (_, offset)) -> Lwt.return (`PackDecoder (hash_pack, offset))
    | None -> Loose.lookup state hash >>= function
      | Some _ -> Lwt.return `Loose
      | None -> Lwt.return `Not_found

  let lookup = lookup_p

  let mem state hash =
    lookup state hash >|= function
    | `Not_found -> false
    | _ -> true

  let list state =
    Loose.list state
    >>= fun looses -> Pack.list state
    >|= fun packed -> List.append looses packed

  let size_p ~ztmp ~dtmp ~raw ~window state hash =
    Pack.size_p ~ztmp ~window state hash >>= function
    | Ok v -> Lwt.return (Ok (Int64.of_int v))
    | Error (#Pack.error as err) ->
      Loose.mem state hash >>= function
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
    list state
    >>= fun lst ->
    Lwt.try_bind
      (fun () -> Lwt_list.map_s
          (fun hash ->
             Log.debug (fun l -> l ~header:"contents" "Try to read the object: %a." Hash.pp hash);

             read state hash
             >>= function
             | Ok v -> Lwt.return (hash, v)
             | Error err ->
               Log.err (fun l -> l ~header:"contents" "Retrieve an error: %a." pp_error err);
               Lwt.fail (Leave err))
          lst)
      (fun lst -> Lwt.return (Ok lst))
      (function Leave err -> Lwt.return (Error err)
              | exn -> Lwt.fail exn)

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

  module Ref =
  struct
    module Packed_refs = Packed_refs.Make(Hash)(FS)
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

    let contents ?locks top =

      let rec lookup acc dir =
        FS.Dir.contents ~rel:true Fpath.(top // dir)
        >?= fun l ->
          Lwt_list.filter_p
            (fun x -> FS.is_dir Fpath.(top // dir // x) >|= function Ok v -> v | Error _ -> false) l
          >>= fun dirs ->
          Lwt_list.filter_p
            (fun x -> FS.is_file Fpath.(top // dir // x) >|= function Ok v -> v | Error _ -> false) l
          >>= Lwt_list.map_p (fun file -> Lwt.return (Fpath.append dir file)) >>= fun files ->

          Lwt_list.fold_left_s
            (function Ok acc -> fun x -> lookup acc Fpath.(dir // x)
                    | Error _ as e -> fun _ -> Lwt.return e)
            (Ok acc) dirs >?= fun acc -> Lwt.return (Ok (acc @ files))
      in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))
        | None -> None
      in

      Lock.with_lock lock
      (fun () -> lookup [] (Fpath.v "."))

    module Graph = Reference.Map

    module Log =
    struct
      let src = Logs.Src.create "git.store.ref" ~doc:"logs git's store reference event"
      include (val Logs.src_log src : Logs.LOG)
    end

    (* XXX(dinosaure): this function does not return any {!Error} value. *)
    let graph_p ~dtmp ~raw ?locks t =
      let lock_gbl = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))
        | None -> None
      in

      let lock_ref reference = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v ("r-" ^ (Fpath.filename (Reference.to_path reference)))))
        | None -> None
      in

      contents ?locks Fpath.(t.dotgit / "refs") >>= function
      | Error sys_err ->
        Log.err (fun l -> l ~header:"graph_p" "Retrieve an error: %a." FS.Dir.pp_error sys_err);
        Lwt.return (Error (`SystemDirectory sys_err))
      | Ok files ->
        Log.debug (fun l -> l ~header:"graph_p" "Retrieve these files: %a."
                      (Fmt.hvbox (Fmt.list Fpath.pp)) files);

        Lwt_list.fold_left_s
          (fun acc abs_ref ->
             (* XXX(dinosaure): we already normalize the reference (which is
                absolute). so we consider than the root as [/]. *)
             Lock.with_lock (lock_ref (Reference.of_path abs_ref))
               (fun () -> Reference.read ~root:t.dotgit (Reference.of_path abs_ref) ~dtmp ~raw)
             >|= function
             | Ok v -> v :: acc
             | Error err ->
               Log.err (fun l -> l ~header:"graph_p" "Retrieve an error when we read reference %a: %a."
                           Reference.pp (Reference.of_path abs_ref)
                           Reference.pp_error err);
               acc)
          [] (Reference.(to_path head) :: files)
        >>= fun lst -> Lwt_list.fold_left_s
          (fun (rest, graph) -> function
             | refname, Reference.Hash hash ->
               Lwt.return (rest, Graph.add refname hash graph)
             | refname, Reference.Ref link ->
               Log.debug (fun l -> l ~header:"graph_p" "Putting the reference %a -> %a as a partial value."
                             Reference.pp refname Reference.pp link);
               Lwt.return ((refname, link) :: rest, graph))
          ([], Graph.empty) lst
        >>= fun (partial, graph) ->
        Lock.with_lock lock_gbl
          (fun () -> Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
             | Ok packed_refs ->
               Hashtbl.reset t.packed;
               List.iter (function
                   | `Ref (refname, hash) ->
                     Hashtbl.add t.packed (Reference.of_string refname) hash
                   | `Peeled _ -> ())
                 packed_refs;
               Lwt.return (Ok packed_refs)
             | Error _ as err -> Lwt.return err)
        >>= function
        | Ok packed_refs ->
          Lwt_list.fold_left_s
            (fun graph -> function
               | `Peeled _ -> Lwt.return graph
               | `Ref (refname, hash) -> Lwt.return (Graph.add (Reference.of_string refname) hash graph))
            graph packed_refs
          >>= fun graph -> Lwt_list.fold_left_s
            (fun graph (refname, link) ->
               Log.debug (fun l -> l ~header:"graph_p" "Resolving the reference %a -> %a."
                             Reference.pp refname Reference.pp link);

               try let hash = Graph.find link graph in Lwt.return (Graph.add refname hash graph)
               with Not_found -> Lwt.return graph)
            graph partial
          >|= fun graph -> Ok graph
        | Error #Packed_refs.error ->
          Lwt_list.fold_left_s
            (fun graph (refname, link) ->
               Log.debug (fun l -> l ~header:"graph_p" "Resolving the reference %a -> %a."
                             Reference.pp refname Reference.pp link);

               try let hash = Graph.find link graph in Lwt.return (Graph.add refname hash graph)
               with Not_found -> Lwt.return graph)
            graph partial
          >|= fun graph -> Ok graph
    [@@warning "-44"]

    let graph ?locks t = graph_p ~dtmp:t.buffer.de ~raw:t.buffer.io ?locks t

    let normalize graph = function
      | Reference.Hash hash -> Lwt.return (Ok hash)
      | Reference.Ref refname ->
        try Lwt.return (Ok (Graph.find refname graph))
        with Not_found -> Lwt.return (Error (`Invalid_reference refname))

    let list_p ~dtmp ~raw ?locks t =
      graph_p ~dtmp ~raw ?locks t >>= function
      | Ok graph ->
        Graph.fold (fun refname hash acc -> (refname, hash) :: acc) graph []
        |> List.stable_sort (fun (a, _) (b, _) -> Reference.compare a b)
        |> fun lst -> Lwt.return lst
      | Error _ -> Lwt.return []

    let list_s ?locks t = list_p ?locks ~dtmp:t.buffer.de ~raw:t.buffer.io t

    let list = list_s

    let mem t reference =
      let in_packed_refs () = Hashtbl.mem t.packed reference in

      Reference.mem ~root:t.dotgit reference >>= function
      | Ok true -> Lwt.return true
      | Ok false -> let v = in_packed_refs () in Lwt.return v
      | Error err ->
        Log.warn (fun l -> l ~header:"exists" "Retrieve an error for %a: %a."
                     Reference.pp reference Reference.pp_error err);
        let v = in_packed_refs () in Lwt.return v

    let remove_p ~dtmp ~raw ?locks t reference =
      let lock = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))[@warning "-44"]
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      (Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
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

    let read_p ~dtmp ~raw ?locks t reference =
      let lock = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v ("r-" ^ (Fpath.filename (Reference.to_path reference)))))
        | None -> None
      in

      FS.is_file Fpath.(t.dotgit // (Reference.to_path reference)) >>= function
      | Ok true ->
        Lock.with_lock lock
          (fun () -> Reference.read ~root:t.dotgit ~dtmp ~raw reference >|= function
             | Ok _ as v -> v
             | Error (#Reference.error as err) -> Error (err : error))
      | Ok false | Error _ ->
        if Hashtbl.mem t.packed reference
        then
          try Lwt.return (Ok (reference, Reference.Hash (Hashtbl.find t.packed reference)))
          with Not_found -> Lwt.return (Error `Not_found)
        else Lwt.return (Error `Not_found)

    let read_s ?locks t reference =
      read_p t ?locks ~dtmp:t.buffer.de ~raw:t.buffer.io reference

    let read = read_s

    let write_p ?locks ~dtmp ~raw t reference value =
      let lock_ref = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v ("w-" ^ (Fpath.filename (Reference.to_path reference)))))
        | None -> None
      in

      let lock_gbl = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))
        | None -> None
      in

      Lock.with_lock lock_ref @@ (fun () ->
          Reference.write ~root:t.dotgit ~raw reference value) >>= function
      | Error (#Reference.error as err) -> Lwt.return (Error (err : error))
      | Ok () ->
        match Hashtbl.mem t.packed reference with
        | false -> Lwt.return (Ok ())
        | true ->
          Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
          | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))
          | Ok packed_refs ->
            Lwt_list.fold_left_s
              (fun acc -> function
                 | `Peeled _ as v -> Lwt.return (v :: acc)
                 | `Ref (refname, hash) when not Reference.(equal (of_string refname) reference) -> Lwt.return (`Ref (refname, hash) :: acc)
                 | _ -> Lwt.return acc)
              [] packed_refs
            >>= fun packed_refs' ->
            Lock.with_lock lock_gbl @@ fun () ->

            Hashtbl.reset t.packed;
            List.iter
              (function `Ref (refname, hash) -> Hashtbl.add t.packed (Reference.of_string refname) hash
                      | `Peeled _ -> ())
              packed_refs';

            Packed_refs.write ~root:t.dotgit ~raw packed_refs' >>= function
            | Ok () -> Lwt.return (Ok ())
            | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))

    let write_s t ?locks reference value =
      write_p t ?locks ~dtmp:t.buffer.de ~raw:t.buffer.io reference value

    let write = write_s

    let unpack_reference t ~dtmp ~raw ?locks reference =
      let lock = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))
        | None -> None
      in

      Lock.with_lock lock @@ fun () ->
      match Hashtbl.mem t.packed reference with
      | false -> Lwt.return (Ok ())
      | true ->
        Packed_refs.read ~root:t.dotgit ~dtmp ~raw >>= function
        | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))
        | Ok packed_refs ->
          Lwt_list.fold_left_s (fun (pi, acc) -> function
              | `Peeled hash -> Lwt.return (pi, `Peeled hash :: acc)
              | `Ref (refname, hash) when not Reference.(equal reference (of_string refname)) ->
                Lwt.return (pi, `Ref (refname, hash) :: acc)
              | `Ref (_, hash) -> Lwt.return (Some (reference, hash), acc))
            (None, []) packed_refs
          >>= function
          | None, _ -> assert false
          (* XXX(dinosaure): we prove than reference is in packed_refs, so it's
             a mandatory to return a [Some v]. *)
          | (Some (_, hash), packed_refs') -> Packed_refs.write ~root:t.dotgit ~raw packed_refs'
            >>= function
            | Error (#Packed_refs.error as err) -> Lwt.return (Error (err : error))
            | Ok () -> Reference.write ~root:t.dotgit ~raw reference (Reference.Hash hash) >|= function
              | Ok () -> Ok ()
              | Error (#Reference.error as err) -> Error (err : error)

    let test_and_set t ?locks reference ~test ~set =
      let lock = match locks with
        | Some locks -> Some (Lock.make locks Fpath.(v "global"))[@warning "-44"]
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
    let raw = Cstruct.create (0x8000 * 2) in
    let buf = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout (2 * 0x8000) in

    { window = Inflate.window ()
    ; zl = Cstruct.sub raw 0 0x8000
    ; de = Cstruct.of_bigarray ~off:0 ~len:0x8000 buf
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
    ; io = Cstruct.sub raw 0x8000 0x8000 }

  let sanitize_filesystem root dotgit =
    FS.Dir.create ~path:true root
    >?= fun _ -> FS.Dir.create ~path:true dotgit
    >?= fun _ -> FS.Dir.create ~path:true Fpath.(dotgit / "objects")[@warning "-44"]
    >?= fun _ -> FS.Dir.create ~path:true Fpath.(dotgit / "objects" / "pack")[@warning "-44"]
    >?= fun _ -> FS.Dir.create ~path:true Fpath.(dotgit / "objects" / "info")[@warning "-44"]
    >?= fun _ -> FS.Dir.create ~path:true Fpath.(dotgit / "refs")[@warning "-44"]
    >?= fun _ -> Lwt.return (Ok ())

  let create ?root ?dotgit ?(compression = 4) () =
    let ( >>== ) v f = v >>= function
      | Ok v -> f v
      | Error _ as err -> Lwt.return err
    in

    (match root, dotgit with
     | None, _ | _, None ->
       (FS.Dir.current ()
        >>= function
        | Ok current ->
          let root = Option.get ~default:current root in
          let[@warning "-44"] dotgit  = Option.get ~default:Fpath.(root / ".git") dotgit in

          sanitize_filesystem root dotgit
          >>== fun () -> indexes dotgit
          >>== fun engine ->
          Lwt.return (Ok { dotgit
                         ; root
                         ; compression
                         ; engine
                         ; packed = Hashtbl.create 64
                         ; cache = cache ()
                         ; buffer = buffer () })
        | Error sys_err -> Lwt.return (Error sys_err))
     | Some root, Some dotgit ->
       sanitize_filesystem root dotgit
       >>== fun () -> indexes dotgit
       >>== fun engine ->
       Lwt.return (Ok { dotgit
                      ; root
                      ; compression
                      ; engine
                      ; packed = Hashtbl.create 64
                      ; cache = cache ()
                      ; buffer = buffer () }))
    >>= function
    | Ok t -> Lwt.return (Ok t)
    | Error sys_err -> Lwt.return (Error (`SystemDirectory sys_err))

  let clear_caches ?locks t =
    let lock = match locks with
      | Some locks -> Some (Lock.make locks Fpath.(v "global"))[@warning "-44"]
      | None -> None
    in

    Lock.with_lock lock @@ fun () ->
    CacheIndex.drop_lru t.cache.indexes;
    CacheRevIndex.drop_lru t.cache.revindexes;
    CachePack.drop_lru t.cache.packs;
    CacheValue.drop_lru t.cache.values;
    CacheObject.drop_lru t.cache.objects;
    Lwt.return ()

  let reset ?locks t =
    Log.info (fun l -> l ~header:"reset" "Start to reset the Git repository");

    let delete_files directory =
      FS.Dir.contents ~dotfiles:true ~rel:false directory
      >?= fun lst ->
      Lwt_result.ok (Lwt_list.fold_left_s
            (fun acc path -> Lwt.Infix.(FS.is_file path >|= function
               | Ok true -> path :: acc
               | _ -> acc))
            [] lst)
      >?= fun lst ->
      Lwt_result.ok (Lwt_list.iter_p
            (fun path -> Lwt.Infix.(FS.File.delete path >|= function
               | Ok () -> ()
               | Error _ -> ())) lst)
    in

    let lock = match locks with
      | Some locks -> Some (Lock.make locks Fpath.(v "global"))[@warning "-44"]
      | None -> None
    in

    Lock.with_lock lock @@ fun () ->
    (FS.Dir.delete ~recurse:true Fpath.(t.dotgit / "objects")
     >>= fun _ -> FS.Dir.create Fpath.(t.dotgit / "objects")
     >>= fun _ -> FS.Dir.create Fpath.(t.dotgit / "objects" / "info")
     >>= fun _ -> FS.Dir.create Fpath.(t.dotgit / "objects" / "pack")
     >>= fun _ -> FS.Dir.delete ~recurse:true Fpath.(t.dotgit / "refs")
     >>= fun _ -> FS.Dir.create Fpath.(t.dotgit / "refs" / "heads")
     >>= fun _ -> FS.Dir.create Fpath.(t.dotgit / "refs" / "tags"))
    >!= (fun err -> Lwt.return (`Store (`SystemDirectory err)))
    >>= fun _ -> (delete_files t.root >!= (fun err -> Lwt.return (`Store (`SystemDirectory err))))
    >>= fun _ -> Ref.write t Reference.head Reference.(Ref (of_string "refs/heads/master"))
    >!= (fun err -> Lwt.return (`Ref err))
  (* XXX(dinosaure): an empty git repository has HEAD which points
     to a non-existing refs/heads/master. *)

  let dotgit      { dotgit; _ }      = dotgit
  let root        { root; _ }        = root
  let compression { compression; _ } = compression

  let buffer_window { buffer; _ } = buffer.window
  let buffer_zl { buffer; _ } = buffer.zl
  let buffer_de { buffer; _ } = buffer.de
  let buffer_io { buffer; _ } = buffer.io
end
