(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = (val Misc.src_log "pack-index" : Logs.LOG)

type f = Hash.t -> int option

type raw = {
  offsets : int Hash.Map.t;
  crcs    : int32 Hash.Map.t;
  pack_checksum: Hash.t;
}

module Raw = struct

  type t = raw
  let hash t = Hashtbl.hash t.pack_checksum
  let compare t1 t2 =  Hash.compare t1.pack_checksum t2.pack_checksum
  let equal t1 t2 = Hash.equal t1.pack_checksum t2.pack_checksum

  let pp ppf t =
    Format.fprintf ppf "@[pack-checksum: %a@ " Hash.pp t.pack_checksum;
    let l = ref [] in
    let offsets = Hash.Map.to_alist t.offsets in
    let crcs = Hash.Map.to_alist t.crcs in
    List.iter2 (fun (key1, offset) (key2, crc) ->
        assert (key1 = key2);
        l := (key1, offset, crc) :: !l
      ) offsets crcs;
    let l = List.sort (fun (s1,_,_) (s2,_,_) -> Hash.compare s1 s2) !l in
    List.iter (fun (h, offset, crc) ->
        Format.fprintf ppf "@[%a@ off:%d@ crc:%ld@;@]" Hash.pp h offset crc
      ) l;
    Format.fprintf ppf "@]"

  let lengths { offsets; _ } =
    let rec aux acc = function
      | []    -> List.rev acc
      | [h,_] -> aux ((h, None)::acc) []
      | (h1,l1)::((_,l2)::_ as t) -> aux ((h1, Some (l2-l1))::acc) t in
    let l = Hash.Map.bindings offsets in
    let l = List.sort (fun (_,x) (_,y) -> Pervasives.compare x y) l in
    Hash.Map.of_alist (aux [] l)

  let keys t = Hash.Set.of_list (Hash.Map.keys t.offsets)

  let find_offset t h =
    try Some (Hash.Map.find h t.offsets)
    with Not_found -> None

  let input_header buf =
    let magic = Mstruct.get_string buf 4 in
    if magic <> "\255tOc" then
      Mstruct.parse_error_buf buf "wrong magic index (%S)" magic;
    let version = Mstruct.get_be_uint32 buf in
    if version <> 2l then
      Mstruct.parse_error_buf buf "wrong index version (%ld)" version

end

module Offset_cache = struct
  type t = int Lru.t
  let create size = Lru.make size
  let add = Lru.add
  let find = Lru.find
end

type t = {
  cache: Offset_cache.t;
  fanout_ofs: int;
  hashes_ofs: int;
  crcs_ofs: int;
  offsets_ofs: int;
  ofs64_ofs: int;
  n_hashes: int;
  ofs64_tbl: (int, int) Hashtbl.t;
  mutable ofs64_size: int option;
  ba: Cstruct.buffer;
}

module Make (D: Hash.DIGEST) = struct

  module A = Hash.Array(D)

  (* The header consists of 256 4-byte network byte order integers.
      N-th entry of this table records the number of objects in the
      corresponding pack, the first byte of whose object name is less
      than or equal to N.  This is called the 'first-level fan-out'
      table. *)
  let fanout t = Mstruct.of_bigarray ~off:t.fanout_ofs ~len:(256 * 4) t.ba

  (* A table of 4-byte offset values (in network byte order). These are
      usually 31-bit pack file offsets, but large offsets are encoded as
      an index into the next table with the msbit set. *)
  let offsets t =
    Mstruct.of_bigarray ~off:t.offsets_ofs ~len:(t.n_hashes * 4) t.ba

  let hashes t =
    Cstruct.of_bigarray ~off:t.hashes_ofs ~len:(t.n_hashes * D.length) t.ba

  let is64_offset buf =
    let buf = Mstruct.to_cstruct buf in
    Cstruct.get_uint8 buf 0 land 128 <> 0

  let create_ofs64_tbl t =
    Log.debug (fun l -> l "create_ofs64_tbl");
    let offsets = offsets t in
    let count = ref 0 in
    for i = 0 to t.n_hashes - 1 do
      if is64_offset offsets then (
        Hashtbl.add t.ofs64_tbl i !count;
        Log.debug (fun l -> l "create_ofs64_tbl: %d -> %d" i !count);
        incr count
      );
      Mstruct.shift offsets 4
    done;
    t.ofs64_size <- Some !count;
    !count

  (* A table of 8-byte offset entries (empty for pack files less than 2
      GiB).  Pack files are organized with heavily used objects toward
      the front, so most object references should not need to refer to
      this table. *)
  let offsets64 t =
    let size64 = match t.ofs64_size with
      | Some sz -> sz
      | None    -> create_ofs64_tbl t
    in
    Mstruct.of_bigarray ~off:t.ofs64_ofs ~len:(size64 * 8) t.ba

  let fanout_of_hash h =
    let s = Hash.to_raw h in
    int_of_char s.[0]

  let fail fmt = Fmt.kstrf failwith ("Pack_index." ^^ fmt)

  let (++) x y = match y with
    | None   -> None
    | Some y -> Some (x + y)

  let get_hash_idx t h =
    Log.debug (fun l -> l "get_hash_idx: %a" Hash.pp h);
    let fanout_idx = fanout_of_hash h in
    let offsets = fanout t in
    let get_int buf = Int32.to_int (Mstruct.get_be_uint32 buf) in
    let offset, len =
      if fanout_idx = 0 then 0, get_int offsets
      else if fanout_idx > 0 then
        let offset = (fanout_idx - 1) * 4 in
        Mstruct.shift offsets offset;
        let off0 = get_int offsets in
        let off1 = get_int offsets in
        let len  = off1 - off0 in
        off0, len
      else
        fail "get_hash_idx %a" Hash.pp h;
    in
    let buf = hashes t in
    let buf = A.sub buf offset len in
    offset ++ A.binary_search buf h

  let find_offset t h =
    Log.debug (fun l -> l "find_offset: %a" Hash.pp h);
    match Offset_cache.find t.cache h with
    | Some _ as x -> Log.debug (fun l -> l "find_offset: cache hit!"); x
    | None ->
      match get_hash_idx t h with
      | None     -> None
      | Some idx ->
        let buf = offsets t in
        Mstruct.shift buf (idx * 4);
        if is64_offset buf then (
          Log.debug (fun l -> l "find_offset: 64bit offset");
          let buf64 = offsets64 t in
          if not (Hashtbl.mem t.ofs64_tbl idx) then fail "Cannot find %d" idx;
          let idx64 = Hashtbl.find t.ofs64_tbl idx in
          Mstruct.shift buf64 (idx64 * 8);
          let o64 = Int64.to_int (Mstruct.get_be_uint64 buf64) in
          Log.debug (fun l -> l "find_offset: found:%d" o64);
          Offset_cache.add t.cache h o64;
          Some o64
        ) else (
          let o = Int32.to_int (Mstruct.get_be_uint32 buf) in
          Log.debug (fun l -> l "find_offset: found:%d" o);
          Offset_cache.add t.cache h o;
          Some o
        )

  let mem t h =
    Log.debug (fun l -> l "mem: %a" Hash.pp h);
    match find_offset t h with
    | Some _ -> Log.debug (fun l -> l "mem: true"); true
    | None   -> Log.debug (fun l -> l "mem: false"); false

  let keys t = A.to_list (hashes t)

  let input ?(cache_size=10) ba =
    let buf = Mstruct.of_bigarray ba in
    Raw.input_header buf;
    (* fanout table *)
    let fanout_ofs = Mstruct.offset buf in
    Log.debug (fun l -> l "create: entering fanout table (ofs=%d)" fanout_ofs);
    Mstruct.shift buf (255 * 4);
    let n_hashes = Int32.to_int (Mstruct.get_be_uint32 buf) in
    Log.debug (fun l -> l "create: n_hashs:%d" n_hashes);
    (* hash listing *)
    let hashes_ofs = Mstruct.offset buf in
    Log.debug (fun l -> l "create: entering hash listing (ofs=%d)" hashes_ofs);
    Mstruct.shift buf (n_hashes * D.length);
    (* crc checksums *)
    let crcs_ofs = Mstruct.offset buf in
    Log.debug (fun l -> l "create: entering crc checksums (ofs=%d)" crcs_ofs);
    Mstruct.shift buf (n_hashes * 4);
    (* packfile offsets *)
    let offsets_ofs = Mstruct.offset buf in
    Log.debug (fun l ->
        l "create: entering packfile offsets (ofs=%d)" offsets_ofs);
    Mstruct.shift buf (n_hashes * 4);
    (* large packfile offsets *)
    let ofs64_ofs = Mstruct.offset buf in
    Log.debug (fun l ->
        l "create: entering large packfile offsets (ofs=%d)" ofs64_ofs);
    let cache = Offset_cache.create cache_size in
    let ofs64_tbl = Hashtbl.create 1024 in
    let ofs64_size = None in
    { cache; fanout_ofs; hashes_ofs; crcs_ofs; offsets_ofs; ofs64_ofs;
      n_hashes; ofs64_size; ofs64_tbl; ba; }

  module Raw = struct

    include Raw
    module Hash_IO = Hash.IO(D)

    let input_keys buf n =
      Log.debug (fun l -> l "input: reading the %d objects IDs" n);
      let a = Array.make n (Hash.of_raw "") in
      for i=0 to n - 1 do
        a.(i) <- Hash_IO.input buf;
      done;
      a

    let input buf =
      Log.debug (fun l -> l "input");
      input_header buf;
      (* Read the first-level fanout *)
      Log.debug (fun l -> l "input: reading the first-level fanout");
      let fanout =
        let a = Array.make 256 0l in
        for i=0 to 255 do
          a.(i) <- Mstruct.get_be_uint32 buf;
        done;
        a in

      let nb_objects = Int32.to_int fanout.(255) in

      (* Read the names *)
      let names = input_keys buf nb_objects in

      (* Read the CRCs *)
      Log.debug (fun l -> l "input: reading the %d CRCs" nb_objects);
      let crcs =
        let a = Array.make nb_objects (Hash.of_raw "", 0l) in
        for i=0 to nb_objects-1 do
          let crc = Mstruct.get_be_uint32 buf in
          a.(i) <- (names.(i), crc);
        done;
        a in

      (* Read the offsets *)
      Log.debug (fun l -> l "input: reading the %d offsets" nb_objects);
      let number_of_conts = ref 0 in
      let offsets, conts =
        let a = Array.make nb_objects 0l in
        let b = Array.make nb_objects false in
        for i=0 to nb_objects-1 do
          let more = match Mstruct.get_uint8 buf land 128 with
            | 0 -> false
            | _ -> true in
          let n =
            Mstruct.shift buf (-1);
            Mstruct.get_be_uint32 buf in
          a.(i) <- n;
          if more then (
            b.(i) <- true;
            incr number_of_conts;
          );
        done;
        a, b in

      Log.debug (fun l ->
          l "input: reading the %d offset continuations" !number_of_conts);
      let offsets = Array.mapi (fun i name ->
          let offset = offsets.(i) in
          let cont = conts.(i) in
          if cont then (
            let offset = Mstruct.get_be_uint64 buf in
            (name, Int64.to_int offset)
          ) else
            (name, Int32.to_int offset)
        ) names in
      let pack_checksum = Hash_IO.input buf in
      let _checksum = Hash_IO.input buf in

      let offsets_alist = Array.to_list offsets in
      let offsets = Hash.Map.of_alist offsets_alist in
      let crcs = Hash.Map.of_alist (Array.to_list crcs) in
      { offsets; crcs; pack_checksum }

    let add buf ?level:_ t =
      let n = Hash.Map.cardinal t.offsets in
      Log.debug (fun l -> l "output: %d packed values" n);
      Buffer.add_string buf "\255tOc";
      Misc.add_be_uint32 buf 2l;

      let cmp (k1,_) (k2,_) = Hash.compare k1 k2 in

      let offsets = List.sort cmp (Hash.Map.to_alist t.offsets) in
      let crcs    = List.sort cmp (Hash.Map.to_alist t.crcs) in

      Log.debug (fun l -> l "output: writing the first-level fanout");
      let fanout = Array.make 256 0l in
      List.iter (fun (key, _) ->
          let str = Hash.to_raw key in
          let n = Char.code str.[0] in
          for i = n to 255 do
            fanout.(i) <- Int32.succ fanout.(i)
          done;
        ) offsets;
      Array.iter (Misc.add_be_uint32 buf) fanout;

      Log.debug (fun l -> l "output: writing the %d object IDs" n);
      List.iter (fun (key, _) ->
          Hash_IO.add buf key
        ) offsets;

      Log.debug (fun l -> l "output: writing the %d CRCs" n);
      List.iter (fun (_, crc) ->
          Misc.add_be_uint32 buf crc
        ) crcs;

      Log.debug (fun l -> l "output: writing the %d offsets" n);
      let conts = ref [] in
      List.iter (fun (_, offset) ->
          if offset <= Int32.(to_int max_int) then (
            let i = Int32.of_int offset in
            Misc.add_be_uint32 buf i
          ) else (
            conts := Int64.of_int offset :: !conts;
            Misc.add_be_uint32 buf 0x80_00_00_00l
          )
        ) offsets;

      Log.debug (fun l ->
          l "output: writing the %d offset continuations" (List.length !conts));
      let str = Bytes.create 8 in
      List.iter (fun cont ->
          EndianString.BigEndian.set_int64 str 0 cont;
          Buffer.add_bytes buf str
        ) (List.rev !conts);

      Hash_IO.add buf t.pack_checksum;

      (* XXX: Hash.of_bigstring *)
      let str = Buffer.contents buf in
      let checksum = D.string str in
      Buffer.add_string buf (Hash.to_raw checksum)

  end

end
