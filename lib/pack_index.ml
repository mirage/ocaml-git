(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Sexplib.Std
open Printf

module Log = Log.Make(struct let section = "pack-index" end)

type t = {
  offsets : int SHA.Map.t;
  crcs    : int32 SHA.Map.t;
  pack_checksum: SHA.t;
} with sexp

let hash = Hashtbl.hash

let compare t1 t2 =
  match SHA.compare t1.pack_checksum t2.pack_checksum with
  | 0 -> begin
      match SHA.Map.compare compare t1.offsets t2.offsets with
      | 0 -> SHA.Map.compare Int32.compare t1.crcs t2.crcs
      | i -> i
    end
  | i -> i

let equal t1 t2 =
  SHA.equal t1.pack_checksum t2.pack_checksum
  && SHA.Map.equal (=) t1.offsets t2.offsets
  && SHA.Map.equal (=) t1.crcs t2.crcs

let empty ?pack_checksum () =
  let pack_checksum = match pack_checksum with
    | None   -> SHA.of_raw "" (* XXX: ugly *)
    | Some c -> c in
  {
    offsets = SHA.Map.empty;
    crcs    = SHA.Map.empty;
    pack_checksum;
  }

let pretty t =
  let buf = Buffer.create 1024 in
  bprintf buf "pack-checksum: %s\n" (SHA.to_hex t.pack_checksum);
  let l = ref [] in
  let offsets = SHA.Map.to_alist t.offsets in
  let crcs = SHA.Map.to_alist t.crcs in
  List.iter2 (fun (key1, offset) (key2, crc) ->
      assert (key1 = key2);
      l := (key1, offset, crc) :: !l
    ) offsets crcs;
  let l = List.sort (fun (_,o1,_) (_,o2,_) -> Pervasives.compare o1 o2) !l in
  List.iter (fun (sha1, offset, crc) ->
      bprintf buf "%s: off:%d crc:%ld\n" (SHA.to_hex sha1) offset crc
    ) l;
  Buffer.contents buf

let id_monad =
  (fun x -> x), (fun x f -> f x)

let lengths { offsets } =
  Log.debugf "lengths";
  let rec aux acc = function
    | []    -> List.rev acc
    | [h,_] -> aux ((h, None)::acc) []
    | (h1,l1)::((_,l2)::_ as t) -> aux ((h1, Some (l2-l1))::acc) t in
  let l = SHA.Map.bindings offsets in
  let l = List.sort (fun (_,x) (_,y) -> Pervasives.compare x y) l in
  SHA.Map.of_alist (aux [] l)

let input_header buf =
  let magic = Mstruct.get_string buf 4 in
  if String.(magic <> "\255tOc") then
    Mstruct.parse_error_buf buf "wrong magic index (%S)" magic;
  let version = Mstruct.get_be_uint32 buf in
  if Int32.(version <> 2l) then
    Mstruct.parse_error_buf buf "wrong index version (%ld)" version

let input_keys buf n =
  Log.debugf "input: reading the %d objects IDs" n;
  let a = Array.create n (SHA.of_raw "") in
  for i=0 to n - 1 do
    a.(i) <- SHA.input buf;
  done;
  a

let keys buf =
  Log.debugf "keys";
  input_header buf;
  Mstruct.shift buf (255 * 4);
  Mstruct.get_be_uint32 buf
  |> Int32.to_int
  |> input_keys buf
  |> Array.to_list
  |> SHA.Set.of_list

let input buf =
  Log.debugf "input";
  input_header buf;
  (* Read the first-level fanout *)
  Log.debugf "input: reading the first-level fanout";
  let fanout =
    let a = Array.create 256 0l in
    for i=0 to 255 do
      a.(i) <- Mstruct.get_be_uint32 buf;
    done;
    a in

  let nb_objects = Int32.to_int fanout.(255) in

  (* Read the names *)
  let names = input_keys buf nb_objects in

  (* Read the CRCs *)
  Log.debugf "input: reading the %d CRCs" nb_objects;
  let crcs =
    let a = Array.create nb_objects (SHA.of_raw "", 0l) in
    for i=0 to nb_objects-1 do
      let crc = Mstruct.get_be_uint32 buf in
      a.(i) <- (names.(i), crc);
    done;
    a in

  (* Read the offsets *)
  Log.debugf "input: reading the %d offsets" nb_objects;
  let number_of_conts = ref 0 in
  let offsets, conts =
    let a = Array.create nb_objects 0l in
    let b = Array.create nb_objects false in
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

  Log.debugf "input: reading the %d offset continuations" !number_of_conts;
  let offsets = Array.mapi (fun i name ->
      let offset = offsets.(i) in
      let cont = conts.(i) in
      if cont then (
        let offset = Mstruct.get_be_uint64 buf in
        (name, Int64.to_int offset)
      ) else
        (name, Int32.to_int offset)
    ) names in
  let pack_checksum = SHA.input buf in
  let _checksum = SHA.input buf in

  let offsets_alist = Array.to_list offsets in
  let offsets = SHA.Map.of_alist offsets_alist in
  let crcs = SHA.Map.of_alist (Array.to_list crcs) in
  { offsets; crcs; pack_checksum }

let add buf t =
  let n = SHA.Map.cardinal t.offsets in
  Log.debugf "output: %d packed values" n;
  Buffer.add_string buf "\255tOc";
  Misc.add_be_uint32 buf 2l;

  let cmp (k1,_) (k2,_) = SHA.compare k1 k2 in

  let offsets = List.sort cmp (SHA.Map.to_alist t.offsets) in
  let crcs    = List.sort cmp (SHA.Map.to_alist t.crcs) in

  Log.debugf "output: writing the first-level fanout";
  let fanout = Array.create 256 0l in
  List.iter (fun (key, _) ->
      let str = SHA.to_raw key in
      let n = Char.code str.[0] in
      for i = n to 255 do
        fanout.(i) <- Int32.succ fanout.(i)
      done;
    ) offsets;
  Array.iter (Misc.add_be_uint32 buf) fanout;

  Log.debugf "output: writing the %d object IDs" n;
  List.iter (fun (key, _) ->
      SHA.add buf key
    ) offsets;

  Log.debugf "output: writing the %d CRCs" n;
  List.iter (fun (_, crc) ->
      Misc.add_be_uint32 buf crc
    ) crcs;

  Log.debugf "output: writing the %d offsets" n;
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

  Log.debugf "output: writing the %d offset continuations" (List.length !conts);
  let str = String.create 8 in
  List.iter (fun cont ->
      EndianString.BigEndian.set_int64 str 0 cont;
      Buffer.add_string buf str
    ) (List.rev !conts);

  SHA.add buf t.pack_checksum;

  (* XXX: SHA.of_bigstring *)
  let str = Buffer.contents buf in
  let checksum = SHA.of_string str in
  Buffer.add_string buf (SHA.to_raw checksum)



let int_of_hex hex = int_of_string ("0x" ^ hex)

class type c_t = object
  method find_offset : SHA.t -> int option
  method mem         : SHA.t -> bool
end

class offset_cache size = object (self)
  val keyq = (Queue.create() : SHA.t Queue.t)
  val tbl = Hashtbl.create 0

  method add (sha1 : SHA.t) (offset : int) =
    Log.debugf "offset_cache#add: %s -> %d" (SHA.to_hex sha1) offset;
    Queue.add sha1 keyq;
    let _ = Hashtbl.add tbl sha1 offset in
    let qsz = Queue.length keyq in
    if qsz > size then begin
      try
        let k = Queue.take keyq in
          Log.debugf "offset_cache#add: qsz=%d shrinking..." qsz;
          Hashtbl.remove tbl k
      with
        Queue.Empty -> ()
    end

  method find (sha1 : SHA.t) =
    Log.debugf "offset_cache#find: %s" (SHA.to_hex sha1);
    Hashtbl.find tbl sha1

end (* of class Pack_index.offset_cache *)


module Oid = struct
  type t =
    | SHA1 of SHA.t
    | Abbrev of string (* hex *)

  let length = function
    | SHA1 _ -> 40
    | Abbrev h -> String.length h

  let of_hex h =
    let len = String.length h in
    if len = 40 then
      SHA1 (SHA.of_hex h)
    else if len > 40 then
      SHA1 (SHA.of_hex (String.sub h 0 40))
    else
      Abbrev h

  let to_hex = function
    | SHA1 sha -> SHA.to_hex sha
    | Abbrev h -> h

  let of_sha1 sha1 = SHA1 sha1

  let sstartswith s s0 =
    let len = String.length s in
    let len0 = String.length s0 in
    if len < len0 then
      false
    else
      try
        for i = 0 to len0 - 1 do
	  if s.[i] != s0.[i] then
	    raise Exit
        done;
        true
      with 
      | Exit -> false

  let pair_to_str_pair = function
    | SHA1 sha, SHA1 sha0 -> SHA.to_raw sha, SHA.to_raw sha0
    | SHA1 sha, Abbrev h0 -> SHA.to_hex sha, h0
    | Abbrev h, SHA1 sha0 -> h, SHA.to_hex sha0
    | Abbrev h, Abbrev h0 -> h, h0

  let startswith oid oid0 =
    let s, s0 = pair_to_str_pair (oid, oid0) in
    sstartswith s s0

  exception Ambiguous

  let slt s0 s1 =
    let len0 = String.length s0 in
    let len1 = String.length s1 in
    let len = min len0 len1 in
    let same = len0 = len1 in

    let rec scan i =
      if i = len then
        if same then
          false
        else
          raise Ambiguous
      else
        let x0 = s0.[i] in
        let x1 = s1.[i] in
        if x0 < x1 then
          true
        else if x0 > x1 then
          false
        else
          scan (i + 1)
    in
    let b = scan 1 in
    Log.debugf "Oid.slt: -> %B" b;
    b

  let lt oid oid0 =
    let s, s0 = pair_to_str_pair (oid, oid0) in
    slt s s0

end

exception Idx_found of int

class c ?(scan_thresh=8) ?(cache_size=1) (ba : Cstruct.buffer) = object (self)

  val cache = new offset_cache cache_size

  val mutable fanout_ofs  = 0
  val mutable sha1s_ofs   = 0
  val mutable crcs_ofs    = 0
  val mutable offsets_ofs = 0
  val mutable ofs64_ofs   = 0

  val mutable n_sha1s    = 0
  val mutable n_sha1s_4  = 0
  val mutable n_sha1s_20 = 0

  val ofs64_tbl = Hashtbl.create 0
  val mutable ofs64_size = None


  method find_offset sha1 =
    Log.debugf "c#find_offset: %s" (SHA.to_hex sha1);
    try
      let o = cache#find sha1 in
      Log.debugf "c#find_offset: cache hit!";
      Some o
    with
      Not_found -> begin
        match self#get_sha1_idx sha1 with
        | Some idx -> begin
            let buf = Mstruct.of_bigarray ~off:offsets_ofs ~len:n_sha1s_4 ba in

            Mstruct.shift buf (idx * 4);

            let is64 =
	      match Mstruct.get_uint8 buf land 128 with
              | 0 -> false
              | _ -> true 
            in
            if is64 then begin
              Log.debugf "c#find_offset: 64bit offset";

              let size64 =
                match ofs64_size with
                | Some sz -> sz
                | None -> self#create_ofs64_tbl
              in
              let buf64 = Mstruct.of_bigarray ~off:ofs64_ofs ~len:(size64 * 8) ba in
              
              try
                let idx64 = Hashtbl.find ofs64_tbl idx in
                Mstruct.shift buf64 (idx64 * 8);
                let o64 = Int64.to_int (Mstruct.get_be_uint64 buf64) in
                Log.debugf "c#find_offset: found:%d" o64;
                cache#add sha1 o64;
                Some o64
              with
                Not_found -> assert false
            end
            else begin
              Mstruct.shift buf (-1);
              let o = Int32.to_int (Mstruct.get_be_uint32 buf) in
              Log.debugf "c#find_offset: found:%d" o;
              cache#add sha1 o;
              Some o
            end
        end
        | None -> None
      end

  method mem sha1 =
    Log.debugf "c#mem: %s" (SHA.to_hex sha1);
    match self#find_offset sha1 with
    | Some o -> 
        Log.debugf "c#mem: true"; 
        true
    | None -> 
        Log.debugf "c#mem: false"; 
        false


  initializer
    let buf = Mstruct.of_bigarray ba in

    input_header buf;

    (* fanout table *)
    fanout_ofs <- Mstruct.offset buf;
    Log.debugf "c#<init>: entering fanout table (ofs=%d)" fanout_ofs;
    Mstruct.shift buf (255 * 4);
    n_sha1s <- Int32.to_int (Mstruct.get_be_uint32 buf);
    n_sha1s_4 <- n_sha1s * 4;
    n_sha1s_20 <- n_sha1s * 20;
    Log.debugf "c#<init>: n_sha1s:%d" n_sha1s;

    (* sha1 listing *)
    sha1s_ofs <- Mstruct.offset buf;
    Log.debugf "c#<init>: entering sha1 listing (ofs=%d)" sha1s_ofs;
    Mstruct.shift buf n_sha1s_20;

    (* crc checksums *)
    crcs_ofs <- Mstruct.offset buf;
    Log.debugf "c#<init>: entering crc checksums (ofs=%d)" crcs_ofs;
    Mstruct.shift buf n_sha1s_4;

    (* packfile offsets *)
    offsets_ofs <- Mstruct.offset buf;
    Log.debugf "c#<init>: entering packfile offsets (ofs=%d)" offsets_ofs;
    Mstruct.shift buf n_sha1s_4;

    (* large packfile offsets *)
    ofs64_ofs <- Mstruct.offset buf;
    Log.debugf "c#<init>: entering large packfile offsets (ofs=%d)" ofs64_ofs;


  method private get_sha1_idx sha1 =
    Log.debugf "c#get_sha1_idx: %s" (SHA.to_hex sha1);
    let fo_idx = self#get_fanout_idx sha1 in
    let buf = Mstruct.of_bigarray ~off:fanout_ofs ~len:(256 * 4) ba in
    let sz0, n =
      if fo_idx = 0 then begin
	0, Int32.to_int (Mstruct.get_be_uint32 buf)
      end
      else if fo_idx > 0 then begin
	Mstruct.shift buf ((fo_idx - 1) * 4);
	let sz0 = Int32.to_int (Mstruct.get_be_uint32 buf) in
	let sz1 = Int32.to_int (Mstruct.get_be_uint32 buf) in
	 sz0, sz1 - sz0
      end 
      else
        failwith "Pack_index.c#get_sha1_idx"
    in
    try
      let ofs = sha1s_ofs + (sz0 * 20) in
      self#scan_sha1s fo_idx sz0 ofs n sha1
    with
      Idx_found i -> 
        Log.debugf "c#get_sha1_idx: found:%d" i;
        Some i

  method private create_ofs64_tbl =
    Log.debugf "c#create_ofs64_tbl";
    let buf = Mstruct.of_bigarray ~off:offsets_ofs ~len:n_sha1s_4 ba in
    let count = ref 0 in
    for i = 0 to n_sha1s - 1 do
      begin
        match Mstruct.get_uint8 buf land 128 with
        | 0 -> ()
        | _ -> 
            let _ = Hashtbl.add ofs64_tbl i !count in
            Log.debugf "c#create_ofs64_tbl: %d -> %d" i !count;
            incr count
      end;
      Mstruct.shift buf 3
    done;
    ofs64_size <- Some !count;
    !count

  method private get_fanout_idx ?(verbose=true) sha1 =
    let s = SHA.to_raw sha1 in
    let fanout_idx = int_of_char s.[0] in
    fanout_idx

  (* implements binary search *)
  method private scan_sha1s fo_idx idx_ofs ofs n sha1 =
    Log.debugf "c#scan_sha1s: fo_idx:%d idx_ofs:%d ofs:%d n:%d" fo_idx idx_ofs ofs n;
    let len = n * 20 in
    let buf = Mstruct.of_bigarray ~off:ofs ~len ba in

    if n > scan_thresh then begin
      Log.debugf "c#scan_sha1s: %d > scan_thresh(=%d)" n scan_thresh;
      let p = n / 2 in
      Log.debugf "c#scan_sha1s: p:%d" p;
      let len' = p * 20 in
      Mstruct.shift buf len';
      let s = SHA.input buf in
      if s = sha1 then begin
        let idx = idx_ofs + p in
        Log.debugf "c#scan_sha1s: idx -> %d" idx;
        raise (Idx_found idx)
      end
      else if self#lt_sha1 sha1 s then
        self#scan_sha1s fo_idx idx_ofs ofs p sha1
      else
        let d = p + 1 in
        self#scan_sha1s fo_idx (idx_ofs + d) (ofs + d * 20) (n - p - 1) sha1
    end
    else begin
      Log.debugf "c#scan_sha1s: scanning...";
      self#scan_sub idx_ofs sha1 buf 0 (n - 1)
    end

  method private lt_sha1 sha1_0 sha1_1 =
    let s0 = SHA.to_raw sha1_0 in
    let s1 = SHA.to_raw sha1_1 in
    let rec scan i =
      let x0 = s0.[i] in
      let x1 = s1.[i] in
      if x0 < x1 then
        true
      else if x0 > x1 then
        false
      else
        scan (i + 1)
    in
    try
      let b = scan 1 in
      Log.debugf "c#lt_sha1: -> %B" b;
      b
    with
      Invalid_argument _ -> assert false

  method private scan_sub idx_ofs sha1 buf i m =
    if i > m then 
      None
    else
      let s = SHA.input buf in
      if s = sha1 then begin
        let idx = idx_ofs + i in
        Log.debugf "c#scan_sub: idx -> %d" idx;
        raise (Idx_found idx)
      end
      else
        self#scan_sub idx_ofs sha1 buf (i + 1) m



end (* Pack_index.c *)
