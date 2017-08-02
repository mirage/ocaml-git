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

module type LAZY =
sig
  module Hash : Ihash.S

  type error =
    | Invalid_header of string
    | Invalid_version of int32
    | Invalid_index
    | Expected_bigoffset_table
    | Invalid_bigoffset_index of int

  val pp_error : error Fmt.t

  type t

  val make : ?cache:int -> Cstruct.t -> (t, error) result
  val find : t -> Hash.t -> (Crc32.t * int64) option
  val iter : t -> (Hash.t -> (Crc32.t * int64) -> unit) -> unit
  val fold : t -> (Hash.t -> (Crc32.t * int64) -> 'a -> 'a) -> 'a -> 'a
end

module Lazy (H : Ihash.S) : LAZY with module Hash = H =
struct
  module Hash = H

  type error =
    | Invalid_header of string
    | Invalid_version of int32
    | Invalid_index
    | Expected_bigoffset_table
    | Invalid_bigoffset_index of int

  let pp_error ppf = function
    | Invalid_header header         -> Fmt.pf ppf "(Invalid_header %s)" header
    | Invalid_version version       -> Fmt.pf ppf "(Invalid_version %ld)" version
    | Invalid_index                 -> Fmt.pf ppf "Invalid_index"
    | Expected_bigoffset_table      -> Fmt.pf ppf "Expected_bigoffset_table"
    | Invalid_bigoffset_index index -> Fmt.pf ppf "(Invalid_bigoffset_index %d)" index

  module Cache = Lru.M.Make(Hash)(struct type t = Crc32.t * int64 let weight _ = 1 end)

  type t =
    { map              : Cstruct.t
    ; number_of_hashes : int
    ; fanout_offset    : int
    ; hashes_offset    : int
    ; crcs_offset      : int
    ; values_offset    : int
    ; v64_offset       : int option
    ; cache            : Cache.t }

  let has map off len =
    if (off < 0 || len < 0 || off + len > Cstruct.len map)
    then raise (Invalid_argument (Fmt.strf "%d:%d:%d" off len (Cstruct.len map)))
    else true

  let check_header map =
    if has map 0 4
    then (if Cstruct.get_char map 0 = '\255'
          && Cstruct.get_char map 1 = '\116'
          && Cstruct.get_char map 2 = '\079'
          && Cstruct.get_char map 3 = '\099'
          then Ok ()
          else Error (Invalid_header (Cstruct.to_string @@ Cstruct.sub map 0 4)))
    else Error Invalid_index

  let check_version map =
    if has map 4 4
    then (if Cstruct.BE.get_uint32 map 4 = 2l
          then Ok ()
          else Error (Invalid_version (Cstruct.BE.get_uint32 map 4)))
    else Error Invalid_index

  let number_of_hashes map =
    if has map 8 (256 * 4)
    then let n = Cstruct.BE.get_uint32 map (8 + (255 * 4)) in
         Ok (8, n)
    else Error Invalid_index

  let bind v f = match v with Ok v -> f v | Error _ as e -> e
  let ( >>= ) = bind
  let ( *> ) u v = match u with Ok _ -> v | Error _ as e -> e

  let make ?(cache = 1024) map =
    check_header map
    *> check_version map
    *> number_of_hashes map
    >>= fun (fanout_offset, number_of_hashes) ->
      let number_of_hashes = Int32.to_int number_of_hashes in
      let hashes_offset = 8 + (256 * 4) in
      let crcs_offset   = 8 + (256 * 4) + (number_of_hashes * 20) in
      let values_offset = 8 + (256 * 4) + (number_of_hashes * 20) + (number_of_hashes * 4) in
      let v64_offset    = 8 + (256 * 4) + (number_of_hashes * 20) + (number_of_hashes * 4) + (number_of_hashes * 4) in

      let v64_offset = if v64_offset + (Hash.Digest.length * 2) = Cstruct.len map then None else Some v64_offset in

      Ok { map
         ; number_of_hashes
         ; fanout_offset
         ; hashes_offset
         ; crcs_offset
         ; values_offset
         ; v64_offset
         ; cache = Cache.create ~random:true cache }

  exception Break

  let compare buf off hash =
    try for i = 0 to Hash.Digest.length - 1
        do if Cstruct.get_char buf (off + i) <> Cstruct.get_char hash i
           then raise Break
        done; true
    with Break -> false

  exception ReturnT
  exception ReturnF

  let lt buf off hash =
    try for i = 0 to Hash.Digest.length - 1
        do let a = Cstruct.get_uint8 buf (off + i) in
           let b = Cstruct.get_uint8 hash i in

           if a > b
           then raise ReturnT
           else if a <> b then raise ReturnF;
        done; false
    with ReturnT -> true
       | ReturnF -> false

  let binary_search buf hash =
    let rec aux off len buf =
      if len = Hash.Digest.length
      then (off / Hash.Digest.length)
      else
        let len' = ((len / (Hash.Digest.length * 2)) * Hash.Digest.length) in
        let off' = off + len' in

        if compare buf off' hash
        then (off' / Hash.Digest.length)
        else if lt buf off' hash
        then (aux[@tailcall]) off len' buf
        else (aux[@tailcall]) off' (len - len') buf
    in

    aux 0 (Cstruct.len buf) buf

  let fanout_idx t hash =
    (match Cstruct.get_uint8 hash 0 with
     | 0 ->
       let n = Cstruct.BE.get_uint32 t.map t.fanout_offset in

       if n = 0l
       then Error Invalid_index
       else Ok (binary_search (Cstruct.sub t.map t.hashes_offset (Int32.to_int n * Hash.Digest.length)) hash)
     | idx ->
       if has t.map (t.fanout_offset + (4 * idx)) 4
       && has t.map (t.fanout_offset + (4 * (idx - 1))) 4
       then let off1 = Int32.to_int @@ Cstruct.BE.get_uint32 t.map (t.fanout_offset + (4 * idx)) in
         let off0 = Int32.to_int @@ Cstruct.BE.get_uint32 t.map (t.fanout_offset + (4 * (idx - 1))) in

         if has t.map (t.hashes_offset + (off0 * Hash.Digest.length)) ((off1 - off0) * Hash.Digest.length) && (off1 - off0) > 0
         then Ok (off0 + binary_search (Cstruct.sub t.map (t.hashes_offset + (off0 * Hash.Digest.length)) ((off1 - off0) * Hash.Digest.length)) hash)
         else Error Invalid_index
       else Error Invalid_index)
    |> function
    | Ok off ->
      let hash' = Cstruct.sub t.map (t.hashes_offset + (off * Hash.Digest.length)) Hash.Digest.length in

      if Cstruct.equal hash hash'
      then Ok off
      else Error Invalid_index
    | Error err -> Error err

  let find t hash =
    match Cache.find hash t.cache with
    | Some (crc, offset) -> Some (crc, offset)
    | None ->
      match fanout_idx t (Hash.to_string hash |> Cstruct.of_string) with
      | Ok idx ->
        let crc = Cstruct.BE.get_uint32 t.map (t.crcs_offset + (idx * 4)) in
        let off = Cstruct.BE.get_uint32 t.map (t.values_offset + (idx * 4)) in

        let off =
          if Int32.equal 0l (Int32.logand off 0x80000000l)
          then Ok (Int64.of_int32 off)
          else (match t.v64_offset with
              | Some v64_offset ->
                let n = Int32.to_int (Int32.logand off 0x7FFFFFFFl) in

                if has t.map (v64_offset + (n * 8)) 8
                then Ok (Cstruct.BE.get_uint64 t.map (v64_offset + (n * 8)))
                else Error (Invalid_bigoffset_index n)
              | None -> Error Expected_bigoffset_table)
        in

        (match off with
         | Ok off ->
           Cache.add hash (Crc32.of_int32 crc, off) t.cache;
           Some (Crc32.of_int32 crc,  off)
         | Error _ -> None)
      | Error _ -> None

  let iter t f =
    for i = 0 to t.number_of_hashes - 1
    do
      let hash = Cstruct.sub t.map (t.hashes_offset + (i * Hash.Digest.length)) Hash.Digest.length in
      let crc = Crc32.of_int32 (Cstruct.BE.get_uint32 t.map (t.crcs_offset + (i * 4))) in
      let off = Cstruct.BE.get_uint32 t.map (t.values_offset + (i * 4)) in

      let off =
        if Int32.equal 0l (Int32.logand off 0x80000000l)
        then Int64.of_int32 off
        else (match t.v64_offset with
            | Some v64_offset ->
              let n = Int32.to_int (Int32.logand off 0x7FFFFFFFl) in

              Cstruct.BE.get_uint64 t.map (v64_offset + (n * 8))
            | None -> raise (Invalid_argument "Expected big offset table"))
      in

      (* XXX(dinosaure): it's the biggest allocation place when we decode all
         git object. *)
      f (Hash.of_string (Cstruct.to_string hash)) (crc, off)
    done

  let fold t f a =
    let a = ref a in

    iter t (fun k v -> a := f k v !a); !a
end

module Option =
struct
  let bind f = function Some x -> Some (f x) | None -> None
  let value ~default = function Some x -> x | None -> default
end

module type DECODER =
sig
  module Hash : Ihash.S

  type error =
    | Invalid_byte of int
    | Invalid_version of Int32.t
    | Invalid_index_of_bigoffset of int
    | Expected_bigoffset_table
    | Invalid_hash of Hash.t * Hash.t

  val pp_error : error Fmt.t

  type t

  val pp     : t Fmt.t
  val make   : unit -> t
  val refill : int -> int -> t -> t
  val eval   : Cstruct.t -> t -> [ `Await of t | `End of t * Hash.t | `Hash of t * (Hash.t * Crc32.t * int64) | `Error of t * error ]
end

module Decoder (H : Ihash.S with type Digest.buffer = Cstruct.t) : DECODER with module Hash = H =
struct
  module Hash = H

  type error =
    | Invalid_byte of int
    | Invalid_version of Int32.t
    | Invalid_index_of_bigoffset of int
    | Expected_bigoffset_table
    | Invalid_hash of Hash.t * Hash.t

  let pp_error ppf = function
    | Invalid_byte byte              -> Fmt.pf ppf "(Invalid_byte %02x)" byte
    | Invalid_version version        -> Fmt.pf ppf "(Invalid_version %ld)" version
    | Invalid_index_of_bigoffset idx -> Fmt.pf ppf "(Invalid_index_of_bigoffset %d)" idx
    | Expected_bigoffset_table       -> Fmt.pf ppf "Expected_bigoffset_table"
    | Invalid_hash (has, expect)     -> Fmt.pf ppf "(Invalid_hash (%a, %a))" Hash.pp has Hash.pp expect

  type t =
    { i_off     : int
    ; i_pos     : int
    ; i_len     : int
    ; fanout    : Int32.t array
    ; hashes    : Hash.t Queue.t
    ; crcs      : Crc32.t Queue.t
    ; offsets   : (Int32.t * bool) Queue.t
    ; hash      : Hash.Digest.ctx
    ; state     : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header    of k
    | Fanout    of k
    | Hashes    of k
    | Crcs      of k
    | Offsets   of k
    | Hash      of k
    | Ret       of int64 array option * Hash.t * Hash.t
    | End       of Hash.t
    | Exception of error
  and res =
    | Wait   of t
    | Error  of t * error
    | Cont   of t
    | Result of t * (Hash.t * Crc32.t * Int64.t)
    | Ok     of t * Hash.t

  let pp_state ppf = function
    | Header _      -> Fmt.pf ppf "(Header #k)"
    | Fanout _      -> Fmt.pf ppf "(Fanout #k)"
    | Hashes _      -> Fmt.pf ppf "(Hashes #k)"
    | Crcs _        -> Fmt.pf ppf "(Crcs #k)"
    | Offsets _     -> Fmt.pf ppf "(Offsets #k)"
    | Hash _        -> Fmt.pf ppf "(Hash #k)"
    | End hash_pack -> Fmt.pf ppf "(End %a)" Hash.pp hash_pack
    | Exception exn -> Fmt.pf ppf "(Exception %a)" pp_error exn
    | Ret (boffs, hash_idx, hash_pack) ->
      Fmt.pf ppf "(Ret (big offsets:%d, idx:%a, pack:%a))"
        (Option.value ~default:0 (Option.bind Array.length boffs))
        Hash.pp hash_idx Hash.pp hash_pack

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>i_off = %d;@ \
                         i_pos = %d;@ \
                         i_len = %d;@ \
                         fanout = #table;@ \
                         hashes = #queue;@ \
                         crcs = #queue;@ \
                         offsets = #queue;@ \
                         hash = #ctx;@ \
                         state = %a;@] }"
      t.i_off t.i_pos t.i_len (Fmt.hvbox pp_state) t.state

  let await src t =
    let () = Hash.Digest.feed t.hash (Cstruct.sub src t.i_off t.i_pos) in
    Wait t
  let error t exn = Error ({ t with state = Exception exn }, exn)
  let ok t hash   = Ok ({ t with state = End hash }, hash)

  let to_int32 b0 b1 b2 b3 =
    let ( << ) = Int32.shift_left in (* >> (tuareg) *)
    let ( || ) = Int32.logor in
    (Int32.of_int b0 << 24)          (* >> (tuareg) *)
    || (Int32.of_int b1 << 16)       (* >> (tuareg) *)
    || (Int32.of_int b2 << 8)        (* >> (tuareg) *)
    || (Int32.of_int b3)

  let to_int64 b0 b1 b2 b3 b4 b5 b6 b7 =
    let ( << ) = Int64.shift_left in (* >> (tuareg) *)
    let ( || ) = Int64.logor in
    (Int64.of_int b0 << 56)          (* >> (tuareg) *)
    || (Int64.of_int b1 << 48)       (* >> (tuareg) *)
    || (Int64.of_int b2 << 40)       (* >> (tuareg) *)
    || (Int64.of_int b3 << 32)       (* >> (tuareg) *)
    || (Int64.of_int b4 << 24)       (* >> (tuareg) *)
    || (Int64.of_int b5 << 16)       (* >> (tuareg) *)
    || (Int64.of_int b6 << 8)        (* >> (tuareg) *)
    || (Int64.of_int b7)

  let rec get_byte ~ctor k src t =
      if (t.i_len - t.i_pos) > 0
      then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
           k byte src { t with i_pos = t.i_pos + 1 }
      else await src { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) }

  let rec get_u32 ~ctor k src t =
    if (t.i_len - t.i_pos) > 3
    then let num = Cstruct.BE.get_uint32 src (t.i_off + t.i_pos) in
          k num src
            { t with i_pos = t.i_pos + 4 }
    else if (t.i_len - t.i_pos) > 0
    then (get_byte ~ctor
          @@ fun byte0 -> get_byte ~ctor
          @@ fun byte1 -> get_byte ~ctor
          @@ fun byte2 -> get_byte ~ctor
          @@ fun byte3 src t ->
              k (to_int32 byte0 byte1 byte2 byte3) src t)
          src t
    else await src { t with state = Header (fun src t -> (get_u32[@tailcall]) ~ctor k src t) }

  let rec get_u64 ~ctor k src t =
    if (t.i_len - t.i_pos) > 7
    then let num = Cstruct.BE.get_uint64 src (t.i_off + t.i_pos) in
          k num src
            { t with i_pos = t.i_pos + 8 }
    else if (t.i_len - t.i_pos) > 0
    then (get_byte ~ctor
          @@ fun byte0 -> get_byte ~ctor
          @@ fun byte1 -> get_byte ~ctor
          @@ fun byte2 -> get_byte ~ctor
          @@ fun byte3 -> get_byte ~ctor
          @@ fun byte4 -> get_byte ~ctor
          @@ fun byte5 -> get_byte ~ctor
          @@ fun byte6 -> get_byte ~ctor
          @@ fun byte7 src t ->
              k (to_int64 byte0 byte1 byte2 byte3 byte4 byte5 byte6 byte7) src t)
          src t
    else await src { t with state = Offsets (fun src t -> (get_u64[@tailcall]) k ~ctor src t) }


  module KHeader =
  struct
    let rec check_byte chr k src t =
      if (t.i_len - t.i_pos) > 0 && Cstruct.get_char src (t.i_off + t.i_pos) = chr
      then k src { t with i_pos = t.i_pos + 1 }
      else if (t.i_len - t.i_pos) = 0
      then await src { t with state = Header (fun src t -> (check_byte[@tailcall]) chr k src t) }
      else error { t with i_pos = t.i_pos + 1 }
                 (Invalid_byte (Cstruct.get_uint8 src (t.i_off + t.i_pos)))

    let get_u32  = get_u32  ~ctor:(fun k -> Header k)
  end

  module KFanoutTable =
  struct
    let get_u32  = get_u32  ~ctor:(fun k -> Fanout k)
  end

  module KHashes =
  struct
    let get_byte = get_byte ~ctor:(fun k -> Hashes k)

    let get_hash k src t =
      let res = Cstruct.create Hash.Digest.length in
      (* XXX(dinosaure): we can replace it by an internal buffer allocated in
         {!default}. *)

      let rec loop i src t =
        if i = Hash.Digest.length
        then k res src t
        else
          get_byte (fun byte src t ->
              Cstruct.set_uint8 res i byte;
              (loop[@tailcall]) (i + 1) src t)
            src t
      in

      loop 0 src t
  end

  module KCrcs =
  struct
    let get_u32  = get_u32  ~ctor:(fun k -> Crcs k)
  end

  module KOffsets =
  struct
    let get_u32_and_msb k src t =
      get_u32 ~ctor:(fun k -> Offsets k)
        (fun u32 src t ->
          k (u32, Int32.equal 0l (Int32.logand u32 0x80000000l)) src t)
        src t

    let get_u64 = get_u64 ~ctor:(fun k -> Offsets k)
  end

  module KHash =
  struct
    let rec get_byte k src t =
      if (t.i_len - t.i_pos) > 0
      then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
           k byte src { t with i_pos = t.i_pos + 1 }
      else Wait { t with state = Hash (fun src t -> (get_byte[@tailcall]) k src t) }
           (* don't use [await] function. *)

    let get_hash k src t =
      let res = Cstruct.create Hash.Digest.length in

      let rec loop i src t =
        if i = Hash.Digest.length
        then k res src t
        else
          get_byte (fun byte src t ->
              Cstruct.set_uint8 res i byte;
              (loop[@tailcall]) (i + 1) src t)
            src t
      in

      loop 0 src t
  end

  let rest ?boffsets (hash_idx, hash_pack) src t =
    match Queue.pop t.hashes, Queue.pop t.crcs, Queue.pop t.offsets with
    | hash, crc, (offset, true) -> Result ({ t with state = Ret (boffsets, hash_idx, hash_pack) }, (hash, crc, Int64.of_int32 offset))
    | exception Queue.Empty -> ok t hash_pack
    | hash, crc, (offset, false) -> match boffsets with
      | None -> error t (Expected_bigoffset_table)
      | Some arr ->
        let idx = Int32.to_int (Int32.logand offset 0x7FFFFFFFl) in
        if idx >= 0 && idx < Array.length arr
        then Result ({ t with state = Ret (boffsets, hash_idx, hash_pack) }, (hash, crc, Array.get arr idx))
        else error t (Invalid_index_of_bigoffset idx)

  let hash ?boffsets src t =
    let aux k src t =
      let () = Hash.Digest.feed t.hash (Cstruct.sub src t.i_off t.i_pos) in
      KHash.get_hash k src t
    in

    (KHash.get_hash
     @@ fun hash_pack -> aux
     @@ fun hash_idx src t ->
     let produce = Hash.Digest.get t.hash in
     let hash_idx =
       Cstruct.to_string hash_idx
       |> Hash.of_string
     in
     let hash_pack =
       Cstruct.to_string hash_pack
       |> Hash.of_string
     in

     if hash_idx <> produce
     then error t (Invalid_hash (Hash.Digest.get t.hash, hash_idx))
     else rest ?boffsets (hash_idx, hash_pack) src t)
    src t

  let rec boffsets arr idx max src t =
    if idx >= max
    then hash ~boffsets:arr src t
    else KOffsets.get_u64
           (fun offset src t ->
              Array.set arr idx offset;
              (boffsets[@tailcall]) arr (succ idx) max src t)
           src t

  let rec offsets idx boffs max src t =
    if Int32.compare idx max >= 0
    then (if boffs > 0 then boffsets (Array.make boffs 0L) 0 boffs src t else hash src t)
    else KOffsets.get_u32_and_msb
           (fun (offset, msb) src t ->
             Queue.add (offset, msb) t.offsets;
             (offsets[@tailcall])
               (Int32.succ idx)
               (if not msb then succ boffs else boffs)
               max
               src t)
           src t

  let rec crcs idx max src t =
    if Int32.compare idx max >= 0
    then offsets 0l 0 max src t
    else KCrcs.get_u32
           (fun crc src t ->
             Queue.add (Crc32.of_int32 crc) t.crcs;
             (crcs[@tailcall]) (Int32.succ idx) max src t)
           src t

  let rec hashes idx max src t =
    if Int32.compare idx max >= 0
    then Cont { t with state = Crcs (crcs 0l max) }
    else KHashes.get_hash
        (fun hash src t ->
           let hash = Cstruct.to_string hash |> Hash.of_string in
              Queue.add hash t.hashes;
              (hashes[@tailcall]) (Int32.succ idx) max src t)
           src t

  (* XXX(dinosaure): Fix this compute. See the serialization to understand. *)
  let rec fanout idx src t =
    match idx with
    | 256 ->
      Cont { t with state = Hashes (hashes 0l (Array.fold_left max 0l t.fanout)) }
    | n ->
      KFanoutTable.get_u32
        (fun entry src t ->
         Array.set t.fanout idx entry; (fanout[@tailcall]) (idx + 1) src t)
        src t

  let header src t =
    (KHeader.check_byte '\255'
     @@ KHeader.check_byte '\116'
     @@ KHeader.check_byte '\079'
     @@ KHeader.check_byte '\099'
     @@ KHeader.get_u32
     @@ fun version src t ->
        if version = 2l
        then Cont { t with state = Fanout (fanout 0) }
        else error t (Invalid_version version))
    src t

  let make () =
    { i_off   = 0
    ; i_pos   = 0
    ; i_len   = 0
    ; fanout  = Array.make 256 0l
    ; hashes  = Queue.create ()
    ; crcs    = Queue.create ()
    ; offsets = Queue.create ()
    ; hash    = Hash.Digest.init ()
    ; state   = Header header }

  let refill off len t =
    if (t.i_len - t.i_pos) = 0
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
    else raise (Invalid_argument (Fmt.strf "I.refill: you lost something \
                                            (pos: %d, len: %d)" t.i_pos t.i_len))

  let eval src t =
    let eval0 t = match t.state with
      | Header k -> k src t
      | Fanout k -> k src t
      | Hashes k -> k src t
      | Crcs k -> k src t
      | Offsets k -> k src t
      | Ret (boffsets, hash_idx, hash_pack) -> rest ?boffsets (hash_idx, hash_pack) src t
      | Hash k -> k src t
      | End hash -> ok t hash
      | Exception exn -> error t exn
    in

    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Wait t -> `Await t
      | Ok (t, hash) -> `End (t, hash)
      | Result (t, hash) -> `Hash (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t
end

module type ENCODER =
sig
  module Hash : Ihash.S

  type error

  val pp_error : error Fmt.t

  type t

  val pp : t Fmt.t

  type 'a sequence = ('a -> unit) -> unit

  val default  : (Hash.t * (Crc32.t * int64)) sequence -> Hash.t -> t
  val flush    : int -> int -> t -> t
  val used_out : t -> int
  val eval     : Cstruct.t -> t -> [ `Flush of t | `End of t | `Error of t * error ]
end

module Encoder (H : Ihash.S with type Digest.buffer = Cstruct.t) : ENCODER with module Hash = H =
struct
  module Hash = H

  type error

  let pp_error = Fmt.nop (* no error. *)

  module K =
  struct
    type t = Hash.t

    let get = Hash.get
    let compare = Hash.compare
  end

  module Fanout = Fanout.Make(K)

  type t =
    { o_off    : int
    ; o_pos    : int
    ; o_len    : int
    ; write    : int
    ; table    : (Crc32.t * int64) Fanout.t
    ; boffsets : int64 array
    ; hash     : Hash.Digest.ctx
    ; pack     : Hash.t
    ; state    : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header     of k
    | Fanout     of k
    | Hashes     of k
    | Crcs       of k
    | Offsets    of k
    | BigOffsets of k
    | Hash       of k
    | End
  and res =
    | Error  of t * error
    | Flush  of t
    | Cont   of t
    | Ok     of t
    [@warning "-37"]

  (* XXX(dinosaure): the state contains only a closure. May be we can optimize
                     the serialization with an hot loop (like Decompress). But
                     the first goal is to work!
   *)

  let pp_state ppf = function
    | Header _     -> Fmt.pf ppf "(Header #k)"
    | Fanout _     -> Fmt.pf ppf "(Fanout #k)"
    | Hashes _     -> Fmt.pf ppf "(Hashes #k)"
    | Crcs _       -> Fmt.pf ppf "(Crcs #k)"
    | Offsets _    -> Fmt.pf ppf "(Offsets #k)"
    | BigOffsets _ -> Fmt.pf ppf "(BigOffsets #k)"
    | Hash _       -> Fmt.pf ppf "(Hash #k)"
    | End          -> Fmt.pf ppf "End"

  let pp ppf { o_off; o_pos; o_len; write; table; boffsets; hash; pack; state; } =
    Fmt.pf ppf "{ @[<hov>o_off = %d;@ \
                         o_pos = %d;@ \
                         o_len = %d;@ \
                         write = %d;@ \
                         table = #table;@ \
                         boffsets = #table;@ \
                         hash = #ctx;@ \
                         pack = %a;@ \
                         state = %a;@] }"
      o_off o_pos o_len write Hash.pp pack (Fmt.hvbox pp_state) state

  let flush dst t =
    Hash.Digest.feed t.hash (Cstruct.sub dst t.o_off t.o_pos);
    Flush t

  module Int32 =
  struct
    include Int32

    let ( >> ) = Int32.shift_right
    let ( && ) = Int32.logand
    let ( ! )  = Int32.to_int
  end

  let rec put_byte ~ctor chr k dst t =
    if (t.o_len - t.o_pos) > 0
    then begin
      Cstruct.set_char dst (t.o_off + t.o_pos) chr;
      k dst { t with o_pos = t.o_pos + 1
                    ; write = t.write + 1 }
    end else flush dst { t with state = ctor (fun dst t -> (put_byte[@tailcall]) ~ctor chr k dst t) }

  let rec put_int32 ~ctor integer k dst t =
    if (t.o_len - t.o_pos) >= 4
    then begin
      Cstruct.BE.set_uint32 dst (t.o_off + t.o_pos) integer;
      k dst { t with o_pos = t.o_pos + 4
                    ; write = t.write + 4 }
    end else if (t.o_len - t.o_pos) > 0
    then let i1 = Char.unsafe_chr @@ Int32.(! ((integer && 0xFF000000l) >> 24)) in
          let i2 = Char.unsafe_chr @@ Int32.(! ((integer && 0x00FF0000l) >> 16)) in
          let i3 = Char.unsafe_chr @@ Int32.(! ((integer && 0x0000FF00l) >> 8)) in
          let i4 = Char.unsafe_chr @@ Int32.(! (integer && 0x000000FFl)) in

          (put_byte ~ctor i1
          @@ put_byte ~ctor i2
          @@ put_byte ~ctor i3
          @@ put_byte ~ctor i4 k)
          dst t
    else flush dst { t with state = ctor (fun dst t -> (put_int32[@tailcall]) ~ctor integer k dst t) }

  module KHeader =
  struct
    let put_byte = put_byte ~ctor:(fun k -> Header k)
    let put_int32 = put_int32 ~ctor:(fun k -> Header k)
  end

  module Int64 =
  struct
    include Int64

    let ( >> ) = Int64.shift_right_logical
    let ( && ) = Int64.logand
    let ( ! )  = Int64.to_int
  end

  module KFanout =
  struct
    let put_int32 = put_int32 ~ctor:(fun k -> Fanout k)
  end

  module KHashes =
  struct
    let put_hash hash k dst t =
      if t.o_len - t.o_pos >= Hash.Digest.length
      then begin
        Cstruct.blit hash 0 dst (t.o_off + t.o_pos) Hash.Digest.length;
        k dst { t with o_pos = t.o_pos + Hash.Digest.length
                     ; write = t.write + Hash.Digest.length }
      end else
        let rec loop rest dst t =
          if rest = 0
          then k dst t
          else
            let n = min rest (t.o_len - t.o_pos) in

            if n = 0
            then flush dst { t with state = Hashes (loop rest) }
            else begin
              Cstruct.blit hash (Hash.Digest.length - rest) dst (t.o_off + t.o_pos) n;
              flush dst { t with state = Hashes (loop (rest - n))
                               ; o_pos = t.o_pos + n
                               ; write = t.write + n }
            end
        in

        loop Hash.Digest.length dst t
  end

  module KCrcs =
  struct
    let put_int32 = put_int32 ~ctor:(fun k -> Crcs k)
  end

  module KOffsets =
  struct
    let put_int32 = put_int32 ~ctor:(fun k -> Offsets k)
  end

  module KBOffsets =
  struct
    let put_byte = put_byte ~ctor:(fun k -> BigOffsets k)

    let rec put_int64 integer k dst t =
      if (t.o_len - t.o_pos) >= 8
      then begin
        Cstruct.BE.set_uint64 dst (t.o_off + t.o_pos) integer;
        k dst { t with o_pos = t.o_pos + 8
                     ; write = t.write + 8 }
      end else if (t.o_len - t.o_pos) > 0
      then let i1 = Char.unsafe_chr @@ Int64.(! ((integer && 0xFF00000000000000L) >> 56)) in
           let i2 = Char.unsafe_chr @@ Int64.(! ((integer && 0x00FF000000000000L) >> 48)) in
           let i3 = Char.unsafe_chr @@ Int64.(! ((integer && 0x0000FF0000000000L) >> 40)) in
           let i4 = Char.unsafe_chr @@ Int64.(! ((integer && 0x000000FF00000000L) >> 32)) in
           let i5 = Char.unsafe_chr @@ Int64.(! ((integer && 0x00000000FF000000L) >> 24)) in
           let i6 = Char.unsafe_chr @@ Int64.(! ((integer && 0x0000000000FF0000L) >> 16)) in
           let i7 = Char.unsafe_chr @@ Int64.(! ((integer && 0x000000000000FF00L) >> 8)) in
           let i8 = Char.unsafe_chr @@ Int64.(! (integer && 0x00000000000000FFL)) in

           (put_byte i1
            @@ put_byte i2
            @@ put_byte i3
            @@ put_byte i4
            @@ put_byte i5
            @@ put_byte i6
            @@ put_byte i7
            @@ put_byte i8 k)
           dst t
      else flush dst { t with state = BigOffsets (put_int64 integer k) }
  end

  module KHash =
  struct
    let put_hash ?(digest = true) hash k dst t =
      if t.o_len - t.o_pos >= Hash.Digest.length
      then begin
        Cstruct.blit_from_string hash 0 dst (t.o_off + t.o_pos) Hash.Digest.length;
        k dst { t with o_pos = t.o_pos + Hash.Digest.length
                     ; write = t.write + Hash.Digest.length }
      end else
        let rec loop rest dst t =
          if rest = 0
          then k dst t
          else
            let n = min rest (t.o_len - t.o_pos) in

            if n = 0
            then let t = { t with state = Hash (loop rest) } in
                 if digest then flush dst t else Flush t
            else begin
              Cstruct.blit_from_string hash (Hash.Digest.length - rest) dst (t.o_off + t.o_pos) n;
              let t = { t with state = Hash (loop (rest - n))
                             ; o_pos = t.o_pos + n
                             ; write = t.write + n }
              in
              if digest then flush dst t else Flush t
            end
        in

        loop Hash.Digest.length dst t
  end

  let ok t = Ok { t with state = End }

  let is_big_offset integer =
    Int64.(integer >> 31) <> 0L

  let hash dst t =
    (KHash.put_hash (Hash.to_string t.pack)
     @@ fun dst t ->
     Hash.Digest.feed t.hash (Cstruct.sub dst t.o_off t.o_pos);
     let hash = Hash.Digest.get t.hash in

     KHash.put_hash ~digest:false (Hash.to_string hash) (fun _ t -> ok t) dst t)
    dst t

  let rec boffsets idx dst t =
    if idx = Array.length t.boffsets
    then Cont { t with state = Hash hash }
    else KBOffsets.put_int64 (Array.get t.boffsets idx)
           (boffsets (idx + 1)) dst t

  let rec offsets idx idx_boffs dst t =
    if idx = 256
    then Cont { t with state = BigOffsets (boffsets 0) }
    else let rec aux acc idx_boffs dst t = match acc with
           | [] -> Cont { t with state = Offsets (offsets (idx + 1) idx_boffs) }
           | (_, (_, off)) :: r ->
               if is_big_offset off
               then let integer = Int32.(0x40000000l && (Int32.of_int idx_boffs)) in
                    KOffsets.put_int32 integer (aux r (idx_boffs + 1)) dst t
               else KOffsets.put_int32 (Int64.to_int32 off) (aux r idx_boffs) dst t
                    (* XXX(dinosaure): safe to convert the offset to an int32. *)
         in
         aux (Fanout.get idx t.table) idx_boffs dst t

  let rec crcs idx dst t =
    if idx = 256
    then Cont { t with state = Offsets (offsets 0 0) }
    else let rec aux acc dst t = match acc with
           | [] -> Cont { t with state = Hashes (crcs (idx + 1)) }
           | (_, (crc, _)) :: r -> KCrcs.put_int32 (Crc32.to_int32 crc) (aux r) dst t
         in
         aux (Fanout.get idx t.table) dst t

  let rec hashes idx dst t =
    if idx = 256
    then Cont { t with state = Crcs (crcs 0) }
    else let rec aux acc dst t = match acc with
           | [] -> Cont { t with state = Hashes (hashes (idx + 1)) }
           | (hash, _) :: r ->
             KHashes.put_hash hash (aux r) dst t
      in
      let lst =
        Fanout.get idx t.table
        |> List.map (fun (hash, crc32) -> Cstruct.of_string (Hash.to_string hash), crc32)
        (* XXX(dinosaure): may be we can optimize this and allocate a big
           [Cstruct.t] instead to use the case. *)
      in

      aux lst dst t

  let rec fanout idx acc dst t =
    match idx with
    | 256 ->
      Cont { t with state = Hashes (hashes 0) }
    | n ->
      let value = Int32.of_int (Fanout.length n t.table) in
      KFanout.put_int32 (Int32.add value acc)
        (fanout (idx + 1) (Int32.add value acc))
        dst t

  let header dst t =
    (KHeader.put_byte '\255'
     @@ KHeader.put_byte '\116'
     @@ KHeader.put_byte '\079'
     @@ KHeader.put_byte '\099'
     @@ KHeader.put_int32 2l
     @@ fun dst t -> Cont { t with state = Fanout (fanout 0 0l) })
    dst t

  let flush off len t =
    { t with o_off = off
           ; o_len = len
           ; o_pos = 0 }

  let used_out t = t.o_pos

  let eval dst t =
    let eval0 t = match t.state with
      | Header k     -> k dst t
      | Fanout k     -> k dst t
      | Hashes k     -> k dst t
      | Crcs   k     -> k dst t
      | Offsets k    -> k dst t
      | BigOffsets k -> k dst t
      | Hash k       -> k dst t
      | End -> ok t
    in

    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Flush t -> `Flush t
      | Ok t -> `End t
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  type 'a sequence = ('a -> unit) -> unit

  (* XXX(dinosaure): The sequence type is an abstraction of the iteration of a
                     data structure. The order of the iteration is not
                     important, the Fanout module takes care about that. So, we
                     let the user to use any data structure to store the CRC and
                     the Offset for each hash.
   *)
  let default : (Hash.t * (Crc32.t * int64)) sequence -> Hash.t -> t = fun seq hash ->

    let boffsets = ref [] in
    let table    = Fanout.make () in
    let f (hash, (crc, offset)) =
      if is_big_offset offset
      then let idx = Int64.of_int (List.length !boffsets) in
           boffsets := offset :: !boffsets;
           Fanout.bind hash (crc, idx) table
      else Fanout.bind hash (crc, offset) table
    in

    (* make the table and the big offsets table to ensure the order. *)
    let () = seq f in

    { o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; write = 0
    ; table
    ; boffsets = Array.of_list (List.rev !boffsets)
    ; hash = Hash.Digest.init ()
    ; pack = hash
    ; state = Header header }
end
