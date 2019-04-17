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

module Kind = struct
  type t = Commit | Tag | Tree | Blob

  let to_int = function Commit -> 0 | Tree -> 1 | Blob -> 2 | Tag -> 3

  let to_bin = function
    | Commit -> 0b001
    | Tree -> 0b010
    | Blob -> 0b011
    | Tag -> 0b100

  let pp ppf = function
    | Commit -> Fmt.pf ppf "Commit"
    | Tree -> Fmt.pf ppf "Tree"
    | Blob -> Fmt.pf ppf "Blob"
    | Tag -> Fmt.pf ppf "Tag"
end

module type ENTRY = sig
  module Hash : S.HASH

  type t
  type source = From of Hash.t | None

  val pp : t Fmt.t
  val pp_source : source Fmt.t
  val hash : string -> int

  val make :
       Hash.t
    -> ?name:string
    -> ?preferred:bool
    -> ?delta:source
    -> Kind.t
    -> int64
    -> t

  val kind : t -> Kind.t
  val preferred : t -> bool
  val delta : t -> source
  val length : t -> int64
  val with_delta : t -> source -> t
  val with_preferred : t -> bool -> t
  val id : t -> Hash.t
  val name : t -> string -> t
  val compare : t -> t -> int
  val topological_sort : t list -> t list
end

module Entry (Hash : S.HASH) = struct
  type t =
    { hash_name: int
    ; hash_object: Hash.t
    ; name: string option
    ; kind: Kind.t
    ; preferred: bool
    ; delta: source
    ; length: int64 }

  and source = From of Hash.t | None

  (* XXX(dinosaure): I try to use GADT in this case and ... god I'm crazy. *)

  let pp_source ppf = function
    | From hash -> Fmt.pf ppf "Δ(%a)" Hash.pp hash
    | None -> Fmt.string ppf "Τ"

  let pp ppf {hash_name; hash_object; name; kind; preferred; delta; length} =
    Fmt.pf ppf
      "{ @[<hov>name = @[<hov>%x and %a@];@ hash = %a;@ kind = %a;@ preferred \
       = %b;@ delta = %a;@ length = %Ld;@] }"
      hash_name (Fmt.option Fmt.string) name (Fmt.hvbox Hash.pp) hash_object
      (Fmt.hvbox Kind.pp) kind preferred (Fmt.hvbox pp_source) delta length

  (* XXX(dinosaure): hash from git to sort git objects. in git, this hash is
     computed in an [int32]. *)
  let hash name =
    let res = ref 0 in
    for i = 0 to String.length name - 1 do
      if name.[i] <> ' ' then res := (!res lsr 2) + (Char.code name.[i] lsl 24)
    done ;
    !res

  (* XXX(dinosaure): git hashes only the basename. *)
  let hash name = hash (Filename.basename name)

  let make :
         Hash.t
      -> ?name:string
      -> ?preferred:bool
      -> ?delta:source
      -> Kind.t
      -> int64
      -> t =
   fun hash_object ?name ?(preferred = false) ?(delta = None) kind length ->
    let hash_name = Helper.Option.(value ~default:0 (name >>= hash)) in
    {hash_name; hash_object; name; kind; preferred; delta; length}

  let id {hash_object; _} = hash_object
  let kind {kind; _} = kind
  let preferred {preferred; _} = preferred
  let delta {delta; _} = delta
  let length {length; _} = length
  let with_delta t delta = {t with delta}
  let with_preferred t preferred = {t with preferred}
  let name x name = {x with hash_name= hash name; name= Some name}

  let compare a b =
    (* - first, sort by type. Different objects never delta with each other. -
       then sort by filename/dirname. hash of the basename occupies the top
       BITS_PER_BITS - DIR_BITS, and bottom DIR_BITS are for hash of leading
       path elements. - then if we are doing "thin" pack, the objects wa are
       not going to pack but we know about are sorted earlier than other
       object. - and finally sort by size, larger to smaller *)
    let int_of_bool v = if v then 1 else 0 in
    if Kind.to_int a.kind > Kind.to_int b.kind then -1
    else if Kind.to_int a.kind < Kind.to_int b.kind then 1
    else if a.hash_name > b.hash_name then -1
    else if a.hash_name < b.hash_name then 1
    else if int_of_bool a.preferred > int_of_bool b.preferred then -1
    else if int_of_bool a.preferred < int_of_bool b.preferred then 1
    else if a.length > b.length then -1
    else if a.length < b.length then 1
    else Pervasives.compare a b

  (* XXX(dinosaure): git compare the memory position then but it's irrelevant
     in OCaml. *)

  let topological_sort lst =
    let lst =
      (* XXX(dinosaure): sanitize and remove self-dependency. *)
      List.map
        (function
          | {delta= From hash; hash_object; _} as x ->
              if Hash.equal hash hash_object then {x with delta= None} else x
          | {delta= None; _} as x -> x)
        lst
    in
    let edges, rest = List.partition (fun entry -> entry.delta = None) lst in
    let deps hash =
      (* XXX(dinosaure): in according to git, result of [List.filter] contains
         only one element. *)
      try List.filter (fun e -> Hash.equal e.hash_object hash) lst
      with Not_found -> []
    in
    let rec loop acc later rest progress =
      match rest, later with
      | [], [] -> List.rev acc
      | [], later ->
          if progress then loop acc [] later false
          else
            raise
              (Invalid_argument "Entry.topological_sort: un-orderable list")
      | ({delta= From hash; _} as x) :: r, later ->
          let deps = deps hash in
          (* We ensure than the [deps] is available previously. *)
          let ensure =
            List.for_all
              (fun dep ->
                List.exists
                  (fun x -> Hash.equal x.hash_object dep.hash_object)
                  acc )
              deps
          in
          if ensure then loop (x :: acc) later r true
          else loop acc (x :: later) r progress
      | ({delta= None; _} as x) :: r, later -> loop (x :: acc) later r true
    in
    loop edges [] rest false
end

module Int32 = struct
  include Int32

  let ( && ) = Int32.logand
  let ( >> ) = Int32.shift_right
end

module Int64 = struct
  include Int64

  let ( && ) = Int64.logand
  let ( - ) = Int64.sub
  let ( >> ) = Int64.shift_right
  let ( / ) = Int64.div
end

module type H = sig
  module Hash : S.HASH

  type error

  val pp_error : error Fmt.t

  type t
  type reference = Offset of int64 | Hash of Hash.t

  val pp : t Fmt.t
  val default : reference -> int -> int -> Duff.t list -> t
  val refill : int -> int -> t -> t
  val flush : int -> int -> t -> t
  val finish : t -> t

  val eval :
       Cstruct.t
    -> Cstruct.t
    -> t
    -> [`Await of t | `Flush of t | `End of t | `Error of t * error]

  val used_in : t -> int
  val used_out : t -> int
end

module Hunk (Hash : S.HASH) = struct
  type error

  let pp_error = Fmt.nop

  (* no error. *)

  type t =
    { o_off: int
    ; o_pos: int
    ; o_len: int
    ; i_off: int
    ; i_pos: int
    ; i_len: int
    ; i_abs: int
    ; write:
        int
        (* XXX(dinosaure): difficult to write a Hunk bigger than [max_int].
           consider that it's safe to use [int] instead [int64]. *)
    ; reference: reference
    ; source_length: int
    ; target_length: int
    ; hunks: Duff.t list
    ; state: state }

  and reference = Offset of int64 | Hash of Hash.t

  and k = Cstruct.t -> t -> res

  and state =
    | Header of k
    | List
    | Hunk of k
    | Insert of (Cstruct.t -> k)
    | Copy of k
    | Consume
    | End
    | Exception of error

  and res = Error of t * error | Cont of t | Flush of t | Wait of t | Ok of t

  let pp_state ppf = function
    | Header _ -> Fmt.pf ppf "(Header #k)"
    | List -> Fmt.pf ppf "List"
    | Hunk _ -> Fmt.pf ppf "(Hunk #k)"
    | Insert _ -> Fmt.pf ppf "(Insert #k)"
    | Copy _ -> Fmt.pf ppf "(Copy #k)"
    | End -> Fmt.pf ppf "End"
    | Consume -> Fmt.pf ppf "Consume"
    | Exception err -> Fmt.pf ppf "(Error %a)" (Fmt.hvbox pp_error) err

  let pp_reference ppf = function
    | Offset off -> Fmt.pf ppf "(Offset %Ld)" off
    | Hash hash -> Fmt.pf ppf "(Hash %a)" Hash.pp hash

  let pp ppf t =
    Fmt.pf ppf
      "{ @[<hov>o_off = %d;@ o_pos = %d;@ o_len = %d;@ i_off = %d;@ i_pos = \
       %d;@ i_len = %d;@ i_abs = %d;@ write = %d;@ reference = %a;@ \
       source_length = %d;@ target_length = %d;@ hunks = [ %a ];@ state = \
       %a;@] }"
      t.o_off t.o_pos t.o_len t.i_off t.i_pos t.i_len t.i_abs t.write
      pp_reference t.reference t.source_length t.target_length
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") Duff.pp))
      t.hunks (Fmt.hvbox pp_state) t.state

  let ok t : res = Ok {t with state= End}
  let await t : res = Wait t
  let error t exn : res = Error ({t with state= Exception exn}, exn)

  let rec put_byte ~ctor byte k dst t =
    if t.o_len - t.o_pos > 0 then (
      Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte ;
      k dst {t with o_pos= t.o_pos + 1; write= t.write + 1} )
    else
      Flush
        { t with
          state= ctor (fun dst t -> (put_byte [@tailcall]) ~ctor byte k dst t)
        }

  module KHeader = struct
    let put_byte byte = put_byte ~ctor:(fun k -> Header k) byte

    let rec length n k dst t =
      let byte = n land 0x7F in
      let rest = n lsr 7 in
      if rest <> 0 then put_byte (byte lor 0x80) (length rest k) dst t
      else put_byte byte k dst t
  end

  module KHunk = struct let put_byte = put_byte ~ctor:(fun k -> Hunk k) end

  module KInsert = struct
    let rec put_raw (off, len) k src dst t =
      if t.o_len - t.o_pos > len then (
        Cstruct.blit src off dst (t.o_off + t.o_pos) len ;
        k src dst {t with o_pos= t.o_pos + len; write= t.write + len} )
      else if t.o_len - t.o_pos > 0 then
        let rec loop rest src dst t =
          let n = min (t.o_len - t.o_pos) rest in
          Cstruct.blit src (off + (len - rest)) dst (t.o_off + t.o_pos) n ;
          if rest - n = 0 then
            k src dst {t with o_pos= t.o_pos + n; write= t.write + n}
          else
            Flush
              { t with
                o_pos= t.o_pos + n
              ; write= t.write + n
              ; state= Insert (loop (rest - n)) }
        in
        loop len src dst t
      else Flush {t with state= Insert (put_raw (off, len) k)}
  end

  module KCopy = struct
    let rec put_byte ?(force = false) byte k dst t =
      if byte <> 0 || force then
        if t.o_len - t.o_pos > 0 then (
          Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte ;
          k dst {t with o_pos= t.o_pos + 1; write= t.write + 1} )
        else Flush {t with state= Copy (put_byte ~force byte k)}
      else k dst t
  end

  let how_many_bytes n =
    let rec aux acc n = if n = 0 then acc else aux (acc + 1) (n lsr 8) in
    if n = 0 then 1 else aux 0 n

  let rec insert res absolute_offset len src dst t =
    if res = 0 then Cont {t with state= List}
    else if
      (* XXX(dinosaure): We ensure than the [absolute_offset] requested is
         available inside [src]. So, [t.i_abs] keep how many bytes we computed
         before __continuously__ and correspond to the current absolute offset
         of [src].

         We just need to check if what we need to write is available inside the
         interval [t.i_abs (t.i_abs + t.i_len)[. *)
      absolute_offset + (len - res) >= t.i_abs
      && absolute_offset + (len - res) < t.i_abs + t.i_len
    then
      let relative_offset = absolute_offset - t.i_abs in
      let n = min res (t.i_len - (relative_offset + (len - res))) in
      (* XXX(dinosaure): then, we have a [relative_offset] correspond to the
         [absolute_offset] but inside the current [src] and the interval
         [t.i_off (t.i_off + t.i_len)[. *)
      ( KInsert.put_raw (t.i_off + relative_offset + (len - res), n)
      @@ fun _ _ t ->
      Cont
        { t with
          state=
            Insert (insert (res - n) absolute_offset len)
            (* XXX(dinosaure): we need to keep how many bytes we rode because,
               externally, we could use [used_in]. *)
        ; i_pos= relative_offset + (len - res) + n } )
        src dst t
    else await {t with i_pos= t.i_len}

  let copy (off, _) (len, _) dst t =
    let o0 = off land 0xFF in
    let o1 = (off land 0xFF00) lsr 8 in
    let o2 = (off land 0xFF0000) lsr 16 in
    let o3 = (off land 0xFF000000) lsr 24 in
    (* XXX(dinosaure): we have a big problem in the last line. Indeed, this
       line believe than the integer is fully encoder in 32-bits. In a 64-bits
       architecture, we don't have problem but in 32-bits, this code can not
       compile (he compiler checks statically the value).

       A solution is to consider [off] as an [int32]. *)
    let l0 = len land 0xFF in
    let l1 = (len land 0xFF00) lsr 8 in
    let l2 = (len land 0xFF0000) lsr 16 in
    ( KCopy.put_byte ~force:true o0
    @@ KCopy.put_byte ~force:(o2 <> 0 || o3 <> 0) o1
    @@ KCopy.put_byte ~force:(o3 <> 0) o2
    @@ KCopy.put_byte o3
    @@ KCopy.put_byte ~force:true l0
    @@ KCopy.put_byte ~force:(l2 <> 0) l1
    @@ KCopy.put_byte l2
    @@ fun _ t -> Cont {t with state= List} )
      dst t

  let consume t = await {t with i_pos= t.i_len}

  let list _ dst t =
    match t.hunks with
    | [] -> Cont {t with state= Consume}
    | hunk :: r -> (
      match hunk with
      | Duff.Insert (off, len) ->
          assert (len > 0 && len <= 0x7F) ;
          (* XXX(dinosaure): the [xdiff] algorithm ensures than an [Duff.I]
             can't be upper than [0x7F]. *)
          let byte = len land 0x7F in
          KHunk.put_byte byte
            (fun _ t ->
              Cont {t with state= Insert (insert len off len); hunks= r} )
            dst t
      | Duff.Copy (off, len) ->
          let n_offset = how_many_bytes off in
          let n_length = if len = 0x10000 then 1 else how_many_bytes len in
          let o, fo =
            match n_offset with
            | 1 -> 0b0001, (false, false, false)
            | 2 -> 0b0011, (true, false, false)
            | 3 -> 0b0111, (true, true, false)
            | 4 -> 0b1111, (true, true, true)
            | _ -> assert false
          in
          let l, fl =
            match n_length with
            | 1 -> 0b001, (false, false)
            | 2 -> 0b011, (true, false)
            | 3 -> 0b111, (true, true)
            | _ -> assert false
          in
          KHunk.put_byte
            (0x80 lor (l lsl 4) lor o)
            (copy (off, fo)
               (if len = 0x10000 then 0, (false, false) else len, fl))
            dst {t with hunks= r} )

  let header dst t =
    ( KHeader.length t.source_length
    @@ KHeader.length t.target_length
    @@ fun _ t -> Cont {t with state= List} )
      dst t

  let eval src dst t =
    let eval0 t =
      match t.state with
      | Header k -> k dst t
      | List -> list src dst t
      | Hunk k -> k dst t
      | Insert k -> k src dst t
      | Copy k -> k dst t
      | End -> ok t
      | Consume -> consume t
      | Exception exn -> error t exn
    in
    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Flush t -> `Flush t
      | Wait t -> `Await t
      | Error (t, exn) -> `Error (t, exn)
      | Ok t -> `End t
    in
    loop t

  let default reference source_length target_length hunks =
    { o_off= 0
    ; o_pos= 0
    ; o_len= 0
    ; i_off= 0
    ; i_pos= 0
    ; i_len= 0
    ; i_abs= 0
    ; write= 0
    ; reference
    ; source_length
    ; target_length
    ; hunks
    ; state= Header header }

  let flush off len t = {t with o_off= off; o_len= len; o_pos= 0}

  let refill off len t =
    {t with i_off= off; i_pos= 0; i_len= len; i_abs= t.i_abs + t.i_len}

  (* XXX(dinosaure): we consider than we [refill] continuously the input fixed
     size buffer. *)

  let finish t = {t with state= End; i_pos= 0; i_len= 0; i_off= 0; i_abs= 0}
  let used_in t = t.i_pos
  let used_out t = t.o_pos
end

module type DELTA = sig
  module Hash : S.HASH
  module Entry : ENTRY with module Hash := Hash

  type t = {mutable delta: delta}

  and delta =
    | Z
    | S of
        { length: int
        ; depth: int
        ; hunks: Duff.t list
        ; src: t
        ; src_length: int64
        ; src_hash: Hash.t }

  type error = Invalid_hash of Hash.t

  val pp_error : error Fmt.t

  val deltas :
       ?memory:bool
    -> Entry.t list
    -> (Hash.t -> Cstruct.t option Lwt.t)
    -> (Entry.t -> bool)
    -> int
    -> int
    -> ((Entry.t * t) list, error) result Lwt.t
end

module Delta (Hash : S.HASH) (Entry : ENTRY with module Hash := Hash) = struct
  type t = {mutable delta: delta}

  and delta =
    | Z
    | S of
        { length: int
        ; depth: int
        ; hunks: Duff.t list
        ; src: t
        ; src_length:
            int64
            (* XXX(dinosaure): this is the length of the inflated raw of [src]. *)
        ; src_hash: Hash.t }

  (* XXX(dinosaure): I try to use GADT (peano number) and ... I really crazy. *)

  module WeightByMemory = struct
    type nonrec t = t * Cstruct.t * Duff.Default.Index.t

    let weight (_, raw, rabin) =
      (* XXX(dinosaure): - 1 word for [t] - len of raw - 1 for ... I don't know
         - memory size of the rabin's fingerprint *)
      1 + Cstruct.len raw + 1 + Duff.Default.Index.memory_size rabin
  end

  module WeightByElement = struct
    type nonrec t = t * Cstruct.t * Duff.Default.Index.t

    let weight _ = 1
  end

  module type WINDOW =
    Lru.F.S
    with type k = Entry.t
     and type v = t * Cstruct.t * Duff.Default.Index.t

  let rec _pp_delta ppf = function
    | Z -> Fmt.string ppf "Τ"
    | S {length; depth; hunks; src; src_length; _} ->
        Fmt.pf ppf
          "(Δ { @[<hov>length = %d;@ depth = %d;@ hunks = [ %a ];@ src = \
           %a;@ src_length = %Ld;@] }"
          length depth
          (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") Duff.pp))
          hunks (Fmt.hvbox _pp) src src_length

  and _pp ppf {delta} =
    Fmt.pf ppf "{ @[<hov>delta = @[<hov>%a@];@] }" (Fmt.hvbox _pp_delta) delta

  type error = Invalid_hash of Hash.t

  let pp_error ppf (Invalid_hash hash) =
    Fmt.pf ppf
      "Got an invalid (non-existing) hash when we apply the delta-ification: %a"
      Hash.pp hash

  let depth = function {delta= S {depth; _}} -> depth | {delta= Z} -> 0

  let size_of_variable_length vl =
    let rec loop acc = function 0 -> acc | n -> loop (acc + 1) (n lsr 7) in
    loop 1 (vl lsr 7)

  let how_many_bytes n =
    let rec aux acc n = if n = 0 then acc else aux (acc + 1) (n lsr 8) in
    if n = 0 then 1 else aux 0 n

  let length src_len trg_len hunks =
    size_of_variable_length src_len
    + size_of_variable_length trg_len
    + List.fold_left
        (fun acc -> function Duff.Insert (_, len) -> 1 + len + acc
          | Duff.Copy (off, len) ->
              1
              + how_many_bytes off
              + (if len = 0x10000 then 1 else how_many_bytes len)
              + acc )
        0 hunks

  let only_insert =
    List.for_all (function Duff.Insert _ -> true | Duff.Copy _ -> false)

  let delta : type window.
         window
      -> (module WINDOW with type t = window)
      -> int
      -> Entry.t
      -> Cstruct.t
      -> t
      -> (Entry.t * Duff.t list * int) option =
   fun window window_pack max trg_entry trg_raw trg ->
    let limit src =
      match trg.delta with
      | S {length; src; _} ->
          length * (max - depth src) / (max - (depth trg + 1))
      | Z ->
          ((Int64.to_int (Entry.length trg_entry) / 2) - 20)
          * (max - depth src)
          / (max - 1)
    in
    let choose a b =
      match a, b with
      | None, None -> None
      | Some a, None ->
          Some a (* XXX(dinosaure): [a] is considered as the best. *)
      | None, Some (a, hunks_a, len_a) ->
          if not (only_insert hunks_a) then Some (a, hunks_a, len_a) else None
      (* XXX(dinosaure): Rabin's fingerprint can produce only [Insert] hunks
         and we avoid that. *)
      | Some (a, hunks_a, len_a), Some (b, hunks_b, len_b) ->
          if len_a < len_b then Some (a, hunks_a, len_a)
          else Some (b, hunks_b, len_b)
    in
    let apply src_entry (src, src_raw, rabin) best =
      let diff =
        if Entry.length src_entry < Entry.length trg_entry then
          Int64.to_int
            (Int64.sub (Entry.length trg_entry) (Entry.length src_entry))
        else 0
      in
      if
        Entry.kind src_entry <> Entry.kind trg_entry
        || depth src = max
        || limit src = 0
        || limit src <= diff
        || (Entry.length trg_entry < Int64.(Entry.length src_entry / 32L))
        || Hash.equal (Entry.id src_entry) (Entry.id trg_entry)
      then best
      else
        let hunks = Duff.Default.delta rabin trg_raw in
        let length =
          length (Cstruct.len src_raw) (Cstruct.len trg_raw) hunks
        in
        choose best (Some (src_entry, hunks, length))
    in
    let module Window = (val window_pack) in
    if (not (Entry.preferred trg_entry)) && depth trg < max then
      Window.fold apply None window
    else None

  let ok v = Ok v

  (* XXX(dinosaure): git prioritize some entries in imperative weird way. we
     can't reproduce the same with a small cost. We need to take care about the
     writing order. Indeed, [git] has an assumption about this and consider all
     base object is before delta-ified object.

     This function needs to take about this and recompute the writing order. If
     a PACK file was not accepted by a server, may be the problem can be found
     here. TODO! *)
  let sort _ lst =
    let edges, rest =
      List.partition
        (function _, {delta= Z} -> true | _, {delta= S _} -> false)
        lst
    in
    let deps hash =
      try List.filter (fun (e, _) -> Hash.equal (Entry.id e) hash) lst
      with Not_found -> []
    in
    let rec loop acc later rest progress =
      match rest, later with
      | [], [] -> List.rev acc
      | [], later ->
          if progress then loop acc [] later false
          else raise (Invalid_argument "Delta.sort: un-orderable list")
      | ((_, {delta= Z}) as x) :: r, later -> loop (x :: acc) later r true
      | ((_, {delta= S {src_hash; _}}) as x) :: r, later ->
          let deps = deps src_hash in
          let ensure =
            List.for_all
              (fun (e, _) ->
                List.exists
                  (fun (x, _) -> Hash.equal (Entry.id x) (Entry.id e))
                  acc )
              deps
          in
          if ensure then loop (x :: acc) later r true
          else loop acc (x :: later) r progress
    in
    loop edges [] rest false

  exception Uncaught_hash of Hash.t

  module MemoryCache = Lru.F.Make (Entry) (WeightByMemory)
  module ElementCache = Lru.F.Make (Entry) (WeightByElement)

  let deltas ?(memory = false) entries get tag window max =
    let to_delta e =
      match Entry.delta e, Entry.preferred e with
      | Entry.None, false -> Entry.length e >= 50L
      | (Entry.From _ | Entry.None), _ -> false
    in
    let tries =
      List.filter to_delta entries |> List.stable_sort Entry.compare
    in
    let untries =
      List.filter (fun x -> not (to_delta x)) entries |> Entry.topological_sort
    in
    let window_pack =
      if memory then (module MemoryCache : WINDOW)
      else (module ElementCache : WINDOW)
    in
    let module Window = (val window_pack) in
    let window_pack = (module Window : WINDOW with type t = Window.t) in
    let window = Window.empty window in
    let normal = Hashtbl.create (List.length tries) in
    let open Lwt.Infix in
    (* XXX(dinosaure): [normalize] applies the diff to all [untries] entries.
       however, we need to apply to [untries] a topological sort to ensure than
       when we try to apply a diff in one /untries/ entry, we already computed
       the source (available in [tries] or [untries]). It's why we keep an
       hash-table and update this hash-table for each diff. *)
    let normalize lst =
      Lwt_list.map_p
        (fun trg_entry ->
          match Entry.delta trg_entry with
          | Entry.None -> Lwt.return (trg_entry, {delta= Z})
          | Entry.From hash -> (
              let src =
                try Hashtbl.find normal hash with Not_found -> {delta= Z}
              in
              (* XXX(dinosaure): if we can't find [src] in the hash-table, that
                 means the source object is outside the PACK file because we
                 ensure than if [trg_entry] has a dependence, by the
                 topological sort, we already computed all /in-PACK/ sources
                 necessary for the next and update the hash-table with these
                 sources. *)
              get hash
              >>= fun a ->
              get (Entry.id trg_entry)
              >>= fun b ->
              match a, b with
              | Some src_raw, Some trg_raw ->
                  let rabin = Duff.Default.Index.make ~copy:false src_raw in
                  (* we don't keep [rabin]. *)
                  let hunks = Duff.Default.delta rabin trg_raw in
                  let length =
                    length (Cstruct.len src_raw) (Cstruct.len trg_raw) hunks
                  in
                  let depth = depth src + 1 in
                  let base =
                    { delta=
                        S
                          { length
                          ; depth
                          ; hunks
                          ; src
                          ; src_length= Int64.of_int (Cstruct.len src_raw)
                          ; src_hash= hash } }
                  in
                  Hashtbl.add normal (Entry.id trg_entry) base ;
                  Lwt.return (trg_entry, base)
              | None, Some _ -> raise (Uncaught_hash hash)
              | Some _, None -> raise (Uncaught_hash (Entry.id trg_entry))
              | None, None -> assert false ) )
        lst
    in
    Lwt.try_bind
      (fun () ->
        Lwt_list.fold_left_s
          (fun (window, acc) entry ->
            get (Entry.id entry)
            >>= function
            | None -> raise (Uncaught_hash (Entry.id entry))
            | Some raw -> (
                let base = {delta= Z} in
                let rabin = Duff.Default.Index.make ~copy:false raw in
                (* we keep [rabin] with [raw] in the [window]. *)
                let window = Window.add entry (base, raw, rabin) window in
                let window = Window.trim window in
                match delta window window_pack max entry raw base with
                | None -> Lwt.return (window, (entry, base) :: acc)
                | Some (src_entry, hunks, length) -> (
                  match Window.find src_entry window with
                  | Some (src, _, _) ->
                      let window = Window.promote src_entry window in
                      let depth = depth src + 1 in
                      base.delta
                      <- S
                           { length
                           ; depth
                           ; hunks
                           ; src
                           ; src_length= Entry.length src_entry
                           ; src_hash= Entry.id src_entry } ;
                      Hashtbl.add normal (Entry.id entry) base ;
                      Lwt.return
                        ( window
                        , ( Entry.with_delta entry Entry.(From (id src_entry))
                          , base )
                          :: acc )
                  | None -> Lwt.return (window, (entry, base) :: acc) ) ) )
          (window, []) tries )
      (fun (_, tries) ->
        Lwt.try_bind
          (fun () -> normalize untries)
          (fun untries ->
            List.append tries untries |> sort tag |> ok |> Lwt.return )
          (function
            | Uncaught_hash hash -> Error (Invalid_hash hash) |> Lwt.return
            | exn -> Lwt.fail exn) )
      (* XXX(dinosaure): could we have a better choice? *)
        (function
        | Uncaught_hash hash -> Error (Invalid_hash hash) |> Lwt.return
        | exn -> Lwt.fail exn)

  (* XXX(dinosaure): same as below. *)
end

module type P = sig
  module Hash : S.HASH
  module Deflate : S.DEFLATE
  module Entry : ENTRY with module Hash := Hash
  module Delta : DELTA with module Hash := Hash and module Entry := Entry
  module Hunk : H with module Hash := Hash

  type error = Deflate_error of Deflate.error | Invalid_hash of Hash.t

  val pp_error : error Fmt.t

  type t

  val used_out : t -> int
  val used_in : t -> int
  val flush : int -> int -> t -> t
  val refill : int -> int -> t -> t
  val finish : t -> t
  val expect : t -> Hash.t
  val idx : t -> (Checkseum.Crc32.t * int64) Hash.Map.t
  val default : Cstruct.t -> (Entry.t * Delta.t) list -> t

  val eval :
       Cstruct.t
    -> Cstruct.t
    -> t
    -> [`Flush of t | `Await of t | `End of t * Hash.t | `Error of t * error]
end

module Pack
    (Hash : S.HASH)
    (Deflate : S.DEFLATE)
    (Entry : ENTRY with module Hash := Hash)
    (Delta : DELTA with module Hash := Hash and module Entry := Entry)
    (Hunk : H with module Hash := Hash) =
struct
  type error = Deflate_error of Deflate.error | Invalid_hash of Hash.t

  let pp_error ppf = function
    | Deflate_error err ->
        Fmt.pf ppf "Got a deflate error: %a" Deflate.pp_error err
    | Invalid_hash hash -> Fmt.pf ppf "Invalid hash: %a" Hash.pp hash

  type t =
    { o_off: int
    ; o_pos: int
    ; o_len: int
    ; i_off: int
    ; i_pos: int
    ; i_len: int
    ; write: int64
    ; map: (Checkseum.Crc32.t * int64) Hash.Map.t
    ; hash: Hash.ctx
    ; h_tmp: Cstruct.t
    ; state: state }

  and k = Cstruct.t -> t -> res

  and state =
    | Header of k
    | Object of k
    | WriteK of k
    | WriteZ of
        { x: Entry.t
        ; r: (Entry.t * Delta.t) list
        ; crc: Checkseum.Crc32.t
        ; off: int64
        ; ui: int
        ; z: Deflate.t }
    | WriteH of
        { x: Entry.t * Delta.t
        ; r: (Entry.t * Delta.t) list
        ; crc: Checkseum.Crc32.t
        ; off: int64
        ; ui: int
        ; h: Hunk.t
        ; z: Deflate.t }
    | Save of
        { x: Entry.t
        ; r: (Entry.t * Delta.t) list
        ; crc: Checkseum.Crc32.t
        ; off: int64 }
    | Hash of k
    | End of Hash.t
    | Exception of error

  and res =
    | Flush of t
    | Wait of t
    | Error of t * error
    | Cont of t
    | Ok of t * Hash.t

  and kind = KindOffset | KindHash | KindRaw

  let flush dst t =
    let hash = Hash.feed_cstruct t.hash (Cstruct.sub dst t.o_off t.o_pos) in
    Flush {t with hash}

  let await t : res = Wait t
  let error t exn : res = Error ({t with state= Exception exn}, exn)
  let ok t hash : res = Ok ({t with state= End hash}, hash)

  module KHeader = struct
    let rec put_byte byte k dst t =
      if t.o_len - t.o_pos > 0 then (
        Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte ;
        k dst {t with o_pos= t.o_pos + 1; write= Int64.add t.write 1L} )
      else flush dst {t with state= Header (put_byte byte k)}

    let rec put_u32 integer k dst t =
      if t.o_len - t.o_pos > 3 then (
        Cstruct.BE.set_uint32 dst (t.o_off + t.o_pos) integer ;
        k dst {t with o_pos= t.o_pos + 4; write= Int64.add t.write 4L} )
      else if t.o_len - t.o_pos > 0 then
        let a1 = Int32.(to_int ((integer && 0xFF000000l) >> 24)) in
        let a2 = Int32.(to_int ((integer && 0x00FF0000l) >> 16)) in
        let a3 = Int32.(to_int ((integer && 0x0000FF00l) >> 8)) in
        let a4 = Int32.(to_int (integer && 0x000000FFl)) in
        (put_byte a1 @@ put_byte a2 @@ put_byte a3 @@ put_byte a4 @@ k) dst t
      else flush dst {t with state= Header (put_u32 integer k)}
  end

  let rec put_byte ~ctor byte k dst t =
    if t.o_len - t.o_pos > 0 then (
      Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte ;
      k dst {t with o_pos= t.o_pos + 1; write= Int64.add t.write 1L} )
    else
      flush dst {t with state= ctor (fun dst t -> put_byte ~ctor byte k dst t)}

  module KWriteK = struct
    let put_byte = put_byte ~ctor:(fun k -> WriteK k)
    let tmp_header = Bytes.create 10

    let header kind len crc k dst t =
      let byt = ref ((kind lsl 4) lor Int64.(to_int (len && 15L))) in
      let len = ref Int64.(len >> 4) in
      let pos = ref 0 in
      while !len <> 0L do
        Bytes.set tmp_header !pos (Char.unsafe_chr (!byt lor 0x80)) ;
        (byt := Int64.(to_int (!len && 0x7FL))) ;
        (len := Int64.(!len >> 7)) ;
        pos := !pos + 1
      done ;
      Bytes.set tmp_header !pos (Char.unsafe_chr !byt) ;
      pos := !pos + 1 ;
      let rec loop idx crc dst t =
        if idx < !pos then
          let byte = Char.code (Bytes.get tmp_header idx) in
          let crc = Crc32.digestc crc byte in
          put_byte byte (loop (idx + 1) crc) dst t
        else k crc dst t
      in
      loop 0 crc dst t

    let tmp_offset = Bytes.create 10

    let offset n crc k dst t =
      let pos = ref 9 in
      let off = ref n in
      Bytes.set tmp_offset !pos (Char.chr Int64.(to_int (!off && 127L))) ;
      while Int64.(!off >> 7) <> 0L do
        (off := Int64.(!off >> 7)) ;
        pos := !pos - 1 ;
        Bytes.set tmp_offset !pos
          (Char.chr (128 lor Int64.(to_int ((!off - 1L) && 127L)))) ;
        off := Int64.sub !off 1L
      done ;
      let rec loop idx crc dst t =
        if idx = 10 then k crc dst t
        else
          let byte = Char.code (Bytes.get tmp_offset idx) in
          let crc = Crc32.digestc crc byte in
          put_byte byte (loop (idx + 1) crc) dst t
      in
      loop !pos crc dst t

    let hash hash crc k dst t =
      if t.o_len - t.o_pos >= Hash.digest_size then (
        let crc = Crc32.digests crc hash in
        Cstruct.blit_from_string hash 0 dst (t.o_off + t.o_pos)
          Hash.digest_size ;
        k crc dst
          { t with
            o_pos= t.o_pos + Hash.digest_size
          ; write= Int64.add t.write (Int64.of_int Hash.digest_size) } )
      else
        let rec loop rest crc dst t =
          if rest = 0 then k crc dst t
          else
            let n = min rest (t.o_len - t.o_pos) in
            if n = 0 then flush dst {t with state= Hash (loop rest crc)}
            else
              let crc =
                Crc32.digests crc ~off:(Hash.digest_size - rest) ~len:n hash
              in
              Cstruct.blit_from_string hash (Hash.digest_size - rest) dst
                (t.o_off + t.o_pos) n ;
              loop (rest - n) crc dst
                { t with
                  o_pos= t.o_pos + n; write= Int64.add t.write (Int64.of_int n)
                }
        in
        loop Hash.digest_size crc dst t
  end

  module KHash = struct
    let put_hash hash k dst t =
      if t.o_len - t.o_pos >= Hash.digest_size then (
        Cstruct.blit_from_string hash 0 dst (t.o_off + t.o_pos)
          Hash.digest_size ;
        k dst
          { t with
            o_pos= t.o_pos + Hash.digest_size
          ; write= Int64.add t.write (Int64.of_int Hash.digest_size) } )
      else
        let rec loop rest dst t =
          if rest = 0 then k dst t
          else
            let n = min rest (t.o_len - t.o_pos) in
            if n = 0 then Flush {t with state= Hash (loop rest)}
            else (
              Cstruct.blit_from_string hash (Hash.digest_size - rest) dst
                (t.o_off + t.o_pos) n ;
              Flush
                { t with
                  state= Hash (loop (rest - n))
                ; o_pos= t.o_pos + n
                ; write= Int64.add t.write (Int64.of_int n) } )
        in
        loop Hash.digest_size dst t
  end

  let hash dst t =
    let ctx = Hash.feed_cstruct t.hash (Cstruct.sub dst t.o_off t.o_pos) in
    let hash = Hash.get ctx in
    KHash.put_hash (Hash.to_raw_string hash)
      (fun _ t -> ok t hash)
      dst {t with hash= ctx}

  let writek kind entry entry_delta rest dst t =
    match kind, entry_delta with
    | KindRaw, {Delta.delta= Delta.Z} ->
        let abs_off = t.write in
        ( KWriteK.header
            (Kind.to_bin (Entry.kind entry))
            (Entry.length entry) Crc32.default
        @@ fun crc _ t ->
        let z = Deflate.default 4 in
        let z = Deflate.flush (t.o_off + t.o_pos) (t.o_len - t.o_pos) z in
        Cont
          { t with
            state= WriteZ {x= entry; r= rest; crc; off= abs_off; ui= 0; z}
          ; i_off= 0
          ; i_pos= 0
          ; i_len= 0 } )
          dst t
    | KindHash, {Delta.delta= Delta.S {length; hunks; src_length; src_hash; _}}
      ->
        let trg_length = Entry.length entry in
        let abs_off = t.write in
        (* XXX(dinosaure): we can obtain the source hash by [entry.delta].
           TODO! *)
        let h =
          Hunk.flush 0 (Cstruct.len t.h_tmp)
          @@ Hunk.default (Hunk.Hash src_hash) (Int64.to_int src_length)
               (Int64.to_int trg_length) hunks
          (* XXX(dinosaure): FIXME: [trg_length] is an [int64] but H expects an
             [int]. *)
        in
        ( KWriteK.header 0b111 (Int64.of_int length) Crc32.default
        @@ fun crc ->
        KWriteK.hash (Hash.to_raw_string src_hash) crc
        @@ fun crc _ t ->
        let z = Deflate.default 4 in
        let z = Deflate.flush (t.o_off + t.o_pos) (t.o_len - t.o_pos) z in
        Cont
          { t with
            state=
              WriteH
                {x= entry, entry_delta; r= rest; crc; off= abs_off; ui= 0; h; z}
          ; i_off= 0
          ; i_len= 0
          ; i_pos= 0 } )
          dst t
    | ( KindOffset
      , {Delta.delta= Delta.S {length; hunks; src_length; src_hash; _}} ) ->
        (* XXX(dinosaure): should not possible to fail. *)
        let _, src_off = Hash.Map.find src_hash t.map in
        let trg_length = Entry.length entry in
        let abs_off = t.write in
        let rel_off = Int64.sub abs_off src_off in
        let h =
          Hunk.flush 0 (Cstruct.len t.h_tmp)
          @@ Hunk.default (Hunk.Offset rel_off) (Int64.to_int src_length)
               (Int64.to_int trg_length) hunks
          (* XXX(dinosaure): FIXME: [trg_length] is an [int64] but H expects an
             [int]. *)
        in
        ( KWriteK.header 0b110 (Int64.of_int length) Crc32.default
        @@ fun crc ->
        KWriteK.offset rel_off crc
        @@ fun crc _ t ->
        let z = Deflate.default 4 in
        let z = Deflate.flush (t.o_off + t.o_pos) (t.o_len - t.o_pos) z in
        Cont
          { t with
            state=
              WriteH
                {x= entry, entry_delta; r= rest; crc; off= abs_off; ui= 0; h; z}
          ; i_off= 0
          ; i_len= 0
          ; i_pos= 0 } )
          dst t
    | (KindRaw | KindHash | KindOffset), {Delta.delta= Delta.S _ | Delta.Z; _}
      ->
        assert false

  (* XXX(dinosaure): impossible case, the code below never produce this
     combinaison. *)

  let writez src dst t x r crc off used_in z =
    match Deflate.eval ~src ~dst z with
    | `Await z ->
        await
          { t with
            state= WriteZ {x; r; crc; off; ui= used_in; z}
          ; i_pos= Deflate.used_in z }
    | `Flush z ->
        let crc =
          Crc32.digest ~off:(t.o_off + t.o_pos) ~len:(Deflate.used_out z) crc
            dst
        in
        flush dst
          { t with
            state= WriteZ {x; r; crc; off; ui= Deflate.used_in z; z}
          ; o_pos= t.o_pos + Deflate.used_out z
          ; i_pos= Deflate.used_in z
          ; write= Int64.add t.write (Int64.of_int (Deflate.used_out z)) }
    | `End z ->
        let crc =
          Crc32.digest ~off:(t.o_off + t.o_pos) ~len:(Deflate.used_out z) crc
            dst
        in
        Cont
          { t with
            state= Save {x; r; crc; off}
          ; o_pos= t.o_pos + Deflate.used_out z
          ; i_pos= Deflate.used_in z
          ; write= Int64.add t.write (Int64.of_int (Deflate.used_out z)) }
    | `Error (_, exn) -> error t (Deflate_error exn)

  let writeh src dst t ((entry, _) as x) r crc off used_in h z =
    match Deflate.eval ~src:t.h_tmp ~dst z with
    | `Await z -> (
      match Hunk.eval src t.h_tmp h with
      | `Await h ->
          await
            { t with
              state= WriteH {x; r; crc; off; ui= 0; z; h}
            ; i_pos= Hunk.used_in h }
      | `Flush h ->
          let used_in' = used_in + Deflate.used_in z in
          let z, h, ui =
            if used_in' = Hunk.used_out h then
              Deflate.no_flush 0 0 z, Hunk.flush 0 (Cstruct.len t.h_tmp) h, 0
            else
              ( Deflate.no_flush used_in' (Hunk.used_out h - used_in') z
              , h
              , used_in' )
          in
          Cont
            { t with
              state= WriteH {x; r; crc; off; ui; h; z}; i_pos= Hunk.used_in h
            }
      | `End h ->
          let used_in' = used_in + Deflate.used_in z in
          let z, h, ui =
            if used_in' = Hunk.used_out h then Deflate.finish z, h, used_in'
            else
              ( Deflate.no_flush used_in' (Hunk.used_out h - used_in') z
              , h
              , used_in' )
          in
          Cont
            { t with
              state= WriteH {x; r; crc; off; ui; h; z}; i_pos= Hunk.used_in h
            }
      | `Error (_, _) -> assert false )
    | `Flush z ->
        let crc =
          Crc32.digest ~off:(t.o_off + t.o_pos) ~len:(Deflate.used_out z) crc
            dst
        in
        let used_in' = used_in + Deflate.used_in z in
        flush dst
          { t with
            state= WriteH {x; r; crc; off; ui= used_in'; h; z}
          ; i_pos= Hunk.used_in h
          ; o_pos= t.o_pos + Deflate.used_out z
          ; write= Int64.add t.write (Int64.of_int (Deflate.used_out z)) }
    | `End z ->
        let crc =
          Crc32.digest ~off:(t.o_off + t.o_pos) ~len:(Deflate.used_out z) crc
            dst
        in
        Cont
          { t with
            state= Save {x= entry; r; crc; off}
          ; o_pos= t.o_pos + Deflate.used_out z
          ; i_pos= 0
          ; i_len= 0
          ; i_off= 0
          ; write= Int64.add t.write (Int64.of_int (Deflate.used_out z)) }
    | `Error (_, exn) -> error t (Deflate_error exn)

  let iter lst _ t =
    match lst with
    | [] -> Cont {t with state= Hash hash}
    | (entry, delta) :: r -> (
      match Entry.delta entry, delta with
      | Entry.From src_hash, {Delta.delta= Delta.S _} ->
          if Hash.Map.mem src_hash t.map then
            Cont {t with state= WriteK (writek KindOffset entry delta r)}
          else Cont {t with state= WriteK (writek KindHash entry delta r)}
      | Entry.None, {Delta.delta= Delta.Z} ->
          Cont {t with state= WriteK (writek KindRaw entry delta r)}
      | (Entry.None | Entry.From _), {Delta.delta= Delta.S _ | Delta.Z} ->
          error t (Invalid_hash (Entry.id entry)) )

  let save _ t x r crc off =
    Cont
      { t with
        state= Object (iter r); map= Hash.Map.add (Entry.id x) (crc, off) t.map
      }

  let number lst dst t =
    (* XXX(dinosaure): problem in 32-bits architecture. TODO! *)
    KHeader.put_u32
      (Int32.of_int (List.length lst))
      (fun _ t -> Cont {t with state= Object (iter lst)})
      dst t

  let version lst dst t =
    KHeader.put_u32 2l
      (fun _ t -> Cont {t with state= Header (number lst)})
      dst t

  let header lst dst t =
    ( KHeader.put_byte (Char.code 'P')
    @@ KHeader.put_byte (Char.code 'A')
    @@ KHeader.put_byte (Char.code 'C')
    @@ KHeader.put_byte (Char.code 'K')
    @@ fun _ t -> Cont {t with state= Header (version lst)} )
      dst t

  let used_out t = t.o_pos
  let idx t = t.map

  let eval src dst t =
    let eval0 t =
      match t.state with
      | Header k -> k dst t
      | Object k -> k dst t
      | WriteK k -> k dst t
      | WriteZ {x; r; crc; off; ui; z} -> writez src dst t x r crc off ui z
      | WriteH {x; r; crc; off; ui; h; z} ->
          writeh src dst t x r crc off ui h z
      | Save {x; r; crc; off} -> save dst t x r crc off
      | Exception exn -> error t exn
      | Hash k -> k dst t
      | End hash -> Ok (t, hash)
    in
    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Flush t -> `Flush t
      | Wait t -> `Await t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in
    loop t

  let flush offset len t =
    if t.o_len - t.o_pos = 0 then
      match t.state with
      | WriteZ {x; r; crc; off; ui; z} ->
          { t with
            o_off= offset
          ; o_len= len
          ; o_pos= 0
          ; state= WriteZ {x; r; crc; off; ui; z= Deflate.flush offset len z}
          }
      | WriteH {x; r; crc; off; ui; h; z} ->
          { t with
            o_off= offset
          ; o_len= len
          ; o_pos= 0
          ; state= WriteH {x; r; crc; off; ui; h; z= Deflate.flush offset len z}
          }
      | Header _ | Object _ | WriteK _ | Save _ | Hash _ | End _ | Exception _
        ->
          {t with o_off= offset; o_len= len; o_pos= 0}
    else
      match t.state with
      | End _ -> {t with o_off= offset; o_len= len; o_pos= 0}
      | Header _ | Object _ | WriteK _ | WriteZ _ | WriteH _ | Save _
       |Hash _ | Exception _ ->
          raise
            (Invalid_argument
               (Fmt.strf
                  "PACKEncoder.flush: you lost something (pos: %d, len: %d)"
                  t.o_pos t.o_len))

  let expect t =
    match t.state with
    | WriteH {x= entry, _; _} -> Entry.id entry
    | WriteZ {x= entry; _} -> Entry.id entry
    | Header _ | Object _ | WriteK _ | Save _ | Hash _ | End _ | Exception _ ->
        raise (Invalid_argument "PACKEncoder.expecti: bad state")

  (* let header_of_expected t = match t.state with | WriteH { x = (entry, _); _
     } | WriteZ { x = entry; _ } -> let typename = match entry.Entry.kind with
     | Kind.Commit -> "commit" | Kind.Tree -> "tree" | Kind.Blob -> "blob" |
     Kind.Tag -> "tag" in

     Fmt.strf "%s %Ld\000" typename entry.Entry.length | (Header _ | Object _ |
     WriteK _ | Save _ | Hash _ | End _ | Exception _) -> raise
     (Invalid_argument "PACKEncoder.header_of_expected: bad state") *)

  let refill offset len t =
    if t.i_len - t.i_pos = 0 then
      match t.state with
      | WriteZ {x; r; crc; off; ui; z} ->
          { t with
            i_off= offset
          ; i_len= len
          ; i_pos= 0
          ; state= WriteZ {x; r; crc; off; ui; z= Deflate.no_flush offset len z}
          }
      | WriteH {x; r; crc; off; ui; z; h} ->
          { t with
            i_off= offset
          ; i_len= len
          ; i_pos= 0
          ; state= WriteH {x; r; crc; off; ui; z; h= Hunk.refill offset len h}
          }
      | Header _ | Object _ | WriteK _ | Save _ | Hash _ | End _ | Exception _
        ->
          {t with i_off= offset; i_len= len; i_pos= 0}
    else
      raise
        (Invalid_argument
           (Fmt.strf
              "PACKEncoder.refill: you lost something (pos: %d, len: %d)"
              t.i_pos t.i_len))

  let finish t =
    if t.i_len - t.i_pos = 0 then
      match t.state with
      | WriteZ {x; r; crc; off; ui; z} ->
          {t with state= WriteZ {x; r; crc; off; ui; z= Deflate.finish z}}
      | WriteH {x; r; crc; off; ui; z; h} ->
          {t with state= WriteH {x; r; crc; off; ui; z; h= Hunk.finish h}}
      | Header _ | Object _ | WriteK _ | Save _ | Hash _ | End _ | Exception _
        ->
          t
    else
      raise
        (Invalid_argument
           (Fmt.strf
              "PACKEncoder.finish: you lost something (pos: %d, len: %d)"
              t.i_pos t.i_len))

  let used_in t =
    match t.state with
    | WriteZ {z; _} -> Deflate.used_in z
    | WriteH {h; _} -> Hunk.used_in h
    | Header _ | Object _ | WriteK _ | Save _ | Hash _ | End _ | Exception _ ->
        raise (Invalid_argument "PACKEncoder.used_in: bad state")

  let default h_tmp objects =
    { o_off= 0
    ; o_pos= 0
    ; o_len= 0
    ; i_off= 0
    ; i_pos= 0
    ; i_len= 0
    ; write= 0L
    ; map= Hash.Map.empty
    ; h_tmp
    ; hash= Hash.init ()
    ; state= Header (header objects) }
end

module Stream (Hash : S.HASH) (Deflate : S.DEFLATE) = struct
  module Entry = Entry (Hash)
  module Delta = Delta (Hash) (Entry)
  module Hunk = Hunk (Hash)
  module Pack = Pack (Hash) (Deflate) (Entry) (Delta) (Hunk)
  include Pack
end
