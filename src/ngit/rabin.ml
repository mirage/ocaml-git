external foreign_rabin_window  : unit -> int = "caml_rabin_window" [@@noalloc]
external foreign_rabin_shift   : unit -> int = "caml_rabin_shift"  [@@noalloc]

external rabin_hash_bytes      : Bytes.t -> int -> int -> int        = "caml_st_rabin_hash" [@@noalloc]
external rabin_hash_bigarray   : Cstruct.buffer -> int -> int -> int = "caml_ba_rabin_hash" [@@noalloc]

external rabin_derive_bytes    : int -> Bytes.t -> int -> int        = "caml_st_rabin_derive" [@@noalloc]
external rabin_derive_bigarray : int -> Cstruct.buffer -> int -> int = "caml_ba_rabin_derive" [@@noalloc]

let rabin_window = foreign_rabin_window ()
let rabin_shift  = foreign_rabin_window ()
let hash_limit   = 64

let hash buf off =
  rabin_hash_bigarray (Cstruct.to_bigarray buf) off (Cstruct.len buf)

let derive hash buf off =
  rabin_derive_bigarray hash (Cstruct.to_bigarray buf) off

type ptr = Entry of int | Null

type unpacked_entry =
  { offset    : int
  ; hash      : int
  ; next      : ptr }

let pp_list ?(sep = (fun fmt () -> ())) pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> Format.fprintf fmt "%a%a" pp_data x sep (); aux r
  in
  aux lst

let pp_unpacked_entry fmt entry =
  let pp_next fmt = function
    | Entry idx -> Format.fprintf fmt "%d" idx
    | Null -> Format.fprintf fmt "<null>"
  in

  Format.fprintf fmt "{ @[<hov>offset = %d;@ \
                               hash = %x;@ \
                               next = %a;@] }"
    entry.offset entry.hash pp_next entry.next

let pp_array pp_data fmt arr =
  let len = Array.length arr in

  let rec aux fmt idx =
    if idx = len
    then ()
    else if idx = len - 1
    then Format.fprintf fmt "%05d: %a" idx pp_data arr.(idx)
    else begin
      Format.fprintf fmt "%05d: %a;@ " idx pp_data arr.(idx);
      aux fmt (idx + 1);
    end
  in

  Format.fprintf fmt "[| @[<hov>%a@] |]"aux 0

let unsafe = function Entry idx -> idx | Null -> assert false
let safe arr = function Entry idx -> arr.(idx).next | Null -> Null

module Entry =
struct
  type t =
    { offset : int
    ; hash   : int }

  let pp fmt entry =
    Format.fprintf fmt "{ @[<hov>offset = %d;@ \
                                 hash = %x;@] }"
      entry.offset entry.hash

  let memory_size _ = 2
end

module Index =
struct
  type t =
    { hash : Entry.t list array
    ; mask : int
    ; buff : Cstruct.t }

  let memory_size { hash; mask; buff; } =
    3 + (Cstruct.len buff + 1) + 1 + (Array.fold_left (fun acc x -> List.length x * 4 + 1 + acc) 1 hash)

  let pp fmt index =
    let pp_lst fmt lst =
      let rec aux fmt = function
        | [] -> ()
        | [ x ] -> Entry.pp fmt x
        | x :: r -> Format.fprintf fmt "%a;@ " Entry.pp x; aux fmt r
      in
      Format.fprintf fmt "[ @[<hov>%a@] ]" aux lst
    in

    let pp_arr fmt arr =
      let len = Array.length arr in

      let rec aux fmt idx =
        if idx = len then ()
        else if idx = len - 1
        then Format.fprintf fmt "%05d: %a" idx pp_lst arr.(idx)
        else begin Format.fprintf fmt "%05d: %a;@ " idx pp_lst arr.(idx); aux fmt (idx + 1) end
      in

      Format.fprintf fmt "[| @[<hov>%a@] |]" aux 0
    in

    Format.fprintf fmt "{ @[<hov>hash = @[<hov>%a@];@ \
                                 mask = %x;@ \
                                 buff = #raw;@] }"
      pp_arr index.hash index.mask

  let unsafe_make buf =
    let len = min (Cstruct.len buf) 0xFFFFFFFE in
    (* XXX(dinosaure): in git, we can't encode on offset upper than
                       [0xFFFFFFFE]. So we limit the index to this area.
     *)
    let max = (len - 1) / rabin_window in
    let idx = ref (max * rabin_window - rabin_window) in
    let rev = ref 0 in
    let unpacked = Array.make max { offset = 0; hash = 0; next = Null; } in

    let hsize, hmask =
      let res = ref 4 in
      while (1 lsl !res) < (max / 4) do incr res done;
      1 lsl !res, (1 lsl !res) - 1
    in
    let htable = Array.make hsize Null in
    let hcount = Array.make hsize 0 in

    let previous = ref (lnot 0) in
    let entries  = ref max in

    while !idx >= 0
    do
      let hash = hash buf (!idx + 1) in

      if hash = !previous
      then begin
        unpacked.(!rev - 1) <- { unpacked.(!rev - 1) with offset = !idx + rabin_window };
        decr entries;
      end
          (* keep the lowest consecutive indentical blocks *)
      else begin
        previous := hash;

        unpacked.(!rev) <-
          { offset = !idx + rabin_window
          ; hash
          ; next   = htable.(hash land hmask) };

        htable.(hash land hmask) <- Entry (!rev);
        hcount.(hash land hmask) <- hcount.(hash land hmask) + 1;
        rev := !rev + 1;
      end;

      idx := !idx - rabin_window;
    done;

    (* Determine a limit on the number of entries in the same hash
       bucket. This guards us against pathological data sets causing
       really bad hash distribution with most entries in the same hash
       bucket that could bring us to O(m*n) computing costs (m and n
       corresponding to reference an target buffer sizes).

       Make sure none of the hash buckets has more entries than we're
       willing to test. Otherwise we cull the entry list
       uniformly to still preserve a good repartition across the
       reference buffer.
    *)
    for i = 0 to hsize - 1
    do
      (* Assume that this loop is gone through exactly HASH_LIMIT times and is
         entered and left with [acc = 0]. So the first statement in the loop
         contributes [(hcount.(i) - hash_limit) * hash_limit] to accumulator, and
         the inner loop consequently is run [hcount.(i) - hash_limit] times,
         removing one element from the list each time. Since [acc] balances out to
         0 at the final run, the inner loop body can't be left with [entry =
         `Null]. So we indeed encounter [entry = `Null] in the outer loop only.

         XXX(dinosaure): this assumption can't be encoded easily in OCaml (like in
                         C) but the unsafe case appear when we use the [unsafe]
                         function and we explain why the [assert false] (or the
                         `Null case) can't appear when we use [unsafe].

                         The [safe] function manipulates the [next] field in the
                         safe way. That means we keep [`Null] if we try to get a
                         [next] field with a [`Null] entry or we get the value of
                         the [next] field. Then, it can be [`Null] or [`Entry i].

                         It's a hard translation of a C code and need an
                         improvement. I don't want an C idiom inside an OCaml
                         code. Otherwise, I use a C stub ... TODO!
      *)

      match htable.(i) with
      | Entry idx when hcount.(i) > hash_limit ->
        (* XXX(dinosaure): htable.(i) = `Null and hcount.(i) > 0 can't appear. *)

        entries := !entries - (hcount.(i) - hash_limit);
        (* we leave exactly HASH_LIMIT entries in the bucket *)

        let acc   = ref (hcount.(i) - (2 * hash_limit)) in
        let entry = ref (Entry idx) in
        let keep  = Entry idx in

        entry := safe unpacked !entry;

        while !acc > 0
        do
          entry := safe unpacked !entry;
          (* XXX(dinosaure): safe get the [next] field at [!entry] in [res] or
                             keep [`Null] if [!entry = `Null]. *)
          acc   := !acc - hash_limit;
        done;

        unpacked.(unsafe keep) <- { unpacked.(unsafe keep) with next = safe unpacked !entry };

        entry := safe unpacked !entry;

        while !entry <> Null
        do
          acc := !acc + (hcount.(i) - hash_limit);

          if !acc > 0
          then begin
            let keep = !entry in

            entry := safe unpacked !entry;
            acc   := !acc - hash_limit;

            while !acc > 0
            do
              entry := safe unpacked !entry;
              acc   := !acc - hash_limit;
            done;

            unpacked.(unsafe keep) <- { unpacked.(unsafe keep) with next = safe unpacked !entry };

            (* XXX(dinosaure): we can use [unsafe] because when [keep = !entry],
                               we check that [!entry <> `Null]. *)
          end;

          entry := safe unpacked !entry;
        done
      | Entry _ | Null -> ()
    done;

    (* let unpacked = Array.sub unpacked 0 !entries in

       XXX(dinosaure): can't sub the result because some references [`Entry idx]
                       follow some upper entries than [!entries]. We need to
                       introspect what is the problem.
     *)
    let packed = Array.make hsize [] in

    for i = 0 to hsize - 1
    do
      (* Coalesce all entries belonging to one linked list
         into consecutive array entries.
       *)
      let rec aux acc = function
        | Null -> List.rev acc
        | Entry idx ->
          let { offset; hash; next; } = unpacked.(idx) in
          aux (Entry.{ offset; hash; } :: acc) next
      in

      packed.(i) <- aux [] htable.(i);
    done;

    { hash = packed
    ; mask = hmask
    ; buff = buf }

  let make ?(copy = false) buf =
    if copy
    then
      let len = Cstruct.len buf in
      let raw = Cstruct.create len in

      Cstruct.blit buf 0 raw 0 len;

      unsafe_make raw
    else unsafe_make buf
end

let cstruct_iteri ?(start = 0) f c =
  let len = Cstruct.len c in

  if start < 0 || start > len
  then raise (Invalid_argument "cstruct_iter");

  let idx = ref start in

  while !idx < len
  do match f !idx (Cstruct.get_uint8 c !idx) with
    | `Stay -> ()
    | `Cont -> incr idx
    | `Move add ->
      let max = len - !idx in
      idx := !idx + (min max add)
  done

let cstruct_foldi ?start f init c =
  let acc = ref init in

  cstruct_iteri
    ?start
    (fun i x -> let action, a = f !acc i x in acc := a; action) c;
  !acc

let same (src, src_off) (dst, dst_off) =
  let idx = ref 0 in
  let src_len = Cstruct.len src in
  let dst_len = Cstruct.len dst in

  while src_off + !idx < src_len
        && dst_off + !idx < dst_len
        && Cstruct.get_char src (src_off + !idx) = Cstruct.get_char dst (dst_off + !idx)
  do incr idx done;

  !idx

let revsame ~limit (src, src_off) (dst, dst_off) =
  let idx = ref 0 in

  while src_off - !idx >= 0
        && dst_off - !idx >= 0
        && Cstruct.get_char src (src_off - !idx) = Cstruct.get_char dst (dst_off - !idx)
        && !idx < limit
  do incr idx done;

  !idx

(* XXX(dinosaure): partial result. *)
type e =
  | C of (int * int)
  | I of (int * int)

let pp fmt = function
  | C (off, len) -> Format.fprintf fmt "(Copy (%d, %d))" off len
  | I (off, len) -> Format.fprintf fmt "(Insert (%d, %d))" off len

let delta index buf =
  let make (acc, (copy_off, copy_len), current_hash) offset _ =
    let (copy_off, copy_len), current_hash =
      if copy_len < 4096
      then
        let current_hash = derive current_hash buf offset in

        List.fold_left
          (fun (copy_off, copy_len) entry ->
             let same = same (index.Index.buff, entry.Entry.offset) (buf, offset) in

             (* XXX(dinosaure): git shortcut this compute when [copy_len >= 4096].
                                In imperative way, it's good but, guy, it's OCaml.
                                We can use an exception and raise it in this situation
                                but I don't think it's *very* relevant.
              *)

            if same > copy_len
            then (entry.Entry.offset, same) (* this is our best match so far *)
            else (copy_off, copy_len))
          (copy_off, copy_len)
          (Array.get index.Index.hash (current_hash land index.Index.mask)
           |> List.filter (fun entry -> entry.Entry.hash = current_hash)),
        current_hash
      else (copy_off, copy_len), current_hash
    in

    if copy_len < 4
    then match acc with
      | I (off, len) :: r ->
        if len = 0x7F
        then `Cont, (I (offset, 1) :: I (off, len) :: r, (offset, 0), current_hash)
        else `Cont, (I (off, len + 1) :: r, (offset, 0), current_hash)
      | (C (_, _) :: _ | []) as acc ->
        `Cont, (I (offset, 1) :: acc, (offset, 0), current_hash)
    else
      let revsame = match acc with
        | I (poff, plen) :: r ->
          revsame ~limit:(offset - poff) (index.Index.buff, copy_off - 1) (buf, offset - 1)
        | _ -> 0
          (* XXX(dinosaure): this case concerns an empty list and a list started with a [C].
                             in any case, we can't move back. *)
      in

      let acc = match acc with
        | I (poff, plen) :: r ->
          if revsame > 0 && plen = revsame
          then r
          else if revsame > 0
          then I (poff, plen - revsame) :: r
          else I (poff, plen) :: r
        | lst -> lst
      in

      (* XXX(dinosaure): in git, the length of a pattern can't be upper than 0x10000. *)
      if copy_len + revsame > 0x10000
      then begin
        `Move (0x10000 - revsame),
        (C (copy_off - revsame, 0x10000) :: acc,
         (copy_off - revsame + 0x10000, copy_len + revsame - 0x10000),
         current_hash)
      end else begin
        `Move copy_len,
        (C (copy_off - revsame, copy_len + revsame) :: acc,
         (copy_off + copy_len + revsame, 0),
         hash buf (offset + copy_len - rabin_window))
      end
  in

  let hash = hash buf 0 in
  let consumed = min rabin_window (Cstruct.len buf) in

  cstruct_foldi ~start:consumed make ([ I (0, consumed) ], (0, 0), hash) buf
  |> fun (res, _, _) -> List.rev res
