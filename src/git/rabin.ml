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

module type VALUE =
sig
  val window : int
  val shift  : int
  val limit  : int
end

module UInt32 =
struct
  include Int32

  let div a b = assert false
  let rem a b = assert false

  (* XXX(dinosaure): we use the [int32] representation as a fake unsigned
     [int32]. So we consider than all number between [-2147483648 .. 0[ is, in
     reality, ]2147483647 .. 4294967295]. Obviously, this hack in only available
     in an 64-bits architecture (in 32-bits, a native integer can not store the
     4294967295 number), so when the number is upper than [max_int] or lower
     than 0, we can not store the unsigned [int32] to a native integer.

     NOTE: in reality, the last case is not common. *)
  let to_int x =
    if Sys.word_size = 64
    then (if compare 0l x = 1 (* number is negative. *)
          then 2147483647 + (2147483648 + (to_int x))
          else to_int x)
    else (if compare 0l x = 1 || compare x 1073741823l = 1
          then raise (Failure "This architecture does not support an unsigned 32-bits integer as native integer")
          else to_int x)

  let compare a b =
    (compare (sub a min_int) (sub b min_int)) (* @dbuenzli <3 *)

  let ( lsl ) = shift_left
  let ( lsr ) = shift_right
  let ( lor ) = logor
  let ( lxor ) = logxor
  let ( lnot ) = lognot
  let ( land ) = logand
  let ( - ) = sub

  let of_char chr = of_int (Char.code chr)
  let to_string = Fmt.strf "%lu"
end

let t =
	[| 0x00000000l; 0xab59b4d1l; 0x56b369a2l; 0xfdeadd73l; 0x063f6795l; 0xad66d344l;
	   0x508c0e37l; 0xfbd5bae6l; 0x0c7ecf2al; 0xa7277bfbl; 0x5acda688l; 0xf1941259l;
	   0x0a41a8bfl; 0xa1181c6el; 0x5cf2c11dl; 0xf7ab75ccl; 0x18fd9e54l; 0xb3a42a85l;
	   0x4e4ef7f6l; 0xe5174327l; 0x1ec2f9c1l; 0xb59b4d10l; 0x48719063l; 0xe32824b2l;
	   0x1483517el; 0xbfdae5afl; 0x423038dcl; 0xe9698c0dl; 0x12bc36ebl; 0xb9e5823al;
	   0x440f5f49l; 0xef56eb98l; 0x31fb3ca8l; 0x9aa28879l; 0x6748550al; 0xcc11e1dbl;
	   0x37c45b3dl; 0x9c9defecl; 0x6177329fl; 0xca2e864el; 0x3d85f382l; 0x96dc4753l;
	   0x6b369a20l; 0xc06f2ef1l; 0x3bba9417l; 0x90e320c6l; 0x6d09fdb5l; 0xc6504964l;
	   0x2906a2fcl; 0x825f162dl; 0x7fb5cb5el; 0xd4ec7f8fl; 0x2f39c569l; 0x846071b8l;
	   0x798aaccbl; 0xd2d3181al; 0x25786dd6l; 0x8e21d907l; 0x73cb0474l; 0xd892b0a5l;
	   0x23470a43l; 0x881ebe92l; 0x75f463e1l; 0xdeadd730l; 0x63f67950l; 0xc8afcd81l;
	   0x354510f2l; 0x9e1ca423l; 0x65c91ec5l; 0xce90aa14l; 0x337a7767l; 0x9823c3b6l;
	   0x6f88b67al; 0xc4d102abl; 0x393bdfd8l; 0x92626b09l; 0x69b7d1efl; 0xc2ee653el;
	   0x3f04b84dl; 0x945d0c9cl; 0x7b0be704l; 0xd05253d5l; 0x2db88ea6l; 0x86e13a77l;
	   0x7d348091l; 0xd66d3440l; 0x2b87e933l; 0x80de5de2l; 0x7775282el; 0xdc2c9cffl;
	   0x21c6418cl; 0x8a9ff55dl; 0x714a4fbbl; 0xda13fb6al; 0x27f92619l; 0x8ca092c8l;
	   0x520d45f8l; 0xf954f129l; 0x04be2c5al; 0xafe7988bl; 0x5432226dl; 0xff6b96bcl;
	   0x02814bcfl; 0xa9d8ff1el; 0x5e738ad2l; 0xf52a3e03l; 0x08c0e370l; 0xa39957a1l;
	   0x584ced47l; 0xf3155996l; 0x0eff84e5l; 0xa5a63034l; 0x4af0dbacl; 0xe1a96f7dl;
	   0x1c43b20el; 0xb71a06dfl; 0x4ccfbc39l; 0xe79608e8l; 0x1a7cd59bl; 0xb125614al;
	   0x468e1486l; 0xedd7a057l; 0x103d7d24l; 0xbb64c9f5l; 0x40b17313l; 0xebe8c7c2l;
	   0x16021ab1l; 0xbd5bae60l; 0x6cb54671l; 0xc7ecf2a0l; 0x3a062fd3l; 0x915f9b02l;
	   0x6a8a21e4l; 0xc1d39535l; 0x3c394846l; 0x9760fc97l; 0x60cb895bl; 0xcb923d8al;
	   0x3678e0f9l; 0x9d215428l; 0x66f4eecel; 0xcdad5a1fl; 0x3047876cl; 0x9b1e33bdl;
	   0x7448d825l; 0xdf116cf4l; 0x22fbb187l; 0x89a20556l; 0x7277bfb0l; 0xd92e0b61l;
	   0x24c4d612l; 0x8f9d62c3l; 0x7836170fl; 0xd36fa3del; 0x2e857eadl; 0x85dcca7cl;
	   0x7e09709al; 0xd550c44bl; 0x28ba1938l; 0x83e3ade9l; 0x5d4e7ad9l; 0xf617ce08l;
	   0x0bfd137bl; 0xa0a4a7aal; 0x5b711d4cl; 0xf028a99dl; 0x0dc274eel; 0xa69bc03fl;
	   0x5130b5f3l; 0xfa690122l; 0x0783dc51l; 0xacda6880l; 0x570fd266l; 0xfc5666b7l;
	   0x01bcbbc4l; 0xaae50f15l; 0x45b3e48dl; 0xeeea505cl; 0x13008d2fl; 0xb85939fel;
	   0x438c8318l; 0xe8d537c9l; 0x153feabal; 0xbe665e6bl; 0x49cd2ba7l; 0xe2949f76l;
	   0x1f7e4205l; 0xb427f6d4l; 0x4ff24c32l; 0xe4abf8e3l; 0x19412590l; 0xb2189141l;
	   0x0f433f21l; 0xa41a8bf0l; 0x59f05683l; 0xf2a9e252l; 0x097c58b4l; 0xa225ec65l;
	   0x5fcf3116l; 0xf49685c7l; 0x033df00bl; 0xa86444dal; 0x558e99a9l; 0xfed72d78l;
	   0x0502979el; 0xae5b234fl; 0x53b1fe3cl; 0xf8e84aedl; 0x17bea175l; 0xbce715a4l;
	   0x410dc8d7l; 0xea547c06l; 0x1181c6e0l; 0xbad87231l; 0x4732af42l; 0xec6b1b93l;
	   0x1bc06e5fl; 0xb099da8el; 0x4d7307fdl; 0xe62ab32cl; 0x1dff09cal; 0xb6a6bd1bl;
	   0x4b4c6068l; 0xe015d4b9l; 0x3eb80389l; 0x95e1b758l; 0x680b6a2bl; 0xc352defal;
	   0x3887641cl; 0x93ded0cdl; 0x6e340dbel; 0xc56db96fl; 0x32c6cca3l; 0x999f7872l;
	   0x6475a501l; 0xcf2c11d0l; 0x34f9ab36l; 0x9fa01fe7l; 0x624ac294l; 0xc9137645l;
	   0x26459dddl; 0x8d1c290cl; 0x70f6f47fl; 0xdbaf40ael; 0x207afa48l; 0x8b234e99l;
	   0x76c993eal; 0xdd90273bl; 0x2a3b52f7l; 0x8162e626l; 0x7c883b55l; 0xd7d18f84l;
	   0x2c043562l; 0x875d81b3l; 0x7ab75cc0l; 0xd1eee811l; |]

let u =
	[| 0x00000000l; 0x7eb5200dl; 0x5633f4cbl; 0x2886d4c6l; 0x073e5d47l; 0x798b7d4al;
     0x510da98cl; 0x2fb88981l; 0x0e7cba8el; 0x70c99a83l; 0x584f4e45l; 0x26fa6e48l;
     0x0942e7c9l; 0x77f7c7c4l; 0x5f711302l; 0x21c4330fl; 0x1cf9751cl; 0x624c5511l;
     0x4aca81d7l; 0x347fa1dal; 0x1bc7285bl; 0x65720856l; 0x4df4dc90l; 0x3341fc9dl;
     0x1285cf92l; 0x6c30ef9fl; 0x44b63b59l; 0x3a031b54l; 0x15bb92d5l; 0x6b0eb2d8l;
     0x4388661el; 0x3d3d4613l; 0x39f2ea38l; 0x4747ca35l; 0x6fc11ef3l; 0x11743efel;
     0x3eccb77fl; 0x40799772l; 0x68ff43b4l; 0x164a63b9l; 0x378e50b6l; 0x493b70bbl;
     0x61bda47dl; 0x1f088470l; 0x30b00df1l; 0x4e052dfcl; 0x6683f93al; 0x1836d937l;
     0x250b9f24l; 0x5bbebf29l; 0x73386befl; 0x0d8d4be2l; 0x2235c263l; 0x5c80e26el;
     0x740636a8l; 0x0ab316a5l; 0x2b7725aal; 0x55c205a7l; 0x7d44d161l; 0x03f1f16cl;
     0x2c4978edl; 0x52fc58e0l; 0x7a7a8c26l; 0x04cfac2bl; 0x73e5d470l; 0x0d50f47dl;
     0x25d620bbl; 0x5b6300b6l; 0x74db8937l; 0x0a6ea93al; 0x22e87dfcl; 0x5c5d5df1l;
     0x7d996efel; 0x032c4ef3l; 0x2baa9a35l; 0x551fba38l; 0x7aa733b9l; 0x041213b4l;
     0x2c94c772l; 0x5221e77fl; 0x6f1ca16cl; 0x11a98161l; 0x392f55a7l; 0x479a75aal;
     0x6822fc2bl; 0x1697dc26l; 0x3e1108e0l; 0x40a428edl; 0x61601be2l; 0x1fd53befl;
     0x3753ef29l; 0x49e6cf24l; 0x665e46a5l; 0x18eb66a8l; 0x306db26el; 0x4ed89263l;
     0x4a173e48l; 0x34a21e45l; 0x1c24ca83l; 0x6291ea8el; 0x4d29630fl; 0x339c4302l;
     0x1b1a97c4l; 0x65afb7c9l; 0x446b84c6l; 0x3adea4cbl; 0x1258700dl; 0x6ced5000l;
     0x4355d981l; 0x3de0f98cl; 0x15662d4al; 0x6bd30d47l; 0x56ee4b54l; 0x285b6b59l;
     0x00ddbf9fl; 0x7e689f92l; 0x51d01613l; 0x2f65361el; 0x07e3e2d8l; 0x7956c2d5l;
     0x5892f1dal; 0x2627d1d7l; 0x0ea10511l; 0x7014251cl; 0x5facac9dl; 0x21198c90l;
     0x099f5856l; 0x772a785bl; 0x4c921c31l; 0x32273c3cl; 0x1aa1e8fal; 0x6414c8f7l;
     0x4bac4176l; 0x3519617bl; 0x1d9fb5bdl; 0x632a95b0l; 0x42eea6bfl; 0x3c5b86b2l;
     0x14dd5274l; 0x6a687279l; 0x45d0fbf8l; 0x3b65dbf5l; 0x13e30f33l; 0x6d562f3el;
     0x506b692dl; 0x2ede4920l; 0x06589de6l; 0x78edbdebl; 0x5755346al; 0x29e01467l;
     0x0166c0a1l; 0x7fd3e0acl; 0x5e17d3a3l; 0x20a2f3ael; 0x08242768l; 0x76910765l;
     0x59298ee4l; 0x279caee9l; 0x0f1a7a2fl; 0x71af5a22l; 0x7560f609l; 0x0bd5d604l;
     0x235302c2l; 0x5de622cfl; 0x725eab4el; 0x0ceb8b43l; 0x246d5f85l; 0x5ad87f88l;
     0x7b1c4c87l; 0x05a96c8al; 0x2d2fb84cl; 0x539a9841l; 0x7c2211c0l; 0x029731cdl;
     0x2a11e50bl; 0x54a4c506l; 0x69998315l; 0x172ca318l; 0x3faa77del; 0x411f57d3l;
     0x6ea7de52l; 0x1012fe5fl; 0x38942a99l; 0x46210a94l; 0x67e5399bl; 0x19501996l;
     0x31d6cd50l; 0x4f63ed5dl; 0x60db64dcl; 0x1e6e44d1l; 0x36e89017l; 0x485db01al;
     0x3f77c841l; 0x41c2e84cl; 0x69443c8al; 0x17f11c87l; 0x38499506l; 0x46fcb50bl;
     0x6e7a61cdl; 0x10cf41c0l; 0x310b72cfl; 0x4fbe52c2l; 0x67388604l; 0x198da609l;
     0x36352f88l; 0x48800f85l; 0x6006db43l; 0x1eb3fb4el; 0x238ebd5dl; 0x5d3b9d50l;
     0x75bd4996l; 0x0b08699bl; 0x24b0e01al; 0x5a05c017l; 0x728314d1l; 0x0c3634dcl;
     0x2df207d3l; 0x534727del; 0x7bc1f318l; 0x0574d315l; 0x2acc5a94l; 0x54797a99l;
     0x7cffae5fl; 0x024a8e52l; 0x06852279l; 0x78300274l; 0x50b6d6b2l; 0x2e03f6bfl;
     0x01bb7f3el; 0x7f0e5f33l; 0x57888bf5l; 0x293dabf8l; 0x08f998f7l; 0x764cb8fal;
     0x5eca6c3cl; 0x207f4c31l; 0x0fc7c5b0l; 0x7172e5bdl; 0x59f4317bl; 0x27411176l;
     0x1a7c5765l; 0x64c97768l; 0x4c4fa3ael; 0x32fa83a3l; 0x1d420a22l; 0x63f72a2fl;
     0x4b71fee9l; 0x35c4dee4l; 0x1400edebl; 0x6ab5cde6l; 0x42331920l; 0x3c86392dl;
     0x133eb0acl; 0x6d8b90a1l; 0x450d4467l; 0x3bb8646al; |]

module type BUFFER =
sig
  type t

  val length : t -> int
  val get : t -> int -> char
end

module Make (B : BUFFER) (V : VALUE) =
struct
  let hash buf off len =
    let i = ref 0 in
    let v = ref 0l in

    while off + !i < len && !i < V.window
    do v := UInt32.(((!v lsl 8) lor (of_char (B.get buf (off + !i)))) lxor t.(to_int (!v lsr V.shift)));
       incr i done;

    !v

  let derive v buf off =
    let r = off - V.window in
    let v = UInt32.(v lxor u.(Char.code (B.get buf r))) in
    let v = UInt32.(((v lsl 8) lor (of_char (B.get buf off))) lxor t.(to_int (v lsr V.shift))) in

    v
end

type ptr = Entry of int | Null

type unpacked_entry =
  { offset    : int
  ; hash      : UInt32.t
  ; next      : ptr }

let pp_unpacked_entry ppf entry =
  let pp_next ppf = function
    | Entry idx -> Fmt.int ppf idx
    | Null -> Fmt.string ppf "<null>"
  in

  Fmt.pf ppf "{ @[<hov>offset = %d;@ \
                       hash = %lxu;@ \
                       next = %a;@] }"
    entry.offset entry.hash pp_next entry.next

let unsafe = function Entry idx -> idx | Null -> assert false
let safe arr = function Entry idx -> arr.(idx).next | Null -> Null

module Entry =
struct
  type t =
    { offset : int
    ; hash   : UInt32.t }

  let pp ppf entry =
    Fmt.pf ppf "{ @[<hov>offset = %d;@ \
                         hash = %lxu;@] }"
      entry.offset entry.hash

  let memory_size _ = 2
end

module Index (V : VALUE) =
struct
  module Hash = Make(struct type t = Cstruct.t let get = Cstruct.get_char let length = Cstruct.len end)(V)

  type t =
    { hash : Entry.t list array
    ; mask : UInt32.t
    ; buff : Cstruct.t }

  let memory_size { hash; mask; buff; } =
    3 + (Cstruct.len buff + 1) + 1 + (Array.fold_left (fun acc x -> List.length x * 4 + 1 + acc) 1 hash)

  let pp ppf index =
    Fmt.pf ppf "{ @[<hov>hash = [ %a ];@ \
                         mask = %lxu;@ \
                         buff = #raw;@] }"
      (Fmt.hvbox (Fmt.array (Fmt.list Entry.pp))) index.hash index.mask

  let unsafe_make buf =
    let len =
      if Sys.word_size = 64
      then min (Cstruct.len buf) 0xFFFFFFFE
      else min (Cstruct.len buf) max_int
    in
    (* XXX(dinosaure): in git, we can't encode on offset upper than
       [0xFFFFFFFE]. So we limit the index to this area. *)
    let max = (len - 1) / V.window in
    let idx = ref (max * V.window - V.window) in
    let rev = ref 0 in
    let unpacked = Array.make max { offset = 0; hash = 0l; next = Null; } in

    let hsize, hmask =
      let res = ref 4 in
      while (1 lsl !res) < (max / 4) do incr res done;
      1 lsl !res, UInt32.((1l lsl !res) - 1l)
    in
    let htable = Array.make hsize Null in
    let hcount = Array.make hsize 0 in

    let previous = ref UInt32.(lnot 0l) in
    let entries  = ref max in

    while !idx >= 0
    do
      let hash = Hash.hash buf (!idx + 1) (Cstruct.len buf) in

      if UInt32.compare hash !previous = 0
      then begin
        unpacked.(!rev - 1) <- { unpacked.(!rev - 1) with offset = !idx + V.window };
        decr entries;
      end
      (* keep the lowest consecutive indentical blocks *)
      else begin
        previous := hash;

        unpacked.(!rev) <-
          { offset = !idx + V.window
          ; hash
          ; next   = htable.(UInt32.(to_int (hash land hmask))) };

        htable.(UInt32.(to_int (hash land hmask))) <- Entry (!rev);
        hcount.(UInt32.(to_int (hash land hmask))) <- hcount.(UInt32.(to_int (hash land hmask))) + 1;
        rev := !rev + 1;
      end;

      idx := !idx - V.window;
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

         XXX(dinosaure): this assumption can't be encoded easily in OCaml (like
         in C) but the unsafe case appear when we use the [unsafe] function and
         we explain why the [assert false] (or the `Null case) can't appear when
         we use [unsafe].

         The [safe] function manipulates the [next] field in the safe way. That
         means we keep [`Null] if we try to get a [next] field with a [`Null]
         entry or we get the value of the [next] field. Then, it can be [`Null]
         or [`Entry i].

         It's a hard translation of a C code and need an improvement. I don't
         want an C idiom inside an OCaml code. Otherwise, I use a C stub ...
         TODO! *)

      match htable.(i) with
      | Entry idx when hcount.(i) > V.limit ->
        (* XXX(dinosaure): htable.(i) = `Null and hcount.(i) > 0 can't appear. *)

        entries := !entries - (hcount.(i) - V.limit);
        (* we leave exactly HASH_LIMIT entries in the bucket *)

        let acc   = ref (hcount.(i) - (2 * V.limit)) in
        let entry = ref (Entry idx) in
        let keep  = Entry idx in

        entry := safe unpacked !entry;

        while !acc > 0
        do
          entry := safe unpacked !entry;
          (* XXX(dinosaure): safe get the [next] field at [!entry] in [res] or
                             keep [`Null] if [!entry = `Null]. *)
          acc   := !acc - V.limit;
        done;

        unpacked.(unsafe keep) <- { unpacked.(unsafe keep) with next = safe unpacked !entry };

        entry := safe unpacked !entry;

        while !entry <> Null
        do
          acc := !acc + (hcount.(i) - V.limit);

          if !acc > 0
          then begin
            let keep = !entry in

            entry := safe unpacked !entry;
            acc   := !acc - V.limit;

            while !acc > 0
            do
              entry := safe unpacked !entry;
              acc   := !acc - V.limit;
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
type t =
  | Copy of (int * int)
  | Insert of (int * int)

let pp ppf = function
  | Copy (off, len) -> Fmt.pf ppf "(Copy (%d, %d))" off len
  | Insert (off, len) -> Fmt.pf ppf "(Insert (%d, %d))" off len

module Delta (V : VALUE) =
struct
  module Index = Index(V)

  let delta index buf =
    let derive = Index.Hash.derive in

    let make (acc, (copy_off, copy_len), current_hash) offset _ =
      let (copy_off, copy_len), current_hash =
        if copy_len < 4096
        then
          let current_hash = derive current_hash buf offset in

          List.fold_left
            (fun (copy_off, copy_len) entry ->
              let same = same (index.Index.buff, entry.Entry.offset) (buf, offset) in

              (* XXX(dinosaure): git shortcut this compute when [copy_len >=
                 4096]. In imperative way, it's good but, guy, it's OCaml. We
                 can use an exception and raise it in this situation but I don't
                 think it's *very* relevant. *)

              if same > copy_len
              then (entry.Entry.offset, same) (* this is our best match so far *)
              else (copy_off, copy_len))
            (copy_off, copy_len)
            (Array.get index.Index.hash UInt32.(to_int (current_hash land index.Index.mask))
            |> List.filter (fun entry -> UInt32.compare entry.Entry.hash current_hash = 0)),
          current_hash
        else (copy_off, copy_len), current_hash
      in

      if copy_len < 4
      then match acc with
        | Insert (off, len) :: r ->
          if len = 0x7F
          then `Cont, (Insert (offset, 1) :: Insert (off, len) :: r, (offset, 0), current_hash)
          else `Cont, (Insert (off, len + 1) :: r, (offset, 0), current_hash)
        | (Copy (_, _) :: _ | []) as acc ->
          `Cont, (Insert (offset, 1) :: acc, (offset, 0), current_hash)
      else
        let revsame = match acc with
          | Insert (poff, plen) :: r ->
            revsame ~limit:(offset - poff) (index.Index.buff, copy_off - 1) (buf, offset - 1)
          | _ -> 0
          (* XXX(dinosaure): this case concerns an empty list and a list started
             with a [C]. in any case, we can't move back. *)
        in

        let acc = match acc with
          | Insert (poff, plen) :: r ->
            if revsame > 0 && plen = revsame
            then r
            else if revsame > 0
            then Insert (poff, plen - revsame) :: r
            else Insert (poff, plen) :: r
          | lst -> lst
        in

        (* XXX(dinosaure): in git, the length of a pattern can't be upper than 0x10000. *)
        if copy_len + revsame > 0x10000
        then begin
          `Move (0x10000 - revsame),
          (Copy (copy_off - revsame, 0x10000) :: acc,
          (copy_off - revsame + 0x10000, copy_len + revsame - 0x10000),
          current_hash)
        end else begin
          `Move copy_len,
          (Copy (copy_off - revsame, copy_len + revsame) :: acc,
          (copy_off + copy_len + revsame, 0),
          Index.Hash.hash buf (offset + copy_len - V.window) (Cstruct.len buf))
        end
    in

    let hash = Index.Hash.hash buf 0 (Cstruct.len buf) in
    let consumed = min V.window (Cstruct.len buf) in

    cstruct_foldi ~start:consumed make ([ Insert (0, consumed) ], (0, 0), hash) buf
    |> fun (res, _, _) -> List.rev res
end

module Default = Delta(struct let window, shift, limit = 16, 23, 64 end)
