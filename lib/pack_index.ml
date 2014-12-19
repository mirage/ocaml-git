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
}

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
