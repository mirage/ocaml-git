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
open Core_kernel.Std
module Log = Log.Make(struct let section = "pack-index" end)

module T = struct
  type t = {
    offsets : int SHA1.Map.t;
    crcs    : int32 SHA1.Map.t;
    pack_checksum: SHA1.t;
  } with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Value"
end
include T
include Identifiable.Make (T)

let empty ~pack_checksum = {
  offsets = SHA1.Map.empty;
  crcs    = SHA1.Map.empty;
  pack_checksum;
}

let pretty t =
  let buf = Buffer.create 1024 in
  bprintf buf "pack-checksum: %s\n" (SHA1.to_hex t.pack_checksum);
  SHA1.Map.iter2 ~f:(fun ~key ~data ->
      match data with
      | `Both (offset, crc) ->
        bprintf buf "%s: off:%d crc:%ld\n" (SHA1.to_hex key) offset crc
      | _ -> assert false
    ) t.offsets t.crcs;
  Buffer.contents buf

let id_monad =
  (fun x -> x), (fun x f -> f x)

let lengths { offsets } =
  Log.debugf "lengths";
  let rec aux acc = function
    | []    -> List.rev acc
    | [h,l] -> aux ((h, None)::acc) []
    | (h1,l1)::((h2,l2)::_ as t) -> aux ((h1, Some (l2-l1))::acc) t in
  let l = SHA1.Map.to_alist offsets in
  let l = List.sort ~cmp:(fun (_,x) (_,y) -> Int.compare x y) l in
  SHA1.Map.of_alist_exn (aux [] l)

let input buf =
  Log.debugf "input";

  let magic = Mstruct.get_string buf 4 in
  if String.(magic <> "\255tOc") then
    Mstruct.parse_error_buf buf "wrong magic index (%S)" magic;
  let version = Mstruct.get_be_uint32 buf in
  if Int32.(version <> 2l) then
    Mstruct.parse_error_buf buf "wrong index version (%ld)" version;

  (* Read the first-level fanout *)
  Log.debugf "input: reading the first-level fanout";
  let fanout =
    let a = Array.create 256 0l in
    for i=0 to 255 do
      a.(i) <- Mstruct.get_be_uint32 buf;
    done;
    a in

  let nb_objects = Int32.to_int_exn fanout.(255) in

  (* Read the names *)
  Log.debugf "input: reading the %d objects IDs" nb_objects;
  let names =
    let a = Array.create nb_objects (SHA1.of_string "") in
    for i=0 to nb_objects-1 do
      a.(i) <- SHA1.input buf;
    done;
    a in

  (* Read the CRCs *)
  Log.debugf "input: reading the %d CRCs" nb_objects;
  let crcs =
    let a = Array.create nb_objects (SHA1.of_string "", 0l) in
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
      let more = match Int.(Mstruct.get_uint8 buf land 128) with
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
        (name, Int64.to_int_exn offset)
      ) else
        (name, Int32.to_int_exn offset)
    ) names in
  let pack_checksum = SHA1.input buf in
  let _checksum = SHA1.input buf in

  let offsets_alist = Array.to_list offsets in
  let offsets = SHA1.Map.of_alist_exn offsets_alist in
  let crcs = SHA1.Map.of_alist_exn (Array.to_list crcs) in
  { offsets; crcs; pack_checksum }

let add buf t =
  let n = SHA1.Map.length t.offsets in
  Log.debugf "output: %d packed values" n;
  Bigbuffer.add_string buf "\255tOc";
  Misc.add_be_uint32 buf 2l;

  let cmp (k1,_) (k2,_) = SHA1.compare k1 k2 in

  let offsets = List.sort ~cmp (SHA1.Map.to_alist t.offsets) in
  let crcs    = List.sort ~cmp (SHA1.Map.to_alist t.crcs) in

  Log.debugf "output: writing the first-level fanout";
  let fanout = Array.create 256 0l in
  List.iter ~f:(fun (key, _) ->
      let str = SHA1.to_string key in
      let n = Char.to_int str.[0] in
      for i = n to 255 do
        fanout.(i) <- Int32.succ fanout.(i)
      done;
    ) offsets;
  Array.iter ~f:(Misc.add_be_uint32 buf) fanout;

  Log.debugf "output: writing the %d object IDs" n;
  List.iter ~f:(fun (key, _) ->
      SHA1.add buf key
    ) offsets;

  Log.debugf "output: writing the %d CRCs" n;
  List.iter ~f:(fun (_, crc) ->
      Misc.add_be_uint32 buf crc
    ) crcs;

  Log.debugf "output: writing the %d offsets" n;
  let conts = ref [] in
  List.iter ~f:(fun (_, offset) ->
      match Int32.of_int offset with
      | Some i -> Misc.add_be_uint32 buf i
      | None   ->
        conts := Int64.of_int_exn offset :: !conts;
        Misc.add_be_uint32 buf 0x80_00_00_00l
    ) offsets;

  Log.debugf "output: writing the %d offset continuations" (List.length !conts);
  let str = String.create 8 in
  List.iter ~f:(fun cont ->
      EndianString.BigEndian.set_int64 str 0 cont;
      Bigbuffer.add_string buf str
    ) (List.rev !conts);

  SHA1.add buf t.pack_checksum;

  (* XXX: slow *)
  let str = Misc.buffer_contents buf in
  let checksum = SHA1.create str in

  Bigbuffer.add_string buf (SHA1.to_string checksum)
