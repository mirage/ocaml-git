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
module Log = Log.Make(struct let section = "pack" end)

module T = struct
  type t = Packed_value.t list
  with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Value"
end
include T
include Identifiable.Make (T)

let pretty t =
  let buf = Buffer.create 1024 in
  List.iter ~f:(fun p ->
      bprintf buf "%s\n" (Packed_value.pretty p)
    ) t;
  Buffer.contents buf

let input_header buf =
  let header = Mstruct.get_string buf 4 in
  if String.(header <> "PACK") then
    Mstruct.parse_error_buf buf "wrong header (%s)" header;
  let version = Int32.to_int_exn (Mstruct.get_be_uint32 buf) in
  if Int.(version <> 2 && version <> 3) then
    Mstruct.parse_error_buf buf "wrong pack version (%d)" version;
  version, Int32.to_int_exn (Mstruct.get_be_uint32 buf)

let add_header ~version buf count =
  Bigbuffer.add_string buf "PACK";
  Misc.add_be_uint32 buf (Int32.of_int_exn version);
  Misc.add_be_uint32 buf (Int32.of_int_exn count)

let input_packed_value ~version buf = match version with
  | 2 -> Packed_value.V2.input buf
  | 3 -> Packed_value.V3.input buf
  | _ -> failwith "pack version should be 2 or 3"

let add_packed_value ~version buf = match version with
  | 2 -> Packed_value.V2.add buf
  | 3 -> Packed_value.V3.add buf
  | _ -> failwith "pack version should be 2 or 3"

let crc32_of_packed_value ~version t = match version with
  | 2 -> Packed_value.V2.crc32 t
  | 3 -> Packed_value.V3.crc32 t
  | _ -> failwith "pack version should be 2 or 3"

let input buf =
  let version, count = input_header buf in
  Log.debugf "input version:%d count:%d" version count;
  let values = ref [] in
  for i = 0 to count - 1 do
    let v = input_packed_value version buf in
    values := v :: !values
  done;
  let str = Bigstring.sub_shared ~len:(Mstruct.offset buf) (Mstruct.to_bigarray buf) in
  let actual_checksum = SHA1.create str in
  let expected_checksum = SHA1.input buf in
  if SHA1.(actual_checksum <> expected_checksum) then (
    eprintf "Pack.input: wrong checksum. Got: %s, expecting %s."
      (SHA1.to_hex actual_checksum) (SHA1.to_hex expected_checksum);
    failwith "Pack.input"
  );
  if Int.(Mstruct.length buf <> 0) then (
    eprintf "Pack.input: unprocessed data.";
    failwith "Pack.input";
  );
  Log.debugf "checksum: %s" (SHA1.to_hex actual_checksum);
  List.rev !values

let add buf t =
  Log.debugf "add";
  let version = 2 in
  add_header ~version buf (List.length t);
  List.iter ~f:(add_packed_value ~version buf) t;
  let str = Misc.buffer_contents buf in
  let sha1 = SHA1.create str in
  Log.debugf "add sha1: %s" (SHA1.to_hex sha1);
  Bigbuffer.add_string buf (Bigstring.to_string str);
  SHA1.add buf sha1

let packed_value ~index:{ Pack_index.offsets; lengths } ~key:sha1 buf =
  let buf = Mstruct.of_bigarray buf in
  let version, size = input_header (Mstruct.clone buf) in
  let packs = SHA1.Table.create () in
  if Hashtbl.mem packs sha1 then
    Hashtbl.find_exn packs sha1
  else (
    let offset = SHA1.Map.find_exn offsets sha1 in
    let buf = Mstruct.clone buf in
    let buf = match SHA1.Map.find_exn lengths sha1 with
      | None   ->
        Mstruct.shift buf offset;
        Mstruct.sub buf 0 (Mstruct.length buf - 20)
      | Some l -> Mstruct.sub buf offset l in
    let value = input_packed_value version buf in
    Hashtbl.replace packs sha1 value;
    value
  )

let index_aux (return, bind) ~compute_sha1_and_crc buf =
  let pack_checksum = SHA1.create buf in
  let buf = Mstruct.of_bigarray buf in
  let version, size = input_header buf in
  Log.debugf "index: version=%d size=%d" version size;
  let empty = Pack_index.empty pack_checksum in
  if Int.(size <= 0) then
    return empty
  else (
    let next_packed_value () =
      input_packed_value version buf in
    let rec loop index i =
      let offset = Mstruct.offset buf in
      let packed_value = next_packed_value () in
      let length = Mstruct.offset buf - offset in
      bind
        (compute_sha1_and_crc ~size ~version ~index ~offset packed_value)
        (fun (sha1, crc) ->
           let index = Pack_index.({
               offsets = SHA1.Map.add index.offsets ~key:sha1 ~data:offset;
               lengths = SHA1.Map.add index.lengths ~key:sha1 ~data:(Some length);
               crcs    = SHA1.Map.add index.crcs    ~key:sha1 ~data:crc;
               pack_checksum;
             }) in
           if Int.(i >= size - 1) then return index
           else loop index (i+1))
    in
    loop empty 0
  )

let lwt_monad = Lwt.return, Lwt.bind
let id_monad = (fun x ->x), (fun x f -> f x)

let index = index_aux lwt_monad
let index_sync = index_aux id_monad

let unpack ~read ~write buf =
  let i = ref 0 in
  let compute_sha1_and_crc ~size ~version ~index ~offset packed_value =
    Printf.printf "\rUnpacking objects: %3d%% (%d/%d)%!" (!i*100/size) (!i+1) size;
    incr i;
    let crc32 = crc32_of_packed_value ~version packed_value in
    Packed_value.to_value ~read ~index ~offset packed_value >>= fun value ->
    write value >>= fun sha1 ->
    return (sha1, crc32)
  in
  index ~compute_sha1_and_crc buf >>= fun index ->
  Printf.printf "\rUnpacking objects: 100%% (%d/%d), done.\n%!" !i !i;
  return index

let index_of_raw buf =
  let buffers = SHA1.Table.create () in
  let read sha1 =
    try Hashtbl.find_exn buffers sha1
    with Not_found ->
      eprintf "Pack_index.of_pack: %s not found" (SHA1.to_hex sha1);
      failwith "Pack_index.of_pack" in
  let write buffer =
    let sha1 = SHA1.create buffer in
    Hashtbl.add_exn buffers sha1 buffer;
    sha1 in
  let compute_sha1_and_crc ~size:_ ~version:_ ~index ~offset packed_value =
    let buf = Misc.with_buffer (fun buf ->
        Packed_value.add_inflated_value_sync ~read ~index ~offset buf packed_value
      ) in
    (write buf, Misc.crc32 buf)
  in
  index_sync ~compute_sha1_and_crc buf

(* XXX: could we do better ? *)
let to_index pack =
  let buf = Misc.with_buffer (fun buf -> add buf pack) in
  index_of_raw buf
