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

module Bigstring = struct
  include Bigstring

  let compare t1 t2 =
    match Int.compare (Bigstring.length t1) (Bigstring.length t2) with
    | 0 -> String.compare (Bigstring.to_string t1) (Bigstring.to_string t2)
    | i -> i

  let pretty t =
    if Int.(Bigstring.length t < 70) then sprintf "%S" (Bigstring.to_string t)
    else sprintf "%S[%d]" (Bigstring.To_string.sub t 0 70) (Bigstring.length t)

end

module Raw = struct

  module Log = Log.Make(struct let section = "pack-raw" end)

  module T = struct
    type t = {
      version : int;
      checksum: SHA1.t;
      values  :  (int * Bigstring.t * Packed_value.t) list;
    } with bin_io, compare, sexp
    let hash (t: t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Value"
  end
  include T
  include Identifiable.Make (T)

  let pretty t =
    let buf = Buffer.create 128 in
    bprintf buf "%s\n" (SHA1.to_hex t.checksum);
    List.iter ~f:(fun (offset, _, p) ->
        bprintf buf "offset: %d\n%s" offset (Packed_value.pretty p)
      ) t.values;
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

  let crc32_of_packed_value ~version t = match version with
    | 2 -> Packed_value.V2.crc32 t
    | 3 -> Packed_value.V3.crc32 t
    | _ -> failwith "pack version should be 2 or 3"

  let input buf =
    let offset = Mstruct.offset buf in
    let version, count = input_header buf in
    Log.debugf "input version:%d count:%d" version count;
    let values = ref [] in
    for i = 0 to count - 1 do
      let pos = Mstruct.offset buf in
      let v = input_packed_value version buf in
      let length = Mstruct.offset buf - pos in
      let raw = Bigstring.sub_shared ~pos ~len:length (Mstruct.to_bigarray buf) in
      values := (pos, raw, v) :: !values
    done;
    let str = Bigstring.sub_shared
        ~len:(Mstruct.offset buf - offset)
        (Mstruct.to_bigarray buf) in
    let expected_checksum = SHA1.input buf in
    let checksum = SHA1.create str in
    if SHA1.(checksum <> expected_checksum) then (
      eprintf "Pack.input: wrong checksum. Got: %s, expecting %s."
        (SHA1.to_hex checksum) (SHA1.to_hex expected_checksum);
      failwith "Pack.input"
    );
    if Int.(Mstruct.length buf <> 0) then (
      eprintf "Pack.input: unprocessed data.";
      failwith "Pack.input";
    );
    Log.debugf "input checksum: %s" (SHA1.to_hex checksum);
    {
      version; checksum;
      values = List.rev !values;
    }

  let add buf t =
    Log.debugf "add";
    let version = 2 in
    add_header ~version buf (List.length t.values);
    List.iter ~f:(fun (offset, raw, p) ->
        let pos = Bigbuffer.length buf in
        if Int.(offset <> pos) then (
          eprintf "Pack.Raw.add: offset differs. Got: %d, expected %d\n" pos offset;
          failwith "Pack.Raw.add";
        );
        Bigbuffer.add_string buf (Bigstring.to_string raw)
      ) t.values;
    let sha1 = SHA1.create (Misc.buffer_contents buf) in
    Log.debugf "add sha1: %s" (SHA1.to_hex sha1);
    SHA1.add buf sha1

  let index_aux (return, bind) ~sha1 raw =
    Log.debugf "index_aux";
    let pack_checksum = raw.checksum in
    let empty = Pack_index.empty pack_checksum in
    let size = List.length raw.values in
    let version = raw.version in
    let rec loop index = function
      | []                 -> return index
      | (pos, raw, p) :: t ->
        let crc = Misc.crc32 raw in
        bind
          (sha1 ~size ~version ~index ~pos p)
          (fun sha1 ->
             let index = Pack_index.({
                 offsets = SHA1.Map.add index.offsets ~key:sha1 ~data:pos;
                 crcs    = SHA1.Map.add index.crcs    ~key:sha1 ~data:crc;
                 pack_checksum;
               }) in
             loop index t)
    in
    loop empty raw.values


  let lwt_monad = Lwt.return, Lwt.bind
  let id_monad = (fun x ->x), (fun x f -> f x)

  let index = index_aux lwt_monad
  let index_sync = index_aux id_monad

  let to_index t =
    Log.debugf "to_index";
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
    let sha1 ~size:_ ~version ~index ~pos packed_value =
      let buf = Misc.with_buffer (fun buf ->
          Packed_value.add_inflated_value_sync ~read ~index ~pos buf packed_value
        ) in
      write buf
    in
    index_sync ~sha1 t

end

module Log = Log.Make(struct let section = "pack" end)

module T = struct
  type t = {
    index: Pack_index.t;
    pics : Packed_value.pic list;
  } with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Value"
end
include T
include Identifiable.Make (T)

let values t = t.pics

let create pics = {
  index = Pack_index.empty (SHA1.of_string "");
  pics;
}

let pretty t =
  let buf = Buffer.create 1024 in
  List.iter ~f:(fun p ->
      let p = (p :> Packed_value.t) in
      bprintf buf "%s---\n" (Packed_value.pretty p)
    ) t.pics;
  Buffer.contents buf

let pic index raw =
  Log.debugf "pic";
  if SHA1.(index.Pack_index.pack_checksum <> raw.Raw.checksum) then (
    eprintf "Pack.pic: the index file does not correspond to the given pack file.\n";
    failwith "Pack.pic"
  );
  let pics = List.map ~f:(fun (pos, _, p) -> Packed_value.pic index ~pos p) raw.Raw.values in
  { index; pics }

let input buf index =
  let raw = Raw.input buf in
  pic index raw

let add_packed_value ~version buf = match version with
  | 2 -> Packed_value.V2.add buf
  | 3 -> Packed_value.V3.add buf
  | _ -> failwith "pack version should be 2 or 3"

let add buf t =
  Log.debugf "add";
  let version = 2 in
  Raw.add_header ~version buf (List.length t.pics);
  List.iter ~f:(fun p ->
      let pos = Bigbuffer.length buf in
      let p = Packed_value.unpic t.index ~pos p in
      add_packed_value ~version buf p
    ) t.pics;
  let sha1 = SHA1.create (Misc.buffer_contents buf) in
  Log.debugf "add sha1: %s" (SHA1.to_hex sha1);
  SHA1.add buf sha1

let packed_value ~index ~key:sha1 buf =
  Log.debugf "packed_value %s" (SHA1.to_hex sha1);
  let buf = Mstruct.of_bigarray buf in
  let version, size = Raw.input_header (Mstruct.clone buf) in
  let packs = SHA1.Table.create () in
  if Hashtbl.mem packs sha1 then
    Hashtbl.find_exn packs sha1
  else (
    let offset = SHA1.Map.find_exn index.Pack_index.offsets sha1 in
    let lengths = Pack_index.lengths index in
    let buf = Mstruct.clone buf in
    let buf = match SHA1.Map.find_exn lengths sha1 with
      | None  ->
        Mstruct.shift buf offset;
        Mstruct.sub buf 0 (Mstruct.length buf - 20)
      | Some l -> Mstruct.sub buf offset l in
    let value = Raw.input_packed_value version buf in
    Hashtbl.replace packs sha1 value;
    value
  )

let unpack ~read ~write buf =
  Log.debugf "unpack";
  let i = ref 0 in
  let sha1 ~size ~version ~index ~pos packed_value =
    Printf.printf "\rUnpacking objects: %3d%% (%d/%d)%!" (!i*100/size) (!i+1) size;
    incr i;
    Packed_value.to_value ~read ~index ~pos packed_value >>= fun value ->
    write value in
  let pack = Raw.input (Mstruct.of_bigarray buf) in
  Raw.index ~sha1 pack >>= fun index ->
  Printf.printf "\rUnpacking objects: 100%% (%d/%d), done.\n%!" !i !i;
  return index

let to_index pack =
  pack.index
