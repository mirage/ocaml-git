(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Printf

module Dolog = Log

module Log = Dolog.Make(struct let section = "pack" end)

module Raw = struct

  module Log = Dolog.Make(struct let section = "pack-raw" end)

  type t = {
    sha1    : SHA.t;
    index   : Pack_index.Raw.t;
    inv_offs: SHA.t Misc.IntMap.t Lazy.t;
    buffer  : Cstruct.t;
    version : int;
    checksum: SHA.t;
    values  :  (int * Cstruct.t * Packed_value.t) list;
  }

  let hash = Hashtbl.hash

  let compare = compare

  let equal t1 t2 =
    SHA.equal t1.sha1 t2.sha1
    && Pack_index.Raw.equal t1.index t2.index
    && t1.buffer = t2.buffer
    && t1.version = t2.version
    && SHA.equal t1.checksum t2.checksum
    && t1.values = t2.values

  let pp_hum ppf t =
    Format.fprintf ppf "%a@." SHA.pp_hum t.checksum;
    List.iter (fun (offset, _, p) ->
        Format.fprintf ppf "offset: %d@,%a" offset Packed_value.pp_hum p
      ) t.values

  let pretty = Misc.pretty pp_hum

  let input_header buf =
    let header = Mstruct.get_string buf 4 in
    if header <> "PACK" then
      Mstruct.parse_error_buf buf "wrong header (%s)" header;
    let version = Int32.to_int (Mstruct.get_be_uint32 buf) in
    if version <> 2 && version <> 3 then
      Mstruct.parse_error_buf buf "wrong pack version (%d)" version;
    version, Int32.to_int (Mstruct.get_be_uint32 buf)

  let add_header ~version buf count =
    Buffer.add_string buf "PACK";
    Misc.add_be_uint32 buf (Int32.of_int version);
    Misc.add_be_uint32 buf (Int32.of_int count)

  let input_packed_value ~version buf = match version with
    | 2 -> Packed_value.V2.input buf
    | 3 -> Packed_value.V3.input buf
    | _ -> failwith "pack version should be 2 or 3"

  let _crc32_of_packed_value ~version t = match version with
    | 2 -> Packed_value.V2.crc32 t
    | 3 -> Packed_value.V3.crc32 t
    | _ -> failwith "pack version should be 2 or 3"

  let index_of_values_aux (return, bind) ~sha1 ~pack_checksum values =
    Log.debug "index_of_values";
    let empty = Pack_index.Raw.empty ~pack_checksum () in
    let rec loop (offsets, index) = function
      | []                 -> return index
      | (pos, raw, p) :: t ->
        let raw = Cstruct.to_string raw in
        let crc = Misc.crc32 raw in
        bind
          (sha1 ~offsets ~pos p)
          (fun sha1 ->
             let index = Pack_index.Raw.({
                 offsets = SHA.Map.add sha1 pos index.offsets;
                 crcs    = SHA.Map.add sha1 crc index.crcs;
                 pack_checksum;
               }) in
             let offsets = Misc.IntMap.add pos sha1 offsets in
             loop (offsets, index) t)
    in
    loop (Misc.IntMap.empty, empty) values

  let lwt_monad = Lwt.return, Lwt.bind
  let id_monad = (fun x ->x), (fun x f -> f x)

  let _index_of_values_async = index_of_values_aux lwt_monad
  let index_of_values_sync  = index_of_values_aux id_monad

  let index_of_values ~pack_checksum cache values =
    let read sha1 = Hashtbl.find cache sha1 in
    let write buffer =
      let sha1 = SHA.of_string buffer in
      Hashtbl.replace cache sha1 buffer;
      sha1
    in
    let size = List.length values in
    let i = ref 0 in
    let sha1 ~offsets ~pos p =
      Log.info "Resolving deltas: %3d%% (%d/%d)" (!i*100/size) (!i+1) size;
      incr i;
      let buf = Misc.with_buffer (fun buf ->
          Packed_value.add_inflated_value_sync ~read ~offsets ~pos buf p
        ) in
      write buf
    in
    let keys = index_of_values_sync ~sha1 ~pack_checksum values in
    Log.info "Resolving deltas: 100%% (%d/%d), done." !i !i;
    keys

  (* Since Git 1.8.5 the naming is hardly reproductible, so pick a
     random but stable one. *)
  let sha1_of_keys keys =
    keys
    |> List.map SHA.to_hex
    |> List.sort String.compare
    |> List.rev
    |> String.concat ""
    |> SHA.of_string

  let values buf ~version ?(max_offset=max_int) n =
    let rec aux acc i =
      let pos = Mstruct.offset buf in
      if pos > max_offset || i >= n then List.rev acc
      else
        let cbuf = Mstruct.to_cstruct buf in
        let v = input_packed_value ~version buf in
        let length = Mstruct.offset buf - pos in
        let raw = Cstruct.sub cbuf 0 length in
        aux ((pos, raw, v) :: acc) (i+1)
    in
    aux [] 0

  let read buf index sha1 =
    let version, count = input_header buf in
    Log.debug "read: version=%d count=%d" version count;
    match Pack_index.find_offset index sha1 with
    | None -> None
    | Some offset ->
      Log.debug "read: offset=%d" offset;
      let orig_off = Mstruct.offset buf in
      let orig_len = Mstruct.length buf in
      Log.debug "read: buf: orig_off=%d orig_len=%d" orig_off orig_len;
      let ba = Mstruct.to_bigarray buf in
      Mstruct.shift buf (offset - orig_off);
      Log.debug "read: buf: off=%d len=%d (after shift:%d)"
        (Mstruct.offset buf) (Mstruct.length buf) (offset-orig_off);
      let packed_v = input_packed_value ~version buf in
      Log.debug "read: buf: off=%d len=%d (after input_packed_value)"
        (Mstruct.offset buf) (Mstruct.length buf);
      Some (Packed_value.to_value ~version ~index ~ba (offset-orig_off, packed_v))

  let input buf ~index =
    let all = Mstruct.to_cstruct buf in
    let offset = Mstruct.offset buf in
    let version, count = input_header buf in
    Log.debug "input version:%d count:%d" version count;
    let values = values buf ~version count in
    let str = Cstruct.sub all 0 (Mstruct.offset buf - offset) in
    let pack_checksum = SHA.input buf in
    let checksum = SHA.of_cstruct str in
    if checksum <> pack_checksum then (
      eprintf "Pack.Raw.input: wrong file checksum. Got: %s, expecting %s."
        (SHA.to_hex checksum) (SHA.to_hex pack_checksum);
      failwith "Pack.input"
    );
    Log.debug "input checksum: %s" (SHA.to_hex pack_checksum);
    if Mstruct.length buf <> 0 then (
      eprintf "Pack.input: unprocessed data.";
      failwith "Pack.input";
    );
    let cache = Hashtbl.create (List.length values) in
    let index = match index with
      | None   -> index_of_values ~pack_checksum cache values
      | Some i ->
        if i.Pack_index.Raw.pack_checksum <> pack_checksum then (
          eprintf "Pack.Raw.input: wrong index checksum. Got: %s, expecting %s."
            (SHA.to_hex i.Pack_index.Raw.pack_checksum) (SHA.to_hex pack_checksum);
          failwith "Pack.input"
        );
        i in
    let sha1 = sha1_of_keys (SHA.Map.keys index.Pack_index.Raw.offsets) in
    let inv_offs = lazy (
      Misc.IntMap.of_alist
        (Misc.inverse_assoc (SHA.Map.to_alist index.Pack_index.Raw.offsets))
    ) in
    {
      sha1; index; values;
      buffer = Cstruct.sub all offset (Cstruct.len all - offset);
      version; checksum; inv_offs;
    }

  let add buf ?level:_ t = Buffer.add_string buf (Cstruct.to_string t.buffer)
  let sha1 t = t.sha1
  let index t = t.index
  let keys t = SHA.Set.of_list (SHA.Map.keys t.index.Pack_index.Raw.offsets)

end

type t = (SHA.t * Packed_value.PIC.t) list

let read (t:t) sha1 =
  match Misc.try_assoc sha1 t with
  | None     -> None
  | Some pic -> Some (Packed_value.PIC.to_value pic)

let to_pic { Raw.values; inv_offs; _ } =
  Log.debug "to_pic";
  let _offsets, _sha1s, pics =
    List.fold_left (fun (offsets, sha1s, pics) (pos, _, p) ->
        let sha1 = Misc.IntMap.find pos (Lazy.force inv_offs) in
        let pic = Packed_value.to_pic offsets sha1s (pos, sha1, p) in
        Misc.IntMap.add pos pic offsets ,
        SHA.Map.add sha1 pic sha1s,
        (sha1, pic) :: pics
      )
      (Misc.IntMap.empty, SHA.Map.empty, [])
      values in
  Log.debug "to_pic: ok";
  List.rev pics

let hash = Hashtbl.hash

let compare = compare

let equal = (=)

let pp_hum ppf t =
  Format.fprintf ppf "@[";
  List.iter (fun (sha1, p) ->
      Format.fprintf ppf "@[%a@,%a@;@]"
        SHA.pp_hum sha1
        Packed_value.PIC.pp_hum p
    ) t;
  Format.fprintf ppf "@]"

let pretty = Misc.pretty pp_hum

let input buf ~index =
  Log.debug "input";
  to_pic (Raw.input buf ~index)

let add_packed_value ~version ?level buf = match version with
  | 2 -> Packed_value.V2.add buf ?level
  | 3 -> Packed_value.V3.add buf ?level
  | _ -> failwith "pack version should be 2 or 3"

let add buf ?level t =
  Log.debug "add";
  let version = 2 in
  Raw.add_header ~version buf (List.length t);
  let _index = List.fold_left (fun index (_, pic) ->
      let pos = Buffer.length buf in
      let p = Packed_value.of_pic index ~pos pic in
      add_packed_value ~version ?level buf p;
      Packed_value.PIC.Map.add pic pos index
    ) Packed_value.PIC.Map.empty t in
  let sha1 = SHA.of_string (Buffer.contents buf) in
  Log.debug "add sha1: %s" (SHA.to_hex sha1);
  SHA.add buf sha1

let keys t =
  List.fold_left (fun set (key, _) ->
      SHA.Set.add key set
    ) SHA.Set.empty t

let unpack ~write buf =
  let i = ref 0 in
  let pack = Raw.input (Mstruct.of_cstruct buf) ~index:None in
  let pack = to_pic pack in
  let size = List.length pack in
  Lwt_list.iter_p (fun (_, pic) ->
      let value = Packed_value.PIC.to_value pic in
      Printf.printf "\rUnpacking objects: %3d%% (%d/%d)%!" (!i*100/size) (!i+1) size;
      incr i;
      write value >>= fun _ ->
      return_unit
    ) pack
  >>= fun () ->
  Printf.printf "\rUnpacking objects: 100%% (%d/%d), done.\n%!" !i !i;
  return (keys pack)

let pack contents =
  let uncompressed =
    List.map (fun (k, v) ->
        let raw = Misc.with_buffer' (fun buf -> Value.add_inflated buf v) in
        k, Packed_value.PIC.raw k raw
      ) contents in
  (* XXX: Patience_diff.be_clever *)
  uncompressed

let of_pic t =
  Log.debug "of_pic";
  let buf = Misc.with_buffer (fun buf -> add buf t) in
  Raw.input (Mstruct.of_string buf) ~index:None

let read_exn (t:t) sha1 =
  let pic = List.assoc sha1 t in
  Packed_value.PIC.to_value pic
