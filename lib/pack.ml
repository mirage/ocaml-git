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
      sha1    : SHA.t;
      index   : Pack_index.t;
      buffer  : Bigstring.t;
      version : int;
      checksum: SHA.t;
      values  :  (int * Bigstring.t * Packed_value.t) list;
    } with bin_io, compare, sexp
    let hash (t: t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Pack.Raw"
  end
  include T
  include Identifiable.Make (T)

  let pretty t =
    let buf = Buffer.create 128 in
    bprintf buf "%s\n" (SHA.to_hex t.checksum);
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

  let index_of_values_aux (return, bind) ~sha1 ~pack_checksum values =
    Log.debugf "index_of_value";
    let empty = Pack_index.empty ~pack_checksum () in
    let rec loop (offsets, index) = function
      | []                 -> return index
      | (pos, raw, p) :: t ->
        let raw = Bigstring.to_string raw in
        let crc = Misc.crc32 raw in
        bind
          (sha1 ~offsets ~pos p)
          (fun sha1 ->
             let index = Pack_index.({
                 offsets = SHA.Map.add index.offsets ~key:sha1 ~data:pos;
                 crcs    = SHA.Map.add index.crcs    ~key:sha1 ~data:crc;
                 pack_checksum;
               }) in
             let offsets = Int.Map.add offsets ~key:pos ~data:sha1 in
             loop (offsets, index) t)
    in
    loop (Int.Map.empty, empty) values

  let lwt_monad = Lwt.return, Lwt.bind
  let id_monad = (fun x ->x), (fun x f -> f x)

  let index_of_values_async = index_of_values_aux lwt_monad
  let index_of_values_sync  = index_of_values_aux id_monad

  let index_of_values ~pack_checksum values =
    let read sha1 = Value.Cache.find_exn sha1 in
    let write buffer =
      let sha1 = SHA.create buffer in
      Value.Cache.add sha1 buffer;
      sha1 in
    let size = List.length values in
    let i = ref 0 in
    let sha1 ~offsets ~pos p =
      Printf.printf "\rResolving deltas: %3d%% (%d/%d)%!" (!i*100/size) (!i+1) size;
      incr i;
      let buf = Misc.with_buffer (fun buf ->
          Packed_value.add_inflated_value_sync ~read ~offsets ~pos buf p
        ) in
      write buf
    in
    let keys = index_of_values_sync ~sha1 ~pack_checksum values in
    Printf.printf "\rResolving deltas: 100%% (%d/%d), done.\n%!" !i !i;
    keys

  (* Since Git 1.8.5 the naming is hardly reproductible, so pick a
     random but stable one. *)
  let sha1_of_keys keys =
    keys
    |> List.map ~f:SHA.to_hex
    |> List.sort ~cmp:String.compare
    |> List.rev
    |> String.concat ~sep:""
    |> SHA.create

  let input buf ~index =
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
    let pack_checksum = SHA.input buf in
    (* XXX: SHA.of_bigstring *)
    let checksum = SHA.create (Bigstring.to_string str) in
    if SHA.(checksum <> pack_checksum) then (
      eprintf "Pack.Raw.input: wrong file checksum. Got: %s, expecting %s."
        (SHA.to_hex checksum) (SHA.to_hex pack_checksum);
      failwith "Pack.input"
    );
    Log.debugf "input checksum: %s" (SHA.to_hex pack_checksum);
    if Int.(Mstruct.length buf <> 0) then (
      eprintf "Pack.input: unprocessed data.";
      failwith "Pack.input";
    );
    let values = List.rev !values in
    let index = match index with
      | None   -> index_of_values ~pack_checksum values
      | Some i ->
        if SHA.(i.Pack_index.pack_checksum <> pack_checksum) then (
          eprintf "Pack.Raw.input: wrong index checksum. Got: %s, expecting %s."
            (SHA.to_hex i.Pack_index.pack_checksum) (SHA.to_hex pack_checksum);
          failwith "Pack.input"
        );
        i in
    let sha1 = sha1_of_keys (SHA.Map.keys index.Pack_index.offsets) in
    {
      sha1; index; values;
      buffer = Bigstring.sub_shared ~pos:offset (Mstruct.to_bigarray buf);
      version; checksum;
    }

  let add buf t =
    Bigbuffer.add_string buf (Bigstring.to_string t.buffer)

  let sha1 t = t.sha1

  let index t = t.index

  let keys t =
    SHA.Set.of_list
      (SHA.Map.keys t.index.Pack_index.offsets)

end

module Log = Log.Make(struct let section = "pack" end)

module T = struct
  type t = (SHA.t * Packed_value.PIC.t) list with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Pack"
end
include T
include Identifiable.Make (T)

let pretty t =
  let buf = Buffer.create 1024 in
  List.iter ~f:(fun (sha1, p) ->
      bprintf buf "%s: %s---\n" (SHA.to_hex sha1) (Packed_value.PIC.pretty p)
    ) t;
  Buffer.contents buf

let to_pic { Raw.values; index }  =
  Log.debugf "to_pic";
  let inv_offsets = Int.Map.of_alist_exn
      (List.Assoc.inverse (SHA.Map.to_alist index.Pack_index.offsets)) in
  let _offsets, _sha1, pics =
    List.fold_left ~f:(fun (offsets, sha1s, pics) (pos, _, p) ->
        match Int.Map.find inv_offsets pos with
        | None      -> failwith "Pack.to_pick"
        | Some sha1 ->
          let pic = Packed_value.to_pic offsets sha1s (pos, sha1, p) in
          Int.Map.add  offsets ~key:pos  ~data:pic,
          SHA.Map.add sha1s   ~key:sha1 ~data:pic,
          (sha1, pic) :: pics
      )
      ~init:(Int.Map.empty, SHA.Map.empty, [])
      values in
  List.rev pics

let input buf ~index =
  Log.debugf "input";
  to_pic (Raw.input buf ~index)

let add_packed_value ~version buf = match version with
  | 2 -> Packed_value.V2.add buf
  | 3 -> Packed_value.V3.add buf
  | _ -> failwith "pack version should be 2 or 3"

let add buf t =
  Log.debugf "add";
  let version = 2 in
  Raw.add_header ~version buf (List.length t);
  let _index = List.fold_left ~f:(fun index (_, pic) ->
      let pos = Bigbuffer.length buf in
      let p = Packed_value.of_pic index ~pos pic in
      add_packed_value ~version buf p;
      Packed_value.PIC.Map.add index pic pos
    ) ~init:Packed_value.PIC.Map.empty t in
  let sha1 = SHA.create (Bigbuffer.contents buf) in
  Log.debugf "add sha1: %s" (SHA.to_hex sha1);
  SHA.add buf sha1

let keys t =
  List.fold_left ~f:(fun set (key, _) ->
      SHA.Set.add set key
    ) ~init:SHA.Set.empty t

let unpack ~write buf =
  Log.debugf "XXX unpack";
  let i = ref 0 in
  let pack = Raw.input (Mstruct.of_bigarray buf) ~index:None in
  let pack = to_pic pack in
  let size = List.length pack in
  Misc.list_iter_p (fun (_, pic) ->
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
    List.map ~f:(fun (k, v) ->
        let raw = Misc.with_bigbuffer (fun buf -> Value.add_inflated buf v) in
        k, Packed_value.PIC.raw k raw
      ) contents in
  (* XXX: Patience_diff.be_clever *)
  uncompressed

let of_pic t =
  Log.debugf "of_pic";
  let buf = Misc.with_bigbuffer (fun buf -> add buf t) in
  Raw.input (Mstruct.of_bigarray buf) ~index:None

let read (t:t) sha1 =
  match List.Assoc.find t sha1 with
  | None     -> None
  | Some pic -> Some (Packed_value.PIC.to_value pic)

let read_exn (t:t) sha1 =
  let pic = List.Assoc.find_exn t sha1 in
  Packed_value.PIC.to_value pic
