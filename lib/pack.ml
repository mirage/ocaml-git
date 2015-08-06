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

open Lwt.Infix
module Dolog = Log

module Log = Dolog.Make(struct let section = "pack" end)

let fail fmt = Printf.ksprintf failwith ("Pack." ^^ fmt)

module T = struct

  type t = Packed_value.pic list
  let hash = Hashtbl.hash
  let compare = compare
  let equal = (=)

  let pp ppf t =
    Format.fprintf ppf "@[";
    List.iter (fun p ->
        Format.fprintf ppf "@[%a@;@]" Packed_value.PIC.pp p
      ) t;
    Format.fprintf ppf "@]"

  let pretty = Misc.pretty pp

end

include T

let keys t =
  List.fold_left (fun set pic ->
      if Packed_value.PIC.shallow pic then set
      else
        let key = Packed_value.PIC.sha1 pic in
        SHA.Set.add key set
    ) SHA.Set.empty t

type raw = {
  sha1    : SHA.t;
  keys    : SHA.Set.t;
  index   : SHA.t -> int option;
  read    : Value.read_inflated;
  buffer  : Cstruct.t;
  version : int;
  checksum: SHA.t;
  values  :  (int32 * Packed_value.t) list;
  shallow : bool;
  raw_index: Pack_index.Raw.t option;
}

module Raw = struct

  type t = raw
  let hash = Hashtbl.hash
  let compare = compare

  let equal t1 t2 =
    SHA.equal t1.sha1 t2.sha1
    && SHA.Set.equal t1.keys t2.keys
    && t1.buffer = t2.buffer
    && t1.version = t2.version
    && SHA.equal t1.checksum t2.checksum
    && t1.values = t2.values

  let pp ppf t =
    Format.fprintf ppf "%a@." SHA.pp t.checksum;
    List.iter (fun (crc, p) ->
        Format.fprintf ppf "%ld: %a" crc Packed_value.pp p
      ) t.values

  let pretty = Misc.pretty pp
  let shallow t = t.shallow
  let sha1 t = t.sha1
  let keys t = t.keys
  let buffer t = t.buffer

  let index t = match t.raw_index with
    | None   -> fail "Invalid raw index"
    | Some i -> i

  let input_header buf =
    let header = Mstruct.get_string buf 4 in
    if header <> "PACK" then
      Mstruct.parse_error_buf buf "wrong header (%s)" header;
    let version = Int32.to_int (Mstruct.get_be_uint32 buf) in
    if version <> 2 && version <> 3 then
      Mstruct.parse_error_buf buf "wrong pack version (%d)" version;
    `Version version, `Count (Int32.to_int (Mstruct.get_be_uint32 buf))

end

module IO (D: SHA.DIGEST) (I: Inflate.S) = struct

  module Packed_value_IO = Packed_value.IO(D)(I)
  module Value_IO = Value.IO(D)(I)
  module SHA_IO = SHA.IO(D)

  module Raw = struct

    module Log = Dolog.Make(struct let section = "pack-raw" end)

    include Raw

    let add_header ~version buf count =
      Buffer.add_string buf "PACK";
      Misc.add_be_uint32 buf (Int32.of_int version);
      Misc.add_be_uint32 buf (Int32.of_int count)

    let input_packed_value ~version buf = match version with
      | 2 -> Packed_value_IO.V2.input buf
      | 3 -> Packed_value_IO.V3.input buf
      | _ -> fail "pack version should be 2 or 3"

    let read_packed_value name ~index buf sha1 =
      let `Version version, `Count count = input_header buf in
      Log.debug "%s: %s version=%d count=%d" name (SHA.pretty sha1) version count;
      match index sha1 with
      | None -> None
      | Some offset ->
        Log.debug "read: offset=%d" offset;
        let orig_off = Mstruct.offset buf in
        let ba = Mstruct.to_bigarray buf in
        let shift = offset - orig_off in
        Mstruct.shift buf shift;
        let kind = input_packed_value ~version buf in
        let v = Packed_value.create ~kind ~offset:shift in
        Some (version, ba, v)

    let read ~index ~read buf sha1 =
      match read_packed_value "read" ~index buf sha1 with
      | None                  -> Lwt.return_none
      | Some (version, ba, v) ->
        Packed_value_IO.to_value ~version ~read ~index ~ba v >|=
        fun x -> Some x

    let read_inflated ~index ~read buf sha1 =
      match read_packed_value "read_inflated" ~index buf sha1 with
      | None                  -> Lwt.return_none
      | Some (version, ba, v) ->
        Packed_value_IO.unpack ~version ~read ~index ~ba v >|=
        fun x -> Some x

    let to_pic ?(progress=fun _ -> ())  ~read values =
      Log.debug "to_pic";
      let get f t x = try Some (f x t) with Not_found -> None in
      let deltas =
        List.filter (fun (_, v) -> Packed_value.is_delta v) values
        |> List.length
      in
      let d = ref 0 in
      let pp () =
        if !d = deltas || !d mod 100 = 1 then (
          let pc = 100 * !d / deltas in
          let done_ = if !d = deltas then ", done.\n" else "" in
          let str =
            Printf.sprintf "Resolving deltas: %3d%% (%d/%d)%s" pc !d deltas done_
          in
          progress str
        )
      in
      Lwt_list.fold_left_s (fun (offsets, crcs, sha1s, pics) (crc, v) ->
          let get_offsets = get Misc.IntMap.find offsets in
          let get_sha1s   = get SHA.Map.find sha1s in
          if Packed_value.is_delta v then (incr d; pp ());
          Packed_value_IO.to_pic ~digest:D.string
            ~read ~offsets:get_offsets ~sha1s:get_sha1s v
          >|= fun pic ->
          let sha1 = Packed_value.PIC.sha1 pic in
          let offset = Packed_value.offset v in
          let offsets = Misc.IntMap.add offset pic offsets in
          let crcs = SHA.Map.add sha1 crc crcs in
          let sha1s = SHA.Map.add sha1 pic sha1s in
          let pics = (offset, pic) :: pics in
          offsets, crcs, sha1s, pics
        )
        (Misc.IntMap.empty, SHA.Map.empty, SHA.Map.empty, [])
        values >|= fun (offsets, crcs, _, pics) ->
      Log.debug "to_pic: ok";
      offsets, crcs, List.rev pics

    let index_of_packed_values ?progress ~pack_checksum ~read values =
      Log.debug "index_of_packed_values";
      to_pic ?progress ~read values >|= fun (_, crcs, pics) ->
      let offsets = List.fold_left (fun offsets (offset, pic) ->
          SHA.Map.add (Packed_value.PIC.sha1 pic) offset offsets
        ) SHA.Map.empty pics
      in
      { Pack_index.offsets; crcs; pack_checksum }

    (* Since Git 1.8.5 the naming is hardly reproductible, so pick a
       random but stable one. *)
    let sha1_of_keys keys =
      keys
      |> SHA.Set.to_list
      |> List.map SHA.to_hex
      |> List.sort String.compare
      |> List.rev
      |> String.concat ""
      |> D.string

    let values buf ~version ?(max_offset=max_int) n =
      let rec aux acc i =
        let pos = Mstruct.offset buf in
        if pos > max_offset || i >= n then List.rev acc
        else
          let cbuf = Mstruct.to_cstruct buf in
          let kind = input_packed_value ~version buf in
          let length = Mstruct.offset buf - pos in
          let raw = Cstruct.sub cbuf 0 length in
          let crc = Crc.Crc32.cstruct raw in
          let v = Packed_value.create ~kind ~offset:pos in
          aux ((crc, v) :: acc) (i+1)
      in
      aux [] 0

    let input_values ~read buf k =
      let all = Mstruct.to_cstruct buf in
      let offset = Mstruct.offset buf in
      let `Version version, `Count count = input_header buf in
      Log.debug "input version:%d count:%d" version count;
      let values = values buf ~version count in
      let str = Cstruct.sub all 0 (Mstruct.offset buf - offset) in
      let pack_checksum = SHA_IO.input buf in
      let checksum = D.cstruct str in
      if checksum <> pack_checksum then
        fail "Raw.input: wrong file checksum. Got: %s, expecting %s."
          (SHA.to_hex checksum) (SHA.to_hex pack_checksum);
      Log.debug "input checksum: %s" (SHA.to_hex pack_checksum);
      if Mstruct.length buf <> 0 then fail "input: unprocessed data.";
      let buffer = Cstruct.sub all offset (Cstruct.len all - offset) in
      let raw_index = None in
      let shallow = false in
      let create index keys =
        let sha1 = sha1_of_keys keys in
        { sha1; keys; read; index; values; buffer; version; checksum;
          raw_index; shallow; }
      in
      k pack_checksum values create

    let input ?progress ~read buf =
      let k pack_checksum values create =
        index_of_packed_values ?progress ~pack_checksum ~read values
        >|= fun index ->
        let keys = Pack_index.Raw.keys index in
        let index_fn = Pack_index.Raw.find_offset index in
        let shallow =
          List.fold_left
            (fun shallow (_, v) -> shallow || Packed_value.shallow keys v)
            false values
        in
        let raw = create index_fn keys in
        { raw with raw_index = Some index; shallow }
      in
      input_values ~read buf k

    let input_with_index ~read ~index ~keys buf =
      let k _ _ create = create index keys in
      input_values ~read buf k

    let add = `Not_defined

    let unpack ?(progress=fun _ -> ()) ~write { values; read; _ } =
      let i = ref 0 in
      to_pic ~progress ~read values >>= fun (_, crcs, pack) ->
      let size = List.length pack in
      Lwt_list.iter_p (fun (_, pic) ->
          let value = Packed_value.PIC.unpack pic in
          let str = Printf.sprintf "\rUnpacking objects: %3d%% (%d/%d)%!"
              (!i*100/size) (!i+1) size in
          progress str;
          incr i;
          write value >>= fun _ ->
          Lwt.return_unit
        ) pack
      >|= fun () ->
      let str =
        Printf.sprintf "\rUnpacking objects: 100%% (%d/%d), done.\n%!" !i !i
      in
      progress str;
      SHA.Map.keys crcs |> SHA.Set.of_list

  end

  include T
  type raw = Raw.t

  let of_raw ?progress { values; read; _ } =
    Raw.to_pic ?progress ~read values >|= fun (_, _, pics) ->
    List.map snd pics

  let input ?progress ~index ~keys ~read buf =
    Log.debug "input";
    let raw = Raw.input_with_index ~read ~index ~keys buf in
    of_raw ?progress raw

  let add_packed_value ~version ?level buf = match version with
    | 2 -> Packed_value_IO.V2.add buf ?level
    | 3 -> Packed_value_IO.V3.add buf ?level
    | _ -> fail "pack version should be 2 or 3"

  let get h k = try Some (SHA.Map.find k h) with Not_found -> None

  let string_lengths s = List.fold_left (fun acc s -> acc + String.length s) 0 s

  let cstruct_of_strings bufs =
    let len = string_lengths bufs in
    let buf = Cstruct.create len in
    let i = List.fold_left (fun i s ->
        let len = String.length s in
        Cstruct.blit_from_string s 0 buf i len;
        i + len
      ) 0 bufs in
    assert (i = len);
    buf

  let add ?level t =
    Log.debug "add";
    let version = 2 in
    let header =
      let buf = Buffer.create 12 in
      Raw.add_header ~version buf (List.length t);
      Buffer.contents buf
    in
    let buf = Buffer.create 128 in
    let bufs, offsets, crcs =
      List.fold_left (fun (bufs, offsets, crcs) pic ->
          let offset = Buffer.length buf in
          let sha1 = Packed_value.PIC.sha1 pic in
          let offsets = SHA.Map.add sha1 offset offsets in
          let index = get offsets in
          let p = Packed_value_IO.of_pic ~index ~offset pic in
          Buffer.reset buf;
          add_packed_value ~version ?level buf (Packed_value.kind p);
          let buf = Buffer.contents buf in
          let crc32 = Crc.Crc32.string buf 0 (String.length buf) in
          let crcs = SHA.Map.add sha1 crc32 crcs in
          let bufs = buf :: bufs in
          bufs, offsets, crcs
        ) ([], SHA.Map.empty, SHA.Map.empty) t
    in
    let sha1 = Buffer.contents buf |> D.string in
    Log.debug "add sha1: %s" (SHA.to_hex sha1);
    let footer =
      let buf = Buffer.create 40 in
      SHA_IO.add buf sha1;
      Buffer.contents buf
    in
    let bufs = header :: List.rev_append bufs [footer] in
    let buf = cstruct_of_strings bufs in
    let pack_checksum = D.cstruct buf in
    { Pack_index.offsets; crcs; pack_checksum }, buf

  let create contents =
    let uncompressed =
      List.map (fun (k, v) ->
          let raw = Misc.with_buffer (fun buf -> Value_IO.add_inflated buf v) in
          Packed_value.PIC.of_raw k raw
        ) contents in
    (* XXX: Patience_diff.be_clever *)
    uncompressed

  let to_raw t =
    Log.debug "to_raw";
    let index_raw, buf = add t in
    let index = Pack_index.Raw.find_offset index_raw in
    let read _ = Lwt.return_none in
    let keys = keys t in
    let buf = Mstruct.of_cstruct buf in
    Raw.input_with_index ~read ~index ~keys buf

  let err_not_found k =
    Printf.ksprintf failwith "Pack.Not_found: %s" (SHA.pretty k)

  let read t sha1 =
    Log.debug "read %s" (SHA.pretty sha1);
    try
      let is_equal x = SHA.equal (Packed_value.PIC.sha1 x) sha1 in
      let pic = List.find is_equal t in
      Some (Packed_value_IO.value_of_pic pic)
    with Not_found ->
      None

  let read_exn t sha1 =
    match read t sha1 with
    | None   -> err_not_found sha1
    | Some x -> x

end

module type IO = sig
  include Object.S with type t = t
  val add: ?level:int -> t -> Pack_index.Raw.t * Cstruct.t
  val input: ?progress:(string -> unit) ->
    index:Pack_index.f -> keys:SHA.Set.t -> read:Value.read_inflated ->
    Mstruct.t -> t Lwt.t
  val read: t -> SHA.t -> Value.t option
  val read_exn: t -> SHA.t -> Value.t
  val create: (SHA.t * Value.t) list -> t
  module Raw: sig
    include Object.S with type t = raw
    val add: [`Not_defined]
    val input: ?progress:(string -> unit) -> read:Value.read_inflated ->
      Mstruct.t -> t Lwt.t
    val unpack: ?progress:(string -> unit) -> write:Value.write_inflated ->
            t -> SHA.Set.t Lwt.t
    val read: index:Pack_index.f -> read:Value.read_inflated ->
            Mstruct.t -> SHA.t -> Value.t option Lwt.t
    val read_inflated: index:Pack_index.f -> read:Value.read_inflated ->
      Mstruct.t -> SHA.t -> string option Lwt.t
  end
  type raw = Raw.t
  val of_raw: ?progress:(string -> unit) -> Raw.t -> t Lwt.t
  val to_raw: t -> Raw.t
end
