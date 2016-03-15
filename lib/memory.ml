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

module Log = Misc.Log_make(struct let section = "memory" end)

let err_not_found n k =
  let str = Printf.sprintf "Git.Memory.%s: %s not found" n k in
  Lwt.fail (Invalid_argument str)

module Make (D: Hash.DIGEST) (I: Inflate.S) = struct

  module Value_IO = Value.IO(D)(I)

  type t = {
    root    : string;
    dot_git : string;
    level   : int;
    values  : (Hash.t, Value.t Lazy.t) Hashtbl.t;
    inflated: (Hash.t, string) Hashtbl.t;
    refs    : (Reference.t, [`H of Hash.t | `R of Reference.t]) Hashtbl.t;
    mutable head : Reference.head_contents option;
  }

  let root t = t.root
  let dot_git t = t.dot_git
  let level t = t.level

  let stores = Hashtbl.create 1024
  let default_root = "root"

  let reset t =
    Log.debug (fun l -> l "reset %s" t.root);
    Hashtbl.clear t.values;
    Hashtbl.clear t.inflated;
    Hashtbl.clear t.refs;
    t.head <- None

  let clear ?(root=default_root) () =
    let () = try reset (Hashtbl.find stores root) with Not_found -> () in
    Hashtbl.remove stores root

  let clear_all () =
    Hashtbl.iter (fun _ t -> reset t) stores

  let (/) = Filename.concat

  let create ?(root=default_root) ?(dot_git=default_root / ".git") ?(level=6) () =
    if level < 0 || level > 9 then failwith "level should be between 0 and 9";
    let t =
      try Hashtbl.find stores root
      with Not_found ->
        let t = {
          root; level; dot_git;
          values   = Hashtbl.create 1024;
          inflated = Hashtbl.create 1024;
          refs     = Hashtbl.create 8;
          head     = None;
        } in
        Hashtbl.add stores root t;
        t in
    Lwt.return t

  let write t value =
    let inflated =
      Misc.with_buffer (fun buf -> Value_IO.add_inflated buf value)
    in
    let h = D.string inflated in
    if Hashtbl.mem t.values h then Lwt.return h
    else (
      Log.info (fun l -> l "Writing %a" Hash.pp h);
      Hashtbl.add t.values h (lazy value);
      Hashtbl.add t.inflated h inflated;
      Lwt.return h
    )

  let write_inflated t inflated =
    let h = D.string inflated in
    if Hashtbl.mem t.values h then Lwt.return h
    else (
      Log.info (fun l -> l "Writing %a" Hash.pp h);
      Hashtbl.add t.inflated h inflated;
      let value =
        (* FIXME: this allocates too much *)
        lazy (Value_IO.input_inflated (Mstruct.of_string inflated))
      in
      Hashtbl.add t.values h value;
      Lwt.return h
    )

  let read_inflated t h =
    try Lwt.return (Some (Hashtbl.find t.inflated h))
    with Not_found -> Lwt.return_none

  let read t h =
    try Lwt.return (Some (Lazy.force (Hashtbl.find t.values h)))
    with Not_found -> Lwt.return_none

  let err_write_pack expected got =
    let str =
      Fmt.strf
        "Git_memory.write_pack: wrong checksum.\n\
         Expecting %a, but got %a."
        Hash.pp expected Hash.pp got
    in
    failwith str

  module Pack_IO = Pack.IO(D)(I)
  module Packed_value_IO = Packed_value.IO(D)(I)

  let write_pack t pack =
    Pack_IO.of_raw pack >>= fun pack ->
    Lwt_list.iter_p (fun p ->
        let v = Packed_value_IO.value_of_pic p in
        let name = Packed_value.PIC.name p in
        write t v >>= fun h ->
        if not (Hash.equal name h) then err_write_pack name h;
        Lwt.return_unit
      ) pack
    >>= fun () ->
    let keys = Pack.keys pack in
    Lwt.return keys

  let keys t =
    Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t =
    Log.debug (fun l -> l "list %s" t.root);
    Lwt.return (keys t.values)

  let mem t h =
    Lwt.return (Hashtbl.mem t.values h)

  let read_exn t h =
    read t h >>= function
    | None   -> err_not_found "read_exn" (Hash.to_hex h)
    | Some v -> Lwt.return v

  let contents t =
    Log.debug (fun l -> l "contents");
    list t >>= fun hashes ->
    Lwt_list.map_s (fun h -> read_exn t h >|= fun value -> h, value) hashes

  let dump t =
    contents t >>= fun contents ->
    List.iter (fun (h, value) ->
        let typ = Value.type_of value in
        Printf.eprintf "%s %s\n" (Hash.to_hex h) (Object_type.to_string typ)
      ) contents;
    Lwt.return_unit

  let references t =
    Lwt.return (keys t.refs)

  let mem_reference t ref =
    Lwt.return (Hashtbl.mem t.refs ref)

  let rec read_reference t r =
    Log.info (fun l -> l "Reading %a" Reference.pp r);
    try match Hashtbl.find t.refs r with
      | `H s -> Lwt.return (Some s)
      | `R r -> read_reference t r
    with Not_found ->
      Lwt.return_none

  let read_head t =
    Log.info (fun l -> l "Reading HEAD");
    Lwt.return t.head

  let remove_reference t r =
    Hashtbl.remove t.refs r;
    Lwt.return_unit

  let read_reference_exn t r =
    read_reference t r >>= function
    | Some s -> Lwt.return s
    | None   ->
      err_not_found "read_reference_exn" (Fmt.to_to_string Reference.pp r)

  let write_head t c =
    Log.info (fun l -> l "Writing HEAD");
    t.head <- Some c;
    Lwt.return_unit

  let write_reference t r h =
    Log.info (fun l -> l "Writing %a" Reference.pp r);
    Hashtbl.replace t.refs r (`H h);
    Lwt.return_unit

  let read_index _t = Lwt.return Index.empty
  let write_index _t ?index:_ _head = Lwt.return_unit

  let kind = `Memory

  module Digest = D
  module Inflate = I
end
