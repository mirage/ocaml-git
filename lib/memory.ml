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

module Log = Log.Make(struct let section = "memory" end)

let err_not_found n k =
  let str = Printf.sprintf "Git.Memory.%s: %s not found" n k in
  Lwt.fail (Invalid_argument str)

module Make (D: SHA.DIGEST) = struct

  module Value_IO = Value.IO(D)(Inflate.None)

  type t = {
    root    : string;
    dot_git : string;
    level   : int;
    values  : (SHA.t, Value.t Lazy.t) Hashtbl.t;
    inflated: (SHA.t, string) Hashtbl.t;
    refs    : (Reference.t, [`S of SHA.Commit.t | `R of Reference.t]) Hashtbl.t;
    mutable head : Reference.head_contents option;
  }

  let root t = t.root
  let dot_git t = t.dot_git
  let level t = t.level

  let stores = Hashtbl.create 1024
  let default_root = "root"

  let reset t =
    Log.debug "reset %s" t.root;
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
    let sha1 = D.string inflated in
    if Hashtbl.mem t.values sha1 then Lwt.return sha1
    else (
      Log.info "Writing %s" (SHA.to_hex sha1);
      Hashtbl.add t.values sha1 (lazy value);
      Hashtbl.add t.inflated sha1 inflated;
      Lwt.return sha1
    )

  let write_inflated t inflated =
    let sha1 = D.string inflated in
    if Hashtbl.mem t.values sha1 then Lwt.return sha1
    else (
      Log.info "Writing %s" (SHA.to_hex sha1);
      Hashtbl.add t.inflated sha1 inflated;
      let value =
        (* FIXME: this allocates too much *)
        lazy (Value_IO.input_inflated (Mstruct.of_string inflated))
      in
      Hashtbl.add t.values sha1 value;
      Lwt.return sha1
    )

  let read_inflated t sha1 =
    try Lwt.return (Some (Hashtbl.find t.inflated sha1))
    with Not_found -> Lwt.return_none

  let read t sha1 =
    try Lwt.return (Some (Lazy.force (Hashtbl.find t.values sha1)))
    with Not_found -> Lwt.return_none

  let err_write_pack expected got =
    let str =
      Printf.sprintf
        "Git_memory.write_pack: wrong checksum.\n\
         Expecting %s, but got %s."
        (SHA.pretty expected) (SHA.pretty got)
    in
    failwith str

  module Pack_IO = Pack.IO(D)(Inflate.None)
  module Packed_value_IO = Packed_value.IO(D)(Inflate.None)

  let write_pack t pack =
    Pack_IO.of_raw pack >>= fun pack ->
    Lwt_list.iter_p (fun p ->
        let v = Packed_value_IO.value_of_pic p in
        let sha1 = Packed_value.PIC.sha1 p in
        write t v >>= fun sha2 ->
        if sha1 <> sha2 then err_write_pack sha1 sha2;
        Lwt.return_unit
      ) pack
    >>= fun () ->
    let keys = Pack_IO.keys pack in
    Lwt.return keys

  let keys t =
    Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t =
    Log.debug "list %s" t.root;
    Lwt.return (keys t.values)

  let mem t sha1 =
    Lwt.return (Hashtbl.mem t.values sha1)

  let read_exn t sha1 =
    read t sha1 >>= function
    | None   -> err_not_found "read_exn" (SHA.pretty sha1)
    | Some v -> Lwt.return v

  let contents t =
    Log.debug "contents";
    list t >>= fun sha1s ->
    Lwt_list.map_s (fun sha1 ->
        read_exn t sha1 >>= fun value ->
        Lwt.return (sha1, value)
      ) sha1s

  let dump t =
    contents t >>= fun contents ->
    List.iter (fun (sha1, value) ->
        let typ = Value.type_of value in
        Printf.eprintf "%s %s\n" (SHA.to_hex sha1) (Object_type.to_string typ)
      ) contents;
    Lwt.return_unit

  let references t =
    Lwt.return (keys t.refs)

  let mem_reference t ref =
    Lwt.return (Hashtbl.mem t.refs ref)

  let rec read_reference t ref =
    Log.info "Reading %s" (Reference.pretty ref);
    try
      match Hashtbl.find t.refs ref with
      | `S s -> Lwt.return (Some s)
      | `R r -> read_reference t r
    with Not_found -> Lwt.return_none

  let read_head t =
    Log.info "Reading HEAD";
    Lwt.return t.head

  let remove_reference t ref =
    Hashtbl.remove t.refs ref;
    Lwt.return_unit

  let read_reference_exn t ref =
    read_reference t ref >>= function
    | None   -> err_not_found "read_reference_exn" (Reference.pretty ref)
    | Some s -> Lwt.return s

  let write_head t c =
    Log.info "Writing HEAD";
    t.head <- Some c;
    Lwt.return_unit

  let write_reference t ref sha1 =
    Log.info "Writing %s" (Reference.pretty ref);
    Hashtbl.replace t.refs ref (`S sha1);
    Lwt.return_unit

  let read_index _t = Lwt.return Index.empty
  let write_index _t ?index:_ _head = Lwt.return_unit

  let kind = `Memory

  module Digest = D
  module Inflate = Inflate.None
end
