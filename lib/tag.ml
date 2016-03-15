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

module Log = (val Misc.src_log "tag" : Logs.LOG)

module T = struct

  type t = {
    obj    : Hash.t;
    typ    : Object_type.t;
    tag    : string;
    tagger : User.t;
    message: string;
  }

  let hash = Hashtbl.hash
  let equal = (=)
  let compare = compare

  let pp ppf t =
    Format.fprintf ppf
      "@[object: %a@ \
       type  : %s@ \
       tag   : %S@ \
       tagger: %a@.\
       %s@]"
      Hash.pp t.obj
      (Object_type.to_string t.typ)
      t.tag
      User.pp t.tagger
      (String.trim t.message)

end

include T

module IO (D: Hash.DIGEST) = struct

  include T
  module Hash_IO = Hash.IO(D)

  let add_key_value buf k v =
    Buffer.add_string buf k;
    Buffer.add_char   buf Misc.sp;
    Buffer.add_string buf v;
    Buffer.add_char   buf Misc.lf

  let input_object_type buf =
    let s = Mstruct.to_string buf in
    match Object_type.of_string s with
    | Some k -> k
    | None   -> Mstruct.parse_error_buf buf "input_object_type: %s" s

  let add buf ?level:_ t =
    add_key_value buf "object" (Hash.to_hex t.obj);
    add_key_value buf "type"   (Object_type.to_string t.typ);
    add_key_value buf "tag"    t.tag;
    Buffer.add_string buf "tagger ";
    User.add buf t.tagger;
    Buffer.add_char buf Misc.lf;
    Buffer.add_char buf Misc.lf;
    Buffer.add_string buf t.message

  let input buf =
    let obj    = Misc.input_key_value buf ~key:"object" Hash_IO.input_hex in
    let typ    = Misc.input_key_value buf ~key:"type" input_object_type in
    let tag    = Misc.input_key_value buf ~key:"tag" Mstruct.to_string in
    let tagger = Misc.input_key_value buf ~key:"tagger" User.input in
    Mstruct.shift buf 1;
    let message = Mstruct.to_string buf in
    { obj; typ; tag; tagger; message }

end
