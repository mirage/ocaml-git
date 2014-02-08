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

open Core_kernel.Std
module Log = Log.Make(struct let section = "tag" end)

module T = struct
  type t = {
    sha1   : SHA1.t;
    typ    : Object_type.t;
    tag    : string;
    tagger : User.t;
    message: string;
  } with bin_io, compare, sexp
  let hash (t: t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Tag"
end
include T
include Identifiable.Make (T)

let pretty t =
  sprintf
    "object: %s\n\
     type  : %s\n\
     tag   : %S\n\
     tagger: %s\n\n\
     %s\n"
    (SHA1.to_hex t.sha1)
    (Object_type.to_string t.typ)
    t.tag
    (User.pretty t.tagger)
    (String.strip t.message)

let add_key_value buf k v =
  Bigbuffer.add_string buf k;
  Bigbuffer.add_char   buf Misc.sp;
  Bigbuffer.add_string buf v;
  Bigbuffer.add_char   buf Misc.lf

let input_object_type buf =
  let s = Mstruct.to_string buf in
  match Object_type.of_string s with
  | Some k -> k
  | None   -> Mstruct.parse_error_buf buf "input_object_type: %s" s

let add buf t =
  add_key_value buf "object" (SHA1.to_hex t.sha1);
  add_key_value buf "type"   (Object_type.to_string t.typ);
  add_key_value buf "tag"    t.tag;
  Bigbuffer.add_string buf "tagger ";
  User.add buf t.tagger;
  Bigbuffer.add_char buf Misc.lf;
  Bigbuffer.add_char buf Misc.lf;
  Bigbuffer.add_string buf t.message

let input buf =
  let sha1   = Misc.input_key_value buf ~key:"object" SHA1.input_hex in
  let typ    = Misc.input_key_value buf ~key:"type" input_object_type in
  let tag    = Misc.input_key_value buf ~key:"tag" Mstruct.to_string in
  let tagger = Misc.input_key_value buf ~key:"tagger" User.input in
  Mstruct.shift buf 1;
  let message = Mstruct.to_string buf in
  { sha1; typ; tag; tagger; message }
