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
module Log = Log.Make(struct let section = "commit" end)

module T = struct
  type t = {
    tree     : SHA1.Tree.t;
    parents  : SHA1.Commit.t list;
    author   : User.t;
    committer: User.t;
    message  : string;
  } with bin_io, compare, sexp
  let hash (t : t) = Hashtbl.hash t
  include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  let module_name = "Commit"
end
include T
include Identifiable.Make (T)

let pretty t =
  sprintf
    "tree     : %s\n\
     parents  : %s\n\
     author   : %s\n\
     committer: %s\n\n\
     %s\n"
    (SHA1.Tree.to_hex t.tree)
    (String.concat ~sep:", " (List.map ~f:SHA1.Commit.to_hex t.parents))
    (User.pretty t.author)
    (User.pretty t.committer)
    (String.strip t.message)

let add_parent buf parent =
  Bigbuffer.add_string buf "parent ";
  SHA1.Commit.add_hex buf parent;
  Bigbuffer.add_char buf Misc.lf

let add buf t =
  Bigbuffer.add_string buf "tree ";
  SHA1.Tree.add_hex buf t.tree;
  Bigbuffer.add_char buf Misc.lf;
  List.iter ~f:(add_parent buf) t.parents;
  Bigbuffer.add_string buf "author ";
  User.add buf t.author;
  Bigbuffer.add_char buf Misc.lf;
  Bigbuffer.add_string buf "committer ";
  User.add buf t.committer;
  Bigbuffer.add_char buf Misc.lf;
  Bigbuffer.add_char buf Misc.lf;
  Bigbuffer.add_string buf t.message

let input_parents buf =
  let rec aux parents =
    match Mstruct.get_string_delim buf Misc.sp with
    | None          -> List.rev parents
    | Some "parent" ->
      begin match Mstruct.get_delim buf Misc.lf SHA1.Commit.input_hex with
        | None   -> Mstruct.parse_error_buf buf "input_parents"
        | Some h -> aux (h :: parents)
      end
    | Some p ->
      (* we cancel the shift we've done to input the key *)
      let n = String.length p in
      Mstruct.shift buf (-n-1);
      List.rev parents
  in
  aux []

let input buf =
  let tree      = Misc.input_key_value buf ~key:"tree" SHA1.Tree.input_hex in
  let parents   = input_parents buf in
  let author    = Misc.input_key_value buf ~key:"author" User.input in
  let committer = Misc.input_key_value buf ~key:"committer" User.input in
  Mstruct.shift buf 1;
  let message   = Mstruct.to_string buf in
  { parents; message; tree; author; committer }
