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

module Log = (val Git_misc.src_log "commit" : Logs.LOG)

module T = struct

  type t = {
    tree     : Git_hash.Tree.t;
    parents  : Git_hash.Commit.t list;
    author   : Git_user.t;
    committer: Git_user.t;
    message  : string;
  }

  let hash = Hashtbl.hash

  let compare = compare

  let equal = (=)

  let pp_parents ppf parents =
    List.iter (fun t ->
        Format.fprintf ppf "\"%a\";@ " Git_hash.Commit.pp t
      ) parents

  let pp ppf t =
    Format.fprintf ppf
      "{@[<hov 2>\
       tree = \"%a\";@ \
       parents = [@,@[<hov 2>%a@]];@ \
       author = %a;@ \
       committer = %a;@.\
       message = %S@]}"
      Git_hash.Tree.pp t.tree
      pp_parents t.parents
      Git_user.pp t.author
      Git_user.pp t.committer
      (String.trim t.message)

end

include T

module IO (D: Git_hash.DIGEST) = struct

  include T
  module Hash_IO = Git_hash.IO(D)

  let add_parent buf parent =
    Buffer.add_string buf "parent ";
    Hash_IO.Commit.add_hex buf parent;
    Buffer.add_char buf Git_misc.lf

  let add buf ?level:_ t =
    Buffer.add_string buf "tree ";
    Hash_IO.Tree.add_hex buf t.tree;
    Buffer.add_char buf Git_misc.lf;
    List.iter (add_parent buf) t.parents;
    Buffer.add_string buf "author ";
    Git_user.add buf t.author;
    Buffer.add_char buf Git_misc.lf;
    Buffer.add_string buf "committer ";
    Git_user.add buf t.committer;
    Buffer.add_char buf Git_misc.lf;
    Buffer.add_char buf Git_misc.lf;
    Buffer.add_string buf t.message

  let commit_sha buf = Hash_IO.Commit.input_hex buf

  let input_parents buf =
    let rec aux parents =
      match Mstruct.get_string_delim buf Git_misc.sp with
      | None          -> List.rev parents
      | Some "parent" ->
        begin
          match Mstruct.get_delim buf Git_misc.lf commit_sha with
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

  let tree_sha buf = Hash_IO.Tree.input_hex buf

  let input buf =
    let tree      = Git_misc.input_key_value buf ~key:"tree" tree_sha in
    let parents   = input_parents buf in
    let author    = Git_misc.input_key_value buf ~key:"author" Git_user.input in
    let committer = Git_misc.input_key_value buf ~key:"committer" Git_user.input in
    Mstruct.shift buf 1;
    let message   = Mstruct.to_string buf in
    { parents; message; tree; author; committer }

end
