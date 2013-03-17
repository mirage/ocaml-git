(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lib
open IO
open Model

let todo feature =
  failwith (Printf.sprintf "TODO(%s)" feature)

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'
let lt  = '<'
let gt  = '>'

let input_hex_tree buf =
  let buf = IO.update buf "hex-tree" in
  let buf, s = IO.input_string buf buf.len in
  buf, Hex.Tree.of_string s

let output_hex_tree buf t =
  Buffer.add_string buf (Hex.Tree.to_string t)

let input_sha1 buf =
  let buf = IO.update buf "SHA1" in
  let buf, s = IO.input_string buf 20 in
  buf, SHA1.of_string s

let output_sha1 buf t =
  Buffer.add_string buf (SHA1.to_string t)

module type SIG = sig
  type t

  val dump: t -> unit

  val output: Buffer.t -> t -> unit
  val to_string: t -> string

  val input: IO.buffer -> t IO.result
  val of_string: File.Name.t -> string -> t
end

module User: sig
  include SIG with type t = user
  val pretty: t -> string
end = struct

  type t = user

  (* XXX needs to escape name/email/date *)
  let output buf t =
    Buffer.add_string buf t.name ;
    Buffer.add_string buf " <"   ;
    Buffer.add_string buf t.email;
    Buffer.add_string buf "> "   ;
    Buffer.add_string buf t.date

  let input buf =
    let buf = IO.update buf "user" in
    let i =
      try IO.index_from buf buf.off lt
      with Not_found -> parse_error buf "invalid user name" in
    let j =
      try IO.index_from buf i gt
      with Not_found -> parse_error buf "invalide user email" in
    let update x = IO.update buf x in
    let name  = IO.sub (update "user.name")  buf.off (i - buf.off - 1) in
    let email = IO.sub (update "user.email") (i + 1) (j - i - 1) in
    let date  = IO.sub (update "user.date")  (j + 2) (buf.off + buf.len - j - 2) in
    IO.shift buf buf.len, { name; email; date }

  let pretty t =
    Printf.sprintf
      "[name : %s] [email: %s] [date : %s]"
      t.name t.email t.date

  let dump t =
    Printf.printf "%s\n" (pretty t)

  let of_string = IO.of_string input
  let to_string = IO.to_string output
end

module Blob: SIG with type t = Blob.t = struct

  type t = Blob.t

  let dump t =
    Printf.printf "%s\n" (Blob.to_string t)

  let input buf =
    let buf = IO.update buf "blob" in
    let buf, s = IO.input_string buf buf.len in
    buf, Blob.of_string s

  let output buf t =
    Buffer.add_string buf (Blob.to_string t)

  let of_string = IO.of_string input
  let to_string = IO.to_string output
end

module Commit: SIG with type t = commit = struct

  type t = commit

  let dump t =
    Printf.printf
      "  tree     : %s\n\
      \  parents  : %s\n\
      \  author   : %s\n\
      \  committer: %s\n\
      \  message  :\n%s\n"
      (Hex.Tree.to_string t.tree)
      (String.concat ", " (List.map Hex.Commit.to_string t.parents))
      (User.pretty t.author)
      (User.pretty t.committer)
      t.message

  let output_parent buf parent =
    Buffer.add_string buf "parent ";
    Buffer.add_string buf (Hex.Commit.to_string parent);
    Buffer.add_char   buf lf

  let output buf t =
    Buffer.add_string buf "tree ";
    Buffer.add_string buf (Hex.Tree.to_string t.tree);
    Buffer.add_char   buf lf;
    List.iter (output_parent buf) t.parents;
    Buffer.add_string buf "author ";
    User.output       buf t.author;
    Buffer.add_char   buf lf;
    Buffer.add_string buf "committer ";
    User.output       buf t.committer;
    Buffer.add_char   buf lf;
    Buffer.add_char   buf lf;
    Buffer.add_string buf t.message

  let input_parents buf =
    let buf = IO.update buf "parents" in
    let rec aux buf parents =
      let nbuf, parent = IO.input_delim buf sp in
      if parent = "parent" then
        let nbuf, hex = IO.input_delim nbuf lf in
        let hex = Hex.Commit.of_string hex in
        aux nbuf (hex :: parents)
      else
        (buf, List.rev parents) in
    aux buf []

  let input_key_value orig_buf expected input_value =
    let buf, key =
      let buf = IO.update orig_buf "key" in
      IO.input_delim buf sp in
    if key <> expected then
      parse_error buf "keys: [actual: %s] [expected: %s]" key expected;
    IO.with_subview buf lf input_value

  let input buf =
    let buf = IO.update buf "commit" in
    let buf, tree      = input_key_value buf "tree" input_hex_tree in
    let buf, parents   = input_parents   buf in
    let buf, author    = input_key_value buf "author" User.input in
    let buf, committer = input_key_value buf "committer" User.input in
    let buf, _         = IO.input_delim  buf lf in
    let buf, message   = IO.input_string buf buf.len in
    buf, { parents; message; tree; author; committer }

  let of_string = IO.of_string input
  let to_string = IO.to_string output
end

module Tree: SIG with type t = tree = struct

  type t = tree

  let pretty_perm = function
    | `normal -> "normal"
    | `exec   -> "exec"
    | `link   -> "link"
    | `dir    -> "dir"

  let perm_of_string buf = function
    | "100644" -> `normal
    | "100755" -> `exec
    | "120000" -> `normal
    | "40000"  -> `dir
    | x -> parse_error buf "%S is not a valid permission." x

  let string_of_perm = function
    | `normal -> "100644"
    | `exec   -> "100755"
    | `link   -> "120000"
    | `dir    -> "40000"

  let dump_entry e =
    Printf.printf
      "perm: %s\n\
       file: %s\n\
       sha1: %S\n"
      (pretty_perm e.perm)
      e.file
      (SHA1.to_string e.sha1)

  let dump t =
    List.iter dump_entry t

  let output buf t =
    todo "output-tree"

  let input_entry buf =
    let buf = IO.update buf "entry" in
    let buf, perm = IO.input_delim buf sp in
    let buf, file = IO.input_delim buf nul in
    let buf, sha1 = input_sha1 buf in
    let entry = {
      perm = perm_of_string buf perm;
      file; sha1
    } in
    buf, entry

  let input buf =
    let buf = IO.update buf "entries" in
    let rec aux buf entries =
      if buf.len <= 0 then
        buf, List.rev entries
      else
        let buf, e = input_entry buf in
        aux buf (e :: entries) in
    aux buf []

  let of_string = IO.of_string input
  let to_string = IO.to_string output
end

module Tag: SIG with type t = tag = struct

  type t = tag
  let dump x = todo "tree"
  let output buf t = todo "tree"
  let input buf = todo "tree"

  let of_string = of_string input
  let to_string = to_string output
end

module Object: SIG with type t = obj = struct

  type t = obj

    let dump = function
      | Blob b ->
        Printf.printf "BLOB:\n";
        Blob.dump b
      | Commit c ->
        Printf.printf "COMMIT:\n";
        Commit.dump c
      | Tag t ->
        Printf.printf "TAG:\n";
        Tag.dump t
      | Tree t ->
        Printf.printf "TREE:\n";
        Tree.dump t

  let obj_type = function
    | Blob _   -> "blob"
    | Commit _ -> "commit"
    | Tag _    -> "tag"
    | Tree _   -> "tree"

  let output_contents buf = function
    | Blob b   -> Blob.output buf b
    | Commit c -> Commit.output buf c
    | Tag t    -> Tag.output buf t
    | Tree t   -> Tree.output buf t

  let output buf t =
    let size, contents =
      let buf = Buffer.create 1024 in
      output_contents buf t;
      let size = Buffer.length buf in
      size, buf in
    let inflated =
      let buf = Buffer.create size in
      Buffer.add_string buf (obj_type t);
      Buffer.add_char   buf sp;
      Buffer.add_string buf (string_of_int size);
      Buffer.add_buffer buf contents;
      Buffer.contents buf in
    let deflated =
      (* call zlib directly on the stream interface *)
      Misc.deflate_string inflated in
    Buffer.add_string buf deflated

  let input buf =
    (* XXX call zlib directly on the stream interface *)
    let _, deflated = IO.input_string buf buf.len in
    let inflated = Misc.inflate_string deflated in
    let buf = IO.buffer (IO.file buf) inflated in
    let buf = IO.update buf "object" in
    let buf, obj_type = IO.input_delim buf sp in
    let buf = IO.update buf "size" in
    let buf, size = IO.input_delim buf nul in
    let expected =
        try int_of_string size
        with _ -> parse_error buf "%S is not a valid integer." size in
    if expected <> buf.len then
      parse_error buf "[expected: %d; actual: %d]\n" expected buf.len;
    match obj_type with
    | "blob"   -> let buf, b = Blob.input buf   in buf, Blob b
    | "commit" -> let buf, c = Commit.input buf in buf, Commit c
    | "tag"    -> let buf, t = Tag.input buf    in buf, Tag t
    | "tree"   -> let buf, t = Tree.input buf   in buf, Tree t
    | x        -> parse_error buf "%S is not a valid object type." x

  let of_string = IO.of_string input
  let to_string = IO.to_string output
end

let dump t =
  List.iter (fun (_,b) -> Blob.dump b) t.blobs;
  List.iter (fun (_,c) -> Commit.dump c) t.commits;
  List.iter (fun (_,t) -> Tree.dump t) t.trees;
  List.iter (fun (_,t) -> Tag.dump t) t.tags

(*
module Pack: SIG = struct

  type object_id =
    | Blob   of Blob.id
    | Commit of Commit.id
    | Tag    of Tag.id
    | Tree   of Tree.id

  type delta = {
    base : object_id;
    delta: string;
  }

  type file =
    | Delta  of delta
    | Object of Object.t

  type t = {
    version: int;
    files  : file array;
  }

  let dump x = todo "pack"
  let to_string t = todo "pack"
  let of_string file s = todo "pack"

end
*)

(*
type repository = {
  head   : string;
  index  : XXX;
  objects: Object.id list;
  refs   : refs;
}

type refs = {
  heads  : (string, commit) list;          (**  Local branches *)
  remotes: (string, commit) list;          (** Remote branches *)
}

*)
