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

let todo () =
  failwith "TODO"

module type ABSTRACT = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
end

module type BLOCK = sig
  module ID: ABSTRACT
  type id = ID.t
  include ABSTRACT
  val dump: t -> unit
end

module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let buf = String.create n in
  really_input ic buf 0 n;
  close_in ic;
  buf

let write_file filename ~contents =
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc

module Zlib = struct

  let refil input =
    let n = String.length input in
    let toread = ref n in
    fun buf ->
      let m =
        if !toread <= String.length buf then !toread
        else String.length buf in
      String.blit input (n - !toread) buf 0 m;
      toread := !toread - m;
      m

  let flush output buf len =
    Buffer.add_substring output buf 0 len

  let deflate input =
    let output = Buffer.create 1024 in
    Zlib.compress (refil input) (flush output);
    Buffer.contents output

  let inflate input =
    let output = Buffer.create 1024 in
    Zlib.uncompress (refil input) (flush output);
    Buffer.contents output

end

let parse_error fmt =
  Printf.kprintf (fun str ->
    Printf.printf "Parse error: %s\n" str;
    raise Parsing.Parse_error
  ) fmt

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'


(* ...|key value\n|...*)
(*    |   |     | |   *)
(*    |   i     j |   *)
(*   off         res  *)

(* ...|key value\0|...*)
(*    |   |     | |   *)
(*    |   i     j |   *)
(*   off         res  *)

type kv = {
  key  : string;
  value: string;
  sep  : char;
}

let string_sub orig buf off len =
  try String.sub buf off len
  with e ->
    Printf.printf "%s: buf=%s (%d) off=%d len=%d\n"
      orig buf (String.length buf) off len;
    raise e

let kv sep key value = { key; value; sep }

let input_kv sep buf off =
  let i =
    try String.index_from buf off sp
    with Not_found -> parse_error "unknonw key." in
  let j =
    try String.index_from buf i sep
    with Not_found -> parse_error "unknowm value." in
  let key   = string_sub "kv.key"   buf off (i - off) in
  let value = string_sub "kv.valye" buf (i + 1) (j - i - 1) in
  {key; value; sep}, j+1


let kv_nul = kv nul
let kv_lf  = kv lf

let input_kv_nul = input_kv nul
let input_kv_lf  = input_kv lf

let output_kv buf t =
  Buffer.add_string buf t.key;
  Buffer.add_char   buf sp;
  Buffer.add_string buf t.value;
  Buffer.add_char   buf t.sep

module User = struct

  type t = {
    name : string;
    email: string;
    date : string;
  }

  (* XXX needs to escape name/email/date *)
  let to_string t =
    Printf.sprintf "%s <%s> %s" t.name t.email t.date

  let of_string s =
    let i =
      try String.index s '<'
      with Not_found -> parse_error "invalid user name" in
    let j =
      try String.index_from s i '>'
      with Not_found -> parse_error "invalide user email" in
    let name  = string_sub "user.name"  s 0 (i-1) in
    let email = string_sub "user.email" s (i + 1) (j - i - 1) in
    let date  = string_sub "user.date"  s (j + 2) (String.length s - j - 2) in
    { name; email; date }

  let pretty t =
    Printf.sprintf
      "[name : %s] [email: %s] [date : %s]"
      t.name t.email t.date

end

module Blob: BLOCK = struct

  module ID = Base

  type id = ID.t

  include Base

  let dump x =
    Printf.printf "%s\n" x

end

module rec Object: BLOCK = struct

   module ID = Base

  type id = ID.t

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tag    of Tag.t
    | Tree   of Tree.t

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

  let contents = function
    | Blob b   -> Blob.to_string b
    | Commit c -> Commit.to_string c
    | Tag t    -> Tag.to_string t
    | Tree t   -> Tree.to_string t

  let to_string t =
    let value = contents t in
    let size = String.length value in
    let kv = kv_nul (obj_type t) (string_of_int size) in
    let buf = Buffer.create (2*size) in
    output_kv buf kv;
    Buffer.add_string buf value;
    Zlib.deflate (Buffer.contents buf)

  let of_string s =
    let s = Zlib.inflate s in
    let kv, off = input_kv_nul s 0 in
    let expected_size =
      try int_of_string kv.value
      with _ -> parse_error "%s is not a valid integer." kv.value in
    let actual_size = String.length s - off in
    if expected_size <> actual_size then
      parse_error "[expected: %d; actual: %d; start='%s']\n"
        expected_size actual_size
        (String.sub s off 10);
    let contents = string_sub "object.contents" s off actual_size in
    match kv.key with
      | "blob"   -> Blob (Blob.of_string contents)
      | "commit" -> Commit (Commit.of_string contents)
      | "tag"    -> Tag (Tag.of_string contents)
      | "tree"   -> Tree (Tree.of_string contents)
      | x        -> parse_error "%s is not a valid object type." x

end and Commit: BLOCK = struct

  module ID = Base

  type id = ID.t

  type t = {
    tree     : Tree.id;
    parents  : id list;
    author   : User.t;
    committer: User.t;
    message  : string;
  }

  let dump t =
    Printf.printf
      "  tree     : %s\n\
      \  parents  : %s\n\
      \  author   : %s\n\
      \  committer: %s\n\
      \  message  :\n%s\n"
      (Tree.ID.to_string t.tree)
      (String.concat ", " (List.map ID.to_string t.parents))
      (User.pretty t.author)
      (User.pretty t.committer)
      t.message

  let to_string t =
    let tree = kv_lf "tree" (Tree.ID.to_string t.tree) in
    let parent id = kv_lf "parent" (Tree.ID.to_string t.tree) in
    let author = kv_lf "author" (User.to_string t.author) in
    let committer = kv_lf "committer" (User.to_string t.committer) in
    let buf = Buffer.create 1024 in
    output_kv buf tree;
    List.iter (fun id -> output_kv buf (parent id)) t.parents;
    output_kv buf author;
    output_kv buf committer;
    Buffer.add_char   buf lf;
    Buffer.add_string buf t.message;
    Buffer.contents buf

  let input_parents s off =
    let rec aux off parents =
      let kv, noff = input_kv_lf s off in
      if kv.key = "parent" then
        let id = ID.of_string kv.value in
        aux noff (id :: parents)
      else
        (List.rev parents, off) in
    aux off []

  let of_string s =
    let tree, off = input_kv_lf s 0 in
    assert (tree.key = "tree");
    let parents, off = input_parents s off in
    let author, off = input_kv_lf s 0 in
    assert (author.key = "key");
    let committer, off = input_kv_lf s 0 in
    assert (committer.key = "committer");
    let message = string_sub "commit.message" s (off + 1) (String.length s - off - 1) in
    {
      parents; message;
      tree      = Tree.ID.of_string tree.value;
      author    = User.of_string author.value;
      committer = User.of_string committer.value;
    }

end and Tree: BLOCK = struct

  module ID = Base

  type id = ID.t

  type entry = {
    perm: [`normal|`exec|`link];
    name: string;
    sha1: Object.t
  }
  type t = entry list

  let dump x      = todo ()
  let to_string t = todo ()
  let of_string s = todo ()

end and Tag: BLOCK = struct

  module ID = Base

  type id = ID.t

  type t = {
    commit : Commit.id;
    tag    : string;
    tagger : User.t;
    message: string;
  }

  let dump x      = todo ()
  let to_string t = todo ()
  let of_string s = todo ()

end

module Pack: BLOCK = struct

  module ID = Base

  type id = ID.t

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

  let dump x      = todo ()
  let to_string t = todo ()
  let of_string s = todo ()

end

(*
type repository = {
  head   : string;
  index  : XXX;
  objects: Object.id list;
  refs   : refs;
}
*)

(*

type 'a sha1


(** Commits *)
module Commit = struct
  include ID
  type t = id entity
end

mo

(** Blobs *)
module Blob = struct
  include ID
  type t = string
  let to_string t =
    Printf.sprintf "blob %d\000%s" (String.length t) t
  let of_string t =


type refs = {
  heads  : (string, commit) list;          (**  Local branches *)
  remotes: (string, commit) list;          (** Remote branches *)
}

*)
