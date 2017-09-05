(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

let err_not_found n k =
  let str = Printf.sprintf "Git.Mem.%s: %s not found" n k in
  Lwt.fail (Invalid_argument str)

module type S =
sig
  module Hash
    : Ihash.S

  module Path
    : Path.S

  module Inflate
    : S.INFLATE

  module Deflate
    : S.DEFLATE

  module Buffer
    : Value.BUFFER

  module Value
    : Value.RAW
      with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module Buffer = Buffer

  module Reference
    : Reference.S
      with module Hash = Hash
       and module Path = Path

  type t

  val create : ?root:string -> ?dot_git:string -> ?level:int -> unit -> t Lwt.t

  val dot_git : t -> string
  val root : t -> string
  val level : t -> int

  val dump : Format.formatter -> t -> unit Lwt.t
  val contents : t -> (Hash.t * Value.t) list Lwt.t
  val size : t -> Hash.t -> int option Lwt.t
  val read : t -> Hash.t -> Value.t option Lwt.t
  val read_exn : t -> Hash.t -> Value.t Lwt.t
  val mem : t -> Hash.t -> bool Lwt.t
  val list : t -> Hash.t list Lwt.t
  val write : t -> Value.t -> Hash.t Lwt.t
  val references : t -> Reference.t list Lwt.t
  val mem_reference : t -> Reference.t -> bool Lwt.t
  val read_reference : t -> Reference.t -> Hash.t option Lwt.t
  val read_reference_exn : t -> Reference.t -> Hash.t Lwt.t
  val write_head : t -> Reference.head_contents -> unit Lwt.t
  val read_head : t -> Reference.head_contents option Lwt.t
  val write_reference : t -> Reference.t -> Hash.t -> unit Lwt.t
  val remove_reference : t -> Reference.t -> unit Lwt.t
  val test_and_set_reference : t -> Reference.t -> test:Hash.t option -> set:Hash.t option -> bool Lwt.t
  val read_inflated : t -> Hash.t -> string option Lwt.t
  val write_inflated : t -> kind:[ `Commit | `Tree | `Blob | `Tag ] -> Cstruct.t -> Hash.t Lwt.t
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (P : Path.S)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    (B : Value.BUFFER with type raw = string
                       and type fixe = Cstruct.t)
  : S with module Hash = H
       and module Path = P
       and module Inflate = I
       and module Deflate = D
       and module Buffer = B
= struct
  module Hash = H
  module Path = P
  module Inflate = I
  module Deflate = D
  module Buffer = B

  module Value = Value.Raw(H)(I)(D)(B)
  module Reference = Reference.Make(H)(P)

  type t =
    { root     : string
    ; dot_git  : string
    ; level    : int
    ; values   : (Hash.t, Value.t Lazy.t) Hashtbl.t
    ; inflated : (Hash.t, string) Hashtbl.t
    ; refs     : (Reference.t, [ `H of Hash.t | `R of Reference.t ]) Hashtbl.t
    ; mutable head : Reference.head_contents option }

  let root t = t.root
  let dot_git t = t.dot_git
  let level t = t.level

  let stores = Hashtbl.create 1024
  let default_root = "root"

  let reset t =
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

  let create ?(root = default_root) ?(dot_git = default_root / ".git") ?(level = 6) () =
    if level < 0 || level > 9
    then failwith "level should be between 0 and 9";

    let t =
      try Hashtbl.find stores root
      with Not_found ->
        let t =
          { root
          ; level
          ; dot_git
          ; values   = Hashtbl.create 1024
          ; inflated = Hashtbl.create 1024
          ; refs     = Hashtbl.create 8
          ; head     = None }
        in

        Hashtbl.add stores root t;
        t
    in
    Lwt.return t

  let write t value =
    let hash = Value.digest value in

    if Hashtbl.mem t.values hash
    then Lwt.return hash
    else match Value.to_raw value with
      | Error `Never -> assert false
      | Ok inflated ->
        Hashtbl.add t.values hash (lazy value);
        Hashtbl.add t.inflated hash inflated;
        Lwt.return hash

  let digest kind raw =
    let len = Cstruct.len raw in
    let ctx = Hash.Digest.init () in
    let hdr = Fmt.strf "%s %d\000%!"
        (match kind with
         | `Commit -> "commit"
         | `Blob -> "blob"
         | `Tree -> "tree"
         | `Tag -> "tag")
        len
    in
    Hash.Digest.feed ctx (Cstruct.of_string hdr);
    Hash.Digest.feed ctx raw;
    Hash.Digest.get ctx

  let write_inflated t ~kind inflated =
    let hash = digest kind inflated in

    if Hashtbl.mem t.values hash
    then Lwt.return hash
    else
      let value = lazy (match Value.of_raw ~kind inflated with Error (`Decoder err) -> raise (Failure err) | Ok value -> value) in
      Hashtbl.add t.inflated hash (kind, Cstruct.to_string inflated);
      Hashtbl.add t.values hash value;
      Lwt.return hash

  let read_inflated t h =
    try Lwt.return (Some (Hashtbl.find t.inflated h))
    with Not_found -> Lwt.return_none

  let read t h =
    try Lwt.return (Some (Lazy.force (Hashtbl.find t.values h)))
    with Not_found -> Lwt.return_none

  let keys t =
    Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t =
    Lwt.return (keys t.values)

  let mem t h =
    Lwt.return (Hashtbl.mem t.values h)

  let size t h =
    read t h >|= function
    | Some (Value.Blob v) -> Some (Int64.to_int (Value.Blob.F.length v))
    | _ -> None

  let read_exn t h =
    read t h >>= function
    | None   -> err_not_found "read_exn" (Hash.to_hex h)
    | Some v -> Lwt.return v

  let contents t =
    list t >>= fun hashes ->
    Lwt_list.map_s (fun h -> read_exn t h >|= fun value -> h, value) hashes

  let dump ppf t =
    contents t >|= fun contents ->
    let f = function
      | (hash, Value.Blob _)   -> Fmt.pf ppf "%a %s\n%!" Hash.pp hash "blob"
      | (hash, Value.Commit _) -> Fmt.pf ppf "%a %s\n%!" Hash.pp hash "commit"
      | (hash, Value.Tag _)    -> Fmt.pf ppf "%a %s\n%!" Hash.pp hash "tag"
      | (hash, Value.Tree _)   -> Fmt.pf ppf "%a %s\n%!" Hash.pp hash "tree"
    in

    List.iter f contents

  let references t =
    Lwt.return (keys t.refs)

  let mem_reference t ref =
    Lwt.return (Hashtbl.mem t.refs ref)

  let rec read_reference t r =
    try match Hashtbl.find t.refs r with
      | `H s -> Lwt.return (Some s)
      | `R r -> read_reference t r
    with Not_found ->
      Lwt.return_none

  let read_head t =
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
    t.head <- Some c;
    Lwt.return_unit

  let write_reference t r h =
    Hashtbl.replace t.refs r (`H h);
    Lwt.return_unit

  let test_and_set_reference t r ~test ~set =
    let v = try Some (Hashtbl.find t.refs r) with Not_found -> None in
    let replace () = match set with
      | None   -> Hashtbl.remove t.refs r
      | Some v -> Hashtbl.replace t.refs r (`H v)
    in
    match v, test with
    | None  , None -> replace (); Lwt.return true
    | Some (`H x), Some y when Hash.equal x y -> replace (); Lwt.return true
    | _ -> Lwt.return false

  let kind = `Mem
end
