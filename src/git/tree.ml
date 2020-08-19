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

(* permission *)

type perm = [ `Normal | `Everybody | `Exec | `Link | `Dir | `Commit ]

let string_of_perm = function
  | `Normal -> "100644"
  | `Everybody -> "100664"
  | `Exec -> "100755"
  | `Link -> "120000"
  | `Dir -> "40000"
  | `Commit -> "160000"

let perm_of_string = function
  | "44" | "100644" -> `Normal
  | "100664" -> `Everybody
  | "100755" -> `Exec
  | "120000" -> `Link
  | "40000" | "040000" -> `Dir
  | "160000" -> `Commit
  | v -> Fmt.invalid_arg "perm_of_string: %s" v

let equal_perm a b =
  match (a, b) with
  | `Normal, `Normal -> true
  | `Everybody, `Everybody -> true
  | `Exec, `Exec -> true
  | `Link, `Link -> true
  | `Dir, `Dir -> true
  | `Commit, `Commit -> true
  | _ -> false

(* entry *)

type 'hash entry = { perm : perm; name : string; node : 'hash }

let pp_entry ~pp ppf { perm; name; node } =
  Fmt.pf ppf "{ @[<hov>perm = %s;@ name = %S;@ node = %a;@] }"
    (match perm with
    | `Normal -> "normal"
    | `Everybody -> "everybody"
    | `Exec -> "exec"
    | `Link -> "link"
    | `Dir -> "dir"
    | `Commit -> "commit")
    name (Fmt.hvbox pp) node

let equal_entry ~equal a b =
  String.equal a.name b.name && equal_perm a.perm b.perm && equal a.node b.node

let entry ~name perm node =
  match String.index name '\000' with
  | _ -> Fmt.invalid_arg "Invalid entry name: %S" name
  | exception Not_found -> { name; perm; node }

(* tree *)

type 'hash t = 'hash entry list

let pp ~pp ppf tree = Fmt.(Dump.list (pp_entry ~pp)) ppf tree

let equal ~equal a b =
  try List.for_all2 (equal_entry ~equal) a b with _ -> false

let hashes tree = List.map (fun { node; _ } -> node) tree

let iter f tree = List.iter f tree

let is_empty t = t = []

external to_list : 'hash t -> 'hash entry list = "%identity"

(* orders *)

type value = Contents of string | Node of string

let ( .![] ) v i =
  match v with
  | Contents v -> if i >= String.length v then '\000' else v.[i]
  | Node v -> if i >= String.length v then '/' else v.[i]

let compare x y =
  match (x, y) with
  | Contents a, Contents b -> String.compare a b
  | (Contents a | Node a), (Contents b | Node b) ->
      let len_a = String.length a in
      let len_b = String.length b in

      let p = ref 0 and c = ref 0 in
      while
        !p < len_a
        && !p < len_b
        &&
        (c := Char.compare a.[!p] b.[!p] ;
         !c = 0)
      do
        incr p
      done ;

      if !p = len_a || !p = len_b
      then
        let res = Char.compare x.![!p] y.![!p] in
        if res = 0 then Char.compare x.![!p + 1] y.![!p + 1] else res
      else !c

let value_of_entry = function
  | { name; perm = `Dir; _ } -> Node name
  | { name; _ } -> Contents name

let v entries =
  List.map (fun entry -> (value_of_entry entry, entry)) entries
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

let remove ~name t =
  let c = Contents name and n = Node name in
  let rec go acc = function
    | [] -> t
    | entry :: rest ->
        if compare (value_of_entry entry) c = 0
        then List.rev_append acc rest
        else if compare (value_of_entry entry) n = 0
        then List.rev_append acc rest
        else go (entry :: acc) rest in
  go [] t

let add entry t =
  let c = Contents entry.name and n = Node entry.name in
  let rec go acc = function
    | [] -> List.rev_append acc [ entry ]
    | x :: r ->
        if compare c (value_of_entry x) = 0
        then go acc r
        else
          let res = compare n (value_of_entry x) in
          if res = 0
          then List.rev_append acc (entry :: r)
          else if res > 0
          then go (x :: acc) r
          else (* res < 0 *) List.rev_append acc (entry :: x :: r) in
  go [] t

let of_list entries = v entries

module type S = sig
  type hash

  type nonrec entry = hash entry

  type nonrec t = hash t

  val entry : name:string -> perm -> hash -> entry

  val v : entry list -> t

  val add : entry -> t -> t

  val remove : name:string -> t -> t

  val is_empty : t -> bool

  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64

  val hashes : t -> hash list

  val to_list : t -> entry list

  val of_list : entry list -> t

  val iter : (entry -> unit) -> t -> unit
end

module Make (Hash : S.HASH) : S with type hash = Hash.t = struct
  type hash = Hash.t

  type nonrec entry = hash entry

  and t = hash t

  let pp ppf t = pp ~pp:Hash.pp ppf t

  let entry ~name perm node = entry ~name perm node

  let v entries = v entries

  let remove ~name t = remove ~name t

  let add entry t = add entry t

  let is_empty t = is_empty t

  let to_list t = t

  let of_list entries = v entries

  let iter t = iter t

  let hashes t = hashes t

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in
    let entry acc x =
      string (string_of_perm x.perm)
      + 1L
      + string x.name
      + 1L
      + Int64.of_int Hash.digest_size
      + acc in
    List.fold_left entry 0L t

  module MakeMeta (Meta : Encore.Meta.S) = struct
    type e = t

    open Helper.BaseIso

    module Iso = struct
      open Encore.Bijection

      let perm =
        make_exn
          ~fwd:(Exn.safe_exn perm_of_string)
          ~bwd:(Exn.safe_exn string_of_perm)

      let hash =
        make_exn
          ~fwd:(Exn.safe_exn Hash.of_raw_string)
          ~bwd:(Exn.safe_exn Hash.to_raw_string)

      let entry =
        make_exn
          ~fwd:(fun ((perm, name), node) -> { perm; name; node })
          ~bwd:(fun { perm; name; node } -> ((perm, name), node))
    end

    type 'a t = 'a Meta.t

    module Meta = Encore.Meta.Make (Meta)
    open Meta

    let is_not_sp = ( <> ) ' '

    let is_not_nl = ( <> ) '\x00'

    let entry =
      let perm = Iso.perm <$> while1 is_not_sp in
      let hash = Iso.hash <$> take Hash.digest_size in
      let name = while1 is_not_nl in
      Iso.entry
      <$> (perm
          <* (char_elt ' ' <$> any)
          <*> (name <* (char_elt '\x00' <$> any))
          <*> hash
          <* commit)

    let p = rep0 entry
  end

  module A = MakeMeta (Encore.Proxy_decoder.Impl)
  module M = MakeMeta (Encore.Proxy_encoder.Impl)
  module D = Helper.MakeDecoder (A)
  module E = Helper.MakeEncoder (M)

  let digest value =
    let tmp = Cstruct.create 0x100 in
    let etmp = Cstruct.create 0x100 in
    Helper.digest (module Hash) (module E) ~etmp ~tmp ~kind:"tree" ~length value

  let equal = ( = )

  let compare = Stdlib.compare

  let hash = Hashtbl.hash

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
