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

module type S = sig
  module Hash : S.HASH

  type perm = [`Normal | `Everybody | `Exec | `Link | `Dir | `Commit]
  type entry = private {perm: perm; name: string; node: Hash.t}

  val pp_entry : entry Fmt.t
  val entry : string -> perm -> Hash.t -> entry

  type t

  val remove : name:string -> t -> t
  val add : t -> entry -> t
  val is_empty : t -> bool
  val perm_of_string : string -> perm
  val string_of_perm : perm -> string

  module MakeMeta (Meta : Encore.Meta.S) : sig
    val p : t Meta.t
  end

  module A : S.DESC with type 'a t = 'a Angstrom.t and type e = t
  module M : S.DESC with type 'a t = 'a Encore.Encoder.t and type e = t

  module D :
    S.DECODER
    with type t = t
     and type init = Cstruct.t
     and type error = Error.Decoder.t

  module E :
    S.ENCODER
    with type t = t
     and type init = Cstruct.t * t
     and type error = Error.never

  include S.DIGEST with type t := t and type hash := Hash.t
  include S.BASE with type t := t

  val length : t -> int64
  val hashes : t -> Hash.t list
  val to_list : t -> entry list
  val of_list : entry list -> t
  val iter : (entry -> unit) -> t -> unit
end

module Make (Hash : S.HASH) = struct
  type entry = {perm: perm; name: string; node: Hash.t}

  and perm = [`Normal | `Everybody | `Exec | `Link | `Dir | `Commit]

  and t = entry list

  let hashes tree = List.map (fun {node; _} -> node) tree

  let pp_entry ppf {perm; name; node} =
    Fmt.pf ppf "{ @[<hov>perm = %s;@ name = %s;@ node = %a;@] }"
      ( match perm with
      | `Normal -> "normal"
      | `Everybody -> "everybody"
      | `Exec -> "exec"
      | `Link -> "link"
      | `Dir -> "dir"
      | `Commit -> "commit" )
      name (Fmt.hvbox Hash.pp) node

  let pp ppf tree =
    Fmt.pf ppf "[ %a ]"
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") pp_entry))
      tree

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
    | _ -> raise (Invalid_argument "perm_of_string")

  external to_list : t -> entry list = "%identity"

  let iter f tree = List.iter f tree
  let is_empty t = t = []

  type value = Contents : string -> value | Node : string -> value

  let str = function Contents s -> s | Node s -> s

  exception Result of int

  let compare x y =
    match x, y with
    | Contents x, Contents y -> String.compare x y
    | _ -> (
        let xs = str x and ys = str y in
        let lenx = String.length xs in
        let leny = String.length ys in
        let i = ref 0 in
        try
          while !i < lenx && !i < leny do
            match
              Char.compare (String.unsafe_get xs !i) (String.unsafe_get ys !i)
            with
            | 0 -> incr i
            | i -> raise (Result i)
          done ;
          let get len s i =
            if i < len then String.unsafe_get (str s) i
            else if i = len then
              match s with Node _ -> '/' | Contents _ -> '\000'
            else '\000'
          in
          match Char.compare (get lenx x !i) (get leny y !i) with
          | 0 -> Char.compare (get lenx x (!i + 1)) (get leny y (!i + 1))
          | i -> i
        with Result i -> i )

  exception Break

  let has predicate s =
    let ln = String.length s in
    try
      for i = 0 to ln - 1 do
        if predicate (String.unsafe_get s i) then raise Break
      done ;
      false
    with Break -> true

  let value_of_contents c = Contents c
  let value_of_node n = Node n

  let entry name perm node =
    if has (( = ) '\000') name then invalid_arg "of_entry" ;
    {name; perm; node}

  let value_of_entry = function
    | {name; perm= `Dir; _} -> value_of_node name
    | {name; _} -> value_of_contents name

  let of_list entries : t =
    List.map (fun x -> value_of_entry x, x) entries
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd

  let remove ~name t =
    let node_key = value_of_node name in
    let contents_key = value_of_contents name in
    let return ~acc rest = List.rev_append acc rest in
    let rec aux acc = function
      | [] -> t
      | h :: l -> (
          let entry_key = value_of_entry h in
          if compare contents_key entry_key = 0 then return ~acc l
          else
            match compare node_key entry_key with
            | i when i > 0 -> aux (h :: acc) l
            | 0 -> return ~acc l
            | _ -> t )
    in
    aux [] t

  let add t e =
    let node_key = value_of_node e.name in
    let contents_key = value_of_contents e.name in
    let return ~acc rest = List.rev_append acc (e :: rest) in
    let rec aux acc = function
      | [] -> return ~acc []
      | ({node; _} as h) :: l -> (
          let entry_key = value_of_entry h in
          (* Remove any contents entry with the same name. This will always
             come before the new succ entry. *)
          if compare contents_key entry_key = 0 then aux acc l
          else
            match compare node_key entry_key with
            | i when i > 0 -> aux (h :: acc) l
            | i when i < 0 -> return ~acc (h :: l)
            | 0 when Hash.equal e.node node -> t
            | _ -> return ~acc l )
    in
    aux [] t

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in
    let entry acc x =
      string (string_of_perm x.perm)
      + 1L
      + string x.name
      + 1L
      + Int64.of_int Hash.digest_size
      + acc
    in
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
          ~fwd:(fun ((perm, name), node) -> {perm; name; node})
          ~bwd:(fun {perm; name; node} -> (perm, name), node)
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
      <$> ( perm
          <* (char_elt ' ' <$> any)
          <*> (name <* (char_elt '\x00' <$> any))
          <*> hash <* commit)

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

  module Set = Set.Make (struct type nonrec t = t

                                let compare = compare end)

  module Map = Map.Make (struct type nonrec t = t

                                let compare = compare end)
end
