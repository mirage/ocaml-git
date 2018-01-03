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

module type S =
sig
  module Hash: S.HASH

  type entry =
    { perm: perm
    ; name: string
    ; node: Hash.t }
  and perm =
    [ `Normal | `Everybody | `Exec | `Link | `Dir | `Commit ]
  and t

  module D: S.DECODER  with type t = t
                        and type init = Cstruct.t
                        and type error = [ `Decoder of string ]
  module A: S.ANGSTROM with type t = t
  module F: S.FARADAY  with type t = t
  module M: S.MINIENC  with type t = t
  module E: S.ENCODER  with type t = t
                        and type init = int * t
                        and type error = [ `Never ]

  include S.DIGEST with type t := t and type hash = Hash.t
  include S.BASE with type t := t

  val hashes: t -> Hash.t list
  val to_list: t -> entry list
  val of_list: entry list -> t
end

module Make (H: S.HASH with type Digest.buffer = Cstruct.t
                         and type hex = string)
: S with module Hash = H
= struct
  module Hash = H

  type entry =
    { perm: perm
    ; name: string
    ; node: Hash.t }
  and perm =
    [ `Normal | `Everybody | `Exec | `Link | `Dir | `Commit ]
  and t = entry list
  and hash = Hash.t

  let hashes tree = List.map (fun { node; _ } -> node) tree

  let pp_entry ppf { perm
                   ; name
                   ; node } =
    Fmt.pf ppf "{ @[<hov>perm = %s;@ \
                         name = %s;@ \
                         node = %a;@] }"
      (match perm with
       | `Normal    -> "normal"
       | `Everybody -> "everybody"
       | `Exec      -> "exec"
       | `Link      -> "link"
       | `Dir       -> "dir"
       | `Commit    -> "commit")
      name (Fmt.hvbox Hash.pp) node

  let pp ppf tree =
    Fmt.pf ppf "[ %a ]"
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") pp_entry)) tree

  let string_of_perm = function
    | `Normal    -> "100644"
    | `Everybody -> "100664"
    | `Exec      -> "100755"
    | `Link      -> "120000"
    | `Dir       -> "40000"
    | `Commit    -> "160000"

  let perm_of_string = function
    | "44"
    | "100644" -> `Normal
    | "100664" -> `Everybody
    | "100755" -> `Exec
    | "120000" -> `Link
    | "40000"  -> `Dir
    | "160000" -> `Commit
    | _ -> raise (Invalid_argument "perm_of_string")

  external to_list: t -> entry list = "%identity"

  type value =
    | Contents: string -> value
    | Node    : string -> value

  let str = function Contents s -> s | Node s -> s

  exception Result of int

  let compare x y = match x, y with
    | Contents x, Contents y -> String.compare x y
    | _ ->
      let xs = str x and ys = str y in
      let lenx = String.length xs in
      let leny = String.length ys in

      let i = ref 0 in

      try
        while !i < lenx && !i < leny
        do match Char.compare (String.unsafe_get xs !i) (String.unsafe_get ys !i) with
          | 0 -> incr i
          | i -> raise (Result i)
        done;

        let get len s i =
          if i < len then String.unsafe_get (str s) i
          else if i = len then match s with
            | Node _ -> '/'
            | Contents _ -> '\000'
          else '\000'
        in

        match Char.compare (get lenx x !i) (get leny y !i) with
        | 0 -> Char.compare (get lenx x (!i + 1)) (get leny y (!i + 1))
        | i -> i
      with Result i -> i

  let of_contents c = Contents c
  let of_node n = Node n
  let of_entry = function
    | { name = n; perm = `Dir; _ } -> of_node n
    | { name = n; _ } -> of_contents n

  let of_list entries: t =
    List.map (fun x -> of_entry x, x) entries
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd

  module A =
  struct
    type nonrec t = t

    let is_not_sp chr = chr <> ' '
    let is_not_nl chr = chr <> '\x00'

    let hash = Angstrom.take Hash.Digest.length

    let entry =
      let open Angstrom in

      take_while is_not_sp >>= fun perm ->
      (try return (perm_of_string perm)
       with _ -> fail (Fmt.strf "Invalid permission %s" perm))
      <* commit
      >>= fun perm -> take 1 *> take_while is_not_nl <* commit
      >>= fun name -> take 1 *> hash <* commit
      >>= fun hash ->
      return { perm
             ; name
             ; node = Hash.of_string hash }
      <* commit

    let decoder = Angstrom.many entry
  end

  module F =
  struct
    type nonrec t = t

    let length t =
      let string x = Int64.of_int (String.length x) in
      let ( + ) = Int64.add in

      let entry acc x =
        (string (string_of_perm x.perm))
        + 1L
        + (string x.name)
        + 1L
        + (Int64.of_int Hash.Digest.length)
        + acc
      in
      List.fold_left entry 0L t

    let sp = ' '
    let nl = '\x00'

    let entry e t =
      let open Farfadet in

      eval e [ !!string; char $ sp; !!string; char $ nl; !!string ]
        (string_of_perm t.perm)
        t.name
        (Hash.to_string t.node)

    let encoder e t =
      (Farfadet.list entry) e t
  end

  module M =
  struct
    open Minienc

    type nonrec t = t

    let sp = ' '
    let nl = '\x00'

    let entry x k e =
      (write_string (string_of_perm x.perm)
       @@ write_char sp
       @@ write_string x.name
       @@ write_char nl
       @@ write_string (Hash.to_string x.node) k)
        e

    let encoder x k e =
      let rec list l k e = match l with
        | x :: r ->
          (entry x
           @@ list r k)
            e
        | [] -> k e
      in

      list x k e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)

  let digest value =
    let tmp = Cstruct.create 0x100 in
    Helper.fdigest (module Hash.Digest) (module E) ~tmp ~kind:"tree" ~length:F.length value

  let equal   = (=)
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end
