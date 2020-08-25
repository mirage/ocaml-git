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

type 'hash t =
  | Blob of Blob.t
  | Commit of 'hash Commit.t
  | Tree of 'hash Tree.t
  | Tag of 'hash Tag.t

let src =
  Logs.Src.create "git.value" ~doc:"logs git's internal value computation"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type hash

  type nonrec t = hash t

  module Blob : Blob.S with type hash = hash

  module Commit : Commit.S with type hash = hash

  module Tree : Tree.S with type hash = hash

  module Tag : Tag.S with type hash = hash

  val blob : Blob.t -> t

  val commit : Commit.t -> t

  val tree : Tree.t -> t

  val tag : Tag.t -> t

  val kind : t -> [ `Commit | `Blob | `Tree | `Tag ]

  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64


  val of_raw :
    kind:[ `Commit | `Blob | `Tree | `Tag ] ->
    Cstruct.t ->

end

module Make (Hash : S.HASH) (Inflate : S.INFLATE) (Deflate : S.DEFLATE) :
  S
    with module Hash := Hash
     and module Inflate := Inflate
     and module Deflate := Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash) = struct
  module Blob = Blob.Make (Hash)
  module Commit = Commit.Make (Hash)
  module Tree = Tree.Make (Hash)
  module Tag = Tag.Make (Hash)

  type t = Blob of Blob.t | Commit of Commit.t | Tree of Tree.t | Tag of Tag.t

  let blob blob = Blob blob

  (* blob *)
  let commit commit = Commit commit
  let tree tree = Tree tree
  let tag tag = Tag tag

  let kind = function
    | Commit _ -> `Commit
    | Blob _ -> `Blob
    | Tree _ -> `Tree
    | Tag _ -> `Tag

  let pp_kind ppf = function
    | `Commit -> Fmt.pf ppf "commit"
    | `Tree -> Fmt.pf ppf "tree"
    | `Tag -> Fmt.pf ppf "tag"
    | `Blob -> Fmt.pf ppf "blob"

  let pp ppf = function
    | Blob blob -> Fmt.pf ppf "(Blob %a)" (Fmt.hvbox Blob.pp) blob
    | Commit commit -> Fmt.pf ppf "(Commit %a)" (Fmt.hvbox Commit.pp) commit
    | Tree tree -> Fmt.pf ppf "(Tree %a)" (Fmt.hvbox Tree.pp) tree
    | Tag tag -> Fmt.pf ppf "(Tag %a)" (Fmt.hvbox Tag.pp) tag

  module MakeMeta (Meta : Encore.Meta.S) = struct
    type e = t

    open Helper.BaseIso

    module Iso = struct
      open Encore.Bijection

      let kind =
        make_exn
          ~fwd:(function
            | "tree" -> `Tree
            | "blob" -> `Blob
            | "commit" -> `Commit
            | "tag" -> `Tag
            | _ -> Exn.fail ())
          ~bwd:(function
            | `Tree -> "tree"
            | `Blob -> "blob"
            | `Tag -> "tag"
            | `Commit -> "commit")

      let value =
        make_exn
          ~fwd:(fun (kind, _, value) ->
            match kind, value with
            | `Tree, Tree _ -> value
            | `Commit, Commit _ -> value
            | `Blob, Blob _ -> value
            | `Tag, Tag _ -> value
            | _, _ -> Exn.fail ())
          ~bwd:(function
            | Tree tree -> `Tree, Tree.length tree, Tree tree
            | Commit commit -> `Commit, Commit.length commit, Commit commit
            | Tag tag -> `Tag, Tag.length tag, Tag tag
            | Blob blob -> `Blob, Blob.length blob, Blob blob)
    end

    module Commit = Commit.MakeMeta (Meta)
    module Blob = Blob.MakeMeta (Meta)
    module Tree = Tree.MakeMeta (Meta)
    module Tag = Tag.MakeMeta (Meta)

    type 'a t = 'a Meta.t

    module Meta = Encore.Meta.Make (Meta)
    open Encore.Bijection
    open Meta

    let is_digit = function '0' .. '9' -> true | _ -> false
    let length = int64 <$> while0 is_digit

    let kind =
      Iso.kind
      <$> (const "tree" <|> const "commit" <|> const "blob" <|> const "tag")

    let commit =
      make_exn
        ~fwd:(fun commit -> Commit commit)
        ~bwd:(function Commit commit -> commit | _ -> Exn.fail ())
      <$> Commit.p

    let blob =
      make_exn
        ~fwd:(fun blob -> Blob blob)
        ~bwd:(function Blob blob -> blob | _ -> Exn.fail ())
      <$> Blob.p

    let tree =
      make_exn
        ~fwd:(fun tree -> Tree tree)
        ~bwd:(function Tree tree -> tree | _ -> Exn.fail ())
      <$> Tree.p

    let tag =
      make_exn
        ~fwd:(fun tag -> Tag tag)
        ~bwd:(function Tag tag -> tag | _ -> Exn.fail ())
      <$> Tag.p

    let p =
      let value kind p =
        Iso.kind
        <$> const kind
        <* (char_elt ' ' <$> any)
        <*> (length <* (char_elt '\000' <$> any))
        <*> p
      in
      Exn.compose obj3 Iso.value
      <$> ( value "commit" commit
          <|> value "tree" tree
          <|> value "blob" blob
          <|> value "tag" tag )
  end

  module A = MakeMeta (Encore.Proxy_decoder.Impl)
  module M = MakeMeta (Encore.Proxy_encoder.Impl)

  let length = function
    | Commit commit -> Commit.length commit
    | Tag tag -> Tag.length tag
    | Tree tree -> Tree.length tree
    | Blob blob -> Blob.length blob

  module D = Helper.MakeInflater (Inflate) (A)
  module E = Helper.MakeDeflater (Deflate) (M)

  let digest = function
    | Blob blob -> Blob.digest blob
    | Commit commit -> Commit.digest commit
    | Tree tree -> Tree.digest tree
    | Tag tag -> Tag.digest tag

  let equal = ( = )
  let hash = Hashtbl.hash

  let int_of_kind = function
    | Commit _ -> 0
    | Tree _ -> 1
    | Blob _ -> 2
    | Tag _ -> 3

  let compare a b =
    match a, b with
    | Commit a, Commit b -> Commit.compare a b
    | Blob a, Blob b -> Blob.compare a b
    | Tree a, Tree b -> Tree.compare a b
    | Tag a, Tag b -> Tag.compare a b
    | ( ((Commit _ | Blob _ | Tree _ | Tag _) as a),
        ((Commit _ | Blob _ | Tree _ | Tag _) as b) ) ->
        if int_of_kind a > int_of_kind b then -1
        else if int_of_kind a < int_of_kind b then 1
        else if length a > length b then -1
        else if length a < length b then 1
        else Stdlib.compare a b

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
