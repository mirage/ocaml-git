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

  val to_raw : t -> string

  val to_raw_without_header : t -> string

  val of_raw :
    kind:[ `Commit | `Blob | `Tree | `Tag ] ->
    Cstruct.t ->
    (t, [> `Msg of string ]) result

  val stream : t -> unit -> string option Lwt.t
end

module Make (Hash : S.HASH) : S with type hash = Hash.t = struct
  type hash = Hash.t

  type nonrec t = Hash.t t

  module Blob = Blob.Make (Hash)
  module Commit = Commit.Make (Hash)
  module Tree = Tree.Make (Hash)
  module Tag = Tag.Make (Hash)

  let blob blob = Blob blob

  let commit commit = Commit commit

  let tree tree = Tree tree

  let tag tag = Tag tag

  let kind = function
    | Commit _ -> `Commit
    | Blob _ -> `Blob
    | Tree _ -> `Tree
    | Tag _ -> `Tag

  let pp ppf = function
    | Blob blob -> Fmt.pf ppf "(Blob %a)" (Fmt.hvbox Blob.pp) blob
    | Commit commit -> Fmt.pf ppf "(Commit %a)" (Fmt.hvbox Commit.pp) commit
    | Tree tree -> Fmt.pf ppf "(Tree %a)" (Fmt.hvbox Tree.pp) tree
    | Tag tag -> Fmt.pf ppf "(Tag %a)" (Fmt.hvbox Tag.pp) tag

  module Syntax = struct
    let safe_exn f x = try f x with _ -> raise Encore.Bij.Bijection

    let kind =
      Encore.Bij.v
        ~fwd:(function
          | "tree" -> `Tree
          | "blob" -> `Blob
          | "commit" -> `Commit
          | "tag" -> `Tag
          | _ -> raise Encore.Bij.Bijection)
        ~bwd:(function
          | `Tree -> "tree"
          | `Blob -> "blob"
          | `Tag -> "tag"
          | `Commit -> "commit")

    let iso =
      Encore.Bij.v
        ~fwd:(fun (kind, _, value) ->
          match (kind, value) with
          | `Tree, Tree _ -> value
          | `Commit, Commit _ -> value
          | `Blob, Blob _ -> value
          | `Tag, Tag _ -> value
          | _, _ -> raise Encore.Bij.Bijection)
        ~bwd:(function
          | Tree tree -> (`Tree, Tree.length tree, Tree tree)
          | Commit commit -> (`Commit, Commit.length commit, Commit commit)
          | Tag tag -> (`Tag, Tag.length tag, Tag tag)
          | Blob blob -> (`Blob, Blob.length blob, Blob blob))

    let is_digit = function '0' .. '9' -> true | _ -> false

    let int64 =
      Encore.Bij.v ~fwd:(safe_exn Int64.of_string)
        ~bwd:(safe_exn Int64.to_string)

    let length =
      let open Encore.Syntax in
      int64 <$> while0 is_digit

    let always x _ = x

    let commit' =
      let open Encore.Syntax in
      Encore.Bij.v
        ~fwd:(fun commit -> Commit commit)
        ~bwd:(function
          | Commit commit -> commit | _ -> raise Encore.Bij.Bijection)
      <$> Commit.format

    let blob =
      let open Encore.Syntax in
      Encore.Bij.v
        ~fwd:(fun blob -> Blob (Blob.of_cstruct (Cstruct.of_string blob)))
        ~bwd:(function
          | Blob blob -> Cstruct.to_string (Blob.to_cstruct blob)
          | _ -> raise Encore.Bij.Bijection)
      <$> while0 (always true)

    let tree =
      let open Encore.Syntax in
      Encore.Bij.v
        ~fwd:(fun tree -> Tree tree)
        ~bwd:(function Tree tree -> tree | _ -> raise Encore.Bij.Bijection)
      <$> Tree.format

    let tag =
      let open Encore.Syntax in
      Encore.Bij.v
        ~fwd:(fun tag -> Tag tag)
        ~bwd:(function Tag tag -> tag | _ -> raise Encore.Bij.Bijection)
      <$> Tag.format

    let format =
      let open Encore.Syntax in
      let value k t =
        kind
        <$> const k
        <* (Encore.Bij.char ' ' <$> any)
        <*> (length <* (Encore.Bij.char '\000' <$> any))
        <*> t in
      Encore.Bij.(compose obj3) iso
      <$> (value "commit" commit'
          <|> value "tree" tree
          <|> value "blob" blob
          <|> value "tag" tag)
  end

  let format = Syntax.format

  let length = function
    | Commit commit -> Commit.length commit
    | Tag tag -> Tag.length tag
    | Tree tree -> Tree.length tree
    | Blob blob -> Blob.length blob

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
    match (a, b) with
    | Commit a, Commit b -> Commit.compare a b
    | Blob a, Blob b -> Blob.compare a b
    | Tree a, Tree b -> Tree.compare a b
    | Tag a, Tag b -> Tag.compare a b
    | ( ((Commit _ | Blob _ | Tree _ | Tag _) as a),
        ((Commit _ | Blob _ | Tree _ | Tag _) as b) ) ->
        if int_of_kind a > int_of_kind b
        then -1
        else if int_of_kind a < int_of_kind b
        then 1
        else if length a > length b
        then -1
        else if length a < length b
        then 1
        else Stdlib.compare a b

  let to_raw v =
    let chunk = Int64.to_int (length v) in
    Encore.Lavoisier.emit_string ~chunk v (Encore.to_lavoisier format)

  type with_parser = [ `Commit | `Tree | `Tag ]

  let of_raw ~kind raw =
    match kind with
    | `Blob -> Ok (Blob (Blob.of_cstruct raw))
    | #with_parser as kind -> (
        let parser =
          let open Angstrom in
          match kind with
          | `Commit -> Encore.to_angstrom Commit.format >>| fun v -> Commit v
          | `Tag -> Encore.to_angstrom Tag.format >>| fun v -> Tag v
          | `Tree -> Encore.to_angstrom Tree.format >>| fun v -> Tree v in
        match
          Angstrom.parse_bigstring ~consume:Angstrom.Consume.All parser
            (Cstruct.to_bigarray raw)
        with
        | Ok v -> Ok v
        | Error _ ->
            Log.err (fun m ->
                m "Object %s is bad: @[<hov>%S@]"
                  (match kind with
                  | `Tree -> "tree"
                  | `Commit -> "commit"
                  | `Tag -> "tag")
                  (Cstruct.to_string raw)) ;
            Error (`Msg "Invalid Git object"))

  let to_raw_without_header = function
    | Blob v -> Cstruct.to_string (Blob.to_cstruct v)
    | Commit v ->
        let chunk = Int64.to_int (Commit.length v) in
        Encore.Lavoisier.emit_string ~chunk v
          (Encore.to_lavoisier Commit.format)
    | Tag v ->
        let chunk = Int64.to_int (Tag.length v) in
        Encore.Lavoisier.emit_string ~chunk v (Encore.to_lavoisier Tag.format)
    | Tree v ->
        let chunk = Int64.to_int (Tree.length v) in
        Encore.Lavoisier.emit_string ~chunk v (Encore.to_lavoisier Tree.format)

  let stream = function
    | Blob v ->
        let consumed = ref false in
        let stream () =
          if !consumed
          then Lwt.return_none
          else (
            consumed := true ;
            Lwt.return_some (Cstruct.to_string (Blob.to_cstruct v))) in
        stream
    | v ->
        let state =
          match v with
          | Commit v ->
              Encore.Lavoisier.emit v (Encore.to_lavoisier Commit.format)
          | Tree v -> Encore.Lavoisier.emit v (Encore.to_lavoisier Tree.format)
          | Tag v -> Encore.Lavoisier.emit v (Encore.to_lavoisier Tag.format)
          | Blob _ -> assert false in
        let state = ref state in
        let stream () =
          match !state with
          | Encore.Lavoisier.Partial { buffer; off; len; continue } ->
              let str = Bigstringaf.substring buffer ~off ~len in
              state := continue ~committed:len ;
              Lwt.return_some str
          | Encore.Lavoisier.Done -> Lwt.return_none
          | Encore.Lavoisier.Fail -> Lwt.fail (Failure "Value.stream") in
        stream

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
