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

  type t
  type kind = Blob | Commit | Tag | Tree

  val make : Hash.t -> kind -> ?tagger:User.t -> tag:string -> string -> t

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
     and type init = int * t
     and type error = Error.never

  include S.DIGEST with type t := t and type hash := Hash.t
  include S.BASE with type t := t

  val length : t -> int64
  val obj : t -> Hash.t
  val tag : t -> string
  val message : t -> string
  val kind : t -> kind
  val tagger : t -> User.t option
end

module Make (Hash : S.HASH) = struct
  type t =
    { obj: Hash.t
    ; kind: kind
    ; tag: string
    ; tagger: User.t option
    ; message: string }

  and kind = Blob | Commit | Tag | Tree

  let make target kind ?tagger ~tag message =
    {obj= target; kind; tag; tagger; message}

  let pp_kind ppf = function
    | Blob -> Fmt.string ppf "Blob"
    | Commit -> Fmt.string ppf "Commit"
    | Tag -> Fmt.string ppf "Tag"
    | Tree -> Fmt.string ppf "Tree"

  let pp ppf {obj; kind; tag; tagger; message} =
    Fmt.pf ppf
      "{ @[<hov>obj = %a;@ kind = %a;@ tag = %s;@ tagger = %a;@ message = \
       %a@] }"
      Hash.pp obj pp_kind kind tag
      (Fmt.hvbox (Fmt.option User.pp))
      tagger (Fmt.hvbox Fmt.text) message

  let string_of_kind = function
    | Commit -> "commit"
    | Tag -> "tag"
    | Tree -> "tree"
    | Blob -> "blob"

  module MakeMeta (Meta : Encore.Meta.S) = struct
    type e = t

    open Helper.BaseIso

    module Iso = struct
      open Encore.Bijection

      let hex =
        let tag = "string", "hex" in
        make_exn ~tag
          ~fwd:(Exn.safe_exn tag Hash.of_hex)
          ~bwd:(Exn.safe_exn (Helper.Pair.flip tag) Hash.to_hex)

      let user =
        make_exn ~tag:("string", "user")
          ~fwd:(fun s ->
            match Angstrom.parse_string User.A.p s with
            | Ok v -> v
            | Error _ -> Exn.fail "string" "user" )
          ~bwd:(Encore.Encoder.to_string User.M.p)

      let kind =
        let tag = "string", "kind" in
        make_exn ~tag
          ~fwd:(function
            | "tree" -> Tree
            | "blob" -> Blob
            | "commit" -> Commit
            | "tag" -> Tag
            | s -> Exn.fail s "kind")
          ~bwd:(function
            | Blob -> "blob"
            | Tree -> "tree"
            | Commit -> "commit"
            | Tag -> "tag")

      let tag =
        make_exn
          ~tag:("hash * kind * string * user * string", "tag")
          ~fwd:(fun ((_, obj), (_, kind), (_, tag), tagger, message) ->
            { obj
            ; kind
            ; tag
            ; tagger= Helper.Option.(tagger >>= Helper.Pair.snd)
            ; message } )
          ~bwd:(fun {obj; kind; tag; tagger; message} ->
            let tagger = Helper.Option.(tagger >>= fun x -> "tagger", x) in
            ("object", obj), ("type", kind), ("tag", tag), tagger, message )
    end

    type 'a t = 'a Meta.t

    module Meta = Encore.Meta.Make (Meta)
    open Encore.Bijection
    open Encore.Either
    open Meta

    let is_not_sp chr = chr <> ' '
    let is_not_lf chr = chr <> '\x0a'

    let to_end =
      let loop m =
        let cons = Exn.cons ~tag:"cons" <$> (buffer <* commit <*> m) in
        let nil = pure ~compare:(fun () () -> 0) () in
        make_exn ~tag:("either", "list")
          ~fwd:(function L cons -> cons | R () -> [])
          ~bwd:(function _ :: _ as lst -> L lst | [] -> R ())
        <$> peek cons nil
      in
      fix loop

    let to_end : string t =
      make_exn ~tag:("string list", "string") ~fwd:(String.concat "")
        ~bwd:(fun x -> [x] )
      <$> to_end

    let binding ?key value =
      let value = value <$> (while1 is_not_lf <* (char_elt '\x0a' <$> any)) in
      match key with
      | Some key -> const key <* (char_elt ' ' <$> any) <*> value
      | None -> while1 is_not_sp <* (char_elt ' ' <$> any) <*> value

    let tag =
      binding ~key:"object" Iso.hex
      <*> binding ~key:"type" Iso.kind
      <*> binding ~key:"tag" Exn.identity
      <*> option (binding ~key:"tagger" Iso.user)
      <*> to_end

    let p = Exn.compose obj5 Iso.tag <$> tag
  end

  module A = MakeMeta (Encore.Proxy_decoder.Impl)
  module M = MakeMeta (Encore.Proxy_encoder.Impl)
  module D = Helper.MakeDecoder (A)
  module E = Helper.MakeEncoder (M)

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in
    let user_length =
      match t.tagger with
      | Some user -> string "tagger" + 1L + User.length user + 1L
      | None -> 0L
    in
    string "object"
    + 1L
    + Int64.of_int (Hash.digest_size * 2)
    + 1L
    + string "type"
    + 1L
    + string (string_of_kind t.kind)
    + 1L
    + string "tag"
    + 1L
    + string t.tag
    + 1L
    + user_length
    + string t.message

  let obj {obj; _} = obj
  let tag {tag; _} = tag
  let message {message; _} = message
  let kind {kind; _} = kind
  let tagger {tagger; _} = tagger

  let digest value =
    let tmp = Cstruct.create 0x100 in
    Helper.fdigest (module Hash) (module E) ~tmp ~kind:"tag" ~length value

  let equal = ( = )
  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  module Set = Set.Make (struct type nonrec t = t

                                let compare = compare end)

  module Map = Map.Make (struct type nonrec t = t

                                let compare = compare end)
end
