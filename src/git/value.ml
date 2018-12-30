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

let src =
  Logs.Src.create "git.value" ~doc:"logs git's internal value computation"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  module Hash : S.HASH
  module Inflate : S.INFLATE
  module Deflate : S.DEFLATE
  module Blob : Blob.S with module Hash := Hash
  module Commit : Commit.S with module Hash := Hash
  module Tree : Tree.S with module Hash := Hash
  module Tag : Tag.S with module Hash := Hash

  type t =
    | Blob of Blob.t
    | Commit of Commit.t
    | Tree of Tree.t
    | Tag of Tag.t

  val blob : Blob.t -> t
  val commit : Commit.t -> t
  val tree : Tree.t -> t
  val tag : Tag.t -> t
  val kind : t -> [`Commit | `Blob | `Tree | `Tag]
  val pp_kind : [`Commit | `Blob | `Tree | `Tag] Fmt.t

  module MakeMeta (Meta : Encore.Meta.S) : sig
    val commit : t Meta.t
    val blob : t Meta.t
    val tree : t Meta.t
    val tag : t Meta.t
    val p : t Meta.t
  end

  module A : sig
    include S.DESC with type 'a t = 'a Angstrom.t and type e = t

    val kind : [`Commit | `Blob | `Tree | `Tag] t
    val length : int64 t
  end

  module M : S.DESC with type 'a t = 'a Encore.Encoder.t and type e = t

  module D :
    S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = [Error.Decoder.t | `Inflate of Inflate.error]

  module E :
    S.ENCODER
    with type t = t
     and type init = Cstruct.t * t * int * Cstruct.t
     and type error = [`Deflate of Deflate.error]

  include S.DIGEST with type t := t and type hash := Hash.t
  include S.BASE with type t := t

  val length : t -> int64
end

module type RAW = sig
  module Hash : S.HASH
  module Inflate : S.INFLATE
  module Deflate : S.DEFLATE

  module Value :
    S
    with module Hash := Hash
     and module Inflate := Inflate
     and module Deflate := Deflate

  include module type of Value

  module EncoderRaw :
    S.ENCODER
    with type t = t
     and type init = Cstruct.t * t
     and type error = Error.never

  module DecoderRaw :
    S.DECODER
    with type t = t
     and type init = Cstruct.t
     and type error = Error.Decoder.t

  module EncoderWithoutHeader :
    S.ENCODER
    with type t = t
     and type init = Cstruct.t * t
     and type error = Error.never

  val to_deflated_raw :
       raw:Cstruct.t
    -> etmp:Cstruct.t
    -> ?level:int
    -> ztmp:Cstruct.t
    -> t
    -> (string, E.error) result

  val to_raw : raw:Cstruct.t -> etmp:Cstruct.t -> t -> (string, EncoderRaw.error) result

  val to_raw_without_header :
    raw:Cstruct.t -> etmp:Cstruct.t -> t -> (string, EncoderWithoutHeader.error) result

  val of_raw :
       kind:[`Commit | `Blob | `Tree | `Tag]
    -> Cstruct.t
    -> (t, Error.Decoder.t) result

  val of_raw_with_header : Cstruct.t -> (t, DecoderRaw.error) result
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

  type t =
    | Blob of Blob.t
    | Commit of Commit.t
    | Tree of Tree.t
    | Tag of Tag.t

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
        let tag = "string", "kind" in
        make_exn ~tag
          ~fwd:(function
            | "tree" -> `Tree
            | "blob" -> `Blob
            | "commit" -> `Commit
            | "tag" -> `Tag
            | s -> Exn.fail s "kind")
          ~bwd:(function
            | `Tree -> "tree"
            | `Blob -> "blob"
            | `Tag -> "tag"
            | `Commit -> "commit")

      let value =
        make_exn
          ~tag:("kind * length * value", "value")
          ~fwd:(fun (kind, _, value) ->
            match kind, value with
            | `Tree, Tree _ -> value
            | `Commit, Commit _ -> value
            | `Blob, Blob _ -> value
            | `Tag, Tag _ -> value
            | _, _ -> Exn.fail "kind" "value" )
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
      make_exn ~tag:("commit", "value")
        ~fwd:(fun commit -> Commit commit)
        ~bwd:(function
          | Commit commit -> commit | _ -> Exn.fail "value" "commit")
      <$> Commit.p

    let blob =
      make_exn ~tag:("blob", "value")
        ~fwd:(fun blob -> Blob blob)
        ~bwd:(function Blob blob -> blob | _ -> Exn.fail "value" "blob")
      <$> Blob.p

    let tree =
      make_exn ~tag:("tree", "value")
        ~fwd:(fun tree -> Tree tree)
        ~bwd:(function Tree tree -> tree | _ -> Exn.fail "value" "tree")
      <$> Tree.p

    let tag =
      make_exn ~tag:("tag", "value")
        ~fwd:(fun tag -> Tag tag)
        ~bwd:(function Tag tag -> tag | _ -> Exn.fail "value" "tag")
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
    | ( ((Commit _ | Blob _ | Tree _ | Tag _) as a)
      , ((Commit _ | Blob _ | Tree _ | Tag _) as b) ) ->
        if int_of_kind a > int_of_kind b then -1
        else if int_of_kind a < int_of_kind b then 1
        else if length a > length b then -1
        else if length a < length b then 1
        else Pervasives.compare a b

  module Set = Set.Make (struct type nonrec t = t

                                let compare = compare end)

  module Map = Map.Make (struct type nonrec t = t

                                let compare = compare end)
end

module Raw (Hash : S.HASH) (Inflate : S.INFLATE) (Deflate : S.DEFLATE) = struct
  module Value = Make (Hash) (Inflate) (Deflate)
  include Value
  module DecoderRaw = Helper.MakeDecoder (A)
  module EncoderRaw = Helper.MakeEncoder (M)

  module MakeWithoutHeader (Meta : Encore.Meta.S) = struct
    type e = t
    type 'a t = 'a Meta.t

    module Meta = Encore.Meta.Make (Meta)
    module ValueMeta = MakeMeta (Meta)
    open Meta
    open ValueMeta

    let p = tag <|> commit <|> tree <|> blob
  end

  module DecoderWithoutHeader =
    Helper.MakeDecoder (MakeWithoutHeader (Encore.Proxy_decoder.Impl))

  module EncoderWithoutHeader =
    Helper.MakeEncoder (MakeWithoutHeader (Encore.Proxy_encoder.Impl))

  module type ENCODER = sig
    type state
    type raw
    type result
    type error

    val raw_length : raw -> int
    val raw_sub : raw -> int -> int -> raw

    val eval :
         raw
      -> state
      -> [`Flush of state | `End of state * result | `Error of state * error]

    val used : state -> int
    val flush : int -> int -> state -> state
  end

  type ('state, 'raw, 'result, 'error) encoder =
    (module
     ENCODER
       with type state = 'state
        and type raw = 'raw
        and type result = 'result
        and type error = 'error)

  let to_ (type state res err_encoder)
      (encoder : (state, Cstruct.t, res, err_encoder) encoder)
      (buffer : Cstruct_buffer.t) (raw : Cstruct.t) (state : state) :
      (string, err_encoder) result =
    let module E = ( val encoder
                       : ENCODER
                       with type state = state
                        and type raw = Cstruct.t
                        and type result = res
                        and type error = err_encoder )
    in
    let rec go state =
      match E.eval raw state with
      | `Error (_, err) -> Error err
      | `End (state, _) ->
          if E.used state > 0 then
            Cstruct_buffer.add buffer (E.raw_sub raw 0 (E.used state)) ;
          Ok (Cstruct_buffer.contents buffer)
      | `Flush state ->
          if E.used state > 0 then
            Cstruct_buffer.add buffer (E.raw_sub raw 0 (E.used state)) ;
          go (E.flush 0 (E.raw_length raw) state)
    in
    go state

  let to_deflated_raw ~raw ~etmp ?(level = 4) ~ztmp value =
    let encoder = E.default (etmp, value, level, ztmp) in
    let buffer = Cstruct_buffer.create (Int64.to_int (length value)) in
    (* XXX(dinosaure): it's an heuristic to consider than the size of the
       result is lower than [F.length value]. In most of cases, it's true but
       sometimes, a deflated Git object can be bigger than a serialized Git
       object. *)
    let module SpecializedEncoder = struct
      type state = E.encoder
      type raw = Cstruct.t
      type result = int
      type error = E.error

      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub

      type rest = [`Flush of state | `End of state * result]

      let eval raw state =
        match E.eval raw state with
        | #rest as rest -> rest
        | `Error err -> `Error (state, err)

      let used = E.used
      let flush = E.flush
    end in
    to_ (module SpecializedEncoder) buffer raw encoder

  let to_raw ~raw ~etmp value =
    let encoder = EncoderRaw.default (etmp, value) in
    let buffer = Cstruct_buffer.create (Int64.to_int (length value)) in
    (* XXX(dinosaure): we are sure than the serialized object has the size
       [F.length value]. So, the [buffer] should not growth. *)
    let module SpecializedEncoder = struct
      type state = EncoderRaw.encoder
      type raw = Cstruct.t
      type result = int
      type error = EncoderRaw.error

      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub

      type rest = [`Flush of state | `End of state * result]

      let eval raw state =
        match EncoderRaw.eval raw state with
        | #rest as rest -> rest
        | `Error err -> `Error (state, err)

      let used = EncoderRaw.used
      let flush = EncoderRaw.flush
    end in
    to_ (module SpecializedEncoder) buffer raw encoder

  let to_raw_without_header ~raw ~etmp value =
    let encoder = EncoderWithoutHeader.default (etmp, value) in
    let buffer = Cstruct_buffer.create (Int64.to_int (length value)) in
    (* XXX(dinosaure): we are sure than the serialized object has the size
       [F.length value]. So, the [buffer] should not growth. *)
    let module SpecializedEncoder = struct
      type state = EncoderWithoutHeader.encoder
      type raw = Cstruct.t
      type result = int
      type error = EncoderWithoutHeader.error

      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub

      type rest = [`Flush of state | `End of state * result]

      let eval raw state =
        match EncoderWithoutHeader.eval raw state with
        | #rest as rest -> rest
        | `Error err -> `Error (state, err)

      let used = EncoderWithoutHeader.used
      let flush = EncoderWithoutHeader.flush
    end in
    to_ (module SpecializedEncoder) buffer raw encoder

  let of_raw_with_header inflated = DecoderRaw.to_result inflated

  let of_raw ~kind inflated =
    match kind with
    | `Commit ->
        Rresult.R.map
          (fun commit -> Commit commit)
          (Value.Commit.D.to_result inflated)
    | `Tree ->
        Rresult.R.map (fun tree -> Tree tree) (Value.Tree.D.to_result inflated)
    | `Tag ->
        Rresult.R.map (fun tag -> Tag tag) (Value.Tag.D.to_result inflated)
    | `Blob ->
        let blob blob = Blob blob in
        Rresult.R.(get_ok (Value.Blob.D.to_result inflated) |> blob |> ok)
end
