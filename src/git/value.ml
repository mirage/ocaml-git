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
  module Hash
    : S.HASH
  module Inflate
    : S.INFLATE
  module Deflate
    : S.DEFLATE

  module Blob
    : Blob.S   with type Hash.t = Hash.t
  module Commit
    : Commit.S with type Hash.t = Hash.t
  module Tree
    : Tree.S   with type Hash.t = Hash.t
  module Tag
    : Tag.S    with type Hash.t = Hash.t

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tree   of Tree.t
    | Tag    of Tag.t

  module A : sig
    include S.ANGSTROM with type t = t

    val kind : [ `Commit | `Tree | `Tag | `Blob ] Angstrom.t
  end

  module F : S.FARADAY  with type t = t
  module D : S.DECODER  with type t = t
                         and type raw = Cstruct.t
                         and type init = Inflate.window * Cstruct.t * Cstruct.t
                         and type error = [ `Decoder of string | `Inflate of Inflate.error ]
  module M : S.MINIENC  with type t = t
  module E : S.ENCODER  with type t = t
                         and type raw = Cstruct.t
                         and type init = int * t * int * Cstruct.t
                         and type error = [ `Deflate of Deflate.error ]

  include S.DIGEST with type t := t and type hash := Hash.t
  include S.BASE with type t := t
end

module type BUFFER =
sig
  type t
  type raw
  type fixe

  val create : int -> t
  val contents : t -> raw
  val add : t -> fixe -> unit
  val clear : t -> unit
  val reset : t -> unit
end

module type RAW =
sig
  module Buffer
    : BUFFER

  include S

  module EE
    : S.ENCODER
      with type t = t
       and type raw = Cstruct.t
       and type init = int * t
       and type error = [ `Never ]

  module DD
    : S.DECODER
      with type t = t
       and type raw = Cstruct.t
       and type init = Cstruct.t
       and type error = [ `Decoder of string ]

  val to_deflated_raw : ?capacity:int -> ?level:int -> ztmp:Cstruct.t -> t -> (Buffer.raw, E.error) result
  val to_raw : ?capacity:int -> t -> (Buffer.raw, EE.error) result
  val of_raw : kind:[ `Commit | `Tree | `Tag | `Blob ] -> Cstruct.t -> (t, [ `Decoder of string ]) result
  val of_raw_with_header : Cstruct.t -> (t, DD.error) result
end

module Make
    (H : S.HASH with type Digest.buffer = Cstruct.t
                 and type hex = string)
    (I : S.INFLATE)
    (D : S.DEFLATE)
  : S with module Hash = H
       and module Inflate = I
       and module Deflate = D
= struct
  module Hash = H
  module Inflate = I
  module Deflate = D

  module Blob   = Blob.Make(H)
  module Commit = Commit.Make(H)
  module Tree   = Tree.Make(H)
  module Tag    = Tag.Make(H)

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tree   of Tree.t
    | Tag    of Tag.t

  let pp ppf = function
    | Blob blob     -> Fmt.pf ppf "(Blob %a)" (Fmt.hvbox Blob.pp) blob
    | Commit commit -> Fmt.pf ppf "(Commit %a)" (Fmt.hvbox Commit.pp) commit
    | Tree tree     -> Fmt.pf ppf "(Tree %a)" (Fmt.hvbox Tree.pp) tree
    | Tag tag       -> Fmt.pf ppf "(Tag %a)" (Fmt.hvbox Tag.pp) tag

  module A =
  struct
    type nonrec t = t

    let kind =
      let open Angstrom in
      (((string "blob" *> return `Blob) <?> "blob")
       <|> ((string "commit" *> return `Commit) <?> "commit")
       <|> ((string "tag" *> return `Tag) <?> "tag")
       <|> ((string "tree" *> return `Tree) <?> "tree"))
      <* commit

    let int64 =
      let open Angstrom in
      take_while (function '0' .. '9' -> true | _ -> false)
      >>| Int64.of_string
      <* commit

    let decoder =
      let open Angstrom in
      kind <* take 1
      >>= fun kind -> int64 <* advance 1
      >>= fun length -> match kind with
      | `Commit -> Commit.A.decoder >>| fun commit -> Commit commit
      | `Blob   -> Blob.A.decoder   >>| fun blob -> Blob blob
      | `Tree   -> Tree.A.decoder   >>| fun tree -> Tree tree
      | `Tag    -> Tag.A.decoder    >>| fun tag -> Tag tag
  end

  module F =
  struct
    type nonrec t = t

    let length = function
      | Commit commit -> Commit.F.length commit
      | Tag tag       -> Tag.F.length tag
      | Tree tree     -> Tree.F.length tree
      | Blob blob     -> Blob.F.length blob

    let string_of_value = function
      | Commit _ -> "commit"
      | Blob _   -> "blob"
      | Tree _   -> "tree"
      | Tag _    -> "tag"

    let int64 e x = Farfadet.string e (Int64.to_string x)

    let sp = ' '
    let nl = '\000'

    let encoder e x =
      let open Farfadet in

      eval e [ !!string; char $ sp; !!int64; char $ nl ]
        (string_of_value x) (length x);

      match x with
      | Commit commit -> Commit.F.encoder e commit
      | Blob blob     -> Blob.F.encoder e blob
      | Tag tag       -> Tag.F.encoder e tag
      | Tree tree     -> Tree.F.encoder e tree
  end

  module M =
  struct
    type nonrec t = t

    open Minienc

    let length = function
      | Blob blob -> Blob.F.length blob
      | Tree tree -> Tree.F.length tree
      | Tag tag -> Tag.F.length tag
      | Commit commit -> Commit.F.length commit

    let string_of_value = function
      | Commit _ -> "commit"
      | Blob _   -> "blob"
      | Tree _   -> "tree"
      | Tag _    -> "tag"

    let sp = ' '
    let nl = '\000'

    let encoder x k e =
      (write_string (string_of_value x)
       @@ write_char sp
       @@ write_string (Int64.to_string (length x))
       @@ write_char nl
       @@ (match x with
           | Tree tree -> Tree.M.encoder tree
           | Tag tag -> Tag.M.encoder tag
           | Blob blob -> write_bigstring (Cstruct.to_bigarray (blob :> Cstruct.t))
           | Commit commit -> Commit.M.encoder commit) k)
      e
  end

  module D = Helper.MakeInflater(Inflate)(A)
  module E = Helper.MakeDeflater(Deflate)(M)

  let digest = function
    | Blob blob     -> Blob.digest blob
    | Commit commit -> Commit.digest commit
    | Tree tree     -> Tree.digest tree
    | Tag tag       -> Tag.digest tag

  let equal   = (=)
  let hash    = Hashtbl.hash

  let int_of_kind = function
    | Commit _ -> 0
    | Tree _ -> 1
    | Blob _ -> 2
    | Tag _ -> 3

  let compare a b = match a, b with
    | Commit a, Commit b -> Commit.compare a b
    | Blob a, Blob b -> Blob.compare a b
    | Tree a, Tree b -> Tree.compare a b
    | Tag a, Tag b -> Tag.compare a b
    | a, b ->
      if int_of_kind a > int_of_kind b
      then (-1)
      else if int_of_kind a < int_of_kind b
      then 1
      else if F.length a > F.length b
      then (-1)
      else if F.length a < F.length b
      then 1
      else Pervasives.compare a b

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

module Raw
    (H : S.HASH with type Digest.buffer = Cstruct.t
                 and type hex = string)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    (B : BUFFER with type raw = string
                 and type fixe = Cstruct.t)
    : RAW with module Hash = H
           and module Inflate = I
           and module Deflate = D
           and module Buffer = B
= struct
  module Buffer = B

  module Value : S with module Hash = H
                    and module Inflate = I
                    and module Deflate = D
    = Make(H)(I)(D)

  include Value

  module EE = Helper.MakeEncoder(M)
  (* XXX(dinosaure): the [Value] module expose only an encoder to a deflated
     value. We provide an encoder for a serialized encoder. *)

  module type ENCODER =
  sig
    type state
    type raw
    type result
    type error

    val raw_length : raw -> int
    val raw_sub : raw -> int -> int -> raw

    val eval : raw -> state -> [ `Flush of state
                               | `End of (state * result)
                               | `Error of (state * error) ]
    val used : state -> int
    val flush : int -> int -> state -> state
  end
  (* XXX(dinosaure): this is close to [Helper.Encoder] used to save a value to a
     file but without Lwt. *)

  type ('state, 'raw, 'result, 'error) encoder =
    (module ENCODER with type state = 'state
                     and type raw = 'raw
                     and type result = 'result
                     and type error = 'error)

  let to_ (type state) (type res) (type err_encoder)
      (encoder : (state, Buffer.fixe, res, err_encoder) encoder)
      (buffer : Buffer.t)
      (raw : Buffer.fixe)
      (state : state) : (Buffer.raw, err_encoder) result
    =
    let module E =
      (val encoder : ENCODER with type state = state
                              and type raw = Buffer.fixe
                              and type result = res
                              and type error = err_encoder)
    in

    let rec go state = match E.eval raw state with
      | `Error (state, err) -> Error err
      | `End (state, res) ->
        if E.used state > 0
        then Buffer.add buffer (E.raw_sub raw 0 (E.used state));

        Ok (Buffer.contents buffer)
      | `Flush state ->
        if E.used state > 0
        then Buffer.add buffer (E.raw_sub raw 0 (E.used state));

        go (E.flush 0 (E.raw_length raw) state)
    in

    go state

  let to_deflated_raw ?(capacity = 0x100) ?(level = 4) ~ztmp value =
    let encoder = E.default (capacity, value, level, ztmp) in
    let raw = Cstruct.create capacity in
    let buffer = Buffer.create (Int64.to_int (F.length value)) in
    (* XXX(dinosaure): it's an heuristic to consider than the size of the result
       is lower than [F.length value]. In most of cases, it's true but sometimes, a
       deflated Git object can be bigger than a serialized Git object. *)

    let module SpecializedEncoder =
    struct
      type state = E.encoder
      type raw = Cstruct.t
      type result = int
      type error = E.error

      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub

      type rest = [ `Flush of state | `End of (state * result) ]

      let eval raw state = match E.eval raw state with
        | #rest as rest -> rest
        | `Error err -> `Error (state, err)

      let used = E.used
      let flush = E.flush
    end in

    to_ (module SpecializedEncoder) buffer raw encoder

  let to_raw ?(capacity = 0x100) value =
    let encoder = EE.default (capacity, value) in
    let raw = Cstruct.create capacity in
    let buffer = Buffer.create (Int64.to_int (F.length value)) in
    (* XXX(dinosaure): we are sure than the serialized object has the size
       [F.length value]. So, the [buffer] should not growth. *)

    let module SpecializedEncoder =
    struct
      type state = EE.encoder
      type raw = Cstruct.t
      type result = int
      type error = EE.error

      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub

      type rest = [ `Flush of state | `End of (state * result) ]

      let eval raw state = match EE.eval raw state with
        | #rest as rest -> rest
        | `Error err -> `Error (state, err)

      let used = EE.used
      let flush = EE.flush
    end in

    to_ (module SpecializedEncoder) buffer raw encoder

  module DD = Helper.MakeDecoder(A)

  let of_raw_with_header inflated = DD.to_result inflated
  let of_raw ~kind inflated = match kind with
    | `Commit ->
      Value.Commit.D.to_result inflated |> (function
          | Ok commit -> Ok (Commit commit)
          | Error (`Decoder err) -> Error (`Decoder err))
    | `Tree ->
      Value.Tree.D.to_result inflated |> (function
          | Ok tree -> Ok (Tree tree)
          | Error (`Decoder err) -> Error (`Decoder err))
    | `Tag ->
      Value.Tag.D.to_result inflated  |> (function
          | Ok tag -> Ok (Tag tag)
          | Error (`Decoder err) -> Error (`Decoder err))
    | `Blob ->
      Value.Blob.D.to_result inflated  |> (function
          | Ok blob -> Ok (Blob blob)
          | Error (`Decoder err) -> Error (`Decoder err))
end

