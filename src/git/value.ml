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
  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module Blob: Blob.S   with module Hash = Hash
  module Commit: Commit.S with module Hash = Hash
  module Tree: Tree.S   with module Hash = Hash
  module Tag: Tag.S    with module Hash = Hash

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tree   of Tree.t
    | Tag    of Tag.t

  val blob   : Blob.t -> t
  val commit : Commit.t -> t
  val tree   : Tree.t -> t
  val tag    : Tag.t -> t
  val kind   : t -> [ `Commit | `Blob | `Tree | `Tag ]

  val pp_kind : [ `Commit | `Blob | `Tree | `Tag ] Fmt.t

  module A : sig
    include S.ANGSTROM with type t = t

    val kind : [ `Commit | `Tree | `Tag | `Blob ] Angstrom.t
    val length : int64 Angstrom.t
  end

  module F: S.FARADAY with type t = t
  module D: S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = [ Error.Decoder.t0
                      | `Inflate of Inflate.error ]
  module M: S.MINIENC with type t = t
  module E: S.ENCODER
    with type t = t
     and type init = int * t * int * Cstruct.t
     and type error = [ `Deflate of Deflate.error ]

  include S.DIGEST with type t := t and type hash := Hash.t
  include S.BASE with type t := t
end

module type RAW = sig
  module Value : S
  include module type of Value

  module EE: S.ENCODER
    with type t = t
     and type init = int * t
     and type error = Error.never

  module EEE: S.ENCODER
    with type t = t
     and type init = int * t
     and type error = Error.never

  module DD: S.DECODER
    with type t = t
     and type init = Cstruct.t
     and type error = Error.Decoder.t0

  val to_deflated_raw : ?capacity:int -> ?level:int -> ztmp:Cstruct.t -> t -> (string, E.error) result
  val to_raw : ?capacity:int -> t -> (string, EE.error) result
  val to_raw_without_header : ?capacity:int -> t -> (string, EEE.error) result
  val of_raw : kind:[ `Commit | `Tree | `Tag | `Blob ] -> Cstruct.t -> (t, Error.Decoder.t0) result
  val of_raw_with_header : Cstruct.t -> (t, DD.error) result
end

module Make (H : S.HASH) (I : S.INFLATE) (D : S.DEFLATE)
  : S with module Hash    = H
       and module Inflate = I
       and module Deflate = D
       and module Blob    = Blob.Make(H)
       and module Commit  = Commit.Make(H)
       and module Tree    = Tree.Make(H)
       and module Tag     = Tag.Make(H)
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

  let blob blob = Blob blob (* blob *)
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
    | Blob blob     -> Fmt.pf ppf "(Blob %a)" (Fmt.hvbox Blob.pp) blob
    | Commit commit -> Fmt.pf ppf "(Commit %a)" (Fmt.hvbox Commit.pp) commit
    | Tree tree     -> Fmt.pf ppf "(Tree %a)" (Fmt.hvbox Tree.pp) tree
    | Tag tag       -> Fmt.pf ppf "(Tag %a)" (Fmt.hvbox Tag.pp) tag

  let src = Logs.Src.create "git.value" ~doc:"logs git's internal value computation"
  module Log = (val Logs.src_log src : Logs.LOG)

  module A =
  struct
    type nonrec t = t

    let kind =
      let open Angstrom in
      (((string "blob" *> return `Blob) <?> "blob")
       <|> ((string "commit" *> return `Commit) <?> "commit")
       <|> ((string "tag" *> return `Tag) <?> "tag")
       <|> ((string "tree" *> return `Tree) <?> "tree"))
      <?> "kind" <* commit >>= fun kind ->
      Log.debug (fun l -> l "Kind of the current object: %s."
                    (match kind with
                     | `Commit -> "commit"
                     | `Blob -> "blob"
                     | `Tag -> "tag"
                     | `Tree -> "tree"));
      return kind

    let int64 =
      let open Angstrom in
      take_while (function '0' .. '9' -> true | _ -> false)
      >>| Int64.of_string
      <?> "size" <* commit >>= fun size ->
      Log.debug (fun l -> l "Length of the current object: %Ld." size);
      return size

    let length = int64

    let decoder =
      let open Angstrom in
      kind <* take 1
      >>= fun kind -> int64 <* take 1
      >>= fun length -> match kind with
      | `Commit -> Commit.A.decoder >>| fun commit -> Commit commit
      | `Blob   -> Blob.A.decoder (Int64.to_int length) >>| fun blob -> Blob blob
          (* XXX(dinosaure): need to take care about this cast. TODO! *)
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

  module M = struct
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
    | ((Commit _ | Blob _ | Tree _ | Tag _) as a),
      ((Commit _ | Blob _ | Tree _ | Tag _) as b) ->
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
    (H : S.HASH)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    : RAW with module Hash = H
           and module Inflate = I
           and module Deflate = D
           and module Value = Make(H)(I)(D)
           and module Blob = Blob.Make(H)
           and module Commit = Commit.Make(H)
           and module Tree = Tree.Make(H)
           and module Tag = Tag.Make(H)
           and type t = Make(H)(I)(D).t
= struct
  module Value = Make(H)(I)(D)

  include Value

  module EE = Helper.MakeEncoder(M)
  (* XXX(dinosaure): the [Value] module expose only an encoder to a deflated
     value. We provide an encoder for a serialized encoder. *)

  module MM =
  struct
    type nonrec t = t

    open Minienc

    let encoder x k e =
      ((match x with
          | Tree tree ->
            Tree.M.encoder tree
          | Tag tag ->
            Tag.M.encoder tag
          | Blob blob ->
            write_bigstring (Cstruct.to_bigarray (blob :> Cstruct.t))
          | Commit commit ->
            Commit.M.encoder commit) k)
        e
  end

  module EEE = Helper.MakeEncoder(MM)

  module type ENCODER = sig
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
  (* XXX(dinosaure): this is close to [Helper.Encoder] used to save a
     value to a file but without Lwt. *)

  type ('state, 'raw, 'result, 'error) encoder =
    (module ENCODER with type state = 'state
                     and type raw = 'raw
                     and type result = 'result
                     and type error = 'error)

  let to_ (type state) (type res) (type err_encoder)
      (encoder : (state, Cstruct.t, res, err_encoder) encoder)
      (buffer : Cstruct_buffer.t)
      (raw : Cstruct.t)
      (state : state) : (string, err_encoder) result
    =
    let module E =
      (val encoder : ENCODER with type state = state
                              and type raw = Cstruct.t
                              and type result = res
                              and type error = err_encoder)
    in

    let rec go state = match E.eval raw state with
      | `Error (_, err) -> Error err
      | `End (state, _) ->
        if E.used state > 0
        then Cstruct_buffer.add buffer (E.raw_sub raw 0 (E.used state));

        Ok (Cstruct_buffer.contents buffer)
      | `Flush state ->
        if E.used state > 0
        then Cstruct_buffer.add buffer (E.raw_sub raw 0 (E.used state));

        go (E.flush 0 (E.raw_length raw) state)
    in

    go state

  let to_deflated_raw ?(capacity = 0x100) ?(level = 4) ~ztmp value =
    let encoder = E.default (capacity, value, level, ztmp) in
    let raw = Cstruct.create capacity in
    let buffer = Cstruct_buffer.create (Int64.to_int (F.length value)) in
    (* XXX(dinosaure): it's an heuristic to consider than the size of the result
       is lower than [F.length value]. In most of cases, it's true but sometimes, a
       deflated Git object can be bigger than a serialized Git object. *)
    let module SpecializedEncoder = struct
      type state = E.encoder
      type raw = Cstruct.t
      type result = int
      type error = E.error
      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub
      type rest = [ `Flush of state | `End of (state * result) ]
      let eval raw state = match E.eval raw state with
        | #rest as rest -> rest
        | `Error err    -> `Error (state, err)
      let used = E.used
      let flush = E.flush
    end in
    to_ (module SpecializedEncoder) buffer raw encoder

  let to_raw ?(capacity = 0x100) value =
    let encoder = EE.default (capacity, value) in
    let raw = Cstruct.create capacity in
    let buffer = Cstruct_buffer.create (Int64.to_int (F.length value)) in
    (* XXX(dinosaure): we are sure than the serialized object has the size
       [F.length value]. So, the [buffer] should not growth. *)
    let module SpecializedEncoder = struct
      type state = EE.encoder
      type raw = Cstruct.t
      type result = int
      type error = EE.error
      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub
      type rest = [ `Flush of state | `End of (state * result) ]
      let eval raw state = match EE.eval raw state with
        | #rest as rest -> rest
        | `Error err    -> `Error (state, err)
      let used = EE.used
      let flush = EE.flush
    end in
    to_ (module SpecializedEncoder) buffer raw encoder

  let to_raw_without_header ?(capacity = 0x100) value =
    let encoder = EEE.default (capacity, value) in
    let raw = Cstruct.create capacity in
    let buffer = Cstruct_buffer.create (Int64.to_int (F.length value)) in
    (* XXX(dinosaure): we are sure than the serialized object has the size
       [F.length value]. So, the [buffer] should not growth. *)

    let module SpecializedEncoder = struct
      type state = EEE.encoder
      type raw = Cstruct.t
      type result = int
      type error = EEE.error
      let raw_length = Cstruct.len
      let raw_sub = Cstruct.sub
      type rest = [ `Flush of state | `End of (state * result) ]
      let eval raw state = match EEE.eval raw state with
        | #rest as rest -> rest
        | `Error err    -> `Error (state, err)
      let used = EEE.used
      let flush = EEE.flush
    end in

    to_ (module SpecializedEncoder) buffer raw encoder

  module DD = Helper.MakeDecoder(A)

  let of_raw_with_header inflated = DD.to_result inflated
  let of_raw ~kind inflated = match kind with
    | `Commit ->
      Rresult.R.map
        (fun commit -> Commit commit)
        (Value.Commit.D.to_result inflated)
    | `Tree ->
      Rresult.R.map
        (fun tree -> Tree tree)
        (Value.Tree.D.to_result inflated)
    | `Tag ->
      Rresult.R.map
        (fun tag -> Tag tag)
        (Value.Tag.D.to_result inflated)
    | `Blob ->
      let blob blob = Blob blob in
      Rresult.R.(get_ok (Value.Blob.D.to_result inflated)
                 |> blob |> ok)
end
