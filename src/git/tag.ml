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
  module Digest : Ihash.IDIGEST

  type t

  module Hash : Common.BASE

  module D : Common.DECODER  with type t = t
                              and type raw = Cstruct.t
                              and type init = Cstruct.t
                              and type error = [ `Decoder of string ]
  module A : Common.ANGSTROM with type t = t
  module F : Common.FARADAY  with type t = t
  module M : Common.MINIENC  with type t = t
  module E : Common.ENCODER  with type t = t
                              and type raw = Cstruct.t
                              and type init = int * t
                              and type error = [ `Never ]

  include Ihash.DIGEST with type t := t and type hash = Hash.t
  include Common.BASE with type t := t

  val obj : t -> Hash.t
end

module Make (Digest : Ihash.IDIGEST with type t = Bytes.t
                                    and type buffer = Cstruct.t)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
= struct
  module Digest = Digest
  module Hash = Helper.BaseBytes

  type t =
    { obj     : Hash.t
    ; kind    : kind
    ; tag     : string
    ; tagger  : User.t option
    ; message : string }
  and kind = Blob | Commit | Tag | Tree
  and hash = Hash.t

  let hash_of_hex_string x =
    Helper.BaseBytes.of_hex x
  let hash_to_hex_string x =
    Helper.BaseBytes.to_hex x

  let pp_kind ppf = function
    | Blob   -> Fmt.string ppf "Blob"
    | Commit -> Fmt.string ppf "Commit"
    | Tag    -> Fmt.string ppf "Tag"
    | Tree   -> Fmt.string ppf "Tree"

  let pp ppf { obj
             ; kind
             ; tag
             ; tagger
             ; message } =
    Fmt.pf ppf "{ @[<hov>obj = %a;@ \
                         kind = %a;@ \
                         tag = %s;@ \
                         tagger = %a;@ \
                         message = %a@] }"
      Hash.pp obj
      pp_kind kind
      tag
      (Fmt.hvbox (Fmt.option User.pp)) tagger
      (Fmt.hvbox Fmt.text) message

  let string_of_kind = function
    | Commit -> "commit"
    | Tag -> "tag"
    | Tree -> "tree"
    | Blob -> "blob"

  module A =
  struct
    type nonrec t = t

    let sp = Angstrom.char ' '
    let lf = Angstrom.char '\x0a'
    let is_not_lf chr = chr <> '\x0a'

    let obj = Angstrom.take (Digest.length * 2)

    let kind =
      let open Angstrom in

      (string "blob"       *> return Blob)
      <|> (string "commit" *> return Commit)
      <|> (string "tag"    *> return Tag)
      <|> (string "tree"   *> return Tree)

    let binding
      : type a. key:string -> value:a Angstrom.t -> a Angstrom.t
      = fun ~key ~value ->
      let open Angstrom in
      string key *> sp *> value <* lf

    let to_end len =
      let buf = Buffer.create len in
      let open Angstrom in

      fix @@ fun m ->
      available >>= function
      | 0 ->
        peek_char
        >>= (function
            | Some _ -> m
            | None ->
              let res = Buffer.contents buf in
              Buffer.clear buf;
              return res)
      | n -> take n >>= fun chunk -> Buffer.add_string buf chunk; m

    let decoder =
      let open Angstrom in
      binding ~key:"object" ~value:obj <* commit
      >>= fun obj    -> binding ~key:"type" ~value:kind
                        <* commit
      >>= fun kind   -> binding ~key:"tag" ~value:(take_while is_not_lf)
                        <* commit
      >>= fun tag    -> (option None
                                (binding ~key:"tagger" ~value:User.A.decoder
                                 >>= fun user -> return (Some user)))
                        <* commit
      >>= fun tagger -> take 1 *> commit *> to_end 1024 <* commit
      >>= fun message ->
        return { obj = hash_of_hex_string obj
               ; kind
               ; tag
               ; tagger
               ; message }
      <* commit
  end

  module F =
  struct
    type nonrec t = t

    let length t =
      let string x = Int64.of_int (String.length x) in
      let ( + ) = Int64.add in

      let user_length = match t.tagger with
        | Some user -> (string "tagger") + 1L + (User.F.length user) + 1L
        | None -> 0L
      in
      (string "object") + 1L + (Int64.of_int (Digest.length * 2)) + 1L
      + (string "type") + 1L + (string (string_of_kind t.kind)) + 1L
      + (string "tag") + 1L + (string t.tag) + 1L
      + user_length
      + 1L + (string t.message)

    let sp = ' '
    let lf = '\x0a'

    let string_of_kind = function
      | Blob   -> "blob"
      | Commit -> "commit"
      | Tree   -> "tree"
      | Tag    -> "tag"

    let encoder e t =
      let open Farfadet in

      let tagger e x = eval e [ string $ "tagger"; char $ sp; !!User.F.encoder; char $ lf ] x in

      eval e [ string $ "object"; char $ sp; !!string; char $ lf
             ; string $ "type"; char $ sp; !!string; char $ lf
             ; string $ "tag"; char $ sp; !!string; char $ lf
             ; !!(option tagger); char $ lf
             ; !!string ]
        (hash_to_hex_string t.obj)
        (string_of_kind t.kind)
        t.tag
        t.tagger
        t.message
  end

  module M =
  struct
    type nonrec t = t

    open Minienc

    let sp = ' '
    let lf = '\x0a'

    let string_of_kind = function
      | Blob   -> "blob"
      | Commit -> "commit"
      | Tree   -> "tree"
      | Tag    -> "tag"

    let encoder x k e =
      let write_tagger x k e = match x with
        | Some x ->
          (write_string "tagger"
           @@ write_char sp
           @@ User.M.encoder x
           @@ write_char lf k)
            e
        | None -> k e
      in

      (write_string "object"
       @@ write_char sp
       @@ write_string (hash_to_hex_string x.obj)
       @@ write_char lf
       @@ write_string "type"
       @@ write_char sp
       @@ write_string (string_of_kind x.kind)
       @@ write_char lf
       @@ write_string "tag"
       @@ write_char sp
       @@ write_string x.tag
       @@ write_char lf
       @@ write_tagger x.tagger
       @@ write_char lf
       @@ write_string x.message k)
      e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)

  let obj { obj; _ } = obj

  let digest value =
    let tmp = Cstruct.create 0x100 in
    Helper.fdigest (module Digest) (module E) ~tmp ~kind:"tag" ~length:F.length value

  let equal   = (=)
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

