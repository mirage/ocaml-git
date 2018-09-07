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
  type t

  module Hash : S.HASH

  val make :
       tree:Hash.t
    -> author:User.t
    -> committer:User.t
    -> ?parents:Hash.t list
    -> ?extra:(string * string list) list
    -> string
    -> t

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
  val parents : t -> Hash.t list
  val tree : t -> Hash.t
  val committer : t -> User.t
  val author : t -> User.t
  val message : t -> string
  val extra : t -> (string * string list) list
  val compare_by_date : t -> t -> int
end

module Make (Hash : S.HASH) = struct
  (* XXX(dinosaure): git seems to be very resilient with the commit. Indeed,
     it's not a mandatory to have an author or a committer and for these
     information, it's not mandatory to have a date.

     Follow this issue if we have any problem with the commit format. *)

  type t =
    { tree: Hash.t
    ; parents: Hash.t list
    ; author: User.t
    ; committer: User.t
    ; extra: (string * string list) list
    ; message: string }

  let make ~tree ~author ~committer ?(parents = []) ?(extra = []) message =
    {tree; parents; author; committer; extra; message}

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

      let commit =
        make_exn
          ~tag:("hash * parents * user * user * fields * string", "commit")
          ~fwd:
            (fun ( (_, tree)
                 , parents
                 , (_, author)
                 , (_, committer)
                 , extra
                 , message ) ->
            let parents = List.map snd parents in
            {tree; parents; author; committer; extra; message} )
          ~bwd:(fun {tree; parents; author; committer; extra; message} ->
            let parents = List.map (fun x -> "parent", x) parents in
            ( ("tree", tree)
            , parents
            , ("author", author)
            , ("committer", committer)
            , extra
            , message ) )
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

    let value =
      let sep = string_elt "\n " <$> const "\n " in
      sep_by0 ~sep (while0 is_not_lf)

    let extra =
      while1 (fun chr -> is_not_sp chr && is_not_lf chr)
      <* (char_elt ' ' <$> any)
      <*> (value <* (char_elt '\x0a' <$> any))

    let binding ?key value =
      let value = value <$> (while1 is_not_lf <* (char_elt '\x0a' <$> any)) in
      match key with
      | Some key -> const key <* (char_elt ' ' <$> any) <*> value
      | None -> while1 is_not_sp <* (char_elt ' ' <$> any) <*> value

    let commit =
      binding ~key:"tree" Iso.hex
      <*> rep0 (binding ~key:"parent" Iso.hex)
      <*> binding ~key:"author" Iso.user
      <*> binding ~key:"committer" Iso.user
      <*> rep0 extra
      <*> to_end

    let p = Exn.compose obj6 Iso.commit <$> commit
  end

  module A = MakeMeta (Encore.Proxy_decoder.Impl)
  module M = MakeMeta (Encore.Proxy_encoder.Impl)
  module D = Helper.MakeDecoder (A)
  module E = Helper.MakeEncoder (M)

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in
    let parents =
      List.fold_left
        (fun acc _ ->
          string "parent" + 1L + Int64.of_int (Hash.digest_size * 2) + 1L + acc
          )
        0L t.parents
    in
    let values l =
      let rec go a = function
        | [] -> 1L + a
        | [x] -> string x + 1L + a
        | x :: r -> go (string x + 2L + a) r
      in
      go 0L l
    in
    string "tree"
    + 1L
    + Int64.of_int (Hash.digest_size * 2)
    + 1L
    + parents
    + string "author"
    + 1L
    + User.length t.author
    + 1L
    + string "committer"
    + 1L
    + User.length t.committer
    + 1L
    + List.fold_left
        (fun acc (key, v) -> string key + 1L + values v + acc)
        0L t.extra
    + string t.message

  let pp ppf {tree; parents; author; committer; extra; message} =
    let chr =
      Fmt.using (function '\000' .. '\031' | '\127' -> '.' | x -> x) Fmt.char
    in
    let pp_message ppf x = Fmt.iter ~sep:Fmt.nop String.iter chr ppf x in
    Fmt.pf ppf
      "{ @[<hov>tree = %a;@ parents = [ %a ];@ author = %a;@ committer = %a;@ \
       extra = %a;@ message = %a;@] }"
      (Fmt.hvbox Hash.pp) tree
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") Hash.pp))
      parents (Fmt.hvbox User.pp) author (Fmt.hvbox User.pp) committer
      Fmt.(hvbox (Dump.list (Dump.pair string (Dump.list string))))
      extra (Fmt.hvbox pp_message) message

  let digest value =
    let tmp = Cstruct.create 0x100 in
    Helper.fdigest (module Hash) (module E) ~tmp ~kind:"commit" ~length value

  let equal = ( = )
  let hash = Hashtbl.hash
  let parents {parents; _} = parents
  let tree {tree; _} = tree
  let committer {committer; _} = committer
  let author {author; _} = author
  let message {message; _} = message
  let extra {extra; _} = extra

  let compare_by_date a b =
    Int64.compare (fst a.author.User.date) (fst b.author.User.date)

  let compare = compare_by_date

  module Set = Set.Make (struct type nonrec t = t

                                let compare = compare end)

  module Map = Map.Make (struct type nonrec t = t

                                let compare = compare end)
end
