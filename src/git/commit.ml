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

type 'hash t = {
  tree : 'hash;
  parents : 'hash list;
  author : User.t;
  committer : User.t;
  extra : (string * string list) list;
  message : string option;
}

module type S = sig
  type hash
  type nonrec t = hash t

  val make :
    tree:hash ->
    author:User.t ->
    committer:User.t ->
    ?parents:hash list ->
    ?extra:(string * string list) list ->
    string option ->
    t

  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash
  include S.BASE with type t := t

  val length : t -> int64
  val parents : t -> hash list
  val tree : t -> hash
  val committer : t -> User.t
  val author : t -> User.t
  val message : t -> string option
  val extra : t -> (string * string list) list
  val compare_by_date : t -> t -> int
end

module Make (Hash : S.HASH) = struct
  type hash = Hash.t

  (* XXX(dinosaure): git seems to be very resilient with the commit. Indeed,
     it's not a mandatory to have an author or a committer and for these
     information, it's not mandatory to have a date.

     Follow this issue if we have any problem with the commit format. *)

  type nonrec t = hash t

  let make ~tree ~author ~committer ?(parents = []) ?(extra = []) message =
    { tree; parents; author; committer; extra; message }

  module Syntax = struct
    let safe_exn f x = try f x with _ -> raise Encore.Bij.Bijection

    let hex =
      Encore.Bij.v ~fwd:(safe_exn Hash.of_hex) ~bwd:(safe_exn Hash.to_hex)

    let user =
      Encore.Bij.v
        ~fwd:(fun str ->
          match
            Angstrom.parse_string ~consume:Angstrom.Consume.All
              (Encore.to_angstrom User.format)
              str
          with
          | Ok v -> v
          | Error _ -> raise Encore.Bij.Bijection)
        ~bwd:(fun v ->
          Encore.Lavoisier.emit_string v (Encore.to_lavoisier User.format))

    let commit =
      Encore.Bij.v
        ~fwd:(fun
            ((_, tree), parents, (_, author), (_, committer), extra, message) ->
          let parents = List.map snd parents in
          { tree; parents; author; committer; extra; message })
        ~bwd:(fun { tree; parents; author; committer; extra; message } ->
          let parents = List.map (fun x -> "parent", x) parents in
          ( ("tree", tree),
            parents,
            ("author", author),
            ("committer", committer),
            extra,
            message ))

    let is_not_sp chr = chr <> ' '
    let is_not_lf chr = chr <> '\x0a'
    let always x _ = x

    let rest =
      let open Encore.Syntax in
      let open Encore.Either in
      fix @@ fun m ->
      let cons = Encore.Bij.cons <$> (while0 (always true) <* commit <*> m) in
      let nil = pure ~compare:(fun () () -> true) () in
      Encore.Bij.v
        ~fwd:(function L cons -> cons | R () -> [])
        ~bwd:(function _ :: _ as lst -> L lst | [] -> R ())
      <$> peek cons nil

    let rest : string Encore.t =
      let open Encore.Syntax in
      Encore.Bij.v ~fwd:(String.concat "") ~bwd:(fun x -> [ x ]) <$> rest

    let value =
      let open Encore.Syntax in
      let sep = Encore.Bij.string "\n " <$> const "\n " in
      sep_by0 ~sep (while0 is_not_lf)

    let extra =
      let open Encore.Syntax in
      while1 (fun chr -> is_not_sp chr && is_not_lf chr)
      <* (Encore.Bij.char ' ' <$> any)
      <*> (value <* (Encore.Bij.char '\x0a' <$> any))

    let binding ?key value =
      let open Encore.Syntax in
      let value =
        value <$> (while1 is_not_lf <* (Encore.Bij.char '\x0a' <$> any))
      in
      match key with
      | Some key -> const key <* (Encore.Bij.char ' ' <$> any) <*> value
      | None -> while1 is_not_sp <* (Encore.Bij.char ' ' <$> any) <*> value

    let rest =
      let open Encore.Syntax in
      let open Encore.Either in
      let fwd = function L str -> Some str | R _ -> None in
      let bwd = function Some str -> L str | None -> R "" in
      map (Encore.Bij.v ~fwd ~bwd)
        (peek ((Encore.Bij.char '\x0a' <$> any) *> rest) (const ""))

    let t =
      let open Encore.Syntax in
      binding ~key:"tree" hex
      <*> rep0 (binding ~key:"parent" hex)
      <*> binding ~key:"author" user
      <*> binding ~key:"committer" user
      <*> rep0 extra
      <*> rest

    let format = Encore.Syntax.map Encore.Bij.(compose obj6 commit) t
  end

  let format = Syntax.format

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in
    let parents =
      List.fold_left
        (fun acc _ ->
          string "parent" + 1L + Int64.of_int (Hash.digest_size * 2) + 1L + acc)
        0L t.parents
    in
    let values l =
      let rec go a = function
        | [] -> 1L + a
        | [ x ] -> string x + 1L + a
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
    + match t.message with Some str -> 1L + string str | None -> 0L

  let pp ppf { tree; parents; author; committer; extra; message } =
    let chr =
      Fmt.using (function '\000' .. '\031' | '\127' -> '.' | x -> x) Fmt.char
    in
    let pp_message ppf x = Fmt.iter ~sep:Fmt.nop String.iter chr ppf x in
    Fmt.pf ppf
      "{ @[<hov>tree = %a;@ parents = [ %a ];@ author = %a;@ committer = %a;@ \
       extra = %a;@ message = %a;@] }"
      (Fmt.hvbox Hash.pp) tree
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.any ";@ ") Hash.pp))
      parents (Fmt.hvbox User.pp) author (Fmt.hvbox User.pp) committer
      Fmt.(hvbox (Dump.list (Dump.pair string (Dump.list string))))
      extra
      Fmt.(option (hvbox pp_message))
      message

  let digest value =
    Stream.digest
      {
        Stream.empty = Hash.empty;
        Stream.feed_string = (fun str ctx -> Hash.feed_string ctx str);
        Stream.feed_bigstring = (fun bstr ctx -> Hash.feed_bigstring ctx bstr);
        Stream.get = Hash.get;
      }
      `Commit length
      (Encore.to_lavoisier format)
      value

  let equal = ( = )
  let hash = Hashtbl.hash
  let parents { parents; _ } = parents
  let tree { tree; _ } = tree
  let committer { committer; _ } = committer
  let author { author; _ } = author
  let message { message; _ } = message
  let extra { extra; _ } = extra

  let compare_by_date a b =
    Int64.compare (fst a.author.User.date) (fst b.author.User.date)

  let compare = compare_by_date

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
