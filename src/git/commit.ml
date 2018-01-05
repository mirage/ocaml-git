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

  type t

  module Hash: S.HASH

  val make:
       author:User.t
    -> committer:User.t
    -> ?parents:Hash.t list
    -> tree:Hash.t
    -> string
    -> t

  module D: S.DECODER
    with type t = t
     and type init = Cstruct.t
     and type error = [ `Decoder of string ]

  module A: S.ANGSTROM with type t = t

  module F: S.FARADAY with type t = t

  module M: S.MINIENC with type t = t

  module E: S.ENCODER
    with type t = t
     and type init = int * t
     and type error = [ `Never ]

  include S.DIGEST with type t := t and type hash = Hash.t
  include S.BASE with type t := t

  val parents: t -> Hash.t list
  val tree: t -> Hash.t
  val committer: t -> User.t
  val author: t -> User.t
  val message: t -> string
  val compare_by_date: t -> t -> int
end

module Make (H: S.HASH): S with module Hash = H = struct

  module Hash = H

  (* XXX(dinosaure): git seems to be very resilient with the commit.
     Indeed, it's not a mandatory to have an author or a committer and
     for these information, it's not mandatory to have a date.

     Follow this issue if we have any problem with the commit
     format. *)

  type t =
    { tree      : Hash.t
    ; parents   : Hash.t list
    ; author    : User.t
    ; committer : User.t
    ; message   : [ `Empty | `Clean of string | `Bazaar of string ] option }
  (* XXX(dinosaure): special note about that, Git adds extra headers
     in a commit. This type is much more because we don't handle these
     extra headers than a bug.

     Indeed, we need to keep the integrity of the commit (for the hash
     function) so we need to separate 4 cases:
     - an empty message which does not have at the beginning '\n'
     (None)
     - an empty message which has at the beginning '\n' (Some `Empty)
     - a message (Some (`Clean msg))
     - extra-headers (so something...) plus the message (Some (`Bazaar
     msg))

     We need to concretize this differentiation to generate from any
     commit exactly the same commit - and. by this way, we avoid a
     corruption of the hash. Then, when we full implemented extra
     headers, we will delete this situation. *)
  and hash = Hash.t

  let make ~author ~committer ?(parents = []) ~tree message =
    { tree
    ; parents
    ; author
    ; committer
    ; message = Some (`Clean message) }

  module A = struct

    type nonrec t = t

    let sp = Angstrom.char ' '
    let lf = Angstrom.char '\x0a'
    let is_not_lf chr = chr <> '\x0a'

    let binding
      : type a. key:string -> value:a Angstrom.t -> a Angstrom.t
      = fun ~key ~value ->
      let open Angstrom in
      string key *> sp *> value <* lf <* commit

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

      binding ~key:"tree" ~value:(take_while is_not_lf) <* commit
      >>= fun tree ->
      many (binding ~key:"parent" ~value:(take_while is_not_lf))
      <* commit
      >>= fun parents ->
      binding ~key:"author" ~value:User.A.decoder
      <* commit
      >>= fun author ->
      binding ~key:"committer" ~value:User.A.decoder
      <* commit
      >>= fun committer -> to_end 1024 <* commit
      >>| (function
      | "" -> None
      | "\n" -> Some `Empty
      | lf_message ->
        let message =
          if String.get lf_message 0 = '\x0a'
          then `Clean (String.sub lf_message 1 (String.length lf_message - 1))
          else `Bazaar lf_message in
        Some message)
      >>= fun message ->
      return { tree = Hash.of_hex tree
             ; parents = List.map Hash.of_hex parents
             ; author
             ; committer
             ; message }
  end

  module F = struct

    type nonrec t = t

    let length t =
      let string x = Int64.of_int (String.length x) in
      let ( + ) = Int64.add in

      let parents =
        List.fold_left
          (fun acc _ ->
             (string "parent")
             + 1L
             + (Int64.of_int (Hash.Digest.length * 2))
             + 1L
             + acc)
          0L t.parents
      in
      (string "tree") + 1L + (Int64.of_int (Hash.Digest.length * 2)) + 1L
      + parents
      + (string "author") + 1L + (User.F.length t.author) + 1L
      + (string "committer") + 1L + (User.F.length t.committer) + 1L
      + (match t.message with
          | None -> 0L
          | Some `Empty -> 1L
          | Some (`Bazaar x) -> string x
          | Some (`Clean x) -> 1L + (string x))

    let sp = ' '
    let lf = '\x0a'

    let message e x = let open Farfadet in match x with
      | None -> ()
      | Some `Empty -> char e lf
      | Some (`Clean x) ->
        eval e [ char $ lf; !!string ] x
      | Some (`Bazaar x) -> string e x

    let parents e x =
      let open Farfadet in
      eval e [ string $ "parent"; char $ sp; !!string ] (Hash.to_hex x)

    let encoder e t =
      let open Farfadet in
      let sep = (fun e () -> char e lf), () in

      eval e [ string $ "tree"; char $ sp; !!string; char $ lf
             ; !!(option (seq (list ~sep parents) (fun e () -> char e lf)))
             ; string $ "author"; char $ sp; !!User.F.encoder; char $ lf
             ; string $ "committer"; char $ sp; !!User.F.encoder; char $ lf
             ; !!message ]
        (Hash.to_hex t.tree)
        (match t.parents with [] -> None | lst -> Some (lst, ()))
        t.author
        t.committer
        t.message
  end

  module M = struct

    open Minienc

    type nonrec t = t

    let sp = ' '
    let lf = '\x0a'

    let parents x k e =
      (write_string "parent"
       @@ write_char sp
       @@ write_string (Hash.to_hex x) k)
      e

    let message x k e = match x with
      | None -> k e
      | Some `Empty -> write_char lf k e
      | Some (`Clean x) -> write_char lf (write_string x k) e
      | Some (`Bazaar x) -> write_string x k e

    let encoder x k e =
      let rec list l k e = match l with
        | [] -> k e
        | x :: r ->
          (parents x
           @@ write_char lf
           @@ list r k) e
      in

      (write_string "tree"
       @@ write_char sp
       @@ write_string (Hash.to_hex x.tree)
       @@ write_char lf
       @@ list x.parents
       @@ write_string "author"
       @@ write_char sp
       @@ User.M.encoder x.author
       @@ write_char lf
       @@ write_string "committer"
       @@ write_char sp
       @@ User.M.encoder x.committer
       @@ write_char lf
       @@ message x.message k)
        e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)

  let pp ppf { tree; parents; author; committer; message; } =
    let chr =
      Fmt.using
        (function '\000' .. '\031'
                | '\127' -> '.' | x -> x)
        Fmt.char
    in

    let pp_message ppf = function
      | None | Some `Empty -> Fmt.string ppf "<empty>"
      | Some (`Clean x | `Bazaar x) -> Fmt.iter ~sep:Fmt.nop String.iter chr ppf x
    in

    Fmt.pf ppf
      "{ @[<hov>tree = %a;@ \
                parents = [ %a ];@ \
                author = %a;@ \
                committer = %a;@ \
                message = %a;@] }"
      (Fmt.hvbox Hash.pp) tree
      (Fmt.hvbox (Fmt.list ~sep:(Fmt.unit ";@ ") Hash.pp)) parents
      (Fmt.hvbox User.pp) author
      (Fmt.hvbox User.pp) committer
      (Fmt.hvbox pp_message) message

  let digest value =
    let tmp = Cstruct.create 0x100 in
    Helper.fdigest (module Hash.Digest) (module E) ~tmp ~kind:"commit" ~length:F.length value

  let equal = (=)
  let hash = Hashtbl.hash

  let parents { parents; _ } = parents
  let tree { tree; _ } = tree
  let committer { committer; _ } = committer
  let author { author; _ } = author
  let message { message; _ } = match message with
    | None | Some `Empty -> ""
    | Some (`Clean x | `Bazaar x) -> x

  let compare_by_date a b =
    Int64.compare (fst a.author.User.date) (fst b.author.User.date)

  let compare = compare_by_date

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end
