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

open Lwt.Infix

let src = Logs.Src.create "git.refs" ~doc:"Git references"
module Log = (val Logs.src_log src : Logs.LOG)

type t = string
let of_string x = x
let to_string x = x

let sep = "/"

module Path =
struct
  type partial = string
  type branch = string

  let ( // ) partial0 partial1 : partial = String.concat sep [ partial0; partial1 ]
  let ( / ) partial branch : t = String.concat sep [ partial; branch ]

  let refs : partial = "refs"
  let heads : partial = "refs/heads"
  let remotes : partial = "refs/remotes"
  let origin : partial = "refs/remotes/origin"

  let master : branch = "master"
end

let head    = "HEAD"
let is_head = String.equal head
let master  = "refs/heads/master"

let to_path x =
  match
    (* XXX(samoht): maybe Fpath.t = path was not a good idea afterall *)
    Fpath.of_string (String.concat Fpath.dir_sep (Astring.String.cuts ~sep x))
  with
  | Error (`Msg x) -> raise (Invalid_argument x)
  | Ok v           -> Fpath.normalize v

let of_path path =
  match List.rev @@ Fpath.segs path with
  | "HEAD" :: _ -> head
  | _ ->
    let segs = Fpath.(segs (normalize (v "refs" // path))) in
    String.concat sep segs

let pp ppf x =
  Fmt.pf ppf "%s" (String.escaped x)

module type S = sig
  module Hash: S.HASH

  type nonrec t = t

  module Path : sig
    type partial
    type branch = string

    val ( // ): partial -> partial -> partial
    val ( / ): partial -> branch -> t

    val refs: partial
    val heads: partial
    val remotes: partial
    val origin: partial

    val master: branch
  end

  val head: t
  val master: t

  val is_head: t -> bool

  val of_string: string -> t
  val to_string: t -> string

  val of_path: Fpath.t -> t
  val to_path: t -> Fpath.t

  include S.BASE with type t := t

  type head_contents =
    | Hash of Hash.t
    | Ref of t

  val pp_head_contents: head_contents Fmt.t
  val equal_head_contents: head_contents -> head_contents -> bool
  val compare_head_contents: head_contents -> head_contents -> int

  module A: S.DESC with type 'a t = 'a Angstrom.t and type e = head_contents
  module M: S.DESC with type 'a t = 'a Encore.Encoder.t and type e = head_contents
  module D: S.DECODER with type t = head_contents and type init = Cstruct.t and type error = Error.Decoder.t
  module E: S.ENCODER with type t = head_contents and type init = int * head_contents and type error = Error.never
end

module type IO = sig

  module FS: S.FS

  include S

  type error =
    [ Error.Decoder.t
    | FS.error Error.FS.t ]

  val pp_error: error Fmt.t

  val mem :
       fs:FS.t -> root:Fpath.t
    -> t -> bool Lwt.t
  val read :
       fs:FS.t -> root:Fpath.t
    -> t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> ((t * head_contents), error) result Lwt.t
  val write :
       fs:FS.t -> root:Fpath.t
    -> ?capacity:int
    -> raw:Cstruct.t
    -> t -> head_contents
    -> (unit, error) result Lwt.t
  val remove :
       fs:FS.t -> root:Fpath.t
    -> t -> (unit, error) result Lwt.t
end

module Make (Hash: S.HASH): S with module Hash = Hash = struct

  module Hash = Hash

  type nonrec t = t

  module Path = Path

  let master = master
  let head = head
  let is_head = is_head
  let of_string = of_string
  let to_string = to_string
  let to_path = to_path
  let of_path = of_path
  let pp = pp

  let equal   = String.equal
  let hash    = Hashtbl.hash

  let compare x y =
    match x, y with
    | "HEAD", "HEAD" -> 0
    | "HEAD", _      -> (-1)
    | _     , "HEAD" -> 1
    | _     , _      -> compare x y

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)

  type head_contents =
    | Hash of Hash.t
    | Ref of t

  let pp_head_contents ppf = function
    | Hash hash -> Fmt.pf ppf "(Hash %a)" Hash.pp hash
    | Ref t -> Fmt.pf ppf "(Ref %a)" pp t

  let equal_head_contents a b = match a, b with
    | Ref a', Ref b' -> equal a' b'
    | Hash a', Hash b' -> Hash.equal a' b'
    | _, _ -> false

  let compare_head_contents a b = match a, b with
    | Ref a', Ref b' -> compare a' b'
    | Hash a', Hash b' -> Hash.compare a' b'
    | Ref _, Hash _ -> 1
    | Hash _, Ref _ -> -1

  module MakeMeta (Meta: Encore.Meta.S) =
    struct
      type e = head_contents

      open Helper.BaseIso

      module Iso =
        struct
          open Encore.Bijection

          let hex =
            let tag = ("string", "hex") in
            make_exn ~tag
              ~fwd:(Exn.safe_exn tag Hash.of_hex)
              ~bwd:(Exn.safe_exn (Helper.Pair.flip tag) Hash.to_hex)

          let refname =
            let tag = ("string", "reference") in
            make_exn
              ~tag
              ~fwd:(Exn.safe_exn tag of_string)
              ~bwd:(Exn.safe_exn (Helper.Pair.flip tag) to_string)

          let hash =
            make_exn
              ~tag:("hash", "head_contents")
              ~fwd:(fun hash -> Hash hash)
              ~bwd:(function
                | Hash hash -> hash
                | _ -> Exn.fail "head_contents" "hash")

          let reference =
            make_exn
              ~tag:("reference", "head_contents")
              ~fwd:(fun reference -> Ref reference)
              ~bwd:(function
                | Ref r -> r
                | _ -> Exn.fail "head_contents" "reference")
        end

      type 'a t = 'a Meta.t

      module Meta = Encore.Meta.Make(Meta)
      open Meta

      let is_not_lf = (<>) '\n'

      let hash = Iso.hex <$> (take (Hash.Digest.length * 2)) <* (char_elt '\n' <$> any)
      let reference = (string_elt "ref: " <$> const "ref: ") *> (Iso.refname <$> (while1 is_not_lf)) <* (char_elt '\n' <$> any)

      let p = (Iso.reference <$> reference) <|> (Iso.hash <$> hash)
    end

  module A = MakeMeta(Encore.Proxy_decoder.Impl)
  module M = MakeMeta(Encore.Proxy_encoder.Impl)
  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)
end

module IO (Hash: S.HASH) (FS: S.FS) = struct

  module FS = Helper.FS(FS)

  include Make(Hash)

  module Encoder = struct
    module E = struct
      type state  = E.encoder
      type result = int
      type error  = E.error
      type rest = [ `Flush of state | `End of (state * result) ]
      let used  = E.used
      let flush = E.flush
      let eval raw state = match E.eval raw state with
        | `Error err    -> Lwt.return (`Error (state, err))
        | #rest as rest -> Lwt.return rest
    end
    include Helper.Encoder(E)(FS)
  end

  module Decoder = Helper.Decoder(D)(FS)

  type fs_error = FS.error Error.FS.t
  type error =
    [ Error.Decoder.t
    | fs_error ]

  let pp_error ppf = function
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #fs_error as err -> Error.FS.pp_error FS.pp_error ppf err

  let normalize path =
    let segs = Astring.String.cuts ~sep:Fpath.dir_sep (Fpath.to_string path) in

    List.fold_left
      (fun (stop, acc) ->
         if stop then fun x -> (true, x :: acc)
         else function
           | "HEAD" as x -> (true, x :: acc)
           (* XXX(dinosaure): special case, HEAD can be stored in a refs
              sub-directory or can be in root of dotgit (so, without refs). *)
           | "refs" as x -> (true, [ x ])
           | _ -> (false, []))
      (false, []) segs
    |> fun (_, refs) -> List.rev refs |> String.concat "/" |> of_string

  let mem ~fs ~root reference =
    let path = Fpath.(root // (to_path reference)) in
    FS.File.exists fs path >|= function
    | Ok v    -> v
    | Error _ -> false

  let read ~fs ~root reference ~dtmp ~raw : (_, error) result Lwt.t =
    let state = D.default dtmp in
    let file  = Fpath.(root // (to_path reference)) in
    Decoder.of_file fs file raw state >|= function
    | Ok v -> (Ok (normalize file, v))
    | Error (`Decoder err) -> Error.(v @@ Error.Decoder.with_path file err)
    | Error #fs_error as err -> err

  let write ~fs ~root ?(capacity = 0x100) ~raw reference value =
    let state = E.default (capacity, value) in
    let path = Fpath.(root // (to_path reference)) in
    let temp_dir = Fpath.(root / "tmp") in
    FS.Dir.create fs (Fpath.parent path)
    >>= function
    | Error err -> Lwt.return Error.(v @@ FS.err_create (Fpath.parent path) err)
    | Ok (true | false) ->
      Encoder.to_file fs ~temp_dir path raw state >|= function
      | Ok _                   -> Ok ()
      | Error #fs_error as err -> err
      | Error (`Encoder #Error.never) -> assert false

  let remove ~fs ~root t =
    let path = Fpath.(root // to_path t) in
    FS.File.delete fs path >|= function
    | Ok _ as v -> v
    | Error err -> Error.(v @@ FS.err_delete path err)
end
