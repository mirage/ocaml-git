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


let head = "HEAD"

let is_head = String.equal head

let master = "refs/heads/master"

let to_path = function "HEAD" -> Path.v "HEAD" | refs -> Path.v refs

let of_path path =
  match Path.segs path with
  | [] -> Fmt.invalid_arg "Reference.of_path: empty path"
  | [ "HEAD" ] -> head
  | "HEAD" :: _ ->
      Fmt.invalid_arg "Reference.of_path: HEAD can not be followed by values"
  | "refs" :: _ as refs -> String.concat sep refs
  | _ :: _ ->
      invalid_arg "Reference.of_path: bad path (need to be prefixed by refs)"

let pp ppf x = Fmt.pf ppf "%s" (String.escaped x)

  type nonrec t = t

end

module type IO = sig
  module FS : S.FS

  include S

  type error = [ Error.Decoder.t | FS.error Error.FS.t ]

  val pp_error : error Fmt.t

  val mem : fs:FS.t -> root:Fpath.t -> t -> bool Lwt.t

  val read :
    fs:FS.t ->
    root:Fpath.t ->
    t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    (head_contents, error) result Lwt.t

  val write :
    fs:FS.t ->
    root:Fpath.t ->
    temp_dir:Fpath.t ->
    etmp:Cstruct.t ->
    raw:Cstruct.t ->
    t ->
    head_contents ->
    (unit, error) result Lwt.t

  val remove : fs:FS.t -> root:Fpath.t -> t -> (unit, error) result Lwt.t
end

module Make (Hash : S.HASH) = struct
  type nonrec t = t

  module P = P

  let master = master

  let head = head

  let is_head = is_head

  let of_string = of_string

  let to_string = to_string

  let to_path = Path.v

  let of_path path =
    let segs = Path.segs path in
    String.concat sep segs

  (* XXX(dinosaure): doublon with [Path.to_string] but this function uses
     [Fmt.to_to_string] and I don't trust this function. *)

  let pp = pp

  let equal = String.equal

  let hash = Hashtbl.hash

  let compare x y =
    match (x, y) with
    | "HEAD", "HEAD" -> 0
    | "HEAD", _ -> -1
    | _, "HEAD" -> 1
    | _, _ -> compare x y

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  type head_contents = Hash of Hash.t | Ref of t

  let pp_head_contents ppf = function
    | Hash hash -> Fmt.pf ppf "(Hash %a)" Hash.pp hash
    | Ref t -> Fmt.pf ppf "(Ref %a)" pp t

  let equal_head_contents a b =
    match (a, b) with
    | Ref a', Ref b' -> equal a' b'
    | Hash a', Hash b' -> Hash.equal a' b'
    | _, _ -> false

  let compare_head_contents a b =
    match (a, b) with
    | Ref a', Ref b' -> compare a' b'
    | Hash a', Hash b' -> Hash.unsafe_compare a' b'
    | Ref _, Hash _ -> 1
    | Hash _, Ref _ -> -1

  module MakeMeta (Meta : Encore.Meta.S) = struct
    type e = head_contents

    open Helper.BaseIso

    module Iso = struct
      open Encore.Bijection

      let hex =
        make_exn ~fwd:(Exn.safe_exn Hash.of_hex) ~bwd:(Exn.safe_exn Hash.to_hex)

      let refname =
        make_exn ~fwd:(Exn.safe_exn of_string) ~bwd:(Exn.safe_exn to_string)

      let hash =
        make_exn
          ~fwd:(fun hash -> Hash hash)
          ~bwd:(function Hash hash -> hash | _ -> Exn.fail ())

      let reference =
        make_exn
          ~fwd:(fun reference -> Ref reference)
          ~bwd:(function Ref r -> r | _ -> Exn.fail ())
    end

    type 'a t = 'a Meta.t

    module Meta = Encore.Meta.Make (Meta)
    open Meta

    let is_not_lf = ( <> ) '\n'

    let hash = Iso.hex <$> take (Hash.digest_size * 2) <* (char_elt '\n' <$> any)

    let reference =
      (string_elt "ref: " <$> const "ref: ")
      *> (Iso.refname <$> while1 is_not_lf)
      <* (char_elt '\n' <$> any)

    let p = Iso.reference <$> reference <|> (Iso.hash <$> hash)
  end

  module A = MakeMeta (Encore.Proxy_decoder.Impl)
  module M = MakeMeta (Encore.Proxy_encoder.Impl)
  module D = Helper.MakeDecoder (A)
  module E = Helper.MakeEncoder (M)
end

module IO (H : S.HASH) (FS : S.FS) = struct
  module FS = Helper.FS (FS)
  include Make (H)

  module Encoder = struct
    module E = struct
      type state = E.encoder

      type result = int

      type error = E.error

      type rest = [ `Flush of state | `End of state * result ]

      let used = E.used

      let flush = E.flush

      let eval raw state =
        match E.eval raw state with
        | `Error err -> Lwt.return (`Error (state, err))
        | #rest as rest -> Lwt.return rest
    end

    include Helper.Encoder (E) (FS)
  end

  module Decoder = Helper.Decoder (D) (FS)

  type fs_error = FS.error Error.FS.t

  type error = [ Error.Decoder.t | fs_error ]

  let pp_error ppf = function
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #fs_error as err -> Error.FS.pp_error FS.pp_error ppf err

  let[@warning "-32"] normalize path =
    let segs = Path.segs path in
    List.fold_left
      (fun (stop, acc) ->
        if stop
        then fun x -> (true, x :: acc)
        else
          function
          | "HEAD" as x -> (true, x :: acc)
          (* XXX(dinosaure): special case, HEAD can be stored in a refs
             sub-directory or can be in root of dotgit (so, without refs). *)
          | "refs" as x -> (true, [ x ])
          | _ -> (false, []))
      (false, []) segs
    |> fun (_, refs) -> List.rev refs |> String.concat "/" |> of_string

  let mem ~fs ~root:dotgit reference =
    let path = Path.(dotgit + to_path reference) in
    FS.File.exists fs path >|= function Ok v -> v | Error _ -> false

  let read ~fs ~root:dotgit reference ~dtmp ~raw : (_, error) result Lwt.t =
    let state = D.default dtmp in
    let path = Path.(dotgit + to_path reference) in
    Decoder.of_file fs path raw state >|= function
    | Ok v -> Ok v
    | Error (`Decoder err) -> Error.(v @@ Error.Decoder.with_path path err)
    | Error #fs_error as err -> err

  let write ~fs ~root:dotgit ~temp_dir ~etmp ~raw reference value =
    let state = E.default (etmp, value) in
    let path = Path.(dotgit + to_path reference) in
    FS.Dir.create fs (Fpath.parent path) >>= function
    | Error err -> Lwt.return Error.(v @@ FS.err_create (Fpath.parent path) err)
    | Ok (true | false) -> (
        Encoder.to_file fs ~temp_dir path raw state >|= function
        | Ok _ -> Ok ()
        | Error #fs_error as err -> err
        | Error (`Encoder #Error.never) -> assert false)

  let remove ~fs ~root:dotgit reference =
    let path = Path.(dotgit + to_path reference) in
    FS.File.delete fs path >|= function
    | Ok _ as v -> v
    | Error err -> Error.(v @@ FS.err_delete path err)
end
