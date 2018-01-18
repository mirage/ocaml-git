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

let to_path x = match Fpath.of_string (String.concat Fpath.dir_sep (Astring.String.cuts ~sep x)) with
  | Error (`Msg x) -> raise (Invalid_argument x)
  | Ok v -> Fpath.normalize v

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

  module A: S.ANGSTROM with type t = head_contents
  module D: S.DECODER
    with type t = head_contents
     and type init = Cstruct.t
     and type error = Error.Decoder.t0
  module M: S.MINIENC with type t = head_contents
  module E: S.ENCODER
    with type t = head_contents
     and type init = int * head_contents
     and type error = Error.never
end

module type IO = sig

  module FS: S.FS

  include S

  type error =
    [ Error.Decoder.t2
    | Error.Angstrom.t2
    | `Read   of Fpath.t * FS.error
    | `Write  of Fpath.t * FS.error
    | `Open   of Fpath.t * FS.error
    | `Close  of Fpath.t * FS.error
    | `Map    of Fpath.t * FS.error
    | `Stack  of Fpath.t
    | `Move   of Fpath.t * Fpath.t * FS.error
    | `Create of Fpath.t * FS.error
    | `Delete of Fpath.t * FS.error
    | `Length of Fpath.t * FS.error ]

  val pp_error: error Fmt.t

  val mem:
       root:Fpath.t
    -> t -> bool Lwt.t
  val read:
       root:Fpath.t
    -> t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> ((t * head_contents), error) result Lwt.t
  val write:
       root:Fpath.t
    -> ?capacity:int
    -> raw:Cstruct.t
    -> t -> head_contents
    -> (unit, error) result Lwt.t
  val remove:
       root:Fpath.t
    -> t -> (unit, error) result Lwt.t
end

module Make (Hash : S.HASH): S with module Hash = Hash = struct

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

  module A = struct

    type nonrec t = head_contents

    open Angstrom

    let refname =
      take_while (function '\000' .. '\039' -> false
                         | '\127'           -> false
                         | '~' | '^'
                         | ':' | '?' | '*'
                         | '['              -> false
                         | _                -> true)

    let hash = take (Hash.Digest.length * 2)
      >>| Hash.of_hex

    let decoder =
      (string "ref: " *> refname <* end_of_line >>| fun refname -> Ref refname)
      <|> (hash <* end_of_line >>| fun hash -> Hash hash)

    (* XXX(dinosaure): [end_of_line] expect a LF at the end of the
       input. In the RFC, it's not clear if we need to write LF
       character or not. In general, we found the LF character but if
       we have a problem to read a reference, may be is about this
       issue. *)
  end

  module M = struct

    type t  = head_contents

    open Minienc

    let lf = '\n'

    let encoder x k e = match x with
      | Hash hash   -> write_string (Hash.to_hex hash) (write_char lf k) e
      | Ref refname ->
        (write_string "ref: "
         @@ write_string refname
         @@ write_char lf k)
          e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)
end

module IO (H : S.HASH) (FS: S.FS) = struct

  module FS = Helper.FS(FS)

  include Make(H)

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

  module ErrFS = Error.FS(FS)

  type error =
    [ ErrFS.t2
    | ErrFS.t3
    | Error.Decoder.t2
    | Error.Angstrom.t2 ]

  let pp_error ppf = function
    | #Error.Decoder.t2 as err -> Error.Decoder.t2_pp_error ppf err
    | #ErrFS.t2 as err -> ErrFS.t2_pp_error ppf err
    | #ErrFS.t3 as err -> ErrFS.t3_pp_error ppf err

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

  let mem ~root reference =
    let path = Fpath.(root // (to_path reference)) in
    FS.File.exists path >|= function
    | Ok v    -> v
    | Error _ -> false

  let read ~root reference ~dtmp ~raw : (_, error) result Lwt.t =
    let decoder = D.default dtmp in
    let path = Fpath.(root // (to_path reference)) in
    Log.debug (fun l -> l "Reading the reference: %a." Fpath.pp path);
    FS.with_open_r path @@ fun read ->
    let rec loop decoder = match D.eval decoder with
      | `End (_, value) -> Lwt.return (Ok value)
      | `Error (_, (#Error.Decoder.t0 as err)) ->
        Lwt.return Error.(v @@ Decoder.t2_of_t0 path err)
      | `Await decoder ->
        FS.File.read raw read >>= function
        | Error err -> Lwt.return Error.(v @@ ErrFS.err_read path err)
        | Ok 0 -> loop (D.finish decoder)
        (* XXX(dinosaure): in this case, we read a file, so when we
           retrieve 0 bytes, that means we get end of the file. We
           can finish the deserialization. *)
        | Ok n -> match D.refill (Cstruct.sub raw 0 n) decoder with
          | Ok decoder -> loop decoder
          | Error (#Error.Decoder.t0 as err) ->
            Lwt.return Error.(v @@ Decoder.t2_of_t0 path err)
    in
    loop decoder >|= function
    | Error _ as e     -> e
    | Ok head_contents ->
      let reference' = normalize path in
      Log.debug (fun l ->
          l "Normalize reference %a = %a." pp reference pp reference');
      assert (equal reference reference');
      Ok (normalize path, head_contents)

  let write ~root ?(capacity = 0x100) ~raw reference value =
    let state = E.default (capacity, value) in
    let path = Fpath.(root // (to_path reference)) in
    FS.Dir.create (Fpath.parent path)
    >>= function
    | Error err -> Lwt.return Error.(v @@ ErrFS.err_create (Fpath.parent path) err)
    | Ok (true | false) ->
      Encoder.to_file path raw state >|= function
      | Ok _                   -> Ok ()
      | Error #ErrFS.t2 as err -> err
      | Error #ErrFS.t3 as err -> err
      | Error (`Encoder #Error.never) -> assert false

  let remove ~root t =
    let path = Fpath.(root // (to_path t)) in
    FS.File.delete path >|= function
    | Ok _ as v -> v
    | Error err -> Error.(v @@ ErrFS.err_delete path err)

end
