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
     and type error = [ `Decoder of string ]
  module M: S.MINIENC with type t = head_contents
  module E: S.ENCODER
    with type t = head_contents
     and type init = int * head_contents
     and type error = [ `Never ]
end

module type IO = sig
  module Lock: S.LOCK
  module FS: S.FS

  include S

  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | D.error ]

  val pp_error  : error Fmt.t

  val mem :
       root:Fpath.t
    -> t -> (bool, error) result Lwt.t
  val read :
       root:Fpath.t
    -> t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> ((t * head_contents), error) result Lwt.t
  val write :
       root:Fpath.t
    -> ?locks:Lock.t
    -> ?capacity:int
    -> raw:Cstruct.t
    -> t -> head_contents
    -> (unit, error) result Lwt.t
  val test_and_set :
       root:Fpath.t
    -> ?locks:Lock.t
    -> t
    -> test:head_contents option
    -> set:head_contents option
    -> (bool, error) result Lwt.t
  val remove :
       root:Fpath.t
    -> ?locks:Lock.t
    -> t -> (unit, error) result Lwt.t
end

module Make(H : S.HASH): S with module Hash = H = struct
  module Hash = H

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

module IO
    (H : S.HASH)
    (L : S.LOCK)
    (FS: S.FS with type File.lock = L.elt)
  : IO with module Hash = H
        and module Lock = L
        and module FS = FS
= struct
  module Lock = L
  module FS = FS

  include Make(H)

  (* XXX(samoht): why this doesn't use the serializer? *)
  let head_contents_to_string = function
    | Hash hash   -> Fmt.strf "%s\n" (Hash.to_hex hash)
    | Ref refname -> Fmt.strf "ref: %a\n" pp refname (* XXX(dinosaure): [pp] or [Fmt.string]? *)

  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | D.error ]

  let pp_error ppf = function
    | `SystemFile sys_err -> Helper.ppe ~name:"`SystemFile" FS.File.pp_error ppf sys_err
    | `SystemDirectory sys_err -> Helper.ppe ~name:"`SystemDirectory" FS.Dir.pp_error ppf sys_err
    | `SystemIO sys_err -> Helper.ppe ~name:"`SystemIO" Fmt.string ppf sys_err
    | #D.error as err -> D.pp_error ppf err

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
    FS.File.exists path >>= function
    | Ok v -> Lwt.return (Ok v)
    | Error err ->
      Lwt.return (Error (`SystemFile err))

  let read ~root reference ~dtmp ~raw =
    let decoder = D.default dtmp in
    let path = Fpath.(root // (to_path reference)) in
    Log.debug (fun l -> l ~header:"read" "Reading the reference: %a." Fpath.pp path);
    FS.File.open_r ~mode:0o400 path >>= function
    | Error sys_err ->
      Log.err (fun l ->
          l "Got an error while reading %a: %a."
            pp reference FS.File.pp_error sys_err);
      Lwt.return (Error (`SystemFile sys_err))
    | Ok read ->
      let rec loop decoder = match D.eval decoder with
        | `End (_, value)               -> Lwt.return (Ok value)
        | `Error (_, (#D.error as err)) -> Lwt.return (Error err)
        | `Await decoder ->
          FS.File.read raw read >>= function
          | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
          | Ok n -> match D.refill (Cstruct.sub raw 0 n) decoder with
            | Ok decoder              -> loop decoder
            | Error (#D.error as err) -> Lwt.return (Error err)
      in
      loop decoder >|= function
      | Error _ as e     -> e
      | Ok head_contents ->
        let reference' = normalize path in
        Log.debug (fun l ->
            l "Normalize reference %a = %a." pp reference pp reference');
        assert (equal reference reference');
        Ok (normalize path, head_contents)

  let write ~root ?locks ?(capacity = 0x100) ~raw reference value =
    let state = E.default (capacity, value) in
    let module E = struct
      type state  = E.encoder
      type raw    = Cstruct.t
      type result = int
      type error  = E.error
      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit
      type rest = [ `Flush of state | `End of (state * result) ]
      let eval raw state = match E.eval raw state with
        | `Error err -> Lwt.return (`Error (state, err))
        | #rest as rest -> Lwt.return rest
      let used  = E.used
      let flush = E.flush
    end in
    let path = Fpath.(root // (to_path reference)) in
    let lock = match locks with
      | Some locks -> Some (Lock.make locks (to_path reference))
      | None       -> None
    in
    Lock.with_lock lock @@ fun () ->
    FS.Dir.create ~path:true Fpath.(root // (parent (to_path reference)))
    >>= function
    | Error sys_err -> Lwt.return (Error (`SystemDirectory sys_err))
    | Ok (true | false) ->
      FS.File.open_w ~mode:0o644 path
      >>= function
      | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
      | Ok write ->
        Helper.safe_encoder_to_file ~limit:50 (module E)
          FS.File.write write raw state
        >>= function
        | Ok _ ->
          FS.File.close write >|=
          (function
            | Ok () -> Ok ()
            | Error sys_err -> Error (`SystemFile sys_err))
        | Error err ->
          FS.File.close write >|= function
          | Error sys_err -> Error (`SystemFile sys_err)
          | Ok ()         -> match err with
            | `Writer sys_err -> Error (`SystemFile sys_err)
            | `Encoder `Never -> assert false
            | `Stack          ->
              Fmt.kstrf (fun x -> Error (`SystemIO x))
                "Impossible to store the reference: %a" pp reference

  (* FIXME: why this doesn't use the encode??? *)
  let test_and_set ~root ?locks t ~test ~set =
    let path = Fpath.(root // (to_path t)) in
    let lock = match locks with
      | Some locks -> Some (Lock.make locks (to_path t))
      | None -> None
    in
    let raw = function
      | None       -> None
      | Some value -> Some (Cstruct.of_string (head_contents_to_string value)) (* XXX: why a copy? *)
    in
    FS.File.test_and_set
      ?lock
      path
      ~test:(raw test)
      ~set:(raw set)
    >>= function
    | Ok _ as v -> Lwt.return v
    | Error err -> Lwt.return (Error (`SystemFile err))

  let remove ~root ?locks t =
    let path = Fpath.(root // (to_path t)) in
    let lock = match locks with
      | Some locks -> Some (Lock.make locks (to_path t))
      | None       -> None
    in
    FS.File.delete ?lock path >|= function
    | Ok _ as v -> v
    | Error err -> Error (`SystemFile err)

end
