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
  module Hash : Ihash.S
  module Path : Path.S
  module FileSystem : Fs.S

  type t = private string

  val head    : t
  val master  : t

  val is_head : t -> bool

  val of_string : string -> t
  val to_string : t -> string

  val of_path : Path.t -> t
  val to_path : t -> Path.t

  include S.BASE with type t := t

  type head_contents =
    | Hash of Hash.t
    | Ref of t

  val pp_head_contents : head_contents Fmt.t

  module A : S.ANGSTROM with type t = head_contents
  module D : S.DECODER with type t = head_contents
                        and type raw = Cstruct.t
                        and type init = Cstruct.t
                        and type error = [ `Decoder of string ]
  module M : S.MINIENC with type t = head_contents
  module E : S.ENCODER with type t = head_contents
                        and type raw = Cstruct.t
                        and type init = int * head_contents
                        and type error = [ `Never ]

  type error =
    [ `SystemFile of FileSystem.File.error
    | `SystemIO of string
    | D.error ]

  val pp_error  : error Fmt.t

  val read : root:Path.t -> t -> dtmp:Cstruct.t -> raw:Cstruct.t -> ((t * head_contents), error) result Lwt.t
  val write : root:Path.t -> ?capacity:int -> raw:Cstruct.t -> t -> head_contents -> (unit, error) result Lwt.t
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (P : Path.S)
    (FS : Fs.S with type path = P.t
                and type File.raw = Cstruct.t)
  : S with module Hash = H
       and module Path = P
       and module FileSystem = FS
= struct
  module Hash = H
  module Path = P
  module FileSystem = FS

  type t = string

  let head    = "HEAD"
  let is_head = String.equal head
  let master  = "refs/heads/master"

  let of_string x = x
  let to_string x = x

  let to_path x = match Path.of_string x with
    | Error (`Msg x) -> raise (Invalid_argument x)
    | Ok v -> v
  let of_path = Path.to_string

  let pp ppf x =
    Fmt.pf ppf "%s" (String.escaped x)

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

  module A =
  struct
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
  end

  module M =
  struct
    type t  = head_contents

    open Minienc

    let encoder x k e = match x with
      | Hash hash ->
        write_string (Hash.to_hex hash) k e
      | Ref refname ->
        (write_string "ref: "
         @@ write_string refname k)
          e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)

  type error =
    [ `SystemFile of FileSystem.File.error
    | `SystemIO of string
    | D.error ]

  let pp_error ppf = function
    | `SystemFile sys_err -> Helper.ppe ~name:"`SystemFile" FileSystem.File.pp_error ppf sys_err
    | `SystemIO sys_err -> Helper.ppe ~name:"`SystemIO" Fmt.string ppf sys_err
    | #D.error as err -> D.pp_error ppf err

  let normalize path =
    let segs = Path.segs path in

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
    |> fun (_, refs) -> List.rev refs |> String.concat "/"

  let read ~root reference ~dtmp ~raw =
    let decoder = D.default dtmp in

    let open Lwt.Infix in

    let path = Path.(root // (to_path reference)) in

    FileSystem.File.open_r ~mode:0o400 ~lock:(Lwt.return ()) path
    >>= function
    | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
    | Ok read ->
      let rec loop decoder = match D.eval decoder with
        | `Await decoder ->
          FileSystem.File.read raw read >>=
          (function
            | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
            | Ok n -> match D.refill (Cstruct.sub raw 0 n) decoder with
              | Ok decoder -> loop decoder
              | Error (#D.error as err) -> Lwt.return (Error err))
        | `End (rest, value) -> Lwt.return (Ok value)
        | `Error (res, (#D.error as err)) -> Lwt.return (Error err)
      in

      loop decoder
      >|= function
      | Ok head_contents ->
        let reference' = normalize path in

        assert (String.equal reference reference');

        Ok (normalize path, head_contents)
      | Error _ as e -> e

  let write ~root ?(capacity = 0x100) ~raw reference value =
    let open Lwt.Infix in

    let state = E.default (capacity, value) in

    let module E =
    struct
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

    let path = Path.(root // (to_path reference)) in

    FileSystem.File.open_w ~mode:0o644 ~lock:(Lwt.return ()) path
    >>= function
    | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
    | Ok write ->
      Helper.safe_encoder_to_file
        ~limit:50
        (module E)
        FileSystem.File.write
        write raw state
      >>= function
      | Ok _ -> FileSystem.File.close write >>=
        (function
          | Ok () -> Lwt.return (Ok ())
          | Error sys_err -> Lwt.return (Error (`SystemFile sys_err)))
      | Error err ->
        FileSystem.File.close write >>= function
        | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
        | Ok () -> match err with
          | `Stack -> Lwt.return (Error (`SystemIO (Fmt.strf "Impossible to store the reference: %a" pp reference)))
          | `Writer sys_err -> Lwt.return (Error (`SystemFile sys_err))
          | `Encoder `Never -> assert false
end
