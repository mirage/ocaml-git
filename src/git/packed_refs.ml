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
  module Hash : S.HASH
  module FS : S.FS

  type t = [`Peeled of Hash.t | `Ref of string * Hash.t] list

  module A : S.DESC with type 'a t = 'a Angstrom.t with type e = t
  module M : S.DESC with type 'a t = 'a Encore.Encoder.t with type e = t

  module D :
    S.DECODER
    with type t = t
     and type init = Cstruct.t
     and type error = Error.Decoder.t

  module E : S.ENCODER with type t = t and type init = int * t

  type error = [Error.Decoder.t | FS.error Error.FS.t]

  val pp_error : error Fmt.t

  val write :
       fs:FS.t
    -> root:Fpath.t
    -> temp_dir:Fpath.t
    -> ?capacity:int
    -> raw:Cstruct.t
    -> t
    -> (unit, error) result Lwt.t

  val read :
       fs:FS.t
    -> root:Fpath.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> (t, error) result Lwt.t
end

module Make (Hash : S.HASH) (FS : S.FS) = struct
  module FS = Helper.FS (FS)

  type t = [`Peeled of hash | `Ref of string * hash] list

  and hash = Hash.t

  module A = struct
    type e = t

    open Angstrom

    type 'a t = 'a Angstrom.t

    let hash = take (Hash.digest_size * 2) >>| Hash.of_hex

    let end_of_line =
      skip_while (function '\r' | '\n' -> false | _ -> true) *> peek_char
      >>= function
      | Some '\n' -> take 1 >>= fun _ -> return ()
      | Some '\r' -> take 2 >>= fun _ -> return ()
      | Some _ -> assert false
      | None -> return ()

    let info =
      option false (char '^' *> return true)
      >>= fun peeled ->
      hash
      >>= fun hash ->
      match peeled with
      | true -> end_of_line *> return (`Peeled hash)
      | false ->
          take_while (function '\x20' | '\x09' -> true | _ -> false)
          *> take_while (function
               | '\000' .. '\039' -> false
               | '\127' -> false
               | '~' | '^' | ':' | '?' | '*' | '[' -> false
               | _ -> true )
          >>= fun refname ->
          end_of_line >>= fun () -> return (`Ref (refname, hash))

    let p =
      fix
      @@ fun m ->
      peek_char
      >>= function
      | Some '#' -> end_of_line *> m
      | Some _ -> info >>= fun x -> m >>= fun r -> return (x :: r)
      | None -> return []
  end

  module M = struct
    type e = t

    open Encore.Lole

    type 'a t = 'a Encore.Encoder.t

    let write_newline k e =
      if Sys.win32 then write_string "\r\n" k e else write_string "\n" k e

    let write_hash x k e = write_string (Hash.to_hex x) k e

    let write_info x k e =
      match x with
      | `Peeled hash -> (write_char '^' @@ write_hash hash k) e
      | `Ref (refname, hash) ->
          (write_hash hash @@ write_char ' ' @@ write_string refname k) e

    let write_list ?(sep = fun k e -> k e) write_data lst k e =
      let rec aux l e =
        match l with
        | [] -> k e
        | [x] -> write_data x k e
        | x :: r -> (write_data x @@ sep @@ aux r) e
      in
      aux lst e

    let encoder l k e =
      ( write_string "# pack-refs with: peeled fully-peeled"
      @@ write_newline
      @@ write_list ~sep:write_newline write_info l k )
        e

    let p =
      let module M = Encore.Encoder.Make (struct
        type a = e

        let run k e l = encoder l k e
      end) in
      M.x
  end

  module D = Helper.MakeDecoder (A)
  module E = Helper.MakeEncoder (M)

  type fs_error = FS.error Error.FS.t
  type error = [fs_error | Error.Decoder.t]

  let pp_error ppf = function
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #fs_error as err -> Error.FS.pp_error FS.pp_error ppf err

  open Lwt.Infix
  module Decoder = Helper.Decoder (D) (FS)

  let read ~fs ~root ~dtmp ~raw =
    let state = D.default dtmp in
    let file = Fpath.(root / "packed-refs") in
    Decoder.of_file fs file raw state
    >|= function
    | Ok _ as v -> v
    | Error (`Decoder err) -> Error.(v @@ Error.Decoder.with_path file err)
    | Error #fs_error as err -> err

  module Encoder = struct
    module E = struct
      type state = E.encoder
      type result = int
      type error = E.error
      type rest = [`Flush of state | `End of state * result]

      let used = E.used
      let flush = E.flush

      let eval raw state =
        match E.eval raw state with
        | `Error err -> Lwt.return (`Error (state, err))
        | #rest as rest -> Lwt.return rest
    end

    include Helper.Encoder (E) (FS)
  end

  let write ~fs ~root ~temp_dir ?(capacity = 0x100) ~raw value =
    let state = E.default (capacity, value) in
    let path = Fpath.(root / "packed-refs") in
    Encoder.to_file fs ~temp_dir path raw state
    >|= function
    | Ok _ -> Ok ()
    | Error #fs_error as err -> err
    | Error (`Encoder #Error.never) -> assert false
end
