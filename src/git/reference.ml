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

end

type ('t, 'uid, 'error, 's) store = {
  atomic_wr : 't -> t -> string -> ((unit, 'error) result, 's) io;
  atomic_rd : 't -> t -> ((string, 'error) result, 's) io;
  uid_of_hex : string -> 'uid option;
  uid_to_hex : 'uid -> string;
  packed : 'uid Packed.packed;
}

let reword_error f = function Ok v -> Ok v | Error err -> Error (f err)

let contents store str =
  match store.uid_of_hex (String.trim str) with
  | Some uid -> Uid uid
  | None -> (
      let is_sep chr = Astring.Char.Ascii.is_white chr || chr = ':' in
      match Astring.String.fields ~empty:false ~is_sep str with
      | [ _ref; value ] -> Ref (v value)
      | _ -> Fmt.invalid_arg "Invalid reference contents: %S" str)

let resolve { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  let rec go visited reference =
    store.atomic_rd t reference >>= function
    | Error _ -> (
        match Packed.get reference store.packed with
        | Some uid -> return (Ok uid)
        | None -> return (Error (`Not_found reference)))
    | Ok str ->
    match contents store str with
    | Uid uid -> return (Ok uid)
    | Ref reference ->
        if List.exists (equal reference) visited
        then return (Error `Cycle)
        else go (reference :: visited) reference in
  go [ reference ] reference

let read { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  store.atomic_rd t reference >>= function
  | Error _ -> (
      match Packed.get reference store.packed with
      | Some uid -> return (Ok (Uid uid))
      | None -> return (Error (`Not_found reference)))
  | Ok str -> return (Ok (contents store str))

let write { Carton.bind; Carton.return } t store reference contents =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in

  let str =
    match contents with
    | Uid uid -> Fmt.strf "%s\n" (store.uid_to_hex uid)
    | Ref t -> Fmt.strf "ref: %s\n" t in
  store.atomic_wr t reference str >>| reword_error (fun err -> `Store err)


end

