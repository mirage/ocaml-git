(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Astring

type entry =
  [ `Newline
  | `Comment of string
  | `Entry of (Hash.t * Reference.t) ]

let to_line ppf = function
  | `Newline -> Format.fprintf ppf "\n"
  | `Comment c -> Format.fprintf ppf "# %s\n" c
  | `Entry (s,r) ->
    Format.fprintf ppf "%s %s" (Hash.to_hex s) (Reference.to_raw r)

module T = struct
  type t = entry list
  let equal t1 t2 = List.length t1 = List.length t2 && t1 = t2
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let pp ppf t = List.iter (to_line ppf) t
  let pretty = Misc.pretty pp
end

include T

let find t r =
  let rec aux = function
    | [] -> None
    | (`Newline | `Comment _) :: t -> aux t
    | `Entry (x, y) :: t -> if Reference.equal y r then Some x else aux t
  in
  aux t

module Set = Set.Make(Reference)

let references (t:t) =
  let rec aux acc = function
    | [] -> Set.elements acc
    | (`Newline | `Comment _) :: t -> aux acc t
    | `Entry (_, r) :: t -> aux (Set.add r acc) t
  in
  aux Set.empty t

module IO (D: Hash.DIGEST) = struct

  module Hash_IO = Hash.IO(D)
  include T

  let of_line line =
    let line = String.trim line in
    if String.length line = 0 then Some `Newline
    else if line.[0] = '#' then
      let str =
        String.sub line ~start:1 ~stop:(String.length line - 1)
        |> String.Sub.to_string
      in
      Some (`Comment str)
    else match String.cut line ~sep:" " with
      | None  -> None
      | Some (h, r) ->
        let h = Hash_IO.of_hex h in
        let r = Reference.of_raw r in
        Some (`Entry (h, r))

  let add buf ?level:_ t =
    let ppf = Format.formatter_of_buffer buf in
    List.iter (to_line ppf) t

  let input buf =
    let rec aux acc =
      let line, cont = match Mstruct.get_string_delim buf '\n' with
        | None   -> Mstruct.to_string buf, false
        | Some s -> s, true
      in
      let acc = match of_line line with
        | None   -> acc
        | Some e -> e :: acc
      in
      if cont then aux acc else List.rev acc
    in
    aux []

end
