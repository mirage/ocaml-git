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

type tz_offset = {sign: [`Plus | `Minus]; hours: int; minutes: int}
type t = {name: string; email: string; date: int64 * tz_offset option}

let pp_sign ppf = function
  | `Plus -> Fmt.pf ppf "`Plus"
  | `Minus -> Fmt.pf ppf "`Minus"

let pp_tz_offset ppf {sign; hours; minutes} =
  Fmt.pf ppf "{ @[<hov>sign = %a;@ hours = %02d;@ minutes = %02d;@] }"
    (Fmt.hvbox pp_sign) sign hours minutes

let pp ppf {name; email; date= n, tz_offset} =
  Fmt.pf ppf "{ @[<hov>name = %s;@ email = %s;@ date = %a;@] }" name email
    (Fmt.hvbox (Fmt.pair Fmt.int64 (Fmt.option pp_tz_offset)))
    (n, tz_offset)

module Make (Meta : Encore.Meta.S) = struct
  type e = t

  open Helper.BaseIso

  module Iso = struct
    open Encore.Bijection

    let tz_offset =
      make_exn
        ~tag:("sign * int * int", "tz_offset")
        ~fwd:(fun (sign, hours, minutes) ->
          if hours = 0 && minutes = 0 then None else Some {sign; hours; minutes}
          )
        ~bwd:(function
          | Some {sign; hours; minutes} -> sign, hours, minutes
          | None -> `Plus, 0, 0)

    let user =
      make_exn
        ~tag:("string * string * int64 * tz_offset", "user")
        ~fwd:(fun (name, email, time, date) -> {name; email; date= time, date})
        ~bwd:(fun {name; email; date= time, date} -> name, email, time, date)
  end

  module Meta = Encore.Meta.Make (Meta)
  open Encore.Bijection
  open Meta

  type 'a t = 'a Meta.t

  let is_not_lt chr = chr <> '<'
  let is_not_gt chr = chr <> '>'
  let is_digit = function '0' .. '9' -> true | _ -> false

  let date =
    let sign =
      make_exn
        ~tag:("unit", "[ `Plus | `Minus ]")
        ~fwd:(function
          | '+' -> `Plus
          | '-' -> `Minus
          | chr ->
              Exn.fail
                (Fmt.strf "char:%02x" (Char.code chr))
                "[ `Plus | `Minus ]")
        ~bwd:(function `Plus -> '+' | `Minus -> '-')
      <$> any
    in
    let digit2 =
      make_exn ~tag:("char * char", "int")
        ~fwd:(function
          | ('0' .. '9' as a), ('0' .. '9' as b) ->
              Char.(((code a - 48) * 10) + (code b - 48))
          | _, _ -> Exn.fail "char * char" "int")
        ~bwd:(fun n ->
          let a, b = n / 10, n mod 10 in
          Char.chr (a + 48), Char.chr (b + 48) )
      <$> (any <*> any)
    in
    Exn.compose obj3 Iso.tz_offset <$> (sign <*> digit2 <*> digit2)

  let chop =
    make_exn ~tag:("string", "chop string")
      ~fwd:(fun s -> String.sub s 0 (String.length s - 1))
      ~bwd:(fun s -> s ^ " ")

  let p =
    Exn.compose obj4 Iso.user
    <$> ( chop
        <$> (while1 is_not_lt <* (char_elt '<' <$> any))
        <*> (while1 is_not_gt <* (string_elt "> " <$> const "> "))
        <*> (int64 <$> while1 is_digit <* (char_elt ' ' <$> any))
        <*> date )
end

module A = Make (Encore.Proxy_decoder.Impl)
module M = Make (Encore.Proxy_encoder.Impl)
module D = Helper.MakeDecoder (A)
module E = Helper.MakeEncoder (M)

let length t =
  let string x = Int64.of_int (String.length x) in
  let ( + ) = Int64.add in
  let tz_offset_length = 5L in
  string t.name
  + 1L
  + 1L
  + string t.email
  + 1L
  + 1L
  + string (Int64.to_string (fst t.date))
  + 1L
  + tz_offset_length

let equal = ( = )
let compare = Pervasives.compare
let hash = Hashtbl.hash

module Set = Set.Make (struct type nonrec t = t

                              let compare = compare end)

module Map = Map.Make (struct type nonrec t = t

                              let compare = compare end)
