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

open Decompress
module Inflate = Zlib_inflate

type t = (B.Bigstring.t, B.Bigstring.t) Inflate.t
type error = [`Inflate of Inflate.error]
type window = B.Bigstring.t Window.t

module Log = struct
  let src = Logs.Src.create "decompress.inflate" ~doc:"logs inflate event"

  include (val Logs.src_log src : Logs.LOG)
end

let pp_error : error Fmt.t =
 fun ppf -> function
  | `Inflate err -> Fmt.pf ppf "(`Inflate %a)" (Fmt.hvbox Inflate.pp_error) err

let pp : t Fmt.t = Inflate.pp
let window_reset : window -> window = Window.reset
let window () = Window.create ~witness:B.bigstring
let default : window -> t = fun w -> Inflate.default ~witness:B.bigstring w

let eval ~src ~dst t :
    [`Await of t | `End of t | `Error of t * error | `Flush of t] =
  Log.debug (fun l -> l "evaluation of %a." (Fmt.hvbox Inflate.pp) t) ;
  let src' = Cstruct.to_bigarray src in
  let dst' = Cstruct.to_bigarray dst in
  Inflate.eval src' dst' t
  |> function
  | `Error (t, err) -> `Error (t, `Inflate err)
  | (`Await _ | `Flush _ | `End _) as x -> x

let used_in : t -> int = Inflate.used_in
let used_out : t -> int = Inflate.used_out
let write : t -> int = Inflate.write
let flush : int -> int -> t -> t = Inflate.flush
let refill : int -> int -> t -> t = Inflate.refill
