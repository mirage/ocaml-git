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
  module Hash: S.HASH
  module Inflate: S.INFLATE

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := HDec

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PDec of PDec.error ]

  val pp_error: error Fmt.t

  type delta =
    | Unresolved of { hash: Hash.t; length: int; }
    | Internal of { hash: Hash.t; abs_off: int64; length: int; }
    | Delta of { hunks_descr: HDec.hunks; inserts: int; depth: int; from: delta; }

  val needed: delta -> int

  val pp_delta: delta Fmt.t

  type path = Load of int | Patch of { hunks: int; target: int; src: path; }

  type 'a t =
    { index : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
    ; delta : (int64, delta) Hashtbl.t
    ; hash_pack : Hash.t
    ; state : 'a }
  constraint 'a = [< `Pass | `Normalized of path | `Resolved of path ]

  val v: Hash.t -> [ `Pass ] t
  val normalize: length:int -> [ `Pass ] t -> [ `Normalized of path ] t
  val resolve: length:int -> [ `Normalized of path ] t -> [ `Resolved of path ] t

  val first_pass:
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> ?idx:(Hash.t -> (Crc32.t * int64) option)
    -> (unit -> Cstruct.t option Lwt.t)
    -> ([ `Normalized of path ] t, error) result Lwt.t
end

module Make
    (Hash: S.HASH)
    (Inflate: S.INFLATE)
    (HDec: Unpack.H with module Hash := Hash)
    (PDec: Unpack.P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := HDec)
  : S with module Hash = Hash
       and module Inflate = Inflate
       and module HDec := HDec
       and module PDec := PDec
