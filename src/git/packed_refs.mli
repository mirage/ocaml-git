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

module Make (H : S.HASH) (FS : S.FS) :
  S with module FS := FS and module Hash := H
