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
  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS: S.FS

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := HDec
  module PInfo: Pack_info.S
    with module Hash := Hash
     and module Inflate := Inflate
     and module HDec := HDec
     and module PDec := PDec
  module RPDec: Unpack.D
    with module Hash := Hash
     and type Mapper.fd = FS.Mapper.fd
     and type Mapper.error = FS.error
     and module Inflate := Inflate
     and module Hunk := HDec
     and module Pack := PDec

  type status =
    | Resolved of Crc32.t * Hash.t
    | Root
    | Unresolved

  val pp_status: status Fmt.t

  val second_pass:
    RPDec.pack -> [ `Normalized of PInfo.path ] PInfo.t ->
    (int64 * (PInfo.delta * status)) array Lwt.t
end

module Make
    (Hash: S.HASH)
    (FS: S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
    (HDec: Unpack.H with module Hash := Hash)
    (PDec: Unpack.P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := HDec)
    (PInfo: Pack_info.S with module Hash := Hash
                         and module Inflate := Inflate
                         and module HDec := HDec
                         and module PDec := PDec)
    (RPDec: Unpack.D with module Hash := Hash
                      and type Mapper.fd = FS.Mapper.fd
                      and type Mapper.error = FS.error
                      and module Inflate := Inflate
                      and module Hunk := HDec
                      and module Pack := PDec)
  : S with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS
       and module HDec := HDec
       and module PDec := PDec
       and module PInfo := PInfo
       and module RPDec := RPDec
