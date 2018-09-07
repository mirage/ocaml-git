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

  module Hash   : S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS     : S.FS

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash    := Hash
     and module Inflate := Inflate
     and module Hunk    := HDec
  module RPDec: Unpack.D
    with module Hash    := Hash
     and module Inflate := Inflate
     and module Hunk    := HDec
     and module Pack    := PDec
     and module Mapper  := FS.Mapper

  module PEnc: Pack.P
    with module Hash    := Hash
     and module Deflate := Deflate

  module IDec: Index_pack.LAZY
    with module Hash := Hash

  module IEnc: Index_pack.ENCODER
    with module Hash := Hash

  module PInfo: Pack_info.S
    with module Hash    := Hash
     and module Inflate := Inflate
     and module HDec    := HDec
     and module PDec    := PDec

  type t

  type ('mmu, 'location) r =
    { mmu         : 'mmu
    ; with_cstruct: 'mmu -> pack -> int -> (('location * Cstruct.t) -> unit Lwt.t) -> unit Lwt.t
    ; free        : 'mmu -> 'location -> unit Lwt.t }
  and pack = Pack of Hash.t | Unrecorded

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Error.Decoder.t
    | `Invalid_hash of Hash.t
    | `Delta of PEnc.Delta.error
    | `Not_found ]

  val pp_error: error Fmt.t

  val v: FS.t -> Fpath.t list -> t Lwt.t
  val lookup: t -> Hash.t -> (Hash.t * (Crc32.t * int64)) option
  val list: t -> Hash.t list
  val mem: t -> Hash.t -> bool

  val add:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Fpath.t
    -> [ `Normalized of PInfo.path ] PInfo.t
    -> (Hash.t * int, error) result Lwt.t

  val read:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> to_result:((RPDec.kind * Cstruct.t * int * RPDec.Ascendant.s) -> ('value, error) result Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Hash.t
    -> ('value, error) result Lwt.t

  val size:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> t
    -> Hash.t
    -> (int, error) result Lwt.t
end

module Make
    (Hash   : S.HASH)
    (FS     : S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
    (HDec: Unpack.H with module Hash    := Hash)
    (PDec: Unpack.P with module Hash    := Hash
                     and module Inflate := Inflate
                     and module Hunk    := HDec)
    (RPDec: Unpack.D with module Hash    := Hash
                      and module Inflate := Inflate
                      and module Hunk    := HDec
                      and module Pack    := PDec
                      and module Mapper  := FS.Mapper)
  : S with module Hash    := Hash
       and module Inflate := Inflate
       and module Deflate := Deflate
       and module FS      := FS
       and module HDec    := HDec
       and module PDec    := PDec
       and module RPDec   := RPDec
