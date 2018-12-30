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
  module FS : S.FS
  module Hash : S.HASH
  module Deflate : S.DEFLATE
  module Inflate : S.INFLATE

  module Value :
    Value.S
    with module Hash := Hash
     and module Deflate := Deflate
     and module Inflate := Inflate

  include module type of Value

  type error =
    [ Error.Decoder.t
    | Inflate.error Error.Inf.t
    | Deflate.error Error.Def.t
    | FS.error Error.FS.t ]

  type kind = [`Commit | `Tree | `Tag | `Blob]

  val pp_error : error Fmt.t
  val mem : fs:FS.t -> root:Fpath.t -> Hash.t -> bool Lwt.t

  val read :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (t, error) result Lwt.t
  (** [read ~fs ~root ~window ~ztmp ~dtmp ~raw hash] tries to extract from a
      loose file localized in [root / objects] a Git object. This function
      reads (uses [raw] buffer to read) the file, inflates (uses [ztmp] and
      [window] to inflate) and decodes (uses [dtmp] to decode) Git object and
      make a [Value.t]. *)

  val read_inflated :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (kind * Cstruct.t, error) result Lwt.t
  (** [read_inflated ~fs ~root ~window ~ztmp ~dtmp ~raw hash] tries to extract
      from a loose file localized in [root / objects] a Git object. This
      function reads (uses [raw] buffer to read) the file, inflates (uses
      [ztmp] and [window] to inflate) and decodes only Git header and body
      (uses [dtmp] to decode) Git object and make a fresh buffer.

      Body is the serialization of the Git object, and, from [kind] (kind of
      Git object), client can call decoder associated to the kind returned
      (like [Tree.Decoder] for example). *)

  val read_inflated_without_allocation :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> result:Cstruct.t
    -> Hash.t
    -> (kind * Cstruct.t, error) result Lwt.t
  (** [read_inflated_without_allocation ~fs ~root ~window ~ztmp ~dtmp ~raw
      ~result hash] is the same as {!read_inflated}. However, instead to make a
      fresh buffer which will contain the Git object serialized, it uses
      [result] to store it. This function does not allocate any buffer. *)

  val list : fs:FS.t -> root:Fpath.t -> Hash.t list Lwt.t

  val size :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (int64, error) result Lwt.t

  val write :
       fs:FS.t
    -> root:Fpath.t
    -> temp_dir:Fpath.t
    -> etmp:Cstruct.t
    -> ?level:int
    -> ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> t
    -> (Hash.t * int, error) result Lwt.t

  val write_deflated :
       fs:FS.t
    -> root:Fpath.t
    -> temp_dir:Fpath.t
    -> ?level:int
    -> raw:Cstruct.t
    -> kind:kind
    -> Cstruct.t
    -> (Hash.t, error) result Lwt.t
end

module Make (H : S.HASH) (FS : S.FS) (I : S.INFLATE) (D : S.DEFLATE) :
  S
  with module Hash := H
   and module Inflate := I
   and module Deflate := D
   and module FS = FS
   and module Blob = Blob.Make(H)
   and module Commit = Commit.Make(H)
   and module Tree = Tree.Make(H)
   and module Tag = Tag.Make(H)
   and type t = Value.Make(H)(I)(D).t
