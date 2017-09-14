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
  module Hash
    : S.HASH
  (** The [Hash] module used to make this interface. *)

  type entry =
    { perm : perm
    ; name : string
    ; node : Hash.t }
  and perm =
    [ `Normal (** A {!Blob.t}. *)
    | `Everybody
    | `Exec   (** An executable. *)
    | `Link   (** A {!Blob.t} that specifies the path of a {i symlink}. *)
    | `Dir    (** A sub-{!Tree.t}. *)
    | `Commit (** A sub-module ({!Commit.t}). *)
    ]
  and t = entry list
  (** A Git Tree object. Git stores content in a manner similar to a
      UNIX {i filesystem}, but a bit simplified. All the content is
      stored as tree and {Blob.t} objects, with trees corresponding to
      UNIX directory entries and blobs corresponding more or less to
      {i inodes} or file contents. A single tree object contains one
      or more tree entries, each of which contains a hash pointer to a
      {Blob.t} or sub-tree with its associated mode, type, and {i
      filename}. *)

  module D
    : S.DECODER  with type t = t
                       and type raw = Cstruct.t
                       and type init = Cstruct.t
                       and type error = [ `Decoder of string ]
  (** The decoder of the Git Tree object. We constraint the input to
      be a {Cstruct.t}. This decoder needs a {Cstruct.t} as an
      internal buffer. *)

  module A
    : S.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git Tree object. *)

  module F
    : S.FARADAY  with type t = t
  (** The Faraday encoder of the Git Tree object. *)

  module M
    : S.MINIENC  with type t = t
  (** The {!Minienc} encoder of the Git Tree object. *)

  module E : S.ENCODER  with type t = t
                         and type raw = Cstruct.t
                         and type init = int * t
                         and type error = [ `Never ]
  (** The encoder (which uses a {Minienc.encoder}) of the Git Tree
      object. We constraint the output to be a {Cstruct.t}. This
      encoder needs the Tree OCaml value and the memory consumption of
      the encoder (in bytes). The encoder can not fail.

      NOTE: we can not unspecified the error type (it needs to be
      concrete) but, because the encoder can not fail, we define the
      error as [`Never]. *)

  include S.DIGEST with type t := t and type hash = Hash.t
  include S.BASE with type t := t

  val hashes : t -> Hash.t list
  (** [hashes t] returns all pointer of the tree [t]. *)

  val list : t -> (string * Hash.t) list
end

module Make
    (H : S.HASH with type Digest.buffer = Cstruct.t
                 and type hex = string)
  : S with module Hash = H
(** The {i functor} to make the OCaml representation of the Git Tree
    object by a specific hash implementation. We constraint the
    {!S.HASH} module to compute a {Cstruct.t} flow and generate a
    [string] as the hexadecimal representation of the hash. *)
