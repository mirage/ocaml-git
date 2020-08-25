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

(** The Value module which represents the Git object. *)
type 'hash t =
  | Blob of Blob.t
  | Commit of 'hash Commit.t
  | Tree of 'hash Tree.t
  | Tag of 'hash Tag.t

(** Interface which describes the Git object. *)
module type S = sig
  type hash

  type nonrec t = hash t
  (** OCaml value which represents a Git object. *)

  module Blob : Blob.S with type hash = hash

  module Commit : Commit.S with type hash = hash

  module Tree : Tree.S with type hash = hash

  module Tag : Tag.S with type hash = hash

  val blob : Blob.t -> t

  val commit : Commit.t -> t

  val tree : Tree.t -> t

  val tag : Tag.t -> t

  val kind : t -> [ `Commit | `Blob | `Tree | `Tag ]
  (** [kind o] returns the kind of the Git object. *)

  val format : t Encore.t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64

  val to_raw : t -> string

  val to_raw_without_header : t -> string

  val of_raw_with_header :
    ?off:int -> ?len:int -> string -> (t, [> `Msg of string ]) result

  val of_raw :
    kind:[ `Commit | `Blob | `Tree | `Tag ] ->
    Cstruct.t ->
    (t, [> `Msg of string ]) result

  val stream : t -> unit -> string option Lwt.t
end

(** The {i functor} to make the OCaml representation of the Git object by a
    specific hash implementation, an {!S.INFLATE} implementation for the
    decompression and a {!S.DEFLATE} implementation for the compression.

    The constraints on git objects can cut the path to let the user to make the
    value module and keep the structural equality. If the [Hash] module is the
    same. That means:

    ```ocaml module V1 = Value.Make(SHA1)(C_inflate)(C_deflate) module V2 =
    Value.Make(SHA1)(OCaml_inflate)(OCaml_deflate) ```

    Types [V1.t] and [V2.t] are equal. *)
module Make (Hash : S.HASH) (Inflate : S.INFLATE) (Deflate : S.DEFLATE) :
  S
    with module Hash := Hash
     and module Inflate := Inflate
     and module Deflate := Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)

