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
    : Ihash.S

  module Path
    : Path.S

  module Inflate
    : S.INFLATE

  module Deflate
    : S.DEFLATE

  module Buffer
    : Value.BUFFER

  module Value
    : Value.RAW
      with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module Buffer = Buffer

  module Reference
    : Reference.S
      with module Hash = Hash
       and module Path = Path

  type t

  val create : ?root:string -> ?dot_git:string -> ?level:int -> unit -> t Lwt.t

  val dot_git : t -> string
  val root : t -> string
  val level : t -> int

  val dump : Format.formatter -> t -> unit Lwt.t
  val contents : t -> (Hash.t * Value.t) list Lwt.t
  val size : t -> Hash.t -> int option Lwt.t
  val read : t -> Hash.t -> Value.t option Lwt.t
  val read_exn : t -> Hash.t -> Value.t Lwt.t
  val mem : t -> Hash.t -> bool Lwt.t
  val list : t -> Hash.t list Lwt.t
  val write : t -> Value.t -> Hash.t Lwt.t
  val references : t -> Reference.t list Lwt.t
  val mem_reference : t -> Reference.t -> bool Lwt.t
  val read_reference : t -> Reference.t -> Hash.t option Lwt.t
  val read_reference_exn : t -> Reference.t -> Hash.t Lwt.t
  val write_head : t -> Reference.head_contents -> unit Lwt.t
  val read_head : t -> Reference.head_contents option Lwt.t
  val write_reference : t -> Reference.t -> Hash.t -> unit Lwt.t
  val remove_reference : t -> Reference.t -> unit Lwt.t
  val test_and_set_reference : t -> Reference.t -> test:Hash.t option -> set:Hash.t option -> bool Lwt.t
  val read_inflated : t -> Hash.t -> ([ `Commit | `Tag | `Blob | `Tree ] * string) option Lwt.t
  val write_inflated : t -> kind:[ `Commit | `Tree | `Blob | `Tag ] -> Cstruct.t -> Hash.t Lwt.t
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (P : Path.S)
    (L : Lock.S with type key = P.t)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    (B : Value.BUFFER with type raw = string
                       and type fixe = Cstruct.t)
  : S with module Hash = H
       and module Path = P
       and module Lock = L
       and module Inflate = I
       and module Deflate = D
       and module Buffer = B
