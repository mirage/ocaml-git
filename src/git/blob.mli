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

(** A Git Blob object.

    A Blob object is used to store file data. *)

type t
(** Type of blob is contents of file. *)

val of_cstruct : Cstruct.t -> t
(** [of_cstruct cs] is the blob from the given {!Cstruct.t} [cs]. This function
    does not take the ownership on [cs].

    {b Note.} If the user set the given [cs], it updates the blob too. This
    function does not copy the given {!Cstruct.t}.

    {[
      let cs = Cstruct.of_string "Hello" in
      let blob = Blob.of_cstruct cs in
      Blob.digest blob ;;
      - : hash = 5ab2f8a4323abafb10abb68657d9d39f1a775057
      Cstruct.set_char cs 0 'V' ;;
      - : unit = ()
      Blob.digest blob ;;
      - : hash = 4081c40614b490bea69ea2ed8fdfdb86f04b5579
    ]} *)

val to_cstruct : t -> Cstruct.t
(** [to_cstruct blob] is the {!Cstruct.t} from the given blob. This function
    does not create a fresh {!Cstruct.t}, *)

val of_string : string -> t
(** [of_string str] is the blob from the given [string] [str].

    {b Note.} Despite {!of_cstruct}, [of_string] does a copy of the given
    [string]. *)

val to_string : t -> string
(** [to_string blob] is the [string] from the given blob. *)

val length : t -> int64
(** [length blob] is the length of the given blob object. *)

val pp : t Fmt.t
(** Pretty-printer for {!t}. *)

val equal : t -> t -> bool
(** The equal function for blobs. *)

val compare : t -> t -> int
(** The comparison function for [blobs]. *)

val hash : t -> int
(** [hash blob] associates a non-negative integer to any value of {!t}. It is
    guaranteed that if [x = y] or [compare x y = 0], then [hash x = hash y]. *)

module type S = sig
  type hash

  type nonrec t = t

  include S.DIGEST with type t := t and type hash := hash

  include S.BASE with type t := t

  val length : t -> int64

  val of_cstruct : Cstruct.t -> t
  (** [of_cstruct cs] is the blob from the given {!Cstruct.t} [cs]. This
      function does not take the ownership on [cs].

      {b Note.} If the user set the given [cs], it updates the blob too. This
      function does not copy the given {!Cstruct.t}.

      {[
        let cs = Cstruct.of_string "Hello" in
        let blob = Blob.of_cstruct cs in
        Blob.digest blob ;;
        - : hash = 5ab2f8a4323abafb10abb68657d9d39f1a775057
        Cstruct.set_char cs 0 'V' ;;
        - : unit = ()
        Blob.digest blob ;;
        - : hash = 4081c40614b490bea69ea2ed8fdfdb86f04b5579
      ]} *)

  val to_cstruct : t -> Cstruct.t
  (** [to_cstruct blob] is the {!Cstruct.t} from the given blob. This function
      does not create a fresh {!Cstruct.t}, *)

  val of_string : string -> t
  (** [of_string str] is the blob from the given [string] [str].

      {b Note.} Despite {!of_cstruct}, [of_string] does a copy of the given
      [string]. *)

  val to_string : t -> string
  (** [to_string blob] is the [string] from the given blob. *)
end

(** {i Functor} building an implementation of the blob structure. The {i
    functor} returns a structure containing a type [hash] of digests and a type
    [t] of blobs (structurally equal to {!t}). *)
module Make (Hash : S.HASH) : S with type hash = Hash.t
