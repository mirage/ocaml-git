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

(** A bucket is a simple option array to contain something. When the
    bucket is full and you try to add a new value, we erase the oldest
    one. *)

type 'a t
(** The type of the bucket. *)

val make: int -> 'a t
(** Make a new bucket with a specified size. *)

val add: 'a t -> 'a -> unit
(** [add t v] adds the new value [v] in the bucket [t]. If the bucket
    is full, we erase by [v] the oldest value of [t]. *)

val iter: 'a t -> ('a -> unit) -> unit
(** [iter t f] applies [f] to all values in the bucket [ŧ]. [f]
    receives each value. *)

val find: 'a t -> ('a -> bool) -> 'a option
(** [find t f] returns the first value which respects the predicate
    [f]. Otherwise, it returns [None]. *)
