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

(** An implementation of the {!Buffer} module but with
    {!Cstruct.t}. *)

type t
(** The abstract type of buffers. *)

type raw = string
(** The type of result. *)

type fixe = Cstruct.t
(** The type of input. *)

val create: int -> t
(** [create n] returns a fresh buffer, initially empty. The [n]
    parameter is the initial size of the internal byte sequence that
    holds the buffer contents. That byte sequence is automatically
    reallocated when more than [n] bytes are stored in the buffer, but
    shrinks back to [n] bytes when [reset] is called.

    For best performance, [n] should be of the same order of magnitude
    as the number of characters that are expected to be stored in the
    buffer. Nothing bad will happen if the buffer grows beyond that
    limit, however. *)

val contents: t -> raw
(** Return a copy of the current contents of the buffer. The buffer
    itself is unchanged. *)

val unsafe_contents: t -> fixe

val has: t -> int

val add: t -> fixe -> unit
(** [add fixe buffer] appends the fixed-size buffer [fixe] at the
    end of the buffer [buffer]. *)

val clear: t -> unit
(** Empty the buffer. *)

val reset: t -> unit
(** Empty the buffer and {i de-allocate} the internal byte sequence
    internal byte sequence of length [n] that was allocated by
    {!Buffer.create} [n]. For long-lived buffers that may have grown a
    lot, [reset] allows faster reclamation of the space used by the
    buffer. *)
