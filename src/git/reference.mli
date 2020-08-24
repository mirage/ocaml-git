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

(** The Git Reference module. *)




























end

(** The interface which describes any I/O operations on Git reference. *)
module type IO = sig
  module FS : S.FS

  include S

  type error = [ Error.Decoder.t | FS.error Error.FS.t ]
  (** The type of error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val mem : fs:FS.t -> root:Fpath.t -> t -> bool Lwt.t
  (** [mem ~fs ~root reference] is [true] iff [reference] can be found in the
      git repository [root]. Otherwise, it is [false]. *)

  val read :
    fs:FS.t ->
    root:Fpath.t ->
    t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    (head_contents, error) result Lwt.t
  (** [read ~fs ~root reference dtmp raw] is the value of the reference
      [reference] (available in the git repository [root]). [dtmp] and [raw] are
      buffers used by the decoder (respectively for the decoder as an internal
      buffer and the input buffer).

      This function can returns an {!error}. *)

  val write :
    fs:FS.t ->
    root:Fpath.t ->
    temp_dir:Fpath.t ->
    etmp:Cstruct.t ->
    raw:Cstruct.t ->
    t ->
    head_contents ->
    (unit, error) result Lwt.t
  (** [write ~fs ~root ~raw reference value] writes the value [value] in the
      mutable representation of the [reference] in the git repository [root].
      [raw] is a buffer used by the decoder to keep the input.

      This function can returns an {!error}. *)

  val remove : fs:FS.t -> root:Fpath.t -> t -> (unit, error) result Lwt.t
  (** [remove ~root reference] removes the reference from the git repository
      [root].

      This function can returns an {!error}. *)
end

(** The {i functor} to make the OCaml representation of the Git Reference object
    by a specific hash. *)
module Make (Hash : S.HASH) : S with module Hash := Hash

(** The {i functor} to make a module which implements I/O operations on
    references on a file-system. *)
module IO (H : S.HASH) (FS : S.FS) :
  IO with module Hash := H and module FS := FS
