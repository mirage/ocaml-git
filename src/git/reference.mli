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

type t = private string
(** A Git Reference object. Which contains a hash to point to an other
    object. *)

val head: t
(** [head] is the {i user-friendly} value of HEAD Git reference. *)

val master: t
(** [master] is the {i user-friendly} value of [refs/heads/master] Git
    reference. *)

val is_head: t -> bool
(** [is_head t] returns [true] if [t = head]. *)

val of_string: string -> t
(** [of_string s] returns a valid reference value. A valid value
    means:

    A [refname] is a hierarchical octet string beginning with
    ["refs/"] and not violating the [git-check-ref-format] command's
    validation rules. More specifically, they:

    {ul
    {- They can include slash ['/'] for hierarchical (directory)
    grouping, but no slash-separated component can begin with a dot
    ['.'].}
    {- They must contain at least one ['/']. This enforces the
    presence of a category like ["heads/"], ["tags/"] etc. but the
    actual names are not restricted.}
    {- They cannot have two consecutive dots [".."] anywhere.}
    {- They cannot have ASCII control characters (i.e. bytes whose
    values are lower than [\040] or [\177] DEL), space, tilde ['~'],
    caret ['^'], colon [':'], question-mark ['?'], asterisk ['*'],
    or open bracket ['\['] anywhere.}
    {- They cannot end with a slash ['/'] or a dot ['.'].}
    {- They cannot end with the sequence [".lock"].}
    {- They cannot contain a sequence ["@{"].}
    {- They cannot contain a ['\\'].}}
*)

val to_string: t -> string
(** [to_string t] returns the string value of the reference [t]. *)

val of_path: Fpath.t -> t
(** [of_path path] casts a path to a reference. *)

val to_path: t -> Fpath.t
(** [to_path ref] casts a reference [ref] to a path (as a Window path or Unix path). *)

(**  Interface to describe the Git reference value [head_contents]. *)
module type S = sig

  module Hash: S.HASH
  (** The [Digest] module used to make the module. *)

  type nonrec t = t

  val head: t
  val master: t
  val is_head: t -> bool
  val of_string: string -> t
  val to_string: t -> string
  val of_path: Fpath.t -> t
  val to_path: t -> Fpath.t

  include S.BASE with type t := t

  (** The type of the value of a Git reference. *)
  type head_contents =
    | Hash of Hash.t (** A pointer to an hash. *)
    | Ref of t (** A reference which one can point to an other reference or an hash. *)

  val pp_head_contents: head_contents Fmt.t
  (** Pretty-printer of {!head_contents}. *)

  val equal_head_contents: head_contents -> head_contents -> bool
  (** [equal_head_contents a b] implies [a = Ref a'] and [b = Ref b']
      and [Reference.equal a' b' = true] or [a = Hash a'] and [b =
      Hash b'] and [Hash.equal a' b'].

      However, semantically [Ref a'] could be equal to [Hash b'] iff
      [Hash b'] is come from the reference [a']. That means this
      function does not handle any indirection when it tests your
      values. *)

  val compare_head_contents: head_contents -> head_contents -> int

  module A: S.ANGSTROM with type t = head_contents
  (** The Angstrom decoder of the Git Reference object. *)

  module D: S.DECODER
    with type t = head_contents
     and type init = Cstruct.t
     and type error = [ `Decoder of string ]
  (** The decoder of the Git Reference object. We constraint the input
      to be a {!Cstruct.t}. This decoder needs a {!Cstruct.t} as an
      internal buffer. *)

  module M: S.MINIENC with type t = head_contents
  (** The {!Minienc} encoder of the Git Reference object. *)

  module E: S.ENCODER
    with type t = head_contents
     and type init = int * head_contents
     and type error = [ `Never ]
  (** The encoder (which uses a {Minienc.encoder}) of the Git
      Reference object. We constraint the output to be a {Cstruct.t}.
      This encoder needs the Reference OCaml value and the memory
      consumption of the encoder (in bytes). The encoder can not fail.

      NOTE: we can not unspecified the error type (it needs to be
      concrete) but, because the encoder can not fail, we define the
      error as [`Never]. *)
end

(** The interface which describes any I/O operations on Git reference. *)
module type IO =
sig
  module Lock: S.LOCK
  module FS: S.FS

  include S

  (** The type of error. *)
  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | D.error ]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  val mem: root:Fpath.t -> t -> (bool, error) result Lwt.t
  (** [mem ~root reference] returns [true] iff we found the
      [reference] find in the git repository [root]. Otherwise, we returns
      [false]. *)

  val read: root:Fpath.t -> t -> dtmp:Cstruct.t -> raw:Cstruct.t -> ((t * head_contents), error) result Lwt.t
  (** [read ~root reference dtmp raw] returns the value contains in
      the reference [reference] (available in the git repository
      [root]). [dtmp] and [raw] are buffers used by the decoder
      (respectively for the decoder as an internal buffer and the
      input buffer).

      This function can returns an {!error}. *)

  val write: root:Fpath.t -> ?locks:Lock.t -> ?capacity:int -> raw:Cstruct.t -> t -> head_contents -> (unit, error) result Lwt.t
  (** [write ~root ~raw reference value] writes the value [value] in
      the mutable representation of the [reference] in the git
      repository [root]. [raw] is a buffer used by the decoder to keep
      the input.

      This function can returns an {!error}. *)

  val test_and_set: root:Fpath.t -> ?locks:Lock.t -> t -> test:head_contents option -> set:head_contents option -> (bool, error) result Lwt.t
  (** Atomic updates (test and set) for references. *)

  val remove: root:Fpath.t -> ?locks:Lock.t -> t -> (unit, error) result Lwt.t
  (** [remove ~root ~lockdir reference] removes the reference from the
      git repository [root]. [lockdir] is to store a {!Lock.t} and
      avoid race condition on the reference [reference].

      This function can returns an {!error}. *)
end

module Make (H: S.HASH): S with module Hash = H
(** The {i functor} to make the OCaml representation of the Git
    Reference object by a specific hash. *)

module IO
    (H: S.HASH)
    (L: S.LOCK)
    (FS: S.FS with type File.lock = L.elt)
 : IO with module Hash = H
        and module Lock = L
        and module FS = FS
(** The {i functor} to make a module which implements I/O operations
    on references on a file-system. *)
