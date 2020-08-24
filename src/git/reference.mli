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
(** A Git Reference object. Which contains a hash to point to an other object. *)

module P : sig
  type partial
  (** Type of the left part of a reference. *)

  type branch = string
  (** Tyoe of the right part of a reference. *)

  val ( // ) : partial -> string -> partial
  (** Infix operator to compose [string] value with a {!partial} reference. *)

  val ( / ) : partial -> branch -> t
  (** Infix operator to compose {!partial} with {!branch} and return a
      full-defined reference {!t}. *)

  val refs : partial
  (** [refs/] *)

  val heads : partial
  (** [refs/heads/] *)

  val remotes : partial
  (** [refs/remotes/] *)

  val origin : partial
  (** [refs/remotes/origin] *)

  val master : branch
  (** [*/master] *)
end

val head : t
(** [head] is the {i user-friendly} value of HEAD Git reference. *)

val master : t
(** [master] is the {i user-friendly} value of [refs/heads/master] Git
    reference. *)

val is_head : t -> bool
(** [is_head t] returns [true] if [t = head]. *)

val of_string : string -> t
(** [of_string s] returns a valid reference value. A valid value means:

    A [refname] is a hierarchical octet string beginning with ["refs/"] and not
    violating the [git-check-ref-format] command's validation rules. More
    specifically, they:

    - They can include slash ['/'] for hierarchical (directory) grouping, but no
      slash-separated component can begin with a dot ['.'].
    - They must contain at least one ['/']. This enforces the presence of a
      category like ["heads/"], ["tags/"] etc. but the actual names are not
      restricted.
    - They cannot have two consecutive dots [".."] anywhere.
    - They cannot have ASCII control characters (i.e. bytes whose values are
      lower than [\040] or [\177] DEL), space, tilde ['~'], caret ['^'], colon
      [':'], question-mark ['?'], asterisk ['*'], or open bracket ['\[']
      anywhere.
    - They cannot end with a slash ['/'] or a dot ['.'].
    - They cannot end with the sequence [".lock"].
    - They cannot contain a sequence ["@{"].
    - They cannot contain a ['\\']. *)

val to_string : t -> string
(** [to_string t] returns the string value of the reference [t]. *)

val of_path : Path.t -> t
(** [of_path path] casts a path to a reference. *)

val to_path : t -> Path.t
(** [to_path ref] casts a reference [ref] to a path (as a Window path or Unix
    path). *)

(** Interface to describe the Git reference value [head_contents]. *)
module type S = sig
  module Hash : S.HASH
  (** The [Digest] module used to make the module. *)

  type nonrec t = t

  module P : sig
    type partial

    type branch = string

    val ( // ) : partial -> partial -> partial

    val ( / ) : partial -> branch -> t

    val refs : partial

    val heads : partial

    val remotes : partial

    val origin : partial

    val master : branch
  end

  val head : t

  val master : t

  val is_head : t -> bool

  val of_string : string -> t

  val to_string : t -> string

  val of_path : Path.t -> t

  val to_path : t -> Path.t

  include S.BASE with type t := t

  (** The type of the value of a Git reference. *)
  type head_contents =
    | Hash of Hash.t  (** A pointer to an hash. *)
    | Ref of t
        (** A reference which one can point to an other reference or an hash. *)

  val pp_head_contents : head_contents Fmt.t
  (** Pretty-printer of {!head_contents}. *)

  val equal_head_contents : head_contents -> head_contents -> bool
  (** [equal_head_contents a b] implies [a = Ref a'] and [b = Ref b'] and
      [Reference.equal a' b' = true] or [a = Hash a'] and [b = Hash b'] and
      [Hash.equal a' b'].

      However, semantically [Ref a'] could be equal to [Hash b'] iff [Hash b']
      is come from the reference [a']. That means this function does not handle
      any indirection when it tests your values. *)

  val compare_head_contents : head_contents -> head_contents -> int

  module A : S.DESC with type 'a t = 'a Angstrom.t and type e = head_contents

  module M :
    S.DESC with type 'a t = 'a Encore.Encoder.t and type e = head_contents

  module D :
    S.DECODER
      with type t = head_contents
       and type init = Cstruct.t
       and type error = Error.Decoder.t

  module E :
    S.ENCODER
      with type t = head_contents
       and type init = Cstruct.t * head_contents
       and type error = Error.never
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
