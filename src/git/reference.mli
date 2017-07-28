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
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  module Path
    : Path.S
  (** The [Path] module used to make the module. *)

  module FileSystem
    : Fs.S
  (** The [FileSystem] module used to make the module. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  type t = private string
  (** A Git Reference object. Which contains a hash to point to an other
      object. *)

  val head : t
  (** [head] is the {i user-friendly} value of HEAD Git reference. *)

  val master : t
  (** [master] is the {i user-friendly} value of [refs/heads/master] Git reference. *)

  val is_head : t -> bool
  (** [is_head t] returns [true] if [t = head]. *)

  val of_string : string -> t
  (** [of_string s] returns a valid reference value. A valid value means:

      A [refname] is a hierarchical octet string beginning with ["refs/"] and
      not violating the [git-check-ref-format] command's validation rules. More specifically, they:

      {ul

      {- They can include slash ['/'] for hierarchical (directory) grouping, but
      no slash-separated component can begin with a dot ['.'].}

      {- They must contain at least one ['/']. This enforces the presence of a
      category like ["heads/"], ["tags/"] etc. but the actual names are not
      restricted.}

      {- They cannot have two consecutive dots [".."] anywhere.}

      {- They cannot have ASCII control characters (i.e. bytes whose values are
      lower than [\040] or [\177] DEL), space, tilde ['~'], caret ['^'], colon
      [':'], question-mark ['?'], asterisk ['*'], or open bracket ['[']
      anywhere.}

      {- They cannot end with a slash ['/'] or a dot ['.'].}

      {- They cannot end with the sequence [".lock"].}

      {- They cannot contain a sequence ["@{"].}

      {- They cannot contain a ['\\'].}}
  *)

  val to_string : t -> string
  (** [to_string t] returns the string value of the reference [t]. *)

  include Common.BASE with type t := t

  type head_contents =
    | Hash of Hash.t (** A pointer to an hash. *)
    | Ref of t (** A reference which one can point to an other reference or an hash. *)

  val pp_head_contents : head_contents Fmt.t
  (** Pretty-printer of {!head_contents}. *)

  module A
    : Common.ANGSTROM with type t = head_contents
  (** The Angstrom decoder of the Git Reference object. *)

  module D
    : Common.DECODER with type t = head_contents
                      and type raw = Cstruct.t
                      and type init = Cstruct.t
                      and type error = [ `Decoder of string ]
  (** The decoder of the Git Reference object. We constraint the input to be a
      {!Cstruct.t}. This decoder needs a {!Cstruct.t} as an internal buffer. *)

  type error =
    [ FileSystem.File.error (** The [FileSystem] error *)
    | D.error (** The decoder {!D} error. *)
    ] (** The type of error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val from_file : Path.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> ((t * head_contents), error) result Lwt.t
  (** [from_file path dtmp raw] returns the value contains in the file [path]
      and the reference semantically equal to [path]. [dtmp] and [raw] are
      buffers used by the decoder (respectively for the decoder as an internal
      buffer and the input buffer).

      This function can returns an {!error}. *)
end

module Make
    (Digest : Ihash.IDIGEST with type t = Bytes.t
                             and type buffer = Cstruct.t)
    (Path : Path.S)
    (FileSystem : Fs.S with type path = Path.t
                        and type File.error = [ `System of string ]
                        and type File.raw = Cstruct.t)
  : S with type Hash.t = Digest.t
       and type Path.t = Path.t
       and module Digest = Digest
       and module Path = Path
       and module FileSystem = FileSystem
(** The {i functor} to make the OCaml representation of the Git Reference object
    by a specific hash, a defined path and an access to the file-system. We
    constraint the {!IDIGEST} module to generate a {Bytes.t} and compute a
    {Cstruct.t}. Then, the content of a file need to be a {Cstruct.t}. The path
    provided by the {!Path} module need to be semantically the same than which
    used by the [FileSystem] module. *)
