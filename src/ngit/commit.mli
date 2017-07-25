module type S =
sig
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  type t
  (** A Git Commit object. Which specifies the top-level {!Tree.t} for the
      snapshot of the project at a point; the author/{i committer} information and
      the commit message. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  module D
    : Common.DECODER  with type t = t
                       and type raw = Cstruct.t
                       and type init = Cstruct.t
                       and type error = [ `Decoder of string ]
  (** The decoder of the Git Commit object. We constraint the input to be a
      {Cstruct.t}. This decoder needs a {Cstruct.t} as an internal buffer. *)

  module A
    : Common.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git Commit object. *)

  module F
    : Common.FARADAY  with type t = t
  (** The Faraday encoder of the Git Commit object. *)

  module M
    : Common.MINIENC  with type t = t
  (** The {!Minienc} encoder of the Git Commit object. *)

  module E
    : Common.ENCODER  with type t = t
                       and type raw = Cstruct.t
                       and type init = int * t
                       and type error = [ `Never ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git Commit object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the Commit
      OCaml value and the memory consumption of the encoder (in bytes). The
      encoder can not fail.

      NOTE: we can not unspecified the error type (it needs to be concrete) but,
      because the encoder can not fail, we define the error as [`Never]. *)

  include Ihash.DIGEST with type t := t
                        and type hash = Hash.t

  include Common.BASE with type t := t

  val parents : t -> Hash.t list
  (** [parents c] returns all parents of the Git Commit object [c]. *)

  val tree : t -> Hash.t
  (** [tree c] returns the hash of top-level {!Tree.t} of the Git Commit object
      [c]. *)

  val compare_with_date : t -> t -> int
  (** [compare_with_date a b] compares the Git Commit object [a] and [b] by the
      date of the author. *)
end

module Make
    (Digest : Ihash.IDIGEST with type t = Bytes.t
                             and type buffer = Cstruct.t)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
(** The {i functor} to make the OCaml representation of the Git Commit object by
    a specific hash implementation. We constraint the {!IDIGEST} module to
    generate a {!Bytes.t} and compute a {Cstruct.t}. *)
