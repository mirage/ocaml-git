module type S =
sig
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  type t = private Cstruct.t
  (** A Git Blob object. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  module D
    : Common.DECODER with type t = t
                      and type raw = Cstruct.t
                      and type init = Cstruct.t
                      and type error = [ `Decoder of string ]
  (** The decoder of the Git Blob object. We constraint the input to be a
      {Cstruct.t}. This decoder needs a {Cstruct.t} as an internal buffer. *)

  module E
    : Common.ENCODER with type t = t
                      and type raw = Cstruct.t
  (** The encoder of the Git Blob object. *)

  module A
    : Common.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git Blob object. *)

  module F
    : Common.FARADAY  with type t = t
  (** The Faraday encoder of the Git Blob object. *)

  include Ihash.DIGEST with type t := t
                        and type hash := Hash.t

  include Common.BASE with type t := t
end

module Make
    (Digest : Ihash.IDIGEST with type t = Bytes.t
                             and type buffer = Cstruct.t)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
(** The {i functor} to make the OCaml representation of the Git Blob object by a
    specific hash implementation. We constraint the {!IDIGEST} module to
    generate a {!Bytes.t} and compute a {Cstruct.t}. *)
