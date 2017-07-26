module type S =
sig
  module Digest
    : Ihash.IDIGEST
  (** The [Digest] module used to make the module. *)

  module Inflate
    : Common.INFLATE
  (** The [Inflate] module used to make the module. *)

  module Deflate
    : Common.DEFLATE
  (** The [Deflate] module used to make the module. *)

  module Hash
    : Common.BASE
  (** The Hash module. *)

  module Blob
    : Blob.S with type Hash.t = Hash.t
  (** The {!Blob} module. *)

  module Commit
    : Commit.S with type Hash.t = Hash.t
  (** The {!Commit} module. *)

  module Tree
    : Tree.S with type Hash.t = Hash.t
  (** The {!Tree} module. *)

  module Tag
    : Tag.S with type Hash.t = Hash.t
  (** The {!Tag} module. *)

  type t =
    | Blob   of Blob.t   (** The {!Blob.t} OCaml value. *)
    | Commit of Commit.t (** The {!Commit.t} OCaml value. *)
    | Tree   of Tree.t   (** The {!Tree.t} OCaml value. *)
    | Tag    of Tag.t    (** The {!Tag.t} OCaml value. *)
  (** OCaml value which represents a Git object. *)

  module A
    : Common.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git object. *)

  module F
    : Common.FARADAY  with type t = t
  (** The Faraday encoder of the Git object. *)

  module D
    : Common.DECODER  with type t = t
                       and type raw = Cstruct.t
                       and type init = Inflate.window * Cstruct.t * Cstruct.t
                       and type error = [ `Decoder of string | `Inflate of Inflate.error ]
  (** The decoder of the Git object. We constraint the input to be an
      {!Inflate.window} and a {Cstruct.t} which used by the {Inflate} module and an
      other {Cstruct.t} as an internal buffer.

      All error from the {!Inflate} module is relayed to the [`Inflate] error
      value. *)

  module M
    : Common.MINIENC  with type t = t
  (** The {!Minienc} encoder of the Git object. *)

  module E
    : Common.ENCODER  with type t = t
                       and type raw = Cstruct.t
                       and type init = int * t * int * Cstruct.t
                       and type error = [ `Deflate of Deflate.error ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the level of the
      compression, the value {!t}, the memory consumption of the encoder (in bytes)
      and an internal buffer between the compression and the encoder.

      All error from the {!Deflate} module is relayed to the [`Deflate] error
      value. *)

  include Ihash.DIGEST with type t := t
                        and type hash := Hash.t
  include Common.BASE with type t := t
end

module Make
    (Digest : Ihash.IDIGEST with type t = Bytes.t
                             and type buffer = Cstruct.t)
    (Inflate : Common.INFLATE)
    (Deflate : Common.DEFLATE)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
       and module Inflate = Inflate
       and module Deflate = Deflate
(** The {i functor} to make the OCaml representation of the Git object by a
    specific hash, an {!Inflate} implementation for the compression and a {!Deflate}
    implementation for the decompression. We constraint the {!IDIGEST} module to
    generate a {Bytes.t} and compute a {Cstruct.t}. *)
