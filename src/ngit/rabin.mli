(** Rabin's fingerprint. *)

(** Index module. *)
module Index :
sig
  type t
  (** The type of the index. *)

  val pp : Format.formatter -> t -> unit
  (** A pretty-printer for {!t}. *)

  val memory_size : t -> int
  (** [memory_size t] returns how many word(s) is needed to store [t]. *)

  val make : ?copy:bool -> Cstruct.t -> t
  (** [make ?copy raw] returns a Rabin's fingerprint of [raw]. [?copy] signals
     to copy the input buffer [raw] or not because [make] expects to take the
     ownership. *)
end

(** The type of the compression. *)
type e =
  | C of (int * int)
  (** It's the copy opcode to copy the byte range from the source to the
      target. *)
  | I of (int * int)
  (** It's the insert opcode to keep a specific byte range of the target. *)

val pp : Format.formatter -> e -> unit
(** A pretty-printer for {!e}. *)

val delta : Index.t -> Cstruct.t -> e list
(** [delta index trg] returns a compression list between the Rabin's fingerprint
   of the source with the target [trg]. *)
