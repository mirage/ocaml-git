module type KEY =
sig
  type t

  val get : t -> int -> char
  val length : t -> int
end

(** A Radix tree is a optimized container to bind a [Key.t] with a value. *)
module Make (Key : KEY) :
sig
  type 'a t
  (** The radix-tree. *)
  type 'a sequence = ('a -> unit) -> unit
  (** An abstract representation of a iterative container. *)

  val empty       : 'a t
  (** The empty radix-tree. *)

  val is_empty    : 'a t -> bool
  (** Test whether is radix-tree is empty or not. *)

  val bind        : 'a t -> Key.t -> 'a -> 'a t
  (** [bind t k v] returns a radix-tree containing the same binding as [t], plus
     a binding [k] to [v]. *)

  val lookup      : 'a t -> Key.t -> 'a option
  (** [lookup t k] returns the current binding of [k] in [t] or returns [None]
     if no such binding exists. *)

  val exists      : 'a t -> Key.t -> bool
  (** [exists t k] checks if at least we have one binding with the key [k]. *)

  val fold        : (Key.t * 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** [fold f a t] computes [(f kN dN (f k1 d1 a))], where [k1 .. kN] are the
     keys of all bindings in [t], and [d1 .. dN] are the associated data. *)

  val iter        : (Key.t * 'a -> unit) -> 'a t -> unit
  (** [iter f t] applies [f] to all bindings in radix-tree [t]. [f] receives a
     pair which contains the key and the associated value. *)

  val to_sequence : 'a t -> (Key.t * 'a) sequence
  (** [to_sequence t] makes a abstract representation of the radix-tree. *)

  val to_list     : 'a t -> (Key.t * 'a) list
  (** [to_list t] makes an associated list from the radix-tree. *)

  val pp          : (Key.t * 'a) Fmt.t -> 'a t Fmt.t
  (** A pretty-printer for the radix-tree. *)
end
