module type KEY =
sig
  type t

  val compare : t -> t -> int
  val get     : t -> int -> char
end

(** A Fanout table is a container to associate a key [Key.t] with a value.
   Internally, we order bindings by the first byte of the [Key.t]. *)
module Make (Key : KEY) :
sig
  type 'a t
  (** The fanout table. *)

  val make   : unit -> 'a t
  (** Make a new fanout table. *)

  val bind   : Key.t -> 'a -> 'a t -> unit
  (** [bind k v t] adds the binding [k] and [v] to [t]. *)

  val length : int -> 'a t -> int
  (** [length t] returns the length of [t]. *)

  val get    : int -> 'a t -> (Key.t * 'a) list
  (** [get byte t] gets a list of bindings where the key start with the byte
     [byte]. *)
end
