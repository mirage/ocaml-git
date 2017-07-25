(** A bucket is a simple option array to contain something. When the bucket is
   full and you try to add a new value, we erase the oldest one. *)

type 'a t
(** The type of the bucket. *)

val make : int -> 'a t
(** Make a new bucket with a specified size. *)

val add : 'a t -> 'a -> unit
(** [add t v] adds the new value [v] in the bucket [t]. If the bucket is full,
   we erase by [v] the oldest value of [t]. *)

val iter : 'a t -> ('a -> unit) -> unit
(** [iter t f] applies [f] to all values in the bucket [ŧ]. [f] receives each
   value. *)

val find : 'a t -> ('a -> bool) -> 'a option
(** [find t f] returns the first value which respects the predicate [f].
   Otherwise, it returns [None]. *)
