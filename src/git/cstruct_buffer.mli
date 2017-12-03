type t
(** The abstract type of buffers. *)

type raw = string
(** The type of result. *)

type fixe = Cstruct.t
(** The type of input. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty. The [n]
    parameter is the initial size of the internal byte sequence that
    holds the buffer contents. That byte sequence is automatically
    reallocated when more than [n] bytes are stored in the buffer,
    but shrinks back to [n] bytes when [reset] is called.

    For best performance, [n] should be of the same order of
    magnitude as the number of characters that are expected to be
    stored in the buffer. Nothing bad will happen if the buffer
    grows beyond that limit, however. *)

val contents : t -> raw
(** Return a copy of the current contents of the buffer. The buffer
    itself is unchanged. *)

val unsafe_contents : t -> fixe

val has : t -> int

val add : t -> fixe -> unit
(** [add fixe buffer] appends the fixed-size buffer [fixe] at the
    end of the buffer [buffer]. *)

val clear : t -> unit
(** Empty the buffer. *)

val reset : t -> unit
  (** Empty the buffer and {i de-allocate} the internal byte sequence
      holding the buffer contents, replacing it with the initial
      internal byte sequence of length [n] that was allocated by
      {!Buffer.create} [n]. For long-lived buffers that may have grown
      a lot, [reset] allows faster reclamation of the space used by
      the buffer. *)
