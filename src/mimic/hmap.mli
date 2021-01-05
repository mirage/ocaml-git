(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Heterogeneous value maps.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:func Functorial interface}

    The functorial interface allows to associate more information to the
    keys. For example a key name or a key value pretty-printer. *)

(** The type for key information. *)
module type KEY_INFO = sig
  type 'a t
  (** The type for key information. *)
end

module type VALUE_INFO = sig
  type 'a t
  (** The type for value information. *)
end

type ('a, 'b) teq = Teq : ('a, 'a) teq

(** Output signature of the functor {!Make} *)
module type S = sig
  (** {1:keys Keys} *)

  type 'a key
  (** The type for keys whose lookup value is of type ['a]. *)

  (** Keys. *)
  module Key : sig
    (** {1:keys Keys} *)

    type 'a info
    (** The type for key information. *)

    val create : 'a info -> 'a key
    (** [create i] is a new key with information [i]. *)

    val info : 'a key -> 'a info
    (** [info k] is [k]'s information. *)

    (** {1:exists Existential keys}

        Exisential keys allow to compare keys. This can be useful for
        functions like {!filter}. *)

    type t
    (** The type for existential keys. *)

    val hide_type : 'a key -> t
    (** [hide_type k] is an existential key for [k]. *)

    val equal : t -> t -> bool
    (** [equal k k'] is [true] iff [k] and [k'] are the same key. *)

    val compare : t -> t -> int
    (** [compare k k'] is a total order on keys compatible with {!equal}. *)

    val proof : 'a key -> 'b key -> ('a, 'b) teq option
  end

  module Make (Value_info : VALUE_INFO) : sig
    type 'a value = 'a Value_info.t
    (** The type for values. *)

    (** {1:maps Maps} *)

    type t
    (** The type for heterogeneous value maps. *)

    val empty : t
    (** [empty] is the empty map. *)

    val is_empty : t -> bool
    (** [is_empty m] is [true] iff [m] is empty. *)

    val mem : 'a key -> t -> bool
    (** [mem k m] is [true] iff [k] is bound in [m]. *)

    val add : 'a key -> 'a value -> t -> t
    (** [add k v m] is [m] with [k] bound to [v]. *)

    val singleton : 'a key -> 'a value -> t
    (** [singleton k v] is [add k v empty]. *)

    val rem : 'a key -> t -> t
    (** [rem k m] is [m] with [k] unbound. *)

    val find : 'a key -> t -> 'a value option
    (** [find k m] is the value of [k]'s binding in [m], if any. *)

    val get : 'a key -> t -> 'a value
    (** [get k m] is the value of [k]'s binding in [m].

        @raise Invalid_argument if [k] is not bound in [m]. *)

    (** The type for bindings. *)
    type binding = B : 'a key * 'a value -> binding

    val iter : (binding -> unit) -> t -> unit
    (** [iter f m] applies [f] to all bindings of [m]. *)

    val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f m acc] folds over the bindings of [m] with [f], starting with
        [acc] *)

    val for_all : (binding -> bool) -> t -> bool
    (** [for_all p m] is [true] iff all bindings of [m] satisfy [p]. *)

    val exists : (binding -> bool) -> t -> bool
    (** [exists p m] is [true] iff there exists a bindings of [m] that
        satisfies [p]. *)

    val filter : (binding -> bool) -> t -> t
    (** [filter p m] are the bindings of [m] that satisfy [p]. *)

    val cardinal : t -> int
    (** [cardinal m] is the number of bindings in [m]. *)

    val any_binding : t -> binding option
    (** [any_binding m] is a binding of [m] (if not empty). *)

    val get_any_binding : t -> binding
    (** [get_any_binding m] is a binding of [m].

        @raise Invalid_argument if [m] is empty. *)

    val bindings : t -> binding list

    type merge = {
      f : 'a. 'a key -> 'a value option -> 'a value option -> 'a value option;
    }

    val merge : merge -> t -> t -> t
  end
end

(** Functor for heterogeneous maps whose keys hold information
    of type [Key_info.t] *)
module Make : functor (Key_info : KEY_INFO) ->
  S with type 'a Key.info = 'a Key_info.t

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
