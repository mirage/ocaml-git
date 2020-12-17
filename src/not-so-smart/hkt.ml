(** This is a module used to share functionality needed by modules that
    contain higher-kinded type behavior.

    HKT = Higher-Kinded Types *)
module HKT = struct
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Make_sched (T : sig
  type +'a t
end) =
struct
  type +'a s = 'a T.t

  include HKT
end

module Make_store (T : sig
  type ('k, 'v) t
end) =
struct
  type ('a, 'b) s = ('a, 'b) T.t

  include HKT
end
