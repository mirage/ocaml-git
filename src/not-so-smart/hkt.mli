module Make_sched (T : sig
  type +'a t
end) : Sigs.SCHED with type +'a s = 'a T.t

module Make_store (T : sig
  type ('k, 'v) t
end) : Sigs.STORE with type ('k, 'v) s = ('k, 'v) T.t
