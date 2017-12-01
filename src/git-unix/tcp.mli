module Default: sig
  val default: Git.Capability.t list
end

module Make (K: Git.Sync.CAPABILITIES) (S: Git.S): sig
  module Negociator : Git.Negociator.S with module Store := S

  include Git.Sync.S with module Store := S

  type error' =
    [ `StoreRef of S.Ref.error
    | `Sync of error ]

  val fetch_all   : S.t -> ?locks:S.Lock.t -> Uri.t -> (unit, error') result Lwt.t
  val fetch_one   : S.t -> ?locks:S.Lock.t -> reference:S.Reference.t -> Uri.t -> (unit, error') result Lwt.t
  val easy_clone  : S.t -> ?locks:S.Lock.t -> reference:S.Reference.t -> Uri.t -> (unit, error') result Lwt.t
  val easy_update : S.t -> reference:S.Reference.t -> Uri.t -> ((S.Reference.t, S.Reference.t * string) result list, error') result Lwt.t
end
