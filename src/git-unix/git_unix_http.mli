module Client
  : Git.Sync_http.CLIENT
    with type +'a io  = 'a Lwt.t
     and type headers = Git_http.Web_cohttp_lwt.HTTP.headers
     and type body    = unit -> (Cstruct.t * int * int) option Lwt.t
     and type meth    = Git_http.Web_cohttp_lwt.HTTP.meth
     and type uri     = Git_http.Web_cohttp_lwt.uri
     and type resp    = Git_http.Web_cohttp_lwt.resp

module Make
    (K : Git.Sync.CAPABILITIES)
    (S : Git.Minimal.S with type Hash.Digest.buffer = Cstruct.t
                        and type Hash.hex = string)
: sig
  module Negociator
    : Git.Negociator.S
      with module Store := S

  include Git.Sync_http.S
    with module Web    := Git_http.Web_cohttp_lwt
     and module Client := Client
     and module Store  := S

  type error' =
    [ `StoreRef of S.Ref.error
    | `Sync of error ]

  val fetch_all : S.t -> ?locks:S.Lock.t -> Uri.t -> (unit, error') result Lwt.t
  val fetch_one : S.t -> ?locks:S.Lock.t -> reference:S.Reference.t -> Uri.t -> (unit, error') result Lwt.t
  val easy_clone : S.t -> ?locks:S.Lock.t -> reference:S.Reference.t -> Uri.t -> (unit, error') result Lwt.t
  val easy_update : S.t -> reference:S.Reference.t -> Uri.t -> ((S.Reference.t, S.Reference.t * string) result list, error') result Lwt.t
end
