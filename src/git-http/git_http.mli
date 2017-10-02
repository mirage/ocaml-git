module Default :
sig
  val default : Git.Capability.t list
end

module Web_cohttp_lwt = Web_cohttp_lwt
module Cstruct_buffer = Cstruct_buffer

module Make
    (K : Git.Sync.CAPABILITIES)
    (C : Git.Sync_http.CLIENT
     with type +'a io = 'a Lwt.t
      and type headers = Web_cohttp_lwt.HTTP.headers
      and type body = unit -> (Cstruct.t * int * int) option Lwt.t
      and type meth = Web_cohttp_lwt.HTTP.meth
      and type uri = Web_cohttp_lwt.uri
      and type resp = Web_cohttp_lwt.resp)
    (S : Git.Minimal.S
     with type Hash.Digest.buffer = Cstruct.t
      and type Hash.hex = string)
  : Git.Sync_http.S
    with module Web    := Web_cohttp_lwt
     and module Client := C
     and module Store  := S
     and module Buffer := Cstruct_buffer
