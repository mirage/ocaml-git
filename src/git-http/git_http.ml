module Web = Web
module Sync = Sync

module Default = struct
  let default =
    [ `Multi_ack_detailed
    ; `Thin_pack
    ; `Side_band_64k
    ; `Ofs_delta
    ; `Agent "git/2.0.0"
    ; `Report_status
    ; `No_done ]
end

module Web_cohttp_lwt = Web_cohttp_lwt

module Make
    (K : Git.Sync.CAPABILITIES)
    (C : Sync.CLIENT
     with type +'a io = 'a Lwt.t
      and type headers = Web_cohttp_lwt.HTTP.headers
      and type body = unit -> (Cstruct.t * int * int) option Lwt.t
      and type meth = Web_cohttp_lwt.HTTP.meth
      and type uri = Web_cohttp_lwt.uri
      and type resp = Web_cohttp_lwt.resp)
    (S : Git.S)
  : Sync.S
    with module Web    := Web_cohttp_lwt
     and module Client := C
     and module Store  := S
= struct
  include Sync.Make(K)(Web_cohttp_lwt)(C)(S)
end
