module Client :
  Git_http.Sync.CLIENT
  with type +'a io = 'a Lwt.t
   and type headers = Git_http.Web_cohttp_lwt.HTTP.headers
   and type body = unit -> (Cstruct.t * int * int) option Lwt.t
   and type meth = Git_http.Web_cohttp_lwt.HTTP.meth
   and type uri = Git_http.Web_cohttp_lwt.uri
   and type resp = Git_http.Web_cohttp_lwt.resp

module Make (S : Git.S) :
  Git_http.Sync.S with module Client = Client and module Store = S
