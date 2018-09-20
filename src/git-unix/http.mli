type endpoint = {
  uri    : Uri.t;
  headers: Cohttp.Header.t;
}

module Client :
  Git_http.Sync.CLIENT
  with type +'a io = 'a Lwt.t
   and type headers = Git_http.Web_cohttp_lwt.HTTP.headers
   and type body = unit -> (Cstruct.t * int * int) option Lwt.t
   and type meth = Git_http.Web_cohttp_lwt.HTTP.meth
   and type endpoint = endpoint
   and type resp = Git_http.Web_cohttp_lwt.resp

module Make (S : Git.S) :
  Git_http.Sync.COHTTP_S with module Client = Client and module Store = S
