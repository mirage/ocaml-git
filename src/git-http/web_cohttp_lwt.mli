type 'a io = 'a Lwt.t
type raw = Cstruct.t
type uri = Uri.t

type req =
  { req : Cohttp.Request.t
  ; query : (string * string list) list
  ; body : (raw * int * int) option -> unit Lwt.t }

type resp =
  { resp : Cohttp.Response.t
  ; body : Cohttp_lwt.Body.t }

include Web.S
  with type req := req
   and type resp := resp
   and type 'a io := 'a io
   and type raw := raw
   and type uri := uri
   and type Request.body = (raw * int * int) option -> unit Lwt.t
   and type Response.body = unit -> (raw * int * int) option Lwt.t
   and type HTTP.headers = Cohttp.Header.t
