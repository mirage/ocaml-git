type +'a io = 'a Lwt.t

type raw = Cstruct.t
type uri = Uri.t

type req =
  { req   : Cohttp.Request.t
  ; query : (string * string list) list
  ; body  : (raw * int * int) option -> unit Lwt.t }

type resp =
  { resp : Cohttp.Response.t
  ; body : Cohttp_lwt.Body.t }

module HTTP =
struct
  type headers = Cohttp.Header.t

  type path = string list

  type meth = Cohttp.Code.meth

  type version = int * int

  module Headers =
  struct
    type name = string

    let name = String.lowercase_ascii
    let name_equal a b = String.equal a b
    let empty = Cohttp.Header.init ()
    let is_empty _ = assert false (* TODO *)
    let find name headers = Cohttp.Header.get headers name
    let def name value headers =
      let values = Astring.String.cuts ~empty:false ~sep:"," value in
      Cohttp.Header.add_multi headers name values
    let def_multi name values headers =
      if List.length values = 0
      then raise (Invalid_argument "HTTP.Headers.def_multi: empty values");

      Cohttp.Header.add_multi headers name values
    let get x headers = match find x headers with
      | Some v -> v
      | None -> raise (Invalid_argument "HTTP.Headers.get: invalid name")
    let user_agent = "User-Agent"
    let content_type = "Content-Type"
    let access_control_allow_origin = "Access-Control-Allow-Origin"
    let access_control_allow_methods = "Access-Control-Allow-Methods"
    let access_control_allow_headers = "Access-Control-Allow-Headers"

    let merge a b =
      Cohttp.Header.fold (fun key value acc -> Cohttp.Header.add_unless_exists acc key value) a b
  end
end

type status = int

let s100_continue = 100
let s200_ok = 200

module Request =
struct
  type body = (raw * int * int) option -> unit Lwt.t

  let headers { req; _ } = Cohttp.Request.headers req

  let with_headers x headers = { x with req = { x.req with Cohttp.Request.headers } }
  let with_path x path = { x with req = { x.req with Cohttp.Request.resource = String.concat "/" path } }
  let with_body (x : req) body = { x with body }

  let v ?(version = (1, 1)) meth ~path ?(query = []) headers body =
    let req =
      { Cohttp.Request.headers
      ; meth
      ; resource = String.concat "/" path
      ; version = (match version with
            | 1, 0 -> `HTTP_1_0
            | 1, 1 -> `HTTP_1_1
            | a, b -> raise (Invalid_argument (Fmt.strf "Request.v: invalid version %d.%d" a b)))
      ; encoding = Cohttp.Transfer.Chunked } (* XXX(dinosaure): work on the API to be able to change encoding? *)
    in

    { req; query; body; }

  let body ({ body; _ } : req) = body
  let uri { req; _ } = Cohttp.Request.uri req
  let meth { req; _ } = Cohttp.Request.meth req
end

module Response =
struct
  type body = unit -> (raw * int * int) option Lwt.t

  let body { body; _ } =
    let with_off_and_len = function
      | Some s -> Some (Cstruct.of_string s, 0, String.length s)
      (* XXX(dinosaure): we can optimize (avoid the [Cstruct.of_string]) this mapper. *)
      | None -> None
    in

    let open Lwt.Infix in

    match body with
    | `Empty -> fun () -> Lwt.return None
    | `Stream s ->  fun () -> Lwt_stream.get s >|= with_off_and_len
    | `String s ->
      let stream = Lwt_stream.of_list [ s ] in
      fun () -> Lwt_stream.get stream >|= with_off_and_len
    | `Strings l ->
      let stream = Lwt_stream.of_list l in
      fun () -> Lwt_stream.get stream >|= with_off_and_len

  let status { resp; _ } = Cohttp.Code.code_of_status resp.Cohttp.Response.status
end
