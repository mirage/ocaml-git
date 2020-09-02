open Lwt.Infix

type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let get ~resolvers ?(headers = []) uri =
  let headers = Cohttp.Header.of_list headers in
  Cohttp_mirage.Client.get ~resolvers ~headers uri >>= fun (_response, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)

let post ~resolvers ?(headers = []) uri body =
  let headers = Cohttp.Header.of_list headers in
  let body = Cohttp_lwt.Body.of_string body in
  Cohttp_mirage.Client.post ~resolvers ~headers ~chunked:false ~body uri
  >>= fun (_response, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)
