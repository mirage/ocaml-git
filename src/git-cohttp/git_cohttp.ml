open Lwt.Infix

module Make
    (Cohttp_client : Cohttp_lwt.S.Client with type ctx = Conduit.resolvers) =
struct
  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let get ~resolvers ?(headers = []) uri =
    let headers = Cohttp.Header.of_list headers in
    Cohttp_client.get ~ctx:resolvers ~headers uri >>= fun (_response, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)

  let post ~resolvers ?(headers = []) uri body =
    let headers = Cohttp.Header.of_list headers in
    let body = Cohttp_lwt.Body.of_string body in
    Cohttp_client.post ~ctx:resolvers ~headers ~chunked:false ~body uri
    >>= fun (_response, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)
end
