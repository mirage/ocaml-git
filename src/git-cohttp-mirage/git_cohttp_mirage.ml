open Lwt.Infix

type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .
let conduit = Mimic.make ~name:"conduit"
let with_conduit v ctx = Mimic.add conduit v ctx

let with_redirects ?(max = 10) ~f uri =
  if max < 10 then invalid_arg "with_redirects";
  let tbl = Hashtbl.create 0x10 in
  let rec go max uri =
    f uri >>= fun (resp, body) ->
    let status_code = Cohttp.(Response.status resp |> Code.code_of_status) in
    if Cohttp.Code.is_redirection status_code then
      match Cohttp.(Response.headers resp |> Header.get_location) with
      | Some uri' when Hashtbl.mem tbl uri' || max = 0 -> Lwt.return (resp, body)
      | Some uri' ->
          Hashtbl.add tbl uri' ();
          Cohttp_lwt.Body.drain_body body >>= fun () -> go (pred max) uri'
      | None -> Lwt.return (resp, body)
    else Lwt.return (resp, body)
  in
  go max uri

let get ~ctx ?(headers = []) uri =
  let ctx =
    match Mimic.get conduit ctx with
    | Some ctx -> ctx
    | None -> Cohttp_mirage.Client.default_ctx
  in
  let headers = Cohttp.Header.of_list headers in
  let f uri = Cohttp_mirage.Client.get ~ctx ~headers uri in
  with_redirects ~f uri >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)

let post ~ctx ?(headers = []) uri body =
  let ctx =
    match Mimic.get conduit ctx with
    | Some ctx -> ctx
    | None -> Cohttp_mirage.Client.default_ctx
  in
  let headers = Cohttp.Header.of_list headers in
  let body = Cohttp_lwt.Body.of_string body in
  let f uri =
    Cohttp_mirage.Client.post ~ctx ~headers ~chunked:false ~body uri
  in
  with_redirects ~f uri >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)
