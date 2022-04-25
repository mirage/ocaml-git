open Lwt.Infix

let src = Logs.Src.create "git.paf"

module Log = (val Logs.src_log src : Logs.LOG)

let git_paf_scheme = Mimic.make ~name:"git-paf-scheme"
let git_paf_port = Mimic.make ~name:"git-paf-port"
let git_paf_hostname = Mimic.make ~name:"git-paf-hostname"
let git_paf_sleep = Mimic.make ~name:"git-paf-sleep"

let with_uri uri ctx =
  let scheme =
    match Uri.scheme uri with
    | Some "http" -> Some `HTTP
    | Some "https" -> Some `HTTPS
    | _ -> None
  in
  let port =
    match Uri.port uri, scheme with
    | Some port, _ -> Some port
    | None, Some `HTTP -> Some 80
    | None, Some `HTTPS -> Some 443
    | _ -> None
  in
  let hostname = Uri.host uri in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_paf_scheme v ctx) scheme
  in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add git_paf_port v ctx) port
  in
  let ctx =
    Option.fold ~none:ctx
      ~some:(fun v -> Mimic.add git_paf_hostname v ctx)
      hostname
  in
  ctx

let with_host headers uri =
  match Uri.host uri with
  | None -> headers
  | Some hostname ->
      let hostname =
        match Uri.port uri with
        | Some port -> Fmt.str "%s:%d" hostname port
        | None -> hostname
      in
      Httpaf.Headers.add_unless_exists headers "host" hostname

let with_transfer_encoding ~chunked (meth : [ `GET | `POST ]) body headers =
  match meth, chunked, body, Httpaf.Headers.get headers "content-length" with
  | `GET, _, _, _ -> headers
  | _, (None | Some false), _, Some _ -> headers
  | _, Some true, _, (Some _ | None) | _, None, Some _, None ->
      Httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked"
  | _, (None | Some false), None, None ->
      Httpaf.Headers.add_unless_exists headers "content-length" "0"
  | _, _, Some str, None ->
      Httpaf.Headers.add_unless_exists headers "content-length"
        (string_of_int (String.length str))

type error = exn

let pp_error ppf exn = Fmt.pf ppf "%s" (Printexc.to_string exn)

let with_redirects ?(max = 10) ~f uri =
  if max < 10 then invalid_arg "with_redirects";
  let tbl = Hashtbl.create 0x10 in
  let rec go max uri =
    f uri >>= fun (resp, body) ->
    let status_code = Httpaf.Status.to_code resp.Httpaf.Response.status in
    if status_code / 100 = 3 then (
      Log.debug (fun m -> m "The request must be redirected.");
      match
        Option.map Uri.of_string
          Httpaf.(Headers.get resp.Response.headers "location")
      with
      | Some uri' when Hashtbl.mem tbl uri' || max = 0 ->
          Log.warn (fun m ->
              m
                "We found a cycle (%a) or reach the limit of redirections \
                 (max: %d)."
                Uri.pp uri' max);
          Lwt.return (resp, body)
      | Some uri' ->
          Log.debug (fun m -> m "Redirection to %a." Uri.pp uri');
          Hashtbl.add tbl uri' ();
          (* Cohttp_lwt.Body.drain_body body >>= fun () -> *) go (pred max) uri'
      | None ->
          Log.debug (fun m -> m "The request did not give to us the location.");
          Lwt.return (resp, body))
    else Lwt.return (resp, body)
  in
  go max uri

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

let error_handler mvar err = Lwt.async @@ fun () -> Lwt_mvar.put mvar err

let response_handler mvar pusher resp body =
  let on_eof () = pusher None in
  let rec on_read buf ~off ~len =
    let str = Bigstringaf.substring buf ~off ~len in
    pusher (Some str);
    Httpaf.Body.Reader.schedule_read ~on_eof ~on_read body
  in
  Httpaf.Body.Reader.schedule_read ~on_eof ~on_read body;
  Lwt.async @@ fun () -> Lwt_mvar.put mvar resp

let transmit body = function
  | None -> Httpaf.Body.Writer.close body
  | Some str ->
      Httpaf.Body.Writer.write_string body str;
      Httpaf.Body.Writer.close body

exception Invalid_response_body_length of Httpaf.Response.t
exception Malformed_response of string

let call ?(ctx = Mimic.empty) ?(headers = Httpaf.Headers.empty) ?body ?chunked
    (meth : [ `GET | `POST ]) uri =
  let ctx = with_uri uri ctx in
  let sleep =
    match Mimic.get git_paf_sleep ctx with
    | Some sleep -> sleep
    | None -> fun _ -> Lwt.return_unit
  in
  let headers = with_host headers uri in
  let headers = with_transfer_encoding ~chunked meth body headers in
  let req =
    Httpaf.Request.create ~headers
      (meth :> Httpaf.Method.t)
      (Uri.path_and_query uri)
  in
  let stream, pusher = Lwt_stream.create () in
  let mvar_res = Lwt_mvar.create_empty () in
  let mvar_err = Lwt_mvar.create_empty () in
  let open Lwt.Infix in
  Mimic.resolve ctx >>= function
  | Error (#Mimic.error as err) -> Fmt.failwith "%a" Mimic.pp_error err
  | Ok flow -> (
      let error_handler = error_handler mvar_err in
      let response_handler = response_handler mvar_res pusher in
      let httpaf_body, conn =
        Httpaf.Client_connection.request ~error_handler ~response_handler req
      in
      Lwt.async (fun () ->
          Paf.run ~sleep (module Httpaf_Client_connection) conn flow);
      transmit httpaf_body body;
      Lwt.pick
        [
          (Lwt_mvar.take mvar_res >|= fun res -> `Response res);
          (Lwt_mvar.take mvar_err >|= fun err -> `Error err);
        ]
      >>= function
      | `Error (`Exn exn) -> Mimic.close flow >>= fun () -> raise exn
      | `Error (`Invalid_response_body_length resp) ->
          Mimic.close flow >>= fun () ->
          raise (Invalid_response_body_length resp)
      | `Error (`Malformed_response err) ->
          Mimic.close flow >>= fun () -> raise (Malformed_response err)
      | `Response resp -> Lwt.return (resp, stream))

let body_to_string stream = Lwt_stream.to_list stream >|= String.concat ""
let http_get ?ctx ?headers uri = call ?ctx ?headers `GET uri

let http_post ?ctx ?body ?chunked ?headers uri =
  call ?ctx ?body ?chunked ?headers `POST uri

let get ~ctx ?(headers = []) uri : (_, error) result Lwt.t =
  let headers = Httpaf.Headers.of_list headers in
  let f uri =
    let edn = Smart_git.Endpoint.of_string (Uri.to_string uri) in
    let edn = Rresult.R.get_ok edn in
    let ctx = Smart_git.Endpoint.to_ctx edn ctx in
    http_get ~ctx ~headers uri
  in
  Lwt.catch (fun () ->
      with_redirects ~f uri >>= fun (_resp, body) ->
      body_to_string body >>= fun body -> Lwt.return_ok ((), body))
  @@ fun exn -> Lwt.return_error exn

let post ~ctx ?(headers = []) uri body : (_, error) result Lwt.t =
  let headers = Httpaf.Headers.of_list headers in
  let f uri =
    let edn = Smart_git.Endpoint.of_string (Uri.to_string uri) in
    let edn = Rresult.R.get_ok edn in
    let ctx = Smart_git.Endpoint.to_ctx edn ctx in
    http_post ~ctx ~headers ~chunked:false ~body uri
  in
  Lwt.catch (fun () ->
      with_redirects ~f uri >>= fun (_resp, body) ->
      body_to_string body >>= fun body -> Lwt.return_ok ((), body))
  @@ fun exn -> Lwt.return_error exn
