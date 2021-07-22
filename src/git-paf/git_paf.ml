open Lwt.Infix

let src = Logs.Src.create "git.paf"

module Log = (val Logs.src_log src : Logs.LOG)

let scheme = Mimic.make ~name:"git-scheme"
let port = Mimic.make ~name:"git-port"
let domain_name = Mimic.make ~name:"git-domain-name"
let ipaddr = Mimic.make ~name:"git-ipaddr"
let sleep = Mimic.make ~name:"git-sleep"

let with_uri uri ctx =
  let scheme_v =
    match Uri.scheme uri with
    | Some "http" -> Some `HTTP
    | Some "https" -> Some `HTTPS
    | _ -> None
  in
  let port_v =
    match Uri.port uri, scheme_v with
    | Some port, _ -> Some port
    | None, Some `HTTP -> Some 80
    | None, Some `HTTPS -> Some 443
    | _ -> None
  in
  let domain_name_v, ipaddr_v =
    match Uri.host uri with
    | Some v -> (
        match
          Rresult.(Domain_name.(of_string v >>= host)), Ipaddr.of_string v
        with
        | _, Ok v -> None, Some v
        | Ok v, _ -> Some v, None
        | _ -> None, None)
    | _ -> None, None
  in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add scheme v ctx) scheme_v
  in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add port v ctx) port_v
  in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add ipaddr v ctx) ipaddr_v
  in
  let ctx =
    Option.fold ~none:ctx
      ~some:(fun v -> Mimic.add domain_name v ctx)
      domain_name_v
  in
  ctx

let with_host headers uri =
  let hostname = Uri.host_with_default ~default:"localhost" uri in
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
      (* XXX(dinosaure): I'm not sure that the [Some _] was right. *)
      Httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked"
  | _, (None | Some false), None, None ->
      Httpaf.Headers.add_unless_exists headers "content-length" "0"
  | _, _, Some str, None ->
      Httpaf.Headers.add_unless_exists headers "content-length"
        (string_of_int (String.length str))

type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

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
    Httpaf.Body.schedule_read ~on_eof ~on_read body
  in
  Httpaf.Body.schedule_read ~on_eof ~on_read body;
  Lwt.async @@ fun () -> Lwt_mvar.put mvar resp

(*
let rec unroll body stream =
  let open Lwt.Infix in
  Lwt_stream.get stream >>= function
  | Some str ->
      Httpaf.Body.write_string body str ;
      unroll body stream
  | None ->
      Httpaf.Body.close_writer body ;
      Lwt.return_unit
*)

let transmit body = function
  | None -> Httpaf.Body.close_writer body
  | Some str ->
      Httpaf.Body.write_string body str;
      Httpaf.Body.close_writer body

exception Invalid_response_body_length of Httpaf.Response.t
exception Malformed_response of string

let call ?(ctx = Mimic.empty) ?(headers = Httpaf.Headers.empty) ?body ?chunked
    (meth : [ `GET | `POST ]) uri =
  let ctx = with_uri uri ctx in
  let sleep =
    match Mimic.get sleep ctx with
    | Some sleep -> sleep
    | None -> fun _ -> Lwt.return_unit
    (* TODO *)
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
  | Error (#Mimic.error as err) ->
      Lwt.fail (Failure (Fmt.str "%a" Mimic.pp_error err))
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
      | `Error (`Exn exn) -> Mimic.close flow >>= fun () -> Lwt.fail exn
      | `Error (`Invalid_response_body_length resp) ->
          Mimic.close flow >>= fun () ->
          Lwt.fail (Invalid_response_body_length resp)
      | `Error (`Malformed_response err) ->
          Mimic.close flow >>= fun () -> Lwt.fail (Malformed_response err)
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
  with_redirects ~f uri >>= fun (_resp, body) ->
  body_to_string body >>= fun body -> Lwt.return_ok ((), body)

let post ~ctx ?(headers = []) uri body : (_, error) result Lwt.t =
  let headers = Httpaf.Headers.of_list headers in
  let f uri =
    let edn = Smart_git.Endpoint.of_string (Uri.to_string uri) in
    let edn = Rresult.R.get_ok edn in
    let ctx = Smart_git.Endpoint.to_ctx edn ctx in
    http_post ~ctx ~headers ~chunked:false ~body uri
  in
  with_redirects ~f uri >>= fun (_resp, body) ->
  body_to_string body >>= fun body -> Lwt.return_ok ((), body)

module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Mirage_stack.V4V6) (TCP' : sig
      val tcp_endpoint : (Stack.t * Ipaddr.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) =
struct
  module Nss = Ca_certs_nss.Make (Pclock)

  module TLS = struct
    module Log = (val Logs.src_log src : Logs.LOG)
    include Tls_mirage.Make (Stack.TCP)

    type endpoint =
      [ `host ] Domain_name.t option
      * Tls.Config.client
      * Stack.t
      * Ipaddr.t
      * int

    let connect (domain_name, cfg, stack, ipaddr, port) =
      let t = Stack.tcp stack in
      Stack.TCP.create_connection t (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow ->
          client_of_flow cfg
            ?host:(Option.map Domain_name.to_string domain_name)
            flow
  end

  let tls_edn, _tls_protocol = Mimic.register ~name:"tls" (module TLS)
  let authenticator = Rresult.R.failwith_error_msg (Nss.authenticator ())
  let default_tls_cfg = Tls.Config.client ~authenticator ()
  let tls = Mimic.make ~name:"git-paf-tls"

  let ctx =
    let k0 scheme stack ipaddr port =
      match scheme with
      | `HTTP -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none
    in
    let k1 scheme host cfg stack ipaddr port =
      match scheme, host with
      | `HTTPS, `Domain domain_name ->
          Lwt.return_some (Some domain_name, cfg, stack, ipaddr, port)
      | `HTTPS, _ -> Lwt.return_some (None, cfg, stack, ipaddr, port)
      | _ -> Lwt.return_none
    in
    Mimic.empty
    |> Mimic.add sleep Time.sleep_ns
    |> Mimic.(
         fold TCP'.tcp_endpoint
           Fun.
             [
               req Smart_git.git_scheme; req TCP'.tcp_stack;
               req TCP'.tcp_ipaddr; dft Smart_git.git_port 80;
             ]
           ~k:k0)
    |> Mimic.(
         fold tls_edn
           Fun.
             [
               req Smart_git.git_scheme; req Smart_git.git_host;
               dft tls default_tls_cfg; req TCP'.tcp_stack; req TCP'.tcp_ipaddr;
               dft Smart_git.git_port 443;
             ]
           ~k:k1)
end
