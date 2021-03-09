open Lwt.Infix

let src = Logs.Src.create "git.paf"

module Log = (val Logs.src_log src : Logs.LOG)

type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let with_redirects ?(max = 10) ~f uri =
  if max < 10 then invalid_arg "with_redirects";
  let tbl = Hashtbl.create 0x10 in
  let rec go max uri =
    f uri >>= fun (resp, body) ->
    let status_code = Cohttp.(Response.status resp |> Code.code_of_status) in
    if Cohttp.Code.is_redirection status_code then (
      Log.debug (fun m -> m "The request must be redirected.");
      match Cohttp.(Response.headers resp |> Header.get_location) with
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
          Cohttp_lwt.Body.drain_body body >>= fun () -> go (pred max) uri'
      | None ->
          Log.debug (fun m -> m "The request did not give to us the location.");
          Lwt.return (resp, body))
    else Lwt.return (resp, body)
  in
  go max uri

let get ~ctx ?(headers = []) uri : (_, error) result Lwt.t =
  let headers = Cohttp.Header.of_list headers in
  let f uri =
    let edn = Smart_git.Endpoint.of_string (Uri.to_string uri) in
    let edn = Rresult.R.get_ok edn in
    let ctx = Smart_git.Endpoint.to_ctx edn ctx in
    Paf_cohttp.get ~ctx ~headers uri
  in
  with_redirects ~f uri >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)

let post ~ctx ?(headers = []) uri body : (_, error) result Lwt.t =
  let headers = Cohttp.Header.of_list headers in
  let body = Cohttp_lwt.Body.of_string body in
  let f uri =
    let edn = Smart_git.Endpoint.of_string (Uri.to_string uri) in
    let edn = Rresult.R.get_ok edn in
    let ctx = Smart_git.Endpoint.to_ctx edn ctx in
    Paf_cohttp.post ~ctx ~headers ~chunked:false ~body uri
  in
  with_redirects ~f uri >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return_ok ((), body)

module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Mirage_stack.V4V6)
    (Paf : Paf_mirage.S with type stack = Stack.t) (TCP : sig
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) =
struct
  module Nss = Ca_certs_nss.Make (Pclock)

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
      Fmt.epr ">>> Try to instanticate a <tls-edn>.\n%!";
      match scheme, host with
      | `HTTPS, `Domain domain_name ->
          Lwt.return_some (Some domain_name, cfg, stack, ipaddr, port)
      | `HTTPS, _ -> Lwt.return_some (None, cfg, stack, ipaddr, port)
      | _ ->
          Fmt.epr ">>> The scheme is wrong.\n%!";
          Lwt.return_none
    in
    Mimic.empty
    |> Mimic.add Paf_cohttp.sleep Time.sleep_ns
    |> Mimic.(
         fold Paf.tcp_edn
           Fun.
             [
               req Smart_git.git_scheme; req TCP.tcp_stack; req TCP.tcp_ipaddr;
               dft Smart_git.git_port 80;
             ]
           ~k:k0)
    |> Mimic.(
         fold Paf.tls_edn
           Fun.
             [
               req Smart_git.git_scheme; req Smart_git.git_host;
               dft tls default_tls_cfg; req TCP.tcp_stack; req TCP.tcp_ipaddr;
               dft Smart_git.git_port 443;
             ]
           ~k:k1)
end
