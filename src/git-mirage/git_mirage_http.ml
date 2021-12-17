open Lwt.Infix

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t

  val with_optional_tls_config_and_headers :
    ?headers:(string * string) list ->
    ?tls_key_fingerprint:string ->
    ?tls_cert_fingerprint:string ->
    Mimic.ctx ->
    Mimic.ctx Lwt.t

  val ctx : Mimic.ctx
end

let git_mirage_http_headers = Mimic.make ~name:"git-mirage-http-headers"
let git_mirage_http_tls_config = Mimic.make ~name:"git-mirage-tls-config"

module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (TCP : Tcpip.Tcp.S)
    (Happy_eyeballs : Git_mirage_happy_eyeballs.S with type flow = TCP.flow) :
  S = struct
  module TCP = struct
    include TCP

    type endpoint = Happy_eyeballs.t * string * int

    type nonrec write_error =
      [ `Write of write_error | `Connect of string | `Closed ]

    let pp_write_error ppf = function
      | `Connect err -> Fmt.string ppf err
      | `Write err -> pp_write_error ppf err
      | `Closed as err -> pp_write_error ppf err

    let write flow cs =
      write flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect (happy_eyeballs, hostname, port) =
      Happy_eyeballs.resolve happy_eyeballs hostname [ port ] >>= function
      | Error (`Msg err) -> Lwt.return_error (`Connect err)
      | Ok ((_ipaddr, _port), flow) -> Lwt.return_ok flow
  end

  let tcp_endpoint, _tcp_protocol = Mimic.register ~name:"tcp" (module TCP)

  (* XXX(dinosaure): refactor with [git_mirage_tcp]? *)

  module TLS = struct
    type endpoint = Happy_eyeballs.t * Tls.Config.client * string * int

    include Tls_mirage.Make (TCP)

    let connect (happy_eyeballs, cfg, hostname, port) =
      let peer_name =
        Result.to_option
          (Result.bind (Domain_name.of_string hostname) Domain_name.host)
      in
      Happy_eyeballs.resolve happy_eyeballs hostname [ port ] >>= function
      | Ok ((_ipaddr, _port), flow) -> client_of_flow cfg ?host:peer_name flow
      | Error (`Msg _) -> assert false
    (* TODO *)
  end

  let tls_endpoint, _tls_protocol = Mimic.register ~name:"tls" (module TLS)

  let context tls ctx =
    (* HTTP *)
    let edn = Mimic.make ~name:"paf-http-endpoint" in
    let k0 happy_eyeballs (hostname, port) =
      Lwt.return_some (happy_eyeballs, hostname, port)
    in
    let k1 git_paf_scheme git_paf_hostname git_paf_port =
      match git_paf_scheme with
      | `HTTP -> Lwt.return_some (git_paf_hostname, git_paf_port)
      | _ -> Lwt.return_none
    in
    let ctx =
      Mimic.fold tcp_endpoint
        Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs; req edn ]
        ~k:k0 ctx
    in
    let ctx =
      Mimic.fold edn
        Mimic.Fun.
          [
            req Git_paf.git_paf_scheme; req Git_paf.git_paf_hostname;
            dft Git_paf.git_paf_port 80;
          ]
        ~k:k1 ctx
    in

    (* HTTPS *)
    let edn = Mimic.make ~name:"paf-https-endpoint" in
    let k0 happy_eyeballs (hostname, port) =
      Lwt.return_some (happy_eyeballs, tls, hostname, port)
    in
    let k1 git_paf_scheme git_paf_hostname git_paf_port =
      match git_paf_scheme with
      | `HTTPS -> Lwt.return_some (git_paf_hostname, git_paf_port)
      | _ -> Lwt.return_none
    in
    let ctx =
      Mimic.fold tls_endpoint
        Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs; req edn ]
        ~k:k0 ctx
    in
    let ctx =
      Mimic.fold edn
        Mimic.Fun.
          [
            req Git_paf.git_paf_scheme; req Git_paf.git_paf_hostname;
            dft Git_paf.git_paf_port 443;
          ]
        ~k:k1 ctx
    in

    Mimic.add Git_paf.git_paf_sleep Time.sleep_ns ctx

  module HTTP = struct
    type state =
      | Handshake
      | Get of {
          advertised_refs : string;
          uri : Uri.t;
          headers : (string * string) list;
          ctx : Mimic.ctx;
        }
      | Post of {
          mutable output : string;
          uri : Uri.t;
          headers : (string * string) list;
          ctx : Mimic.ctx;
        }
      | Error

    type flow = { happy_eyeballs : Happy_eyeballs.t; mutable state : state }
    type error = [ `Msg of string ]
    type write_error = [ `Closed | `Msg of string ]

    let pp_error ppf (`Msg err) = Fmt.string ppf err

    let pp_write_error ppf = function
      | `Closed -> Fmt.string ppf "Connection closed by peer"
      | `Msg err -> Fmt.string ppf err

    let write t cs =
      match t.state with
      | Handshake | Get _ ->
          Lwt.return_error (`Msg "Handshake has not been done")
      | Error -> Lwt.return_error (`Msg "Handshake got an error")
      | Post ({ output; _ } as v) ->
          let output = output ^ Cstruct.to_string cs in
          v.output <- output;
          Lwt.return_ok ()

    let writev t css =
      let rec go = function
        | [] -> Lwt.return_ok ()
        | x :: r -> (
            write t x >>= function
            | Ok () -> go r
            | Error _ as err -> Lwt.return err)
      in
      go css

    let read t =
      match t.state with
      | Handshake -> Lwt.return_error (`Msg "Handshake has not been done")
      | Error -> Lwt.return_error (`Msg "Handshake got an error")
      | Get { advertised_refs; uri; headers; ctx } ->
          t.state <- Post { output = ""; uri; headers; ctx };
          Lwt.return_ok (`Data (Cstruct.of_string advertised_refs))
      | Post { output; uri; headers; ctx } -> (
          Git_paf.post ~ctx ~headers uri output >>= function
          | Ok (_resp, contents) ->
              Lwt.return_ok (`Data (Cstruct.of_string contents))
          | Error err ->
              Lwt.return_error (`Msg (Fmt.str "%a" Git_paf.pp_error err)))

    let close _ = Lwt.return_unit

    type endpoint = Happy_eyeballs.t

    let connect happy_eyeballs =
      Lwt.return_ok { happy_eyeballs; state = Handshake }
  end

  let http_endpoint, http_protocol = Mimic.register ~name:"http" (module HTTP)

  type http_endpoint = HTTP_endpoint

  let connect ctx =
    let module T = (val Mimic.repr http_protocol) in
    let edn = Mimic.make ~name:"http-endpoint" in
    let k0 happy_eyeballs HTTP_endpoint = Lwt.return_some happy_eyeballs in
    let k1 git_transmission git_scheme =
      match git_transmission, git_scheme with
      | `HTTP _, (`HTTP | `HTTPS) -> Lwt.return_some HTTP_endpoint
      | _ -> Lwt.return_none
    in
    let k2 happy_eyeballs git_scheme git_uri git_http_headers
        git_mirage_http_headers git_mirage_http_tls_config =
      match git_scheme with
      | `Git | `SSH | `Scheme _ -> Lwt.return_none
      | `HTTP | `HTTPS ->
          let headers = git_http_headers @ git_mirage_http_headers in
          let handshake ~uri0 ~uri1 = function
            | T.T flow -> (
                let ctx =
                  Mimic.add Happy_eyeballs.happy_eyeballs happy_eyeballs
                    Mimic.empty
                in
                let ctx = context git_mirage_http_tls_config ctx in
                Git_paf.get ~ctx ~headers uri0 >>= function
                | Ok (_resp, advertised_refs) ->
                    flow.state <-
                      HTTP.Get { advertised_refs; uri = uri1; headers; ctx };
                    Lwt.return_unit
                | Error _ ->
                    flow.state <- Error;
                    Lwt.return_unit)
            | _ -> Lwt.return_unit
          in
          let git_transmission = `HTTP (git_uri, handshake) in
          Lwt.return_some git_transmission
    in
    let ctx =
      Mimic.fold http_endpoint
        Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs; req edn ]
        ~k:k0 ctx
    in
    let ctx =
      Mimic.fold edn
        Mimic.Fun.[ req Smart_git.git_transmission; req Smart_git.git_scheme ]
        ~k:k1 ctx
    in
    let ctx =
      Mimic.fold Smart_git.git_transmission
        Mimic.Fun.
          [
            req Happy_eyeballs.happy_eyeballs; req Smart_git.git_scheme;
            req Smart_git.git_uri; dft Smart_git.git_http_headers List.[];
            dft git_mirage_http_headers List.[]; req git_mirage_http_tls_config;
          ]
        ~k:k2 ctx
    in
    Lwt.return ctx

  module NSS = Ca_certs_nss.Make (Pclock)

  let of_fp str =
    let hash, fp =
      let hash_of_string = function
        | "md5" -> Some `MD5
        | "sha" | "sha1" -> Some `SHA1
        | "sha224" -> Some `SHA224
        | "sha256" -> Some `SHA256
        | "sha384" -> Some `SHA384
        | "sha512" -> Some `SHA512
        | _ -> None
      in
      match String.split_on_char ':' str with
      | [] -> Fmt.failwith "Invalid fingerprint %S" str
      | [ fp ] -> `SHA256, fp
      | hash :: rest -> (
          match hash_of_string (String.lowercase_ascii hash) with
          | Some hash -> hash, String.concat "" rest
          | None -> Fmt.failwith "Invalid hash algorithm: %S" hash)
    in
    let fp =
      try Hex.to_cstruct (`Hex fp)
      with _ -> Fmt.failwith "Invalid hex fingerprint value: %S" fp
    in
    hash, fp

  let with_optional_tls_config_and_headers ?headers ?tls_key_fingerprint
      ?tls_cert_fingerprint ctx =
    let time () = Some (Ptime.v (Pclock.now_d_ps ())) in
    let authenticator =
      match tls_key_fingerprint, tls_cert_fingerprint with
      | None, None -> (
          match NSS.authenticator () with
          | Ok authenticator -> authenticator
          | Error (`Msg err) -> failwith err)
      | Some _, Some _ ->
          failwith "You can not provide certificate and key fingerprint"
      | Some fp, None ->
          let hash, fingerprint = of_fp fp in
          X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint
      | None, Some fp ->
          let hash, fingerprint = of_fp fp in
          X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint
    in
    let tls = Tls.Config.client ~authenticator () in
    let ctx = Mimic.add git_mirage_http_tls_config tls ctx in
    let ctx =
      Option.fold ~none:ctx
        ~some:(fun headers -> Mimic.add git_mirage_http_headers headers ctx)
        headers
    in
    Lwt.return ctx

  let ctx = Mimic.empty
end
