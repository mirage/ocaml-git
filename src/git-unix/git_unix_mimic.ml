open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module TCP = struct
  type flow = Lwt_unix.file_descr
  type error = [ `Error of Unix.error * string * string ]
  type write_error = [ `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf = function
    | `Error (err, f, v) ->
        Fmt.pf ppf "%s(%s) : %s" f v (Unix.error_message err)

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.pf ppf "Connection closed by peer"

  let read fd =
    let tmp = Bytes.create 0x1000 in
    let process () =
      Lwt_unix.read fd tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_ok `Eof
      | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let write fd ({ Cstruct.len; _ } as cs) =
    let rec process buf off max =
      Lwt_unix.write fd buf off max >>= fun len ->
      if max - len = 0 then Lwt.return_ok ()
      else process buf (off + len) (max - len)
    in
    let buf = Cstruct.to_bytes cs in
    Lwt.catch (fun () -> process buf 0 len) @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let rec writev fd = function
    | [] -> Lwt.return_ok ()
    | x :: r -> write fd x >>? fun () -> writev fd r

  let close fd = Lwt_unix.close fd

  type endpoint = Lwt_unix.sockaddr

  let connect sockaddr =
    let process () =
      let domain = Unix.domain_of_sockaddr sockaddr in
      let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket sockaddr >>= fun () -> Lwt.return_ok socket
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) -> Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn
end

let tcp, _ = Mimic.register ~name:"tcp" (module TCP)

module TLS = struct
  include Tls_mirage.Make (TCP)

  type endpoint =
    Tls.Config.client * [ `host ] Domain_name.t option * Unix.sockaddr

  let connect (tls, domain_name, sockaddr) =
    TCP.connect sockaddr >|= R.reword_error (fun err -> `Read err)
    >>? fun flow ->
    let host = Option.map Domain_name.to_string domain_name in
    client_of_flow tls ?host flow
end

module Nss = Ca_certs_nss.Make (Ptime_clock)

let tls, _ = Mimic.register ~name:"tls" (module TLS)
let inet_addr = Mimic.make ~name:"inet_addr"
let authenticator = R.get_ok (Nss.authenticator ())
let nss = Tls.Config.client ~authenticator ()
let cfg = Mimic.make ~name:"cfg"

let ctx =
  let open Mimic in
  let k0 = function
    | `Addr ipaddr -> Lwt.return_some (Ipaddr_unix.to_inet_addr ipaddr)
    | `Domain domain_name -> (
        match Unix.gethostbyname (Domain_name.to_string domain_name) with
        | { Unix.h_addr_list; _ } ->
            if Array.length h_addr_list > 0 then Lwt.return_some h_addr_list.(0)
            else Lwt.return_none
        | exception _ -> Lwt.return_none)
  in
  let k1 scheme inet_addr port =
    match scheme with
    | `Git -> Lwt.return_some (Unix.ADDR_INET (inet_addr, port))
    | _ -> Lwt.return_none
  in
  let k2 scheme tls domain_name inet_addr port =
    match scheme with
    | `HTTPS ->
        let domain_name =
          match domain_name with Some (`Domain v) -> Some v | _ -> None
        in
        Lwt.return_some (tls, domain_name, Unix.ADDR_INET (inet_addr, port))
    | _ -> Lwt.return_none
  in
  let k3 scheme inet_addr port =
    match scheme with
    | `HTTP -> Lwt.return_some (Unix.ADDR_INET (inet_addr, port))
    | _ -> Lwt.return_none
  in
  Mimic.empty
  |> Mimic.fold inet_addr Fun.[ req Smart_git.git_host ] ~k:k0
  |> Mimic.fold tcp
       Fun.
         [
           req Smart_git.git_scheme; req inet_addr; dft Smart_git.git_port 9418;
         ]
       ~k:k1
  |> Mimic.fold tls
       Fun.
         [
           req Smart_git.git_scheme; dft cfg nss; opt Smart_git.git_host;
           req inet_addr; dft Smart_git.git_port 443;
         ]
       ~k:k2
  |> Mimic.fold tcp
       Fun.
         [ req Smart_git.git_scheme; req inet_addr; dft Smart_git.git_port 80 ]
       ~k:k3
