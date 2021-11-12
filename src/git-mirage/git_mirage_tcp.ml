open Lwt.Infix

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t val ctx : Mimic.ctx
end

module Make
    (TCP : Mirage_protocols.TCP)
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

  let connect ctx =
    let edn = Mimic.make ~name:"tcp-endpoint" in
    let k0 happy_eyeballs (hostname, port) =
      Lwt.return_some (happy_eyeballs, hostname, port)
    in
    let k1 git_transmission git_scheme git_hostname git_port =
      match git_transmission, git_scheme with
      | `Git, `Git -> Lwt.return_some (git_hostname, git_port)
      | _ -> Lwt.return_none
    in
    let k2 git_scheme =
      match git_scheme with
      | `Git -> Lwt.return_some `Git
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
            req Smart_git.git_transmission; req Smart_git.git_scheme;
            req Smart_git.git_hostname; dft Smart_git.git_port 9418;
          ]
        ~k:k1 ctx
    in
    let ctx =
      Mimic.fold Smart_git.git_transmission
        Mimic.Fun.[ req Smart_git.git_scheme ]
        ~k:k2 ctx
    in
    Lwt.return ctx

  let ctx = Mimic.empty
end
