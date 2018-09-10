module type CONDUIT = sig
  include Conduit_mirage.S

  val context : t
  val resolver : Resolver_lwt.t
end

module Make (C : CONDUIT) = struct
  module Flow = Conduit_mirage.Flow
  module Channel = Mirage_channel_lwt.Make (Flow)

  type socket = {ic: Channel.t; oc: Channel.t}
  type error = Channel.write_error

  let pp_error = Channel.pp_write_error

  module Log = struct
    let src =
      Logs.Src.create "git.mirage.net" ~doc:"logs git's mirage net event"

    include (val Logs.src_log src : Logs.LOG)
  end

  let close socket =
    let open Lwt.Infix in
    let safe_close c =
      Channel.close c
      >>= function
      | Ok () | Error `Closed -> Lwt.return ()
      | Error e ->
          Log.debug (fun l -> l "Ignoring error: %a" Channel.pp_write_error e) ;
          Lwt.return ()
    in
    safe_close socket.ic >>= fun () -> safe_close socket.oc

  let write {oc; _} raw off len =
    let open Lwt.Infix in
    Channel.write_string oc (Bytes.unsafe_to_string raw) off len ;
    Channel.flush oc >|= function Ok () -> Ok len | Error _ as e -> e

  let read {ic; _} raw off len =
    let open Lwt.Infix in
    Channel.read_some ~len ic
    >>= function
    | Ok `Eof -> Lwt.return_ok 0
    | Ok (`Data cs) ->
        let len' = Cstruct.len cs in
        Cstruct.blit_to_bytes cs 0 raw off len' ;
        Lwt.return_ok len'
    | Error err ->
        Log.err (fun l ->
            l ~header:"read" "Catch error when we read: %a." Channel.pp_error
              err ) ;
        (* XXX(dinosaure): Channel.error is not a variant. *)
        Lwt.return_ok 0

  let socket uri =
    let open Lwt.Infix in
    Resolver_lwt.resolve_uri ~uri C.resolver
    >>= fun endp ->
    Conduit_mirage.client endp
    >>= fun client ->
    Conduit_mirage.connect C.context client
    >>= fun flow ->
    let ic = Channel.create flow in
    let oc = Channel.create flow in
    Lwt.return {ic; oc}
end
