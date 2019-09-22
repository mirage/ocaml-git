module Net = Net

type endpoint = Net.endpoint =
  { uri: Uri.t
  ; conduit: Conduit_mirage.t
  ; resolver: Resolver_lwt.t
  ; headers: Cohttp.Header.t }

let endpoint ?(conduit = Conduit_mirage.empty) ?resolver ?headers uri =
  let resolver =
    match resolver with None -> Resolver_lwt.init () | Some r -> r
  in
  let headers =
    match headers with None -> Cohttp.Header.of_list [] | Some h -> h
  in
  {Net.conduit; resolver; headers; uri}

module Sync (G : Git.S) = struct
  module Endpoint = struct
    type t = endpoint

    let uri e = e.uri
    let with_uri uri e = {e with uri}

    type headers = Cohttp.Header.t

    let headers e = e.headers
  end

  module Tcp = Git.Tcp.Make (Net) (Endpoint) (G)

  module Client = struct
    (* XXX(samoht): too much copy/paste from git-unix ... *)

    open Lwt.Infix

    type headers = Cohttp.Header.t

    type resp = Git_http.Web_cohttp_lwt.resp =
      {resp: Cohttp.Response.t; body: Cohttp_lwt.Body.t}

    type body = unit -> (Cstruct.t * int * int) option Lwt.t
    type meth = Cohttp.Code.meth
    type nonrec endpoint = endpoint
    type +'a io = 'a Lwt.t

    module Log = struct
      let src = Logs.Src.create "cohttp" ~doc:"logs cohttp event"

      include (val Logs.src_log src : Logs.LOG)
    end

    let ctx e = Cohttp_mirage.Client.ctx e.resolver e.conduit
    let uri e = e.uri

    let call ?headers ?body meth (e : endpoint) =
      let body_if_redirection, push = Lwt_stream.create () in
      let body =
        match body with
        | None -> None
        | Some stream ->
            Lwt_stream.from stream
            |> Lwt_stream.map (fun (buf, off, len) ->
                   let s = Cstruct.to_string (Cstruct.sub buf off len) in
                   push (Some s) ; s )
            |> fun stream -> Some (`Stream stream)
      in
      (* XXX(dinosaure): [~chunked:false] is mandatory, I don't want to explain
         why (I lost one day to find this bug) but believe me. *)
      Log.debug (fun l -> l "Send a request to %a." Uri.pp_hum (uri e)) ;
      Cohttp_mirage.Client.call ~ctx:(ctx e) ?headers ?body ~chunked:false meth
        (uri e)
      >>= fun ((resp, _) as v) ->
      if
        Cohttp.Code.is_redirection
          (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
      then (
        let uri' =
          Cohttp.Response.headers resp
          |> Cohttp.Header.to_list
          |> List.assoc "location"
          |> Uri.of_string
        in
        push None ;
        Log.debug (fun l ->
            l "Redirection from %a to %a." Uri.pp_hum (uri e) Uri.pp_hum uri'
        ) ;
        Cohttp_mirage.Client.call ~ctx:(ctx e) ?headers
          ~body:(`Stream body_if_redirection) ~chunked:false meth uri'
        >|= fun (resp, body) -> {resp; body} )
      else Lwt.return {resp; body= snd v}
  end

  module Http = Git_http.Sync.CohttpMake (Client) (Endpoint) (G)

  type error = Tcp of Tcp.error | Http of Http.error

  let pp_error ppf = function
    | Tcp x -> Tcp.pp_error ppf x
    | Http x -> Http.pp_error ppf x

  let dispatch e f =
    match Uri.scheme e.uri with
    | Some "git" -> f `Tcp
    | Some ("http" | "https") -> f `Http
    | Some s -> Fmt.invalid_arg "%a: invalid scheme (%s)" Uri.pp_hum e.uri s
    | None -> Fmt.invalid_arg "%a: missing scheme" Uri.pp_hum e.uri

  type command =
    [ `Create of G.Hash.t * Git.Reference.t
    | `Delete of G.Hash.t * Git.Reference.t
    | `Update of G.Hash.t * G.Hash.t * Git.Reference.t ]

  let pp_command = Tcp.pp_command
  let pp_fetch_one = Tcp.pp_fetch_one

  let tcp_error x =
    Lwt.map (function Ok _ as x -> x | Error e -> Error (Tcp e)) x

  let http_error x =
    Lwt.map (function Ok _ as x -> x | Error e -> Error (Http e)) x

  let push t ~push ?capabilities e =
    dispatch e (function
      | `Tcp -> Tcp.push t ~push ?capabilities e |> tcp_error
      | `Http -> Http.push t ~push ?capabilities e |> http_error )

  let ls t ?capabilities e =
    dispatch e (function
      | `Tcp -> Tcp.ls t ?capabilities e |> tcp_error
      | `Http -> Http.ls t ?capabilities e |> http_error )

  let fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want ?deepen e =
    dispatch e (function
      | `Tcp ->
          Tcp.fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want
            ?deepen e
          |> tcp_error
      | `Http ->
          Http.fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want
            ?deepen e
          |> http_error )

  let clone t ?capabilities ~reference e =
    dispatch e (function
      | `Tcp -> Tcp.clone t ?capabilities ~reference e |> tcp_error
      | `Http -> Http.clone t ?capabilities ~reference e |> http_error )

  let fetch_some t ?capabilities ~references e =
    dispatch e (function
      | `Tcp -> Tcp.fetch_some t ?capabilities ~references e |> tcp_error
      | `Http -> Http.fetch_some t ?capabilities ~references e |> http_error )

  let fetch_all t ?capabilities ~references e =
    dispatch e (function
      | `Tcp -> Tcp.fetch_all t ?capabilities ~references e |> tcp_error
      | `Http -> Http.fetch_all t ?capabilities ~references e |> http_error )

  let fetch_one t ?capabilities ~reference e =
    dispatch e (function
      | `Tcp -> Tcp.fetch_one t ?capabilities ~reference e |> tcp_error
      | `Http -> Http.fetch_one t ?capabilities ~reference e |> http_error )

  let update_and_create t ?capabilities ~references e =
    dispatch e (function
      | `Tcp ->
          Tcp.update_and_create t ?capabilities ~references e |> tcp_error
      | `Http ->
          Http.update_and_create t ?capabilities ~references e |> http_error )
end
