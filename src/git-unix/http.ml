open Lwt.Infix

module E = struct
  type headers = Cohttp.Header.t
  type t = Net.endpoint
  let uri t = t.Net.uri
  let headers t = t.Net.headers
  let with_uri uri t = { t with Net.uri }
end

module Client = struct
  type headers = Cohttp.Header.t

  type resp = Git_http.Web_cohttp_lwt.resp =
    {resp: Cohttp.Response.t; body: Cohttp_lwt.Body.t}

  type body = unit -> (Cstruct.t * int * int) option Lwt.t
  type meth = Cohttp.Code.meth
  type endpoint = E.t
  type +'a io = 'a Lwt.t

  module Log = struct
    let src = Logs.Src.create "cohttp" ~doc:"logs cohttp event"

    include (val Logs.src_log src : Logs.LOG)
  end

  let call ?headers ?body meth e =
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
    let uri = e.Net.uri in
    let headers = match headers with
      | None   -> e.headers
      | Some h -> Cohttp.Header.add_list e.headers (Cohttp.Header.to_list h)
    in
    Log.debug (fun l -> l ~header:"call" "Send a request to %a." Uri.pp_hum uri) ;
    Cohttp_lwt_unix.Client.call ~headers ?body ~chunked:false meth uri
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
          l ~header:"call" "Redirection from %a to %a." Uri.pp_hum uri
            Uri.pp_hum uri' ) ;
      Cohttp_lwt_unix.Client.call ~headers ~body:(`Stream body_if_redirection)
        ~chunked:false meth uri'
      >|= fun (resp, body) -> {resp; body} )
    else Lwt.return {resp; body= snd v}
end

module Make (S : Git.S) = Git_http.Sync.CohttpMake (Client) (E) (S)
