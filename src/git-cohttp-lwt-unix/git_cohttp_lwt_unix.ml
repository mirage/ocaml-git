module CohttpClient =
struct
  type headers = Cohttp.Header.t
  type resp = Git_http.Web_cohttp_lwt.resp =
    { resp : Cohttp.Response.t
    ; body : Cohttp_lwt.Body.t }
  type body = unit -> (Cstruct.t * int * int) option Lwt.t
  type meth = Cohttp.Code.meth
  type uri = Uri.t

  type +'a io = 'a Lwt.t

  module Log =
  struct
    let src = Logs.Src.create "cohttp" ~doc:"logs cohttp event"
    include (val Logs.src_log src : Logs.LOG)
  end

  let call ?headers ?body meth uri =
    let open Lwt.Infix in

    let body = match body with
      | None -> None
      | Some stream ->
        Lwt_stream.from stream
        |> Lwt_stream.map (fun (buf, off, len) -> Cstruct.to_string (Cstruct.sub buf off len))
        |> fun stream -> Some (`Stream stream)
    in

    (* XXX(dinosaure): [~chunked:true] is mandatory, I don't want to
       explain why (I lost one day to find this bug) but believe me. *)
    Cohttp_lwt_unix.Client.call ?headers ?body ~chunked:false meth uri >>= fun ((resp, _) as v) ->
    if Cohttp.Code.is_redirection (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
    then begin
      let uri' =
        Cohttp.Response.headers resp
        |> Cohttp.Header.to_list
        |> List.assoc "location"
        |> Uri.of_string
      in

      Log.info (fun l -> l ~header:"call" "Redirection to %a." Uri.pp_hum uri);

      Cohttp_lwt_unix.Client.call ?headers ?body ~chunked:false meth uri >>= fun (resp, body) ->
      Lwt.return { resp; body; }
    end else Lwt.return { resp; body = snd v; }
end

module Make
    (K : Git.Sync.CAPABILITIES)
    (S : Git.Minimal.S with type Hash.Digest.buffer = Cstruct.t
                        and type Hash.hex = string)
= struct
  include Git_http.Make(K)(CohttpClient)(S)
  module Negociator = Git.Negociator.Make(S)

  let fetch_all t repository =
    let open Lwt.Infix in

    Negociator.find_common t >>= fun (has, state, continue) ->
    let continue { Decoder.acks; shallow; unshallow } state =
      continue { Git.Negociator.acks; shallow; unshallow } state
    in

    let want refs =
      Lwt_list.filter_map_p
        (function (hash, refname, false) ->
           let reference = S.Reference.of_string refname in
           Lwt.return (Some (reference, hash))
           (* XXX(dinosaure): we consider than remote reference was binded with local reference (which has the same name). *)
                | _ -> Lwt.return None)
        refs
    in
    let host = match Uri.host repository with
      | Some host -> host
      | None -> raise (Invalid_argument (Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository))
    in
    let https = match Uri.scheme repository with
      | Some "https" -> true
      | _ -> false
    in

    fetch t ~https ?port:(Uri.port repository) ~negociate:(continue, state) ~has ~want host (Uri.path_and_query repository)

  let easy_update t ~reference repository =
    let open Lwt.Infix in

    let push_handler git remote_refs =
      S.Ref.list git >>=
      Lwt_list.find_s (fun (reference', _) -> Lwt.return S.Reference.(equal reference reference')) >>= fun (_, local_hash) ->
      Lwt_list.find_s (function (_, refname, false) -> Lwt.return S.Reference.(equal reference (of_string refname))
                              | (_, _, true) -> Lwt.return false) remote_refs >>= fun (remote_hash, remote_refname, _) ->
      if S.Hash.equal local_hash remote_hash
      then Lwt.return ([], [])
      else Lwt.return ([], [ `Update (remote_hash, local_hash, remote_refname) ])
    in

    let host = match Uri.host repository with
      | Some host -> host
      | None -> raise (Invalid_argument (Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository))
    in
    let https = match Uri.scheme repository with
      | Some "https" -> true
      | _ -> false
    in

    let open Lwt_result in

    push t ~push:push_handler ~https ?port:(Uri.port repository) host (Uri.path_and_query repository) >>= fun lst ->
    ok (Lwt_list.map_p (function
        | Ok refname -> Lwt.return (Ok (S.Reference.of_string refname))
        | Error (refname, err) -> Lwt.return (Error (S.Reference.of_string refname, err))) lst)
end
