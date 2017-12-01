module Client = struct
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

    let body_if_redirection, push = Lwt_stream.create () in

    let body = match body with
      | None -> None
      | Some stream ->
        Lwt_stream.from stream
        |> Lwt_stream.map (fun (buf, off, len) ->
            let s = Cstruct.to_string (Cstruct.sub buf off len) in
            push (Some s); s)
        |> fun stream -> Some (`Stream stream)
    in

    (* XXX(dinosaure): [~chunked:false] is mandatory, I don't want to
       explain why (I lost one day to find this bug) but believe me. *)

    Log.debug (fun l -> l ~header:"call" "Send a request to %a." Uri.pp_hum uri);

    Cohttp_lwt_unix.Client.call ?headers ?body ~chunked:false meth uri >>= fun ((resp, _) as v) ->
    if Cohttp.Code.is_redirection (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
    then begin
      let uri' =
        Cohttp.Response.headers resp
        |> Cohttp.Header.to_list
        |> List.assoc "location"
        |> Uri.of_string
      in

      push None;
      Log.debug (fun l -> l ~header:"call" "Redirection from %a to %a." Uri.pp_hum uri Uri.pp_hum uri');

      Cohttp_lwt_unix.Client.call ?headers ~body:(`Stream body_if_redirection) ~chunked:false meth uri' >>= fun (resp, body) ->
      Lwt.return { resp; body; }
    end else Lwt.return { resp; body = snd v; }
end

module Option =
struct
  let mem v x ~equal = match v with Some x' -> equal x x' | None -> false
  let value_exn v ~error = match v with Some v -> v | None -> raise (Invalid_argument error)
end

module Make (K : Git.Sync.CAPABILITIES) (S : Git.S) = struct

  include Git_http.Make(K)(Client)(S)

  type error' =
    [ `StoreRef of S.Ref.error
    | `Sync of error ]

  module Negociator = Git.Negociator.Make(S)

  exception Jump of S.Ref.error

  let fetch_all t ?locks repository =
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
                | _ -> Lwt.return None)
        refs
    in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository) in
    let https = Option.mem (Uri.scheme repository) "https" ~equal:String.equal in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    fetch t ~https ?port:(Uri.port repository) ~negociate:(continue, state) ~has ~want host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= fun (lst, _) ->
    Lwt.catch
      (fun () ->
         let open Lwt.Infix in
         Lwt_list.iter_s
           (fun (reference, hash) ->
              S.Ref.write t ?locks reference (S.Reference.Hash hash) >>= function
              | Ok _ -> Lwt.return ()
              | Error err -> Lwt.fail (Jump err))
           lst >>= fun () -> Lwt.return (Ok ()))
      (function Jump err -> Lwt.return (Error (`StoreRef err))
              | exn -> Lwt.fail exn) (* XXX(dinosaure): should never happen. *)

  let easy_clone t ?locks ~reference repository =
    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository) in
    let https = Option.mem (Uri.scheme repository) "https" ~equal:String.equal in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    clone t ~https ?port:(Uri.port repository) host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= function
    | hash' ->
      S.Ref.write t ?locks reference (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`StoreRef err))
      >>= fun () -> S.Ref.write t ?locks S.Reference.head (S.Reference.Ref reference)
      >!= (fun err -> Lwt.return (`StoreRef err))

  let fetch_one t ?locks ~reference repository =
    let open Lwt.Infix in

    Negociator.find_common t >>= fun (has, state, continue) ->
    let continue { Decoder.acks; shallow; unshallow } state =
      continue { Git.Negociator.acks; shallow; unshallow } state
    in

    let want refs =
      Lwt_list.filter_map_p
        (function (hash, refname, false) ->
           let reference' = S.Reference.of_string refname in
           if S.Reference.equal reference reference'
           then Lwt.return (Some (reference, hash))
           else Lwt.return None
                | _ -> Lwt.return None)
        refs
    in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository) in
    let https = Option.mem (Uri.scheme repository) "https" ~equal:String.equal in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    fetch t ~https ?port:(Uri.port repository) ~negociate:(continue, state) ~has ~want host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= function
    | [ (reference', hash') ], _ ->
      S.Ref.write t ?locks reference' (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`StoreRef err))
    | _ -> Lwt.return (Ok ()) (* TODO *)

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

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected an http url with host: %a." Uri.pp_hum repository) in
    let https = Option.mem (Uri.scheme repository) "https" ~equal:String.equal in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    push t ~push:push_handler ~https ?port:(Uri.port repository) host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= fun lst ->
    ok (Lwt_list.map_p (function
        | Ok refname -> Lwt.return (Ok (S.Reference.of_string refname))
        | Error (refname, err) -> Lwt.return (Error (S.Reference.of_string refname, err))) lst)
end
