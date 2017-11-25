module Default =
struct
  let default =
    [ `Multi_ack_detailed
    ; `Thin_pack
    ; `Side_band_64k
    ; `Ofs_delta
    ; `Agent "git/2.0.0"
    ; `Report_status
    ; `No_done ]
end

module Option =
struct
  let mem v x ~equal = match v with Some x' -> equal x x' | None -> false
  let value_exn v ~error = match v with Some v -> v | None -> raise (Invalid_argument error)
end

module Net =
struct
  module Log =
  struct
    let src = Logs.Src.create "git-tcp.net" ~doc:"logs git's net I/O event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type socket = Lwt_unix.file_descr

  let read = Lwt_unix.read
  let write = Lwt_unix.write
  let close = Lwt_unix.close

  let socket host port =
    let open Lwt.Infix in

    Log.debug (fun l -> l ~header:"socket" "Start to open connection on %s:%d." host port);

    let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.gethostbyname host >>= fun he ->
    Lwt_unix.connect socket (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >|= fun () -> socket
end

module Make
    (K : Git.Sync.CAPABILITIES)
    (S : Git.Minimal.S
           with type Hash.Digest.buffer = Cstruct.t
            and type Hash.hex = string)
= struct
  include Git.Sync.Make(Net)(S)(K)

  type error' =
    [ `StoreRef of S.Ref.error
    | `Sync of error ]

  module Negociator = Git.Negociator.Make(S)

  module Log =
  struct
    let src = Logs.Src.create "git-tcp" ~doc:"logs git's tcp event"
    include (val Logs.src_log src : Logs.LOG)
  end

  exception Jump of S.Ref.error

  let fetch_all t ?locks repository =
    let open Lwt.Infix in

    Negociator.find_common t >>= fun (has, state, continue) ->
    let continue { Client.Decoder.acks; shallow; unshallow } state =
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

    let notify _ = Lwt.return () in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    fetch t ?port:(Uri.port repository) ~notify ~negociate:(continue, state) ~has ~want host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= fun (lst, _) ->
    Lwt.catch
      (fun () ->
         let open Lwt.Infix in
         Lwt_list.iter_s
           (fun (reference, hash) ->
              Log.debug (fun l -> l ~header:"fetch_all" "Update reference %a to %a."
                            S.Reference.pp reference S.Hash.pp hash);

              S.Ref.write t ?locks reference (S.Reference.Hash hash) >>= function
              | Ok _ -> Lwt.return ()
              | Error err -> Lwt.fail (Jump err))
           lst >>= fun () -> Lwt.return (Ok ()))
      (function Jump err -> Lwt.return (Error (`StoreRef err)))

  let fetch_one t ?locks ~reference repository =
    let open Lwt.Infix in

    Negociator.find_common t >>= fun (has, state, continue) ->
    let continue { Client.Decoder.acks; shallow; unshallow } state =
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

    let notify _ = Lwt.return () in

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    fetch t ?port:(Uri.port repository) ~notify ~negociate:(continue, state) ~has ~want host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= function
    | [ (reference', hash') ], _ ->
      Log.debug (fun l -> l ~header:"fetch_one" "Update reference %a to %a."
                    S.Reference.pp reference' S.Hash.pp hash');

      S.Ref.write t ?locks reference' (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`StoreRef err))
    | _ -> Lwt.return (Ok ()) (* TODO *)

  let easy_clone t ?locks ~reference repository =
    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    clone t ?port:(Uri.port repository) ~reference host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= function
    | hash' ->
      Log.debug (fun l -> l ~header:"easy_clone" "Update reference %a to %a."
                    S.Reference.pp reference S.Hash.pp hash');

      S.Ref.write t ?locks reference (S.Reference.Hash hash')
      >!= (fun err -> Lwt.return (`StoreRef err))
      >>= fun () -> S.Ref.write t ?locks S.Reference.head (S.Reference.Ref reference)
      >!= (fun err -> Lwt.return (`StoreRef err))

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

    let host = Option.value_exn (Uri.host repository) ~error:(Fmt.strf "Expected a git url with host: %a." Uri.pp_hum repository) in
    let _ =
      if not (Option.mem (Uri.scheme repository) "git" ~equal:String.equal)
      then raise (Invalid_argument "Expected a git url");
    in

    let open Lwt_result in
    let ( >!= ) = Lwt_result.bind_lwt_err in

    push t ~push:push_handler ?port:(Uri.port repository) host (Uri.path_and_query repository)
    >!= (fun err -> Lwt.return (`Sync err))
    >>= fun lst ->
    ok (Lwt_list.map_p (function
        | Ok refname -> Lwt.return (Ok (S.Reference.of_string refname))
        | Error (refname, err) -> Lwt.return (Error (S.Reference.of_string refname, err))) lst)
end
