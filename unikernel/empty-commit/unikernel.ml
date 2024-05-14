open Lwt.Infix

module K = struct
  open Cmdliner

 let remote =
    let doc = Arg.info ~doc:"Remote Git repository." ["r"; "remote"] in
    Arg.(required & opt (some string) None doc)
end

module Make
    (Store : Git.S)
    (_ : sig end) =
struct
  module Sync = Git.Mem.Sync (Store)

  let author () =
    { Git.User.name= "Romain Calascibetta"
    ; email= "romain.calascibetta@gmail.com"
    ; date=
      let ptime = Ptime.unsafe_of_d_ps (Pclock.now_d_ps ()) in
      let tz = match Pclock.current_tz_offset_s () with
        | Some s ->
          let sign = if s < 0 then `Minus else `Plus in
          let hours = s / 3600 in
          let minutes = (s mod 3600) / 60 in
          Some { Git.User.sign; hours; minutes; }
        | None -> None in
      Int64.of_float (Ptime.to_float_s ptime), tz }

  let empty_tree = Store.Value.(tree (Tree.v []))

  let ( >>? ) = Lwt_result.bind

  let commit ?parent ~tree:root ~author msg =
    let open Store.Value in
    let parents = Option.fold ~none:[] ~some:(fun x -> [ x ]) parent in
    commit (Commit.make ~parents ~tree:root ~author ~committer:author msg)

  let failwith pp = function
    | Ok v -> Lwt.return v
    | Error err -> Lwt.fail (Failure (Fmt.str "%a" pp err))

  let empty_commit branch git = function
    | None ->
      Store.write git empty_tree >>? fun (tree, _) ->
      Store.write git (commit ~tree ~author:(author ()) None) >>? fun (hash, _) ->
      Store.Ref.write git branch (Git.Reference.uid hash)
    | Some (_, _) ->
      Store.Ref.resolve git branch >>= failwith Store.pp_error >>= fun hash ->
      Store.read_exn git hash >>= fun obj ->
      let[@warning "-8"] Git.Value.Commit parent = obj in
      let tree = Store.Value.Commit.tree parent in
      Store.write git (commit ~parent:hash ~tree ~author:(author ()) None) >>? fun (hash, _) ->
      Store.Ref.write git branch (Git.Reference.uid hash)

  let capabilities =
    [ `Side_band_64k; `Multi_ack_detailed; `Ofs_delta; `Thin_pack; `Report_status ]

  let start git ctx remote =
    let edn, branch = match String.split_on_char '#' remote with
      | [ edn; branch ] ->
        Rresult.R.failwith_error_msg (Smart_git.Endpoint.of_string edn),
        Rresult.R.failwith_error_msg (Git.Reference.of_string branch)
      | _ ->
        Rresult.R.failwith_error_msg (Smart_git.Endpoint.of_string remote),
        Git.Reference.master in
    Sync.fetch ~capabilities ~ctx edn git ~deepen:(`Depth 1) `All
    >>= failwith Sync.pp_error >>= empty_commit branch git
    >>= failwith Store.pp_error >>= fun () ->
    Sync.push ~capabilities ~ctx edn git
      [ `Update (branch, branch) ]
    >>= failwith Sync.pp_error >>= fun () ->
    Sync.fetch ~capabilities ~ctx edn git ~deepen:(`Depth 1) `All >>= function
    | Ok (Some (hash, references)) -> Lwt.return_unit
    | Ok None -> Lwt.return_unit
    | Error err -> Fmt.failwith "%a" Sync.pp_error err
end
