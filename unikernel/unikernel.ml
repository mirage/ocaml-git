open Lwt.Infix

module Make
    (Store : Git.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Console : Mirage_console.S)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S)
    (_ : sig end) =
struct
  module Sync = Git.Mem.Sync (Store) (Git_cohttp_mirage)

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

  let blob0 =
    let open Store.Value in
    blob (Blob.of_string "")
  let hash_blob0 = Store.Value.digest blob0

  let tree0 =
    let open Store.Value in
    tree (Tree.v [ Tree.entry ~name:"foo" `Normal hash_blob0 ])
  let hash_tree0 = Store.Value.digest tree0

  let commit0 ~author =
    let open Store.Value in
    commit (Commit.make ~tree:hash_tree0 ~author ~committer:author ".")

  let root_commit git =
    let ( >>? ) x f = x >>= function
      | Ok x -> f x | Error err -> Lwt.return_error err in
    let author = author () in
    Store.write git (commit0 ~author) >>? fun (hash, _) ->
    Store.Ref.write git Git.Reference.master (Git.Reference.uid hash)

  let failwith_store = function
    | Ok v -> Lwt.return v
    | Error err -> Fmt.failwith "%a" Store.pp_error err

  let failwith_sync = function
    | Ok v -> Lwt.return v
    | Error err -> Fmt.failwith "%a" Sync.pp_error err

  let start git _pclock time console resolver conduit ctx =
    let ctx =
      Git_cohttp_mirage.with_conduit
        (Cohttp_mirage.Client.ctx resolver conduit)
        ctx
    in
    let edn =
      match Smart_git.Endpoint.of_string (Key_gen.remote ()) with
      | Ok edn -> edn
      | Error (`Msg err) -> Fmt.failwith "%s" err in
    root_commit git >>= failwith_store >>= fun () ->
    let capabilities = [ `Side_band_64k ] in
    Sync.push ~capabilities ~ctx edn git [ `Update (Git.Reference.master, Git.Reference.master) ] >>= failwith_sync >>= fun () ->
    Sync.fetch ~capabilities ~ctx edn git ~deepen:(`Depth 1) `All >>= function
    | Ok (Some (hash, references)) -> Lwt.return_unit
    | Ok None -> Lwt.return_unit
    | Error err -> Fmt.failwith "%a" Sync.pp_error err
end
