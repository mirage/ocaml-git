open Lwt.Infix

module Make
    (Store : Git.S)
    (Time : Mirage_time.S)
    (Console : Mirage_console.S)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S)
    (_ : sig end) =
struct
  module Sync = Git.Mem.Sync (Store) (Git_cohttp_mirage)

  exception Invalid_flow

  let start git time console resolver conduit ctx =
    let ctx =
      Git_cohttp_mirage.with_conduit
        (Cohttp_mirage.Client.ctx resolver conduit)
        ctx
    in
    let edn =
      match Smart_git.Endpoint.of_string (Key_gen.remote ()) with
      | Ok edn -> edn
      | Error (`Msg err) -> failwith err in
    Sync.fetch ~ctx edn git ~deepen:(`Depth 1) `All >>= function
    | Ok (Some (hash, references)) -> Lwt.return_unit
    | Ok None -> Lwt.return_unit
    | Error err -> Fmt.failwith "%a" Sync.pp_error err
end
