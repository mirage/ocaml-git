open GitTypes
open Lwt

let main () =
(*  Log.(set_log_level DEBUG); *)
  GitLocal.set_auto_flush false;
  let t = GitLocal.create () in
  GitLocal.dump t >>= fun () ->
  GitGraph.to_dot t "graph.dot" >>= fun () ->
  return_unit

let () =
  Log.color_on ();
  Lwt_unix.run (main ())
