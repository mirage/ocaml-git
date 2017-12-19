open Misc

type t = Fpath.t
type elt = Fpath.t

let rec is_stale max_age path =
  let open Lwt.Infix in

  Lwt_unix.file_exists (Fpath.to_string path) >>= function
  | true ->
    Lwt.try_bind
      (fun () -> Lwt_unix.stat (Fpath.to_string path))
      (fun s -> Lwt.return (Unix.gettimeofday () -. s.Unix.st_mtime > max_age))
      (function
        | Unix.Unix_error (Unix.EINTR, _, _) -> is_stale max_age path
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
        | exn -> Lwt.fail exn)
  | false -> Lwt.return false

let rec unlock path =
  Lwt.catch
    (fun () -> Lwt_unix.unlink (Fpath.to_string path))
    (function
      | Unix.Unix_error (Unix.EINTR, _, _) -> unlock path
      | exn -> Lwt.fail exn)

let lock ?(max_age = 10. *. 60. (* 10 minutes *)) ?(sleep = 0.001) path =
  let open Lwt.Infix in

  let rec step i =
    is_stale max_age path >>= function
    | true -> unlock path >>= fun () -> step 1
    | false ->
      let make () =
        let pid = Unix.getpid () in
        safe_mkdir (Fpath.parent path) >>= function
        | Error _ -> assert false (* TODO *)
        | Ok _ ->
          Log.debug (fun l -> l ~header:"lock" "Create the lock file %a." Fpath.pp path);

          Lwt_unix.openfile
            (Fpath.to_string path)
            [ Unix.O_CREAT
            ; Unix.O_RDWR
            ; Unix.O_EXCL ]
            0o600
          >>= fun fd ->
          let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
          Lwt_io.write_int oc pid >>= fun () ->
          Lwt_unix.close fd
      in

      let rec go () =
        Lwt.catch make
          (function
            | Unix.Unix_error (Unix.EEXIST, _, _) ->
              let backoff = 1. +. Random.float (let i = float i in i *. i) in
              Lwt_unix.sleep (sleep *. backoff) >>= fun () ->
              step (i + 1)
            | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
            | exn -> Lwt.fail exn)
      in

      go ()
  in step 1

let make root key = Fpath.(root / "locks" // key)
let v x = x
let lock path = lock path

let with_lock path f =
  let open Lwt.Infix in

  match path with
  | None -> f ()
  | Some path -> lock path >>= fun () -> Lwt.finalize f (fun () -> unlock path)
