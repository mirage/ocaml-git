open Misc
open Lwt.Infix

let (>>?) = Lwt_result.(>>=)
let (>|?) = Lwt_result.(>|=)

let src = Logs.Src.create "git-unix.fs" ~doc:"logs unix file-system's event"
module Log = (val Logs.src_log src : Logs.LOG)

type error = [ `System of string ]

let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

module Dir = struct

  (* XXX(samoht): add fpath.t parameters *)
  type error =
    [ `Stat of (string * Fpath.t)
    | `Unlink of (string * Fpath.t)
    | `Rmdir of (string * Fpath.t)
    | `Readdir of (string * Fpath.t)
    | `Path of string
    | `Getcwd of string
    | `Mkdir of string ]

  let err x = Lwt.return (Error x)
  let err_unlink file e = err (`Unlink (Unix.error_message e, file))
  let err_readdir dir e = err (`Readdir (Unix.error_message e, dir))
  let err_rmdir dir e = err (`Rmdir (Unix.error_message e, dir))
  let err_stat dir e = err (`Stat (Unix.error_message e, dir))

  (* XXX(samoht): better error messages in plain english *)
  let pp_pair = Fmt.(Dump.pair string Fpath.pp)
  let pp_error ppf = function
    | `Stat err     -> Fmt.pf ppf "(`Stat %a)" pp_pair err
    | `Unlink err   -> Fmt.pf ppf "(`Unlink %a)" pp_pair err
    | `Rmdir err    -> Fmt.pf ppf "(`Rmdir %a)" pp_pair err
    | `Readdir err  -> Fmt.pf ppf "(`Readdir %a)" pp_pair err
    | `Path err     -> Fmt.pf ppf "(`Path %s)" err
    | `Getcwd err   -> Fmt.pf ppf "(`Getcwd %s)" err
    | `Mkdir err    -> Fmt.pf ppf "(`Mkdir %s)" err

  let create = safe_mkdir

  let rec protect ?(n=0) err f =
    let sleep_t = 0.001 in
    if n >= 10_000 then Lwt.fail_with "timeout"
    else Lwt.catch
        (fun () -> f () >|= fun x -> Ok x)
        (fun e ->
           let open Unix in
           Log.debug (fun l -> l "error: %a" Fmt.exn e);
           match e with
           | Unix_error (EINTR, _, _) -> Lwt.return (Error `EAGAIN)
           | Unix_error (EACCES, _, _)
             when Sys.win32           ->
             let s = sleep_t *. (1. +. Random.float (float n)**2.) in
             Log.debug (fun l -> l "sleeping for %.2fs" s);
             Lwt_unix.sleep s >|= fun () ->
             Error `EAGAIN
           | Unix_error (e, _, _)     -> err e
           | exn                      -> expected_unix_error exn)
      >>= function
      | Ok _ as x         -> Lwt.return x
      | Error `EAGAIN     -> protect ~n:(n+1) err f
      | Error #error as e -> Lwt.return e

  let kind path =
    let open Unix in
    let err = function
      | ENOENT -> Lwt.return (Ok `None)
      | e      -> err_stat path e
    in
    protect err (fun () ->
      Lwt_unix.stat (Fpath.to_string path) >|= fun stat ->
      match stat.Lwt_unix.st_kind with
      | Lwt_unix.S_DIR -> `Dir
      | _              -> `File
    )

  let exists path =
    kind path >|? function
    | `Dir -> true
    | _    -> false

  let readdir dir =
    protect (err_readdir dir) (fun () ->
        Lwt_unix.files_of_directory (Fpath.to_string dir)
        |> Lwt_stream.filter (function "." | ".." -> false | _ -> true)
        |> Lwt_stream.map Fpath.v
        |> Lwt_stream.to_list
      )

  let rec delete_file k file =
    kind file >>? function
    | `None   -> Lwt.return (Ok ())
    | `Dir    -> delete_dir k file
    | `File   ->
      let open Unix in
      let err = function
        | ENOENT -> Lwt.return (Ok ())
        | e      -> err_unlink file e
      in
      protect err (fun () ->
          Log.debug (fun l -> l "unlink %a" Fpath.pp file);
          Lwt_unix.unlink (Fpath.to_string file)
        ) >>= k

  and delete_files k = function
    | []       -> k (Ok ())
    | file:: t ->
      delete_file (function
          | Ok ()        -> delete_files k t
          | Error _ as e -> Lwt.return e
        ) file

  and delete_dir k dir: (unit, error) result Lwt.t =
    readdir dir >>? fun files ->
    let files = List.map (fun x -> Fpath.(dir // x)) files in
    let rmdir = function
      | Error _ as e -> Lwt.return e
      | Ok ()        ->
        protect (err_rmdir dir) (fun () ->
            Log.debug (fun l -> l "rmdir %a" Fpath.pp dir);
            Lwt_unix.rmdir (Fpath.to_string dir)
          ) >>= k
    in
    delete_files rmdir files

  let delete ?(recurse=true) dir =
    Log.debug (fun l -> l "Dir.delete %a" Fpath.pp dir);
    assert recurse;
    delete_dir Lwt.return dir

  let contents ?(dotfiles = false) ?(rel = false) dir =
    Log.debug (fun l -> l "Dir.contents %a" Fpath.pp dir);
    let rec aux acc = function
      | []   -> acc
      | h::t ->
        let f = Fpath.to_string h in
        if dotfiles || not (String.get f 0 = '.') then
          aux ((if rel then h else Fpath.(dir // h)) :: acc) t
        else
          aux acc t
    in
    readdir dir >|? aux []

  let rec current () =
    Lwt.try_bind
      (fun () -> Unix.getcwd () |> Lwt.return)
      (fun p -> match Fpath.of_string p with
         | Ok dir ->
           if Fpath.is_abs dir
           then Lwt.return (Ok dir)
           else Lwt.return (Error (`Getcwd (Fmt.strf "getcwd(3) returned a relative path: \
                                                      (%s)"
                                              (Fpath.to_string dir))))
         | Error _ ->
           Lwt.return (Error (`Getcwd (Fmt.strf "get current working directory: \
                                                 cannot parse it to a path (%S)" p))))
      (function Unix.Unix_error (Unix.EINTR, _, _) -> current ()
              | Unix.Unix_error (e, _, _) ->
                Lwt.return (Error (`Getcwd (Fmt.strf "get current working directory: \
                                                      %s" (Unix.error_message e))))
              | exn -> expected_unix_error exn)

  let temp () =
    let from_env var ~absent =
      match try Some (Sys.getenv var) with Not_found -> None with
      | None -> absent
      | Some v ->
        match Fpath.of_string v with
        | Error _ -> absent
        | Ok v -> v
    in

    (if Sys.os_type = "Win32"
     then from_env "TEMP" ~absent:Fpath.(v "./")
     else from_env "TMPDIR" ~absent:Fpath.(v "/tmp/"))
    |> Lwt.return
end

module File = struct

  (* XXX(samoht): add Fpath.t parameters *)
  type error =
    [ `Open of string
    | `Write of string
    | `Read of string
    | `Close of string
    | `Stat of (string * Fpath.t)
    | `Rename of string
    | `Unlink of (string * Fpath.t)
    | `Mkdir of string ]

  type lock = Lock.t

  type 'a fd = Lwt_unix.file_descr
    constraint 'a = [< `Read | `Write ]

  (* XXX(samoht): better error messages in plain english *)
  let pp_pair = Fmt.(Dump.pair string Fpath.pp)
  let pp_error ppf = function
    | `Open err   -> Fmt.pf ppf "(`Open %s)" err
    | `Write err  -> Fmt.pf ppf "(`Write %s)" err
    | `Read err   -> Fmt.pf ppf "(`Read %s)" err
    | `Close err  -> Fmt.pf ppf "(`Close %s)" err
    | `Stat err   -> Fmt.pf ppf "(`Stat %a)" pp_pair err
    | `Rename err -> Fmt.pf ppf "(`Rename %s)" err
    | `Unlink err -> Fmt.pf ppf "(`Unlink %a)" pp_pair err
    | `Mkdir err  -> Fmt.pf ppf "(`Mkdir %s)" err

  let open_w ?lock path ~mode =
    Lock.with_lock lock @@ fun () ->
    Lwt_pool.use open_pool @@ fun () ->
    let rec go () =
      Lwt.try_bind (fun () ->
          Lwt_unix.openfile (Fpath.to_string path) [
            Lwt_unix.O_CREAT;
            Lwt_unix.O_WRONLY;
            Lwt_unix.O_TRUNC;
            Lwt_unix.O_NONBLOCK;
          ] mode)
        (fun fd -> Lwt.return (Ok (fd :> [ `Write ] fd)))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) ->
            Log.warn (fun l ->
                l "Got EINTR while trying to open %a, trying again."
                  Fpath.pp path);
            go ()
          | Unix.Unix_error _ as err ->
            Log.err (fun l ->
                l "Got an error while trying to open %a: %a."
                  Fpath.pp path Fmt.exn err);
            Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let open_r ?lock:_ path ~mode =
    Lwt_pool.use open_pool @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () ->
           Lwt_unix.openfile (Fpath.to_string path) [Lwt_unix.O_RDONLY] mode)
        (fun fd -> Lwt.return (Ok (fd :> [ `Read ] fd)))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let write raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_bytes.write fd (Cstruct.to_bigarray raw) off len)
        (fun n -> Lwt.return (Ok n))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Write x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let read raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_bytes.read fd (Cstruct.to_bigarray raw) off len)
        (fun n -> Lwt.return (Ok n))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Read x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let close fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.close fd)
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Close x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let exists path =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.stat (Fpath.to_string path))
        (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_REG)))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Stat (x, path)) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  (* XXX(samoht): this needs to be protected against EACCESS as
     windows can throw these when moving a directory which is in
     used. *)
  let move ?lock path_a path_b =
    Lock.with_lock lock @@ fun () ->
    let rec go () =
      Lwt.try_bind (fun () ->
          Lwt_unix.rename (Fpath.to_string path_a) (Fpath.to_string path_b))
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Rename x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let delete ?lock path =
    Lock.with_lock lock @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.unlink (Fpath.to_string path))
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error (Unix.ENOENT, _ ,_ ) -> Lwt.return (Ok ())
          (* XXX(dinosaure) decides to quiet this error. *)
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Unlink (x, path)) err)
          | exn -> expected_unix_error exn)
    in
    go ()

end

module Mapper = struct

  type error =
    [ `Stat of string
    | `Close of string
    | `Mmap of string
    | `Open of string ]

  let pp_error ppf = function
    | `Stat err -> Fmt.pf ppf "(`Stat %s)" err
    | `Close err -> Fmt.pf ppf "(`Close %s)" err
    | `Mmap err -> Fmt.pf ppf "(`Mmap %s)" err
    | `Open err -> Fmt.pf ppf "(`Open %s)" err

  type fd = Unix.file_descr

  let length fd =
    Lwt.try_bind
      (fun () -> Unix.LargeFile.fstat fd |> Lwt.return)
      (fun fstat -> Lwt.return (Ok fstat.Unix.LargeFile.st_size))
      (function exn ->
         Log.err (fun l -> l ~header:"Unix.LargeFile.fstat" "Retrieve an exception %s." (Printexc.to_string exn));
         Lwt.return (Error (`Stat "Invalid file descriptor")))

  let openfile path =
    Lwt.try_bind
      (fun () -> Unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY ] 0o644 |> Lwt.return)
      (fun fd -> Lwt.return (Ok fd))
      (function exn ->
         Log.err (fun l -> l ~header:"Unix.openfile" "Retrieve an exception: %s." (Printexc.to_string exn));
         Lwt.return (Error (`Open (Fmt.strf "Invalid file: %s" (Fpath.to_string path)))))

  let close fd =
    Lwt.try_bind
      (fun () -> let () = Unix.close fd in Lwt.return ())
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err ->
         Log.err (fun l -> l ~header:"Unix.close" "Retrieve an exception: %s." (Printexc.to_string err));
         Lwt.return (error_to_result ~ctor:(fun x -> `Close x) err)
              | exn -> expected_unix_error exn)

  let map fd ?pos ~share:_ len =
    length fd >>= function
    | Ok max ->
      let max = match pos with
        | Some pos -> Int64.sub max pos
        | None -> max
      in

      Lwt.try_bind
        (fun () ->
           Lwt_bytes.map_file ~fd ?pos ~shared:false ~size:(Int64.to_int (min (Int64.of_int len) max)) ()
           |> Lwt.return)
        (fun rs -> Lwt.return (Ok (Cstruct.of_bigarray rs)))
        (function exn ->
           Log.err (fun l -> l ~header:"Lwt_bytes.map_file" "Retrieve an exception %s." (Printexc.to_string exn));
           Lwt.return (Error (`Mmap "Impossible to map the file descriptor")))
    | Error err -> Lwt.return (Error err)

end

let is_dir = is_dir
let is_file = is_file
let has_global_checkout = true
let has_global_watches = true
