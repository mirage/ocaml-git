open Misc
open Lwt.Infix

let src = Logs.Src.create "git-unix.fs" ~doc:"logs unix file-system's event"
module Log = (val Logs.src_log src : Logs.LOG)

type error = [ `System of string ]

let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

module Dir = struct

  type error =
    [ `Stat of string
    | `Unlink of string
    | `Rmdir of string
    | `Opendir of (string * Fpath.t)
    | `Path of string
    | `Getcwd of string
    | `Mkdir of string ]

  let pp_error ppf = function
    | `Stat err    -> Fmt.pf ppf "(`Stat %s)" err
    | `Unlink err  -> Fmt.pf ppf "(`Unlink %s)" err
    | `Rmdir err   -> Fmt.pf ppf "(`Rmdir %s)" err
    | `Opendir err -> Fmt.pf ppf "(`Opendir %a)" (Fmt.Dump.pair Fmt.string Fpath.pp) err
    | `Path err    -> Fmt.pf ppf "(`Path %s)" err
    | `Getcwd err  -> Fmt.pf ppf "(`Getcwd %s)" err
    | `Mkdir err   -> Fmt.pf ppf "(`Mkdir %s)" err

  let exists path =
    Lwt.try_bind
      (fun () -> Lwt_unix.stat (Fpath.to_string path))
      (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
        | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Stat x) err)
        | exn -> expected_unix_error exn)

  let create = safe_mkdir

  let delete ?(recurse = false) dir =
    let open Lwt.Infix in

    let rec delete_files to_rmdir dirs = match dirs with
      | [] -> Lwt.return (Ok to_rmdir)
      | dir :: todo ->
        let rec delete_dir_files dh dirs =
          Lwt.try_bind
            (fun () -> Lwt_unix.readdir dh)
            (fun v -> Lwt.return (Some v))
            (fun _ -> Lwt.return None)
          >>= function
          | None -> Lwt.return (Ok dirs)
          | Some (".." | ".") -> delete_dir_files dh dirs
          | Some file ->
            let rec try_unlink file =
              Lwt.try_bind
                (fun () -> Lwt_unix.unlink (Fpath.to_string file))
                (fun () -> Lwt.return (Ok dirs))
                (function Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok dirs)
                        | Unix.Unix_error ((Unix.EISDIR | Unix.EPERM), _, _) ->
                          Lwt.return (Ok (file :: dirs))
                        | Unix.Unix_error (Unix.EACCES, _, _) when Sys.win32 ->
                          Lwt.return (Ok (file :: dirs))
                        | Unix.Unix_error (Unix.EINTR, _, _) -> try_unlink file
                        | Unix.Unix_error (e, _, _) ->
                          Lwt.return (Error (`Unlink (Fmt.strf "%s: %s" (Fpath.to_string file) (Unix.error_message e))))
                        | exn -> expected_unix_error exn)
            in

            try_unlink Fpath.(dir / file) >>= function
            | Ok dirs -> delete_dir_files dh dirs
            | Error _ as err -> Lwt.return err
        in

        Lwt.try_bind
          (fun () -> Lwt_unix.opendir (Fpath.to_string dir))
          (fun dh ->
             Lwt.try_bind
               (fun () -> delete_dir_files dh [])
               (fun v -> Lwt_unix.closedir dh >>= fun () -> match v with
                  | Ok dirs -> delete_files (dir :: to_rmdir) (List.rev_append dirs todo)
                  | Error _ as err -> Lwt.return err)
               (function exn -> Lwt_unix.closedir dh >>= fun () -> Lwt.fail exn))
          (function Unix.Unix_error (Unix.ENOENT, _, _) -> delete_files to_rmdir todo
                  | Unix.Unix_error (Unix.EINTR, _, _) -> delete_files to_rmdir dirs
                  | Unix.Unix_error (e, _, _) ->
                    Lwt.return (Error (`Unlink (Fmt.strf "%s: %s" (Fpath.to_string dir) (Unix.error_message e))))
                  | exn -> expected_unix_error exn)
    in

    let rec delete_dirs = function
      | [] -> Lwt.return (Ok ())
      | dir :: dirs ->
        let rec rmdir dir =
          Lwt.try_bind
            (fun () -> Lwt_unix.rmdir (Fpath.to_string dir))
            (fun () -> Lwt.return (Ok ()))
            (function Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok ())
                    | Unix.Unix_error (Unix.EINTR, _, _) -> rmdir dir
                    | Unix.Unix_error (e, _, _) -> Lwt.return (Error (`Rmdir (Unix.error_message e)))
                    | exn -> expected_unix_error exn)
        in
        rmdir dir >>= function
        | Ok () -> delete_dirs dirs
        | Error _ as err -> Lwt.return err
    in

    let delete recurse dir =
      if not recurse
      then let rec rmdir dir =
             Lwt.try_bind
               (fun () -> Lwt_unix.rmdir (Fpath.to_string dir))
               (fun () -> Lwt.return (Ok ()))
               (function Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok ())
                       | Unix.Unix_error (Unix.EINTR, _, _) -> rmdir dir
                       | Unix.Unix_error (e, _, _) ->
                         Lwt.return (Error (`Rmdir (Unix.error_message e)))
                       | exn -> expected_unix_error exn)
        in
        rmdir dir
      else
        delete_files [] [dir] >>= function
        | Ok rmdirs -> delete_dirs rmdirs
        | Error _ as err -> Lwt.return err
    in

    delete recurse dir >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Rmdir msg) ->
      Lwt.return (Error (`Rmdir (Fmt.strf "delete directory %s: %s" (Fpath.to_string dir) msg)))
    | Error (`Unlink msg) ->
      Lwt.return (Error (`Unlink (Fmt.strf "delete directory %s: %s" (Fpath.to_string dir) msg)))


  let contents ?(dotfiles = false) ?(rel = false) dir =
    let rec readdir dh acc =
      Lwt.try_bind
        (fun () -> Lwt_unix.readdir dh)
        (fun v -> Lwt.return (Some v))
        (fun _ -> Lwt.return None)
      >>= function
      | None -> Lwt.return (Ok acc)
      | Some (".." | ".") -> readdir dh acc
      | Some f when dotfiles || not (String.get f 0 = '.') ->
        (match Fpath.of_string f with
         | Ok f -> readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
         | Error (`Msg _) ->
           Lwt.return (Error (`Path (Fmt.strf "directory contents %s: cannot parse element to a path (%S)"
                                       (Fpath.to_string dir) f))))
      | Some _ -> readdir dh acc
    in

    Lwt.try_bind
      (fun () -> Lwt_unix.opendir (Fpath.to_string dir))
      (fun dh ->
         Lwt.try_bind
           (fun () -> readdir dh [])
           (fun rs -> Lwt_unix.closedir dh >|= fun () -> rs)
           (function exn -> Lwt_unix.closedir dh >>= fun () -> Lwt.fail exn))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Opendir (x, dir)) err)
              | exn -> expected_unix_error exn)

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

  type error =
    [ `Open of string
    | `Write of string
    | `Read of string
    | `Close of string
    | `Stat of string
    | `Rename of string
    | `Unlink of string
    | `Mkdir of string ]

  type lock = Lock.t

  type 'a fd = Lwt_unix.file_descr
    constraint 'a = [< `Read | `Write ]

  let pp_error ppf = function
    | `Open err   -> Fmt.pf ppf "(`Open %s)" err
    | `Write err  -> Fmt.pf ppf "(`Write %s)" err
    | `Read err   -> Fmt.pf ppf "(`Read %s)" err
    | `Close err  -> Fmt.pf ppf "(`Close %s)" err
    | `Stat err   -> Fmt.pf ppf "(`Stat %s)" err
    | `Rename err -> Fmt.pf ppf "(`Rename %s)" err
    | `Unlink err -> Fmt.pf ppf "(`Unlink %s)" err
    | `Mkdir err  -> Fmt.pf ppf "(`Mkdir %s)" err

  let open_w ?lock path ~mode =
    Lock.with_lock lock
    @@ fun () ->
    Lwt_pool.use open_pool
    @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.openfile
            (Fpath.to_string path)
            [ Lwt_unix.O_CREAT
            ; Lwt_unix.O_WRONLY
            ; Lwt_unix.O_TRUNC
            ; Lwt_unix.O_NONBLOCK ] mode)
        (fun fd -> Lwt.return (Ok (fd :> [ `Write ] fd)))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) ->
            Log.warn (fun l -> l "Retrieve EINTR unix error code.");
            go ()
          | Unix.Unix_error _ as err ->
            Log.err (fun l -> l "Retrieve an exception: %s." (Printexc.to_string err));
            Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let open_r ?lock:_ path ~mode =
    Lwt_pool.use open_pool
    @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY; ] mode)
        (fun fd -> Lwt.return (Ok (fd :> [ `Read ] fd)))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err)
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Write x) err)
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Read x) err)
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Close x) err)
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Stat x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let move ?lock path_a path_b =
    Lock.with_lock lock
    @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.rename (Fpath.to_string path_a) (Fpath.to_string path_b))
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Rename x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let delete ?lock path =
    Lock.with_lock lock
    @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.unlink (Fpath.to_string path))
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error (Unix.ENOENT, _ ,_ ) -> Lwt.return (Ok ())
          (* XXX(dinosaure) decides to quiet this error. *)
          | Unix.Unix_error _ as err ->
            Lwt.return (error_to_result ~ctor:(fun x -> `Unlink x) err)
          | exn -> expected_unix_error exn)
    in
    go ()

  let atomic_read_with_read path size =
    let chunk = min size 65536 (* IO_BUFFER_SIZE *) in
    let res = Cstruct.create size in
    let flg = [ Unix.O_RDONLY ] in
    let prm = 0o644 in

    Lwt_unix.openfile (Fpath.to_string path) flg prm >>= fun fd ->

    let rec go off =
      let len = min chunk (size - off) in
      Lwt_bytes.read fd res.Cstruct.buffer off len >>= fun rd ->
      let off = off + rd in

      if off >= size
      then Lwt.return res
      else go off
    in

    Lwt.finalize
      (fun () -> go 0)
      (fun () -> Lwt_unix.close fd)

  let atomic_read_with_mmap path =
    let fd = Unix.(openfile (Fpath.to_string path) [ O_RDONLY; O_NONBLOCK; ] 0o644) in
    let ba = Lwt_bytes.map_file ~fd ~shared:false () in
    Unix.close fd;
    Lwt.return (Cstruct.of_bigarray ba)

  let atomic_read path =
    Lwt.try_bind
      (fun () ->
         Lwt_unix.stat (Fpath.to_string path) >>= fun stats ->
         let size = stats.Lwt_unix.st_size in

         if size >= mmap_threshold
         then atomic_read_with_mmap path
         else atomic_read_with_read path size)
      (fun res -> Lwt.return (Some res))
      (function
        | Unix.Unix_error _ | Sys_error _ -> Lwt.return None
        | exn -> Lwt.fail exn)

  let make_temp ?temp path prefix =
    let dir, file = Fpath.split_base path in
    let x = Fmt.strf "%s-%a" prefix Fpath.pp file in

    match temp with
    | Some temp -> Fpath.(temp / x)
    | None -> Fpath.(dir / x)

  let with_atomic_write ?lock ?temp path writer =
    (match temp with
     | None -> Lwt.return (Ok false) (* XXX(dinosaure): keep the semantic of [safe_mkdir]. *)
     | Some dir -> safe_mkdir dir)
    >>= function
    | Error _ as err -> Lwt.return err
    | Ok _ ->
      let directory = Fpath.parent path in
      safe_mkdir directory >>= function
      | Error _ as err -> Lwt.return err
      | Ok _ ->
        let tmp = make_temp ?temp path "write" in

        Lwt_pool.use open_pool
        @@ fun () ->
        Lwt_unix.(openfile (Fpath.to_string tmp) [ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC; ] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () ->
             Lwt.try_bind
               (fun () -> writer fd)
               (fun v -> Lwt.return (Ok v))
               (fun exn -> Lwt.return (Error exn)))
          (fun () -> Lwt_unix.close fd)
        >>= function
        | Ok () -> move ?lock tmp path
        | Error (Unix.Unix_error _ as err) ->
          Lwt.return (error_to_result ~ctor:(fun x -> `Write x) err)
        | Error exn ->
          let x = Printexc.to_string exn in
          Lwt.return (Error (`Write x))

  let write_cstruct fd cs =
    let rec go fd buf off len =
      Lwt.try_bind
        (fun () -> Lwt_bytes.write fd buf off len)
        (fun n ->
           if len = 0
           then Lwt.fail End_of_file
           else if n < len then go fd buf (off + n) (len - n)
           else Lwt.return ())
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go fd buf off len
          | exn -> Lwt.fail exn)
    in

    match Cstruct.len cs with
    | 0 -> Lwt.return ()
    | n -> go fd (Cstruct.to_bigarray cs) 0 n

  let atomic_write ?lock ?temp path value =
    let writer () =
      with_atomic_write ?temp path (fun fd -> write_cstruct fd value)
    in

    Lock.with_lock lock
    @@ fun () ->
    Lwt.catch writer
      (function
        | Unix.Unix_error (Unix.EISDIR, _, _) ->
          Dir.delete ~recurse:true path >>= fun _ -> writer ()
        | exn -> Lwt.fail exn)

  let test_and_set ?lock ?temp path ~test ~set =
    Lock.with_lock lock
    @@ fun () ->
    atomic_read path >>= fun v ->
    let equal = match test, v with
      | None, None -> true
      | Some x, Some y -> Cstruct.equal x y
      | _ -> false
    in

    (if not equal
     then Lwt.return (Ok false)
     else
       (match set with
        | None -> delete path
        | Some v -> atomic_write ?temp path v)
       >|= result_bind (fun () -> Ok true))
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
