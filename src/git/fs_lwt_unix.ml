type path = Fpath.t

type error = [ `System of string ]

let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

let error_to_result ~ctor = function
  | Unix.Unix_error (err_code, caller, _) ->
    Error (ctor (Format.sprintf "%s: %s" caller (Unix.error_message err_code)))

let is_file path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_REG)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `System x) err))

let is_dir path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `System x) err))

let open_pool = Lwt_pool.create 200 (fun () -> Lwt.return ())
let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return ())

(* XXX(samoht): Files smaller than this are loaded using [read].

   Use of mmap is necessary to handle PACK files efficiently. Since these are
   stored in a weak map,we don't run out of open files if we keep accessing the
   same one.

   Using read is necessary to handle references, since these are mutable and
   can't be cached. Using mmap here leads to hitting the OS limit on the number
   of open files.

   This threshold must be larger than the size of a reference.

   XXX(dinosaure): About the PACK file, we use directly mmap by the [MAPPER]
   abstraction to compute efficiently all. I don't know if it's reliable to keep
   this behaviour when we ensure than we use [read] only for the loose object
   and mmap for the PACK file.

   But, I think, it stills relevant for the blob object.
*)
let mmap_threshold = 4096

let result_bind f a = match a with
  | Ok x -> f x
  | Error err -> Error err

let safe_mkdir ?(path = true) ?(mode = 0o755) dir =
  let mkdir d mode =
    Lwt.try_bind
      (fun () -> Lwt_unix.mkdir (Fpath.to_string d) mode)
      (fun () -> Lwt.return (Ok ()))
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return (Ok ())
        | Unix.Unix_error (e, _, _) ->
          if d = dir
          then Lwt.return (Error (`Mkdir (Format.sprintf "create directory %s: %s"
                                            (Fpath.to_string d)
                                            (Unix.error_message e))))
          else Lwt.return (Error (`Mkdir (Format.sprintf "create directory %s: %s: %s"
                                            (Fpath.to_string dir)
                                            (Fpath.to_string d)
                                            (Unix.error_message e)))))
  in

  let open Lwt.Infix in

  is_dir dir >>= function
  | Error (`System err) -> Lwt.return (Error (`Stat err))
  | Ok true -> Lwt.return (Ok false)
  | Ok false ->
    match path with
    | false -> Lwt_pool.use mkdir_pool (fun () -> mkdir dir mode) >|= result_bind (fun _ -> Ok false)
    | true ->
      let rec dirs_to_create p acc =
        is_dir p >>= function
        | Error (`System err) -> Lwt.return (Error (`Stat err))
        | Ok true -> Lwt.return (Ok acc)
        | Ok false -> dirs_to_create (Fpath.parent p) (p :: acc)
      in

      let rec create_them dirs () = match dirs with
        | [] -> Lwt.return (Ok ())
        | dir :: dirs -> mkdir dir mode >>= function
          | Error err -> Lwt.return (Error err)
          | Ok () -> create_them dirs ()
      in

      dirs_to_create dir []
      >>= (function Ok dirs -> create_them dirs ()
                  | Error err -> Lwt.return (Error err))
      >|= result_bind (fun x -> Ok true)

module Lock =
struct
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
          safe_mkdir (Fpath.base path) >>= function
          | Error _ -> assert false (* TODO *)
          | Ok _ ->
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

  let with_lock path f =
    let open Lwt.Infix in

    match path with
    | None -> f ()
    | Some path -> lock path >>= fun () -> Lwt.finalize f (fun () -> unlock path)
end

module Dir
  : Fs.DIR with type path = path
            and type error = [ `Stat of string
                             | `Unlink of string
                             | `Rmdir of string
                             | `Opendir of string
                             | `Path of string
                             | `Getcwd of string
                             | `Mkdir of string ]
= struct
  type path = Fpath.t

  type error =
    [ `Stat of string
    | `Unlink of string
    | `Rmdir of string
    | `Opendir of string
    | `Path of string
    | `Getcwd of string
    | `Mkdir of string ]

  let pp_error ppf = function
    | `Stat err    -> Fmt.pf ppf "(`Stat %s)" err
    | `Unlink err  -> Fmt.pf ppf "(`Unlink %s)" err
    | `Rmdir err   -> Fmt.pf ppf "(`Rmdir %s)" err
    | `Opendir err -> Fmt.pf ppf "(`Opendir %s)" err
    | `Path err    -> Fmt.pf ppf "(`Path %s)" err
    | `Getcwd err  -> Fmt.pf ppf "(`Getcwd %s)" err
    | `Mkdir err   -> Fmt.pf ppf "(`Mkdir %s)" err

  let exists path =
    Lwt.try_bind
      (fun () -> Lwt_unix.stat (Fpath.to_string path))
      (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
        | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Stat x) err))

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
                          Lwt.return (Error (`Unlink (Fmt.strf "%s: %s" (Fpath.to_string file) (Unix.error_message e)))))
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
                  | Ok dirs -> delete_dir_files dh dirs
                  | Error _ as err -> Lwt.return err)
                (function exn -> Lwt_unix.closedir dh >>= fun () -> Lwt.fail exn))
          (function Unix.Unix_error (Unix.ENOENT, _, _) -> delete_files to_rmdir todo
                  | Unix.Unix_error (Unix.EINTR, _, _) -> delete_files to_rmdir dirs
                  | Unix.Unix_error (e, _, _) ->
                    Lwt.return (Error (`Unlink (Fmt.strf "%s: %s" (Fpath.to_string dir) (Unix.error_message e)))))
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
                    | Unix.Unix_error (e, _, _) -> Lwt.return (Error (`Rmdir (Unix.error_message e))))
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
                          Lwt.return (Error (`Rmdir (Unix.error_message e))))
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
    let open Lwt.Infix in

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
          | Error (`Msg err) ->
            Lwt.return (Error (`Path (Fmt.strf "directory contents %s: connot parse element to a path (%S)"
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
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Opendir x) err))

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
                                                      %s" (Unix.error_message e)))))

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

module File
  : Fs.FILE with type path = path
             and type error = [ `Open of string
                              | `Write of string
                              | `Read of string
                              | `Close of string
                              | `Stat of string
                              | `Rename of string
                              | `Unlink of string
                              | `Mkdir of string ]
= struct
  type error =
    [ `Open of string
    | `Write of string
    | `Read of string
    | `Close of string
    | `Stat of string
    | `Rename of string
    | `Unlink of string
    | `Mkdir of string ]

  type lock = Fpath.t
  type path = Fpath.t
  type raw = Cstruct.t

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

  open Lwt.Infix

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
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err))
    in
    go ()

  let open_r ?lock path ~mode =
    Lwt_pool.use open_pool
    @@ fun () ->
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY; ] mode)
        (fun fd -> Lwt.return (Ok (fd :> [ `Read ] fd)))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err))
    in
    go ()

  let open_wr ?lock path ~mode =
    Lock.with_lock lock
    @@ fun () ->
    Lwt_pool.use open_pool
    @@ fun () ->
    Lwt.try_bind
      (fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDWR; Lwt_unix.O_TRUNC ] mode)
      (fun fd -> Lwt.return (Ok (fd :> [ `Read | `Write ] fd)))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Open x) err))

  let write raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_bytes.write fd (Cstruct.to_bigarray raw) off len)
        (fun n -> Lwt.return (Ok n))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Write x) err))
    in
    go ()

  let read raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_bytes.read fd (Cstruct.to_bigarray raw) off len)
        (fun n -> Lwt.return (Ok n))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Read x) err))
    in
    go ()

  let close fd =
    let rec go () =
      Lwt.try_bind
        (fun () -> Lwt_unix.close fd)
        (fun () -> Lwt.return (Ok ()))
        (function
          | Unix.Unix_error (Unix.EINTR, _ ,_) -> go ()
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Close x) err))
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Stat x) err))
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Rename x) err))
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
          | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Unlink x) err))
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

module Mapper
  : Fs.MAPPER with type raw = Cstruct.t
               and type path = path
               and type error = [ `Stat of string
                                | `Close of string
                                | `Mmap of string
                                | `Open of string ]
= struct
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
  type raw = Cstruct.t
  type path = Fpath.t

  open Lwt

  let length fd =
    Lwt.try_bind
      (fun () -> Unix.LargeFile.fstat fd |> Lwt.return)
      (fun fstat -> return (Ok fstat.Unix.LargeFile.st_size))
      (function _ -> return (Error (`Stat "Invalid file descriptor")))

  let openfile path =
    Lwt.try_bind
      (fun () -> Unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY ] 0o644 |> Lwt.return)
      (fun fd -> return (Ok fd))
      (function _ -> return (Error (`Open (Fmt.strf "Invalid file: %s" (Fpath.to_string path)))))

  let close fd =
    Lwt.try_bind
      (fun () -> let () = Unix.close fd in Lwt.return ())
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `Close x) err))

  let map fd ?pos ~share len =
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
        (fun rs -> return (Ok (Cstruct.of_bigarray rs)))
        (function _ -> return (Error (`Mmap "Impossible to map the file descriptor")))
    | Error err -> return (Error err)
end
