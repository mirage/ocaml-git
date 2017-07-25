type path = Fpath.t
type error = [ `System of string ]

let pp_error fmt = function
  | `System err -> Format.fprintf fmt "(`System %s)" err

let error_to_result = function
  | Unix.Unix_error (err_code, caller, _) ->
    Error (`System (Format.sprintf "%s: %s" caller (Unix.error_message err_code)))

let is_file path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_REG)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

let is_dir path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

module File
  : Fs.FILE with type raw = Cstruct.t
              and type path = Fpath.t
              and type error = [ `System of string ]
= struct
  type error = [ `System of string ]
  type path = Fpath.t
  type raw = Cstruct.t
  type 'a fd =
    { fd : Lwt_unix.file_descr
    ; _i : Bytes.t
    ; _o : Bytes.t } constraint 'a = [< `Read | `Write ]

  let pp_error fmt = function `System err -> Format.fprintf fmt "(`System %s)" err

  open Lwt.Infix

  let open_w path ~mode ~lock =
    let _i = Bytes.create 0 in
    let _o = Bytes.create 0x8000 in

    Lwt.try_bind
      (fun () -> lock >>= fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY; Lwt_unix.O_TRUNC ] mode)
      (fun fd -> Lwt.return (Ok ({ fd; _i; _o; } :> [ `Write ] fd)))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let open_r path ~mode ~lock =
    let _i = Bytes.create 0x8000 in
    let _o = Bytes.create 0 in

    Lwt.try_bind
      (fun () -> lock >>= fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY; ] mode)
      (fun fd -> Lwt.return (Ok ({ fd; _i; _o; } :> [ `Read ] fd)))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let open_wr path ~mode ~lock =
    let _i = Bytes.create 0x8000 in
    let _o = Bytes.create 0x8000 in

    Lwt.try_bind
      (fun () -> lock >>= fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDWR; Lwt_unix.O_TRUNC ] mode)
      (fun fd -> Lwt.return (Ok ({ fd; _i; _o; } :> [ `Read | `Write ] fd)))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let write raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    Lwt.try_bind
      (fun () ->
          let rec go ?(limit = 0) acc rest =
            if rest = 0 || limit >= 50
            then Lwt.return acc
            else
              let pos = off + (len - rest) in
              let wrr = min rest (Bytes.length fd._o) in
              Cstruct.blit_to_bytes raw pos fd._o 0 wrr;
              Lwt_unix.write fd.fd fd._o 0 wrr >>= fun n -> go ~limit:(limit + 1) (acc + n) (rest - n)
          in

          go 0 len)
      (fun n -> Lwt.return (Ok n))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let read raw ?(off = 0) ?(len = Cstruct.len raw) fd =
    Lwt.try_bind
      (fun () ->
          let rec go ?(limit = 0) acc rest =
            if rest = 0 || limit >= 50
            then Lwt.return acc
            else
              let pos = off + (len - rest) in
              let wrr = min rest (Bytes.length fd._i) in
              Lwt_unix.read fd.fd fd._i 0 wrr >>= fun n ->
              Cstruct.blit_from_bytes fd._i 0 raw pos n;
              go ~limit:(limit + 1) (acc + n) (rest - n)
          in

          go 0 len)
      (fun n -> Lwt.return (Ok n))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let close fd =
    Lwt.try_bind
      (fun () -> Lwt_unix.close fd.fd)
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let exists path =
    Lwt.try_bind
      (fun () -> Lwt_unix.stat (Fpath.to_string path))
      (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_REG)))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
        | Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let move path_a path_b =
    Lwt.try_bind
      (fun () -> Lwt_unix.rename (Fpath.to_string path_a) (Fpath.to_string path_b))
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let delete path =
    Lwt.try_bind
      (fun () -> Lwt_unix.unlink (Fpath.to_string path))
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))
end

module Dir
  : Fs.DIR with type path = Fpath.t
= struct
  type path = Fpath.t
  type error = [ `System of string ]

  let pp_error fmt = function `System err -> Format.fprintf fmt "(`System %s)" err

  let exists path =
    Lwt.try_bind
      (fun () -> Lwt_unix.stat (Fpath.to_string path))
      (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
        | Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let bind f a = match a with
    | Ok x -> f x
    | Error err -> Error err

  let create ?(path = true) ?(mode = 0o755) dir =
    let mkdir d mode =
      Lwt.try_bind
        (fun () -> Lwt_unix.mkdir (Fpath.to_string d) mode)
        (fun () -> Lwt.return (Ok ()))
        (function Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return (Ok ())
                | Unix.Unix_error (e, _, _) ->
                  if d = dir
                  then Lwt.return (Error (`System (Format.sprintf "create directory %s: %s"
                                                      (Fpath.to_string d)
                                                      (Unix.error_message e))))
                  else Lwt.return (Error (`System (Format.sprintf "create directory %s: %s: %s"
                                                      (Fpath.to_string dir)
                                                      (Fpath.to_string d)
                                                      (Unix.error_message e)))))
    in

    let open Lwt.Infix in

    exists dir >>= function
    | Error err -> Lwt.return (Error err)
    | Ok true -> Lwt.return (Ok false)
    | Ok false ->
      match path with
      | false -> mkdir dir mode >|= bind (fun _ -> Ok false)
      | true ->
        let rec dirs_to_create p acc =
          exists p >>= function
          | Error err -> Lwt.return (Error err)
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
        >|= bind (fun x -> Ok true)

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
                          Lwt.return (Error (`System (Format.sprintf "%s: %s" (Fpath.to_string file) (Unix.error_message e)))))
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
                    Lwt.return (Error (`System (Format.sprintf "%s: %s" (Fpath.to_string dir) (Unix.error_message e)))))
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
                    | Unix.Unix_error (e, _, _) -> Lwt.return (Error (`System (Unix.error_message e))))
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
                          Lwt.return (Error (`System (Unix.error_message e))))
        in
        rmdir dir
      else
        delete_files [] [dir] >>= function
        | Ok rmdirs -> delete_dirs rmdirs
        | Error _ as err -> Lwt.return err
    in

    delete recurse dir >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`System msg) -> Lwt.return (Error (`System (Format.sprintf "delete directory %s: %s" (Fpath.to_string dir) msg)))


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
          | Error (`Msg err) -> Lwt.return (Error (`System (Format.sprintf "directory contents %s: connot parse element to a path (%S)"
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
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let rec current () =
    Lwt.try_bind
      (fun () -> Unix.getcwd () |> Lwt.return)
      (fun p -> match Fpath.of_string p with
        | Ok dir ->
          if Fpath.is_abs dir
          then Lwt.return (Ok dir)
          else Lwt.return (Error (`System (Format.sprintf "getcwd(3) returned a relative path: (%s)" (Fpath.to_string dir))))
        | Error _ ->
          Lwt.return (Error (`System (Format.sprintf "get current working directory: cannot parse it to a path (%S)" p))))
      (function Unix.Unix_error (Unix.EINTR, _, _) -> current ()
              | Unix.Unix_error (e, _, _) ->
                Lwt.return (Error (`System (Format.sprintf "get current working directory: %s" (Unix.error_message e)))))

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
     else from_env "TMPDIR" ~absent:Fpath.(v "/tmp"))
    |> Lwt.return
end

module Mapper
  : Fs.MAPPER with type raw = Cstruct.t
               and type path = Fpath.t
               and type error = [ `System of string ]
= struct
  type error = [ `System of string ]

  let pp_error fmt = function `System err -> Format.fprintf fmt "(`System %s)" err

  type fd = Lwt_unix.file_descr
  type raw = Cstruct.t
  type path = Fpath.t

  open Lwt

  let length fd =
    Lwt.try_bind
      (fun () -> Lwt_unix.LargeFile.fstat fd)
      (fun fstat -> return (Ok fstat.Lwt_unix.LargeFile.st_size))
      (function _ -> return (Error (`System "Invalid file descriptor")))

  let openfile path =
    Lwt.try_bind
      (fun () -> Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY ] 0o644)
      (fun fd -> return (Ok fd))
      (function _ -> return (Error (`System (Format.sprintf "Invalid file: %s" (Fpath.to_string path)))))

  let close fd =
    Lwt.try_bind
      (fun () -> Lwt_unix.close fd)
      (fun () -> Lwt.return (Ok ()))
      (function Unix.Unix_error _ as err -> Lwt.return (error_to_result err))

  let map fd ?pos ~share len =
    length fd >>= function
    | Ok max ->
      let max = match pos with
        | Some pos -> Int64.sub max pos
        | None -> max
      in

      Lwt.try_bind
        (fun () ->
            Bigarray.Array1.map_file
            (Lwt_unix.unix_file_descr fd)
            ?pos
            Bigarray.Char
            Bigarray.c_layout
            share
            (Int64.to_int (min (Int64.of_int len) max))
            |> return)
        (fun rs -> return (Ok (Cstruct.of_bigarray rs)))
        (function _ -> return (Error (`System "Impossible to map the file descriptor")))
    | Error err -> return (Error err)
end
