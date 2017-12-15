open Lwt.Infix

let src = Logs.Src.create "git-unix" ~doc:"logs unix event"
module Log = (val Logs.src_log src : Logs.LOG)

let expected_unix_error exn =
  raise (Invalid_argument (Fmt.strf "Expected an unix error: %s." (Printexc.to_string exn)))

let error_to_result ~ctor = function
  | Unix.Unix_error (err_code, caller, _) ->
    Error (ctor (Format.sprintf "%s: %s" caller (Unix.error_message err_code)))
  | exn -> expected_unix_error exn

let is_file path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_REG)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `System x) err)
      | exn -> expected_unix_error exn)

let is_dir path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat (Fpath.to_string path))
    (fun stat -> Lwt.return (Ok (stat.Lwt_unix.st_kind = Lwt_unix.S_DIR)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
      | Unix.Unix_error _ as err -> Lwt.return (error_to_result ~ctor:(fun x -> `System x) err)
      | exn -> expected_unix_error exn)

let open_pool = Lwt_pool.create 200 (fun () -> Lwt.return ())
let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return ())

(* XXX(samoht): Files smaller than this are loaded using [read].

   Use of mmap is necessary to handle PACK files efficiently. Since
   these are stored in a weak map, we don't run out of open files if
   we keep accessing the same one.

   Using read is necessary to handle references, since these are
   mutable and can't be cached. Using mmap here leads to hitting the
   OS limit on the number of open files.

   This threshold must be larger than the size of a reference.

   XXX(dinosaure): About the PACK file, we use directly mmap by the
   [MAPPER] abstraction to compute efficiently all. I don't know if
   it's reliable to keep this behaviour when we ensure than we use
   [read] only for the loose object and mmap for the PACK file.

   But, I think, it stills relevant for the blob object. *)
let mmap_threshold = 4096

let result_bind f a = match a with
  | Ok x -> f x
  | Error err -> Error err

let err_stat path e = Lwt.return (Error (`Stat (e, path)))

(* XXX(samoht): review *)
let safe_mkdir ?(path = true) ?(mode = 0o755) dir =
  let mkdir d mode =
    Lwt.try_bind
      (fun () ->
         Log.debug (fun l -> l ~header:"safe-mkdir" "create a new directory %s." (Fpath.to_string d));
         Lwt_unix.mkdir (Fpath.to_string d) mode)
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
                                            (Unix.error_message e))))
        | exn -> expected_unix_error exn)
  in
  is_dir dir >>= function
  | Error (`System err) -> err_stat dir err
  | Ok true ->
    Log.debug (fun l -> l ~header:"mkdir" "The directory %a already exists" Fpath.pp dir);
    Lwt.return (Ok false)
  | Ok false ->
    Log.debug (fun l -> l ~header:"mkdir" "The directory %a does not exist" Fpath.pp dir);
    match path with
    | false ->
      Lwt_pool.use mkdir_pool (fun () -> mkdir dir mode) >|=
      result_bind (fun _ -> Ok false)
    | true ->
      let rec dirs_to_create p acc =
        is_dir p >>= function
        | Error (`System err) -> err_stat dir err
        | Ok true -> Lwt.return (Ok acc)
        | Ok false -> dirs_to_create (Fpath.parent p) (p :: acc)
      in
      let rec create_them dirs () = match dirs with
        | [] -> Lwt.return (Ok ())
        | dir :: dirs -> mkdir dir mode >>= function
          | Error _ as err -> Lwt.return err
          | Ok () -> create_them dirs ()
      in
      dirs_to_create dir []
      >>= (function Ok dirs -> create_them dirs ()
                  | Error _ as err -> Lwt.return err)
      >|= result_bind (fun _ -> Ok true)
