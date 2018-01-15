open Lwt.Infix

let (>>?) = Lwt_result.(>>=)
let (>|?) = Lwt_result.(>|=)

let src = Logs.Src.create "git-unix.fs" ~doc:"logs unix file-system's event"
module Log = (val Logs.src_log src : Logs.LOG)

type error = Fpath.t * [
  | `Stat of string
  | `Unlink of string
  | `Rmdir of string
  | `Readdir of string
  | `Path of string
  | `Getcwd of string
  | `Open of string
  | `Write of string
  | `Read of string
  | `Close of string
  | `Rename of string
  | `Mkdir of string
  | `Mmap of string
  ]

(* XXX(samoht): better error messages in plain english *)
let pp_error ppf (path, e) =
  let pp ppf = function
  | `Stat err    -> Fmt.pf ppf "(`Stat %s)" err
  | `Unlink err  -> Fmt.pf ppf "(`Unlink %s)" err
  | `Rmdir err   -> Fmt.pf ppf "(`Rmdir %s)" err
  | `Readdir err -> Fmt.pf ppf "(`Readdir %s)" err
  | `Path err    -> Fmt.pf ppf "(`Path %s)" err
  | `Getcwd err  -> Fmt.pf ppf "(`Getcwd %s)" err
  | `Mkdir err   -> Fmt.pf ppf "(`Mkdir %s)" err
  | `Open err    -> Fmt.pf ppf "(`Open %s)" err
  | `Write err   -> Fmt.pf ppf "(`Write %s)" err
  | `Read err    -> Fmt.pf ppf "(`Read %s)" err
  | `Close err   -> Fmt.pf ppf "(`Close %s)" err
  | `Rename err  -> Fmt.pf ppf "(`Rename %s)" err
  | `Mmap err    -> Fmt.pf ppf "(`Mmap %s)" err
  in
  Fmt.pf ppf "Error while processing %a: %a" Fpath.pp path pp e

let err path x: (_, error) result Lwt.t =
  Lwt.return (Error (path, x))

let err_unlink dir e = err dir (`Unlink (Unix.error_message e))
let err_readdir dir e = err dir (`Readdir (Unix.error_message e))
let err_mkdir dir e = err dir (`Mkdir (Unix.error_message e))
let err_rmdir dir e = err dir (`Rmdir (Unix.error_message e))
let err_getcwd dir fmt = Fmt.kstrf (fun s -> err dir (`Getcwd s)) fmt
let err_open file e = err file (`Open (Unix.error_message e))
let err_write file e = err file (`Write (Unix.error_message e))
let err_read file e = err file (`Read (Unix.error_message e))
let err_close file e = err file (`Close (Unix.error_message e))
let err_stat file e = err file (`Stat (Unix.error_message e))
let err_rename file e = err file (`Rename (Unix.error_message e))
let err_delete file e = err file (`Unlink (Unix.error_message e))
let err_mmap file e = err file (`Mmap (Unix.error_message e))

let expected_unix_error exn =
  Fmt.kstrf invalid_arg "Expected an unix error: %a." Fmt.exn exn

let open_pool = Lwt_pool.create 200 (fun () -> Lwt.return ())

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
         | Unix_error (e, _, _)     ->
           (err e >|= function Ok _ as x  -> x | Error e -> Error (`E e))
         | exn                      -> expected_unix_error exn)
    >>= function
    | Ok _ as x      -> Lwt.return x
    | Error `EAGAIN  -> protect ~n:(n+1) err f
    | Error (`E e)   -> Lwt.return (Error e)

let is_file path =
  let err = function
    | Unix.ENOENT -> Lwt.return (Ok false)
    | e           -> err_stat path e
  in
  protect err @@ fun () ->
  Lwt_unix.stat (Fpath.to_string path) >|= fun stat ->
  stat.Lwt_unix.st_kind = Lwt_unix.S_REG

let is_dir path =
  let err = function
    | Unix.ENOENT -> Lwt.return (Ok false)
    | e           -> err_stat path e
  in
  protect err @@ fun () ->
  Lwt_unix.stat (Fpath.to_string path) >|= fun stat ->
  stat.Lwt_unix.st_kind = Lwt_unix.S_DIR

let result_map f a = match a with
  | Ok x      -> f x
  | Error err -> Error err

module Dir = struct

  let create dir =
    let mode = 0o755 in
    let mkdir d =
      let err = function
        | Unix.EEXIST -> Lwt.return (Ok ())
        | e           -> err_mkdir d e
      in
      protect err @@ fun () ->
      Log.debug (fun l -> l "create a new directory %a." Fpath.pp d);
      Lwt_unix.mkdir (Fpath.to_string d) mode
    in
    is_dir dir >>= function
    | Error _ as e -> Lwt.return e
    | Ok true      -> Lwt.return (Ok false)
    | Ok false     ->
      let rec dirs_to_create p acc =
        is_dir p >>= function
        | Error _ as e -> Lwt.return e
        | Ok true      -> Lwt.return (Ok acc)
        | Ok false     -> dirs_to_create (Fpath.parent p) (p :: acc)
      in
      let rec create_them dirs () = match dirs with
        | []          -> Lwt.return (Ok ())
        | dir :: dirs ->
          mkdir dir >>= function
          | Error _ as err -> Lwt.return err
          | Ok ()          -> create_them dirs ()
      in
      (dirs_to_create dir [] >>= function
        | Ok dirs        -> create_them dirs ()
        | Error _ as err -> Lwt.return err)
      >|= result_map (fun _ -> Ok true)

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

  let delete dir =
    Log.debug (fun l -> l "Dir.delete %a" Fpath.pp dir);
    delete_dir Lwt.return dir

  let contents ?(rel = false) dir =
    Log.debug (fun l -> l "Dir.contents %a" Fpath.pp dir);
    let rec aux acc = function
      | []   -> acc
      | h::t -> aux ((if rel then h else Fpath.(dir // h)) :: acc) t
    in
    readdir dir >|? aux []

  let rec current () =
    Lwt.try_bind
      (fun () -> Unix.getcwd () |> Lwt.return)
      (fun p ->
         match Fpath.of_string p with
         | Ok dir ->
           if Fpath.is_abs dir then Lwt.return (Ok dir)
           else
             err_getcwd dir
               "getcwd(3) returned a relative path: (%s)" (Fpath.to_string dir)
         | Error _ ->
           err_getcwd Fpath.(v "CWD")
             "get current working directory: cannot parse it to a path (%S)" p)
      (function
        | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
        | Unix.Unix_error (e, _, _) ->
          err_getcwd Fpath.(v "CWD")
            "get current working directory: %s" (Unix.error_message e)
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
     then from_env "TEMP  " ~absent:Fpath.(v "./")
     else from_env "TMPDIR" ~absent:Fpath.(v "/tmp/"))
    |> Lwt.return
end

module File = struct

  type 'a fd = {
    path: Fpath.t;
    fd  : Lwt_unix.file_descr;
  } constraint 'a = [< `Read | `Write ]

  let open_w path =
    let mode = 0o644 in
    let err e = err_open path e in
    Lwt_pool.use open_pool @@ fun () ->
    protect err @@ fun () ->
    Lwt_unix.openfile (Fpath.to_string path) [
      Lwt_unix.O_CREAT;
      Lwt_unix.O_WRONLY;
      Lwt_unix.O_TRUNC;
      Lwt_unix.O_NONBLOCK;
    ] mode >|= fun fd ->
    ( { fd; path } :> [ `Write ] fd)

  let open_r path =
    let mode = 0o400 in
    let err e = err_open path e in
    Lwt_pool.use open_pool @@ fun () ->
    protect err @@ fun () ->
    Lwt_unix.openfile (Fpath.to_string path) [Lwt_unix.O_RDONLY] mode >|=
    fun fd -> ( { fd; path } :> [ `Read ] fd)

  let write raw ?(off = 0) ?(len = Cstruct.len raw) { fd; path } =
    let err e = err_write path e in
    protect err @@ fun () ->
    Lwt_bytes.write fd (Cstruct.to_bigarray raw) off len

  let read raw ?(off = 0) ?(len = Cstruct.len raw) { fd; path } =
    let err e = err_read path e in
    protect err @@ fun () ->
    Lwt_bytes.read fd (Cstruct.to_bigarray raw) off len

  let close { fd; path } =
    let err e = err_close path e in
    protect err @@ fun () ->
    Lwt_unix.close fd

  let exists path =
    let err = function
      | Unix.ENOENT -> Lwt.return (Ok false)
      | e           -> err_stat path e
    in
    protect err @@ fun () ->
    Lwt_unix.stat (Fpath.to_string path) >|= fun stat ->
    stat.Lwt_unix.st_kind = Lwt_unix.S_REG

  let move path_a path_b =
    let err e = err_rename path_a e in
    protect err @@ fun () ->
    Lwt_unix.rename (Fpath.to_string path_a) (Fpath.to_string path_b)

  let delete path =
    let err = function
      | Unix.ENOENT -> Lwt.return (Ok ())
      | e           -> err_delete path e
    in
    protect err @@ fun () ->
    Lwt_unix.unlink (Fpath.to_string path)

end

module Mapper = struct

  let pp_error = pp_error

  type fd = {
    fd  : Lwt_unix.file_descr;
    path: Fpath.t
  }

  let length { fd; path } =
    let err e = err_stat path e in
    protect err @@ fun () ->
    Lwt_unix.LargeFile.fstat fd >|= fun fstat ->
    fstat.Unix.LargeFile.st_size

  let openfile path =
    let err e = err_open path e in
    protect err @@ fun () ->
    Lwt_unix.openfile (Fpath.to_string path) [ Lwt_unix.O_RDONLY ] 0o644
    >|= fun fd -> { fd; path }

  let close { fd; path } =
    let err e = err_close path e in
    protect err @@ fun () ->
    Lwt_unix.close fd

  let map t ?pos ~share:_ len =
    length t >>= function
    | Error err -> Lwt.return (Error err)
    | Ok max    ->
      let max = match pos with
        | Some pos -> Int64.sub max pos
        | None     -> max
      in
      let err e = err_mmap t.path e in
      let fd = Lwt_unix.unix_file_descr t.fd in
      let size = Int64.to_int (min (Int64.of_int len) max) in
      protect err @@ fun () ->
      try
        let rs = Lwt_bytes.map_file ~fd ?pos ~shared:false ~size () in
        Lwt.return (Cstruct.of_bigarray rs)
      with e ->
        Lwt.fail e

end

let is_dir = is_dir
let is_file = is_file
let has_global_checkout = true
let has_global_watches = true
