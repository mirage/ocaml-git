open Lwt.Infix
let ( >>!= ) x f = Lwt_result.map_err f x
let ( >>?= ) = Lwt_result.bind
let ( >|?= ) = Lwt_result.(>|=)

let src = Logs.Src.create "git-mirage.fs" ~doc:"logs mirage file-system's event"
module Log = (val Logs.src_log src : Logs.LOG)

(* XXX(samoht): this should be removed *)
module type GAMMA = sig
  val current : Fpath.t
  val temp    : Fpath.t
end

(* XXX(samoht): this should be removed *)
module type S = sig
  include Mirage_fs_lwt.S
  val connect : unit -> t Lwt.t
end

(* mirage-fs-unix paths are separated by '/' and should not end by a
   separator. XXX(samoht): there is probably something to revisit
   in mirage-fs-unix here... *)
let fpath_to_string path =
  let segs = Fpath.segs path in
  let segs = List.filter (function "."|"" -> false | _ -> true) segs in
  String.concat "/" segs

let fpath_of_string str =
  let segs = Astring.String.cuts ~sep:"/" str in
  Fpath.of_string (String.concat Fpath.dir_sep segs)

let err_exn e = Error (`Exn e)
let err_stat p fmt = Fmt.kstrf (fun e -> Error (`Stat (p, e))) fmt
let err_read p fmt = Fmt.kstrf (fun e -> Error (`Read (p, e))) fmt
let err_fs_read e = Error (`FS_read e)
let err_fs_write e = Error (`FS_write e)
let fs_read e = `FS_read e
let fs_write e = `FS_write e

let err_listdir p fmt =
  Fmt.kstrf (fun e -> Lwt.return (Error (`Listdir (p, e)))) fmt

module Make (Gamma: GAMMA) (FS: S) = struct

  let connect fn =  Lwt.bind (FS.connect ()) fn

  type error = [ `Exn of exn ]
  let pp_error ppf (`Exn e) = Fmt.pf ppf "System error: %a" Fmt.exn e

  let is_file t path =
    let str_path = fpath_to_string path in
    Lwt.try_bind
      (fun () -> FS.stat t str_path)
      (function
        | Ok { Mirage_fs.directory; _ } -> Lwt.return (Ok (not directory))
        | Error _ -> Lwt.return (Ok false))
      (* FIXME: it seems that mirage-fs-unix can also raise Sys_error,
         see https://github.com/mirage/mirage-fs-unix/issues/37 *)
      (function
        | Sys_error _ -> Lwt.return (Ok false)
        | exn         -> Lwt.return (err_exn exn))

  let is_dir t path =
    let str_path = fpath_to_string path in
    Lwt.try_bind
      (fun () -> FS.stat t str_path)
      (function
        | Ok { Mirage_fs.directory; _ } -> Lwt.return (Ok directory)
        | Error _ -> Lwt.return (Ok false))
      (* FIXME: see above. *)
      (function
        | Sys_error _ -> Lwt.return (Ok false)
        | exn         -> Lwt.return (err_exn exn))

  module Dir = struct

    type error =
      [ `Destroy of Fpath.t * string
      | `Listdir of Fpath.t * string
      | `Stat of Fpath.t * string
      | `Read of Fpath.t * string
      | `FS_read of FS.error
      | `FS_write of FS.write_error ]

    let pp_error ppf = function
      | `Destroy(f,e) -> Fmt.pf ppf "Error while destroying %a: %s" Fpath.pp f e
      | `Listdir(f,e) -> Fmt.pf ppf "Error while listing %a: %s" Fpath.pp f e
      | `Stat(f,e)    -> Fmt.pf ppf "Error while stating %a: %s" Fpath.pp f e
      | `Read(f,e)    -> Fmt.pf ppf "Error while reading %a: %s" Fpath.pp f e
      | `FS_read e    -> FS.pp_error ppf e
      | `FS_write e   -> FS.pp_write_error ppf e

    let exists t path =
      Log.debug (fun l -> l "Dir.exists %s" @@ fpath_to_string path);
      is_dir t path >|= function
      | Ok _ as v        -> v
      | Error (`Exn err) -> err_stat path "%a" Fmt.exn err

    let create t d =
      let path = fpath_to_string d in
      Log.debug (fun l -> l "Dir.create %s" path);
      FS.mkdir t path >|= function
      | Ok ()   -> Ok true
      | Error e -> err_fs_write e

    let delete t d =
      let path = fpath_to_string d in
      Log.debug (fun l -> l "Dir.delete %s" path);
      FS.destroy t path >|= function
      | Ok ()   -> Ok ()
      | Error e -> err_fs_write e

    let contents t ?(dotfiles = false) ?(rel = false) dir =
      let path = fpath_to_string dir in
      Log.debug (fun l -> l "Dir.contents %s" path);
      let rec readdir files acc =
        match files with
        | [] -> Lwt.return (Ok acc)
        | (".." | ".") :: rest -> readdir rest acc
        | f :: rest when dotfiles || not (String.get f 0 = '.') ->
          (match fpath_of_string f with
           | Ok f -> readdir rest ((if rel then f else Fpath.(dir // f)) :: acc)
           | Error (`Msg e) -> Lwt.return (err_read dir "%s" e))
        | _ :: rest -> readdir rest acc
      in
      Lwt.try_bind
        (fun () -> FS.listdir t path)
        (function
          | Ok files -> readdir files []
          | Error e  -> Lwt.return (err_fs_read e))
        (fun exn -> err_listdir dir "%a" Fmt.exn exn)

    let current () = Lwt.return (Ok Gamma.current)
    let temp () = Lwt.return Gamma.temp

    let contents ?dotfiles ?rel dir = connect @@ fun t -> contents t ?dotfiles ?rel dir
    let exists path = connect @@ fun t -> exists t path
    let create ?path:_ ?mode:_ dir = connect @@ fun t -> create t dir
    let delete ?recurse path =
      (* mirage-fs only supports recursive deletions *)
      let () = match recurse with None -> () | Some x -> assert x in
      connect @@ fun t -> delete t path
  end

  module File = struct
    type lock = Git.Mem.Lock.elt

    type error =
      [ `Stat of Fpath.t * string
      | `FS_read of FS.error
      | `FS_write of FS.write_error ]

    let pp_error ppf = function
      | `Stat (f,e)   -> Fmt.pf ppf "Error while stating %a: %s" Fpath.pp f e
      | `FS_read err  -> Fmt.pf ppf "%a" FS.pp_error err
      | `FS_write err -> Fmt.pf ppf "%a" FS.pp_write_error err

    type 'a fd =
      { fd   : string
      ; mutable seek : int }
      constraint 'a = [< `Read | `Write ]

    let exists t path =
      Log.debug (fun l -> l "File.exists %s" @@ fpath_to_string path);
      is_file t path >|= function
      | Ok _ as v        -> v
      | Error (`Exn err) -> err_stat path "%a" Fmt.exn err

    let open' t ?lock ~create path ~mode:_ =
      Git.Mem.Lock.with_lock lock  @@ fun () ->
      let fd = fpath_to_string path in
      FS.stat t fd >>= function
      | Ok { Mirage_fs.directory = false; _ } -> Lwt.return (Ok fd)
      | Ok _    -> Lwt.return (err_fs_write `Is_a_directory)
      | Error e ->
        if not create then Lwt.return (err_fs_read e)
        else
          FS.create t (fpath_to_string path) >|= function
          | Ok ()   -> Ok fd
          | Error e -> err_fs_write e

    (* XXX(dinosaure): we can inform the size of the file and check at any time
       if [seek <= max]. *)

    let open_r t ?lock path ~mode =
      Log.debug (fun l -> l "File.open_r %s" @@ fpath_to_string path);
      open' t ?lock ~create:false path ~mode >|?= fun fd ->
      ({ fd; seek = 0 } :> [ `Read ] fd)

    let open_w t ?lock path ~mode =
      Log.debug (fun l -> l "File.open_w %s" @@ fpath_to_string path);
      open' t ?lock ~create:true path ~mode >|?= fun fd ->
      ({ fd; seek = 0 } :> [ `Write ] fd)

    let write t raw ?(off = 0) ?(len = Cstruct.len raw) fd =
      Log.debug (fun l -> l "File.write %s" fd.fd);
      FS.write t fd.fd fd.seek (Cstruct.sub raw off len) >|= function
      | Ok ()   -> fd.seek <- fd.seek + len; Ok len
      | Error e -> err_fs_write e

    let read t dst ?(off = 0) ?(len = Cstruct.len dst) fd =
      Log.debug (fun l -> l "File.read %s" fd.fd);
      FS.read t fd.fd fd.seek len >|= function
      | Error e -> err_fs_read e
      | Ok src  ->
        let (len, _) = Cstruct.fillv ~src ~dst:(Cstruct.sub dst off len) in
        fd.seek <- fd.seek + len;
        Ok len

    let close fd =
      Log.debug (fun l -> l "File.close %s" fd.fd);
      Lwt.return (Ok ())

    let delete t ?lock path =
      Log.debug (fun l -> l "File.delete %s" @@ fpath_to_string path);
      Git.Mem.Lock.with_lock lock @@ fun () ->
      FS.destroy t (fpath_to_string path) >|= function
      | Ok _ as v -> v
      | Error e   -> err_fs_write e

    let atomic_read t path =
      FS.stat t (fpath_to_string path) >>= function
      | Error _ -> Lwt.return None
      | Ok stat ->
        if stat.Mirage_fs.directory
        then Lwt.fail (Failure (Fmt.strf "%a is a directory" Fpath.pp path))
        else
          FS.read t (fpath_to_string path) 0 (Int64.to_int stat.Mirage_fs.size)
          >|= function
          | Error _ -> None
          | Ok cs   -> Some (Cstruct.of_string (Cstruct.copyv cs))

    let atomic_write t ?lock path content =
      FS.mkdir t (Filename.dirname (fpath_to_string path)) >>= fun _ ->
      Git.Mem.Lock.with_lock lock @@ fun () ->
      FS.create t (fpath_to_string path) >>= function
      | Error e -> Lwt.return (err_fs_write e)
      | Ok ()   ->
        FS.write t (fpath_to_string path) 0 content >|= function
        | Ok _ as v -> v
        | Error e   -> err_fs_write e

    let test_and_set t ?lock ?temp:_ file ~test ~set =
      FS.mkdir t (Filename.dirname (fpath_to_string file)) >>= fun _ ->
      Git.Mem.Lock.with_lock lock @@ fun () ->
      let set () =
        (match set with
         | None   -> delete t file
         | Some v -> atomic_write t file v)
        >|= function
        | Ok ()          -> Ok true
        | Error _ as err -> err
      in
      atomic_read t file >>= fun old ->
      match old, test with
      | None, None -> set ()
      | Some old, Some x when Cstruct.equal old x -> set ()
      | _ -> Lwt.return (Ok false)

    let move t ?lock patha pathb =
      Git.Mem.Lock.with_lock lock @@ fun () ->
      open' t patha ~create:false ~mode:0o644 >>?= fun fda ->
      open' t pathb ~create:true  ~mode:0o644 >>?= fun fdb ->
      (FS.size t fda >>!= fs_read) >>?= fun size ->
      let stream, push = Lwt_stream.create () in
      let rec read pos rest = match rest with
        | 0L   -> push None; Lwt.return (Ok ())
        | rest ->
          let len = Int64.to_int (min (Int64.of_int max_int) rest) in
          FS.read t fda pos len >>?= fun cs ->
          push (Some (Cstruct.concat cs));
          read (pos + len) Int64.(sub rest (of_int len))
      in
      let rec write pos () =
        Lwt_stream.get stream >>= function
        | Some cs -> FS.write t fdb pos cs >>?= write (pos + (Cstruct.len cs))
        | None    -> Lwt.return (Ok ())
      in
      (read 0 size >>!= fs_read) <?> (write 0 () >>!= fs_write)

    let exists path = connect @@ fun t -> exists t path
    let delete ?lock path = connect @@ fun t -> delete t ?lock path
    let move ?lock patha pathb = connect @@ fun t -> move t ?lock patha pathb

    let test_and_set ?lock ?temp path ~test ~set =
      connect @@ fun t -> test_and_set t ?lock ?temp path ~test ~set
    let open_w ?lock path ~mode = connect @@ fun t -> open_w t ?lock path ~mode
    let open_r ?lock path ~mode = connect @@ fun t -> open_r t ?lock path ~mode
    let write raw ?off ?len fd = connect @@ fun t -> write t raw ?off ?len fd
    let read raw ?off ?len fd = connect @@ fun t -> read t raw ?off ?len fd
  end

  module Mapper = struct

    type fd = { fd : string ; sz : int64 }
    type error = FS.error

    let pp_error = FS.pp_error

    let openfile t path =
      let fd = fpath_to_string path in
      Log.debug (fun l -> l "Mapper.openfile %s" fd);
      FS.stat t fd >|= function
      | Ok stat      -> Ok { fd; sz = stat.Mirage_fs.size }
      | Error _ as e -> e

    let length { sz; fd } =
      Log.debug (fun l -> l "Mapper.length %s" fd);
      Lwt.return (Ok sz)

    let map t { fd; _ } ?(pos = 0L) ~share:_ len =
      Log.debug (fun l -> l "Mapper.map %s" fd);
      FS.read t fd (Int64.to_int pos) len >|= function
      | Ok cs        -> Ok (Cstruct.concat cs)
      | Error _ as e -> e

    let close { fd; _ } =
      Log.debug (fun l -> l "Mapper.close %s" fd);
      Lwt.return (Ok ())

    let openfile path = connect @@ fun t -> openfile t path
    let map fd ?pos ~share len = connect @@ fun t -> map t fd ?pos ~share len
  end

  let is_dir path = connect @@ fun t -> is_dir t path
  let is_file path = connect @@ fun t -> is_file t path

  let has_global_watches = false
  let has_global_checkout = false
end
