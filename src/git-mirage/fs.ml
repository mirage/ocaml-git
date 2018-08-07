open Lwt.Infix
let ( >>!= ) x f = Lwt_result.map_err f x
let ( >>?= ) = Lwt_result.bind
let ( >|?= ) = Lwt_result.(>|=)

let src = Logs.Src.create "git-mirage.fs" ~doc:"logs mirage file-system's event"
module Log = (val Logs.src_log src : Logs.LOG)

module H = Hashtbl.Make(struct
    type t = Fpath.t
    let equal = Fpath.equal
    let hash = Hashtbl.hash
  end)

let make locks path =
  try H.find locks path
  with Not_found ->
    let m = Lwt_mutex.create () in
    H.add locks path m;
    m

let with_lock t p f =
  let m = make t p in
  Lwt.finalize
    (fun () -> Lwt_mutex.with_lock m f)
    (fun () ->
       if Lwt_mutex.is_empty m then H.remove t p;
       Lwt.return ())

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

module Make (FS: Mirage_fs_lwt.S) = struct

  type t = {
    fs     : FS.t;
    locks  : Lwt_mutex.t H.t;
    current: Fpath.t;
  }

  let v ?(current_dir=Fpath.v ".") fs =
    { fs; locks = H.create 13; current=current_dir }

  type error =
    [ `Destroy of Fpath.t * string
    | `Listdir of Fpath.t * string
    | `Stat of Fpath.t * string
    | `Read of Fpath.t * string
    | `Exn of exn
    | `FS_read of FS.error
    | `FS_write of FS.write_error ]

  let pp_error ppf = function
    | `Destroy(f,e) -> Fmt.pf ppf "Error while destroying %a: %s" Fpath.pp f e
    | `Listdir(f,e) -> Fmt.pf ppf "Error while listing %a: %s" Fpath.pp f e
    | `Stat(f,e)    -> Fmt.pf ppf "Error while stating %a: %s" Fpath.pp f e
    | `Read(f,e)    -> Fmt.pf ppf "Error while reading %a: %s" Fpath.pp f e
    | `FS_read e    -> FS.pp_error ppf e
    | `FS_write e   -> FS.pp_write_error ppf e
    | `Exn e        -> Fmt.pf ppf "System error: %a" Fmt.exn e

  let err_exn e = Error (`Exn e)
  let err_stat p fmt = Fmt.kstrf (fun e -> Error (`Stat (p, e))) fmt
  let err_read p fmt = Fmt.kstrf (fun e -> Error (`Read (p, e))) fmt
  let err_fs_read e = Error (`FS_read e)
  let err_fs_write e = Error (`FS_write e)
  let fs_read e = `FS_read e
  let fs_write e = `FS_write e
  let err_listdir p fmt =
    Fmt.kstrf (fun e -> Lwt.return (Error (`Listdir (p, e)))) fmt

  let is_file t path =
    let str_path = fpath_to_string path in
    Lwt.try_bind
      (fun () -> FS.stat t.fs str_path)
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
      (fun () -> FS.stat t.fs str_path)
      (function
        | Ok { Mirage_fs.directory; _ } -> Lwt.return (Ok directory)
        | Error _ -> Lwt.return (Ok false))
      (* FIXME: see above. *)
      (function
        | Sys_error _ -> Lwt.return (Ok false)
        | exn         -> Lwt.return (err_exn exn))

  module Dir = struct

    let exists t path =
      Log.debug (fun l -> l "Dir.exists %s" @@ fpath_to_string path);
      is_dir t path >|= function
      | Ok _ as v        -> v
      | Error (`Exn err) -> err_stat path "%a" Fmt.exn err

    let create t d =
      let path = fpath_to_string d in
      Log.debug (fun l -> l "Dir.create %s" path);
      FS.mkdir t.fs path >|= function
      | Ok ()   -> Ok true
      | Error e -> err_fs_write e

    (* Note: this breaks atomocity of move, but it's used only during
       `reset` operations, so that should be fine. *)
    let delete t d =
      let path = fpath_to_string d in
      Log.debug (fun l -> l "Dir.delete %s" path);
      FS.destroy t.fs path >|= function
      | Ok ()   -> Ok ()
      | Error e -> err_fs_write e

    (* Note: this breaks atomicity of move: if a concurrent move is in
       progress [contents] can contain both the source and the
       destination. Hopefully we only move files from a temp directory
       into its final destination so that should be fine.

       XXX(samoht): maybe the primivite operations should be
       restricted somehow? *)
    let contents t ?(rel = false) dir =
      let path = fpath_to_string dir in
      Log.debug (fun l -> l "Dir.contents %s" path);
      let rec readdir files acc =
        match files with
        | [] -> Lwt.return (Ok acc)
        | (".." | ".") :: rest -> readdir rest acc
        | f :: rest ->
          match fpath_of_string f with
          | Ok f -> readdir rest ((if rel then f else Fpath.(dir // f)) :: acc)
          | Error (`Msg e) -> Lwt.return (err_read dir "%s" e)
      in
      Lwt.try_bind
        (fun () -> FS.listdir t.fs path)
        (function
          | Ok files -> readdir files []
          | Error e  -> Lwt.return (err_fs_read e))
        (fun exn -> err_listdir dir "%a" Fmt.exn exn)

    let current t = Lwt.return (Ok t.current)
  end

  module File = struct

    (* Note: all the operations (including reads) need to be locked
       because of the lack of atomic moves in MirageOS's FS
       signature. This should probably be fixed in mirage-fs
       directly. *)

    type 'a fd =
      { t    : t;
        fd   : string
      ; path : Fpath.t
      ; mutable seek : int }
      constraint 'a = [< `Read | `Write ]

    let exists t path =
      Log.debug (fun l -> l "File.exists %s" @@ fpath_to_string path);
      with_lock t.locks path @@ fun () ->
      is_file t path >|= function
      | Ok _ as v        -> v
      | Error (`Exn err) -> err_stat path "%a" Fmt.exn err

    let open' t ~create path =
      let fd = fpath_to_string path in
      FS.stat t.fs fd >>= function
      | Ok { Mirage_fs.directory = false; _ } -> Lwt.return (Ok fd)
      | Ok _    -> Lwt.return (err_fs_write `Is_a_directory)
      | Error e ->
        if not create then Lwt.return (err_fs_read e)
        else
          FS.create t.fs (fpath_to_string path) >|= function
          | Ok ()   -> Ok fd
          | Error e -> err_fs_write e

    (* XXX(dinosaure): we can inform the size of the file and check at any time
       if [seek <= max]. *)

    let open_r t path =
      Log.debug (fun l -> l "File.open_r %s" @@ fpath_to_string path);
      open' t ~create:false path >|?= fun fd ->
      ({ t; path; fd; seek = 0 } :> [ `Read ] fd)

    let open_w t path =
      Log.debug (fun l -> l "File.open_w %s" @@ fpath_to_string path);
      open' t ~create:true path >|?= fun fd ->
      ({ t; path; fd; seek = 0 } :> [ `Write ] fd)

    let write raw ?(off = 0) ?(len = Cstruct.len raw) fd =
      Log.debug (fun l -> l "File.write %s" fd.fd);
      with_lock fd.t.locks fd.path @@ fun () ->
      FS.write fd.t.fs fd.fd fd.seek (Cstruct.sub raw off len) >|= function
      | Ok ()   -> fd.seek <- fd.seek + len; Ok len
      | Error e -> err_fs_write e

    let read dst ?(off = 0) ?(len = Cstruct.len dst) fd =
      Log.debug (fun l -> l "File.read %s" fd.fd);
      with_lock fd.t.locks fd.path @@ fun () ->
      FS.read fd.t.fs fd.fd fd.seek len >|= function
      | Error e -> err_fs_read e
      | Ok src  ->
        let (len, _) = Cstruct.fillv ~src ~dst:(Cstruct.sub dst off len) in
        fd.seek <- fd.seek + len;
        Ok len

    let close fd =
      Log.debug (fun l -> l "File.close %s" fd.fd);
      Lwt.return (Ok ())

    let delete t path =
      Log.debug (fun l -> l "File.delete %s" @@ fpath_to_string path);
      with_lock t.locks path @@ fun () ->
      FS.destroy t.fs (fpath_to_string path) >|= function
      | Ok _ as v -> v
      | Error e   -> err_fs_write e

    (* [join_results l] is similar to {Lwt.join}: it waits for all
       elements of [l] to be resolved. The result is either [Ok ()] if
       all the promises are resolving to [Ok ()] or [Error e] if there
       is at least one element resolving to [Error e]. *)
    let join_results l =
      let result = ref (Ok ()) in
      let run f =
        f >|= function
        | Ok () -> ()
        | e     -> result := e
      in
      Lwt.join (List.map run l) >|= fun () ->
      !result

    let move t patha pathb =
      Log.debug (fun l ->
          l "File.move %s %s" (fpath_to_string patha) (fpath_to_string pathb));
      let path1, path2 =
        (* we sort the order in which we lock to avoid dead-locks with
           2 concurrent move a->b and b<-a *)
        if Fpath.compare patha pathb <= 0 then patha, pathb else pathb, patha
      in
      with_lock t.locks path1 @@ fun () ->
      with_lock t.locks path2 @@ fun () ->
      open' t patha ~create:false >>?= fun fda ->
      open' t pathb ~create:true  >>?= fun fdb ->
      (FS.size t.fs fda >>!= fs_read) >>?= fun size ->
      let stream, push = Lwt_stream.create () in
      let rec read pos = function
        | 0L   -> push None; Lwt.return (Ok ())
        | rest ->
          let len = Int64.to_int (min (Int64.of_int max_int) rest) in
          FS.read t.fs fda pos len >>?= fun cs ->
          List.iter (fun c -> push (Some c)) cs;
          read (pos + len) Int64.(sub rest (of_int len))
      in
      let rec write pos () =
        Lwt_stream.get stream >>= function
        | Some cs -> FS.write t.fs fdb pos cs >>?=  write (pos + Cstruct.len cs)
        | None    -> Lwt.return (Ok ())
      in
      join_results [
        (read 0 size >>!= fs_read);
        (write 0 () >>!= fs_write)
      ]

  end

  module Mapper = struct

    let pp_error = pp_error

    type fd = { t: t; path: Fpath.t ; sz: int64 }

    let openfile t path =
      with_lock t.locks path @@ fun () ->
      Log.debug (fun l -> l "Mapper.openfile %a" Fpath.pp path);
      FS.stat t.fs (fpath_to_string path) >|= function
      | Ok stat -> Ok { t; path; sz = stat.Mirage_fs.size }
      | Error e -> Error (`FS_read e)

    let length { sz; path; _ } =
      Log.debug (fun l -> l "Mapper.length %a" Fpath.pp path);
      Lwt.return (Ok sz)

    let map { t; path; _ } ?(pos = 0L) len =
      Log.debug (fun l -> l "Mapper.map %a" Fpath.pp path);
      with_lock t.locks path @@ fun () ->
      FS.read t.fs (fpath_to_string path) (Int64.to_int pos) len >|= function
      | Ok cs   -> Ok (Cstruct.concat cs)
      | Error e -> Error (`FS_read e)

    let close { path; _ } =
      Log.debug (fun l -> l "Mapper.close %a" Fpath.pp path);
      Lwt.return (Ok ())

  end

  let has_global_watches = false
  let has_global_checkout = false
end
