module Log =
struct
  let src = Logs.Src.create "fs.mirage" ~doc:"logs mirage file-system's event"
  include (val Logs.src_log src : Logs.LOG)
end

let result_bind f a = match a with
  | Ok x -> f x
  | Error err -> Error err

let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return ())

module type GAMMA =
sig
  type path

  val current : path
  val temp    : path
end

module type FS =
sig
  include Mirage_fs.S
    with type page_aligned_buffer = Cstruct.t
     and type +'a io = 'a Lwt.t

  val connect : unit -> t Lwt.t
end

module Lock = Lwt_lock

module Make
    (Gamma : GAMMA with type path = Fpath.t)
    (FS : FS)
  : Git.S.FS
    with type path = Fpath.t
     and type +'a io = 'a Lwt.t
     and type error = [ `System of string ]
     and type File.path = Fpath.t
     and type File.lock = Lock.elt
     and type File.error = [ `Stat of string
                           | Mirage_fs.write_error
                           | Mirage_fs.error ]
     and type File.raw = Cstruct.t
     and type +'a File.io = 'a Lwt.t
     and type Dir.path = Fpath.t
     and type Dir.error = [ `Destroy of string
                          | `Listdir of string
                          | `Stat of string
                          | `Path of string
                          | Mirage_fs.write_error ]
     and type +'a Dir.io = 'a Lwt.t
     and type Mapper.raw = Cstruct.t
     and type Mapper.path = Fpath.t
     and type Mapper.error = Mirage_fs.error
     and type +'a Mapper.io = 'a Lwt.t
     (* XXX(Jean Gabin): oui, je sais. *)
= struct
  let connect fn =
    Lwt.bind (FS.connect ()) fn

  type path = Fpath.t
  type +'a io = 'a Lwt.t

  type error = [ `System of string ]

  let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

  let is_file t path =
    Lwt.try_bind
      (fun () -> FS.stat t (Fpath.to_string path))
      (function
        | Ok { Mirage_fs.directory; _ } -> Lwt.return (Ok (not directory))
        | Error _ -> Lwt.return (Ok false))
      (* FIXME: it seems that mirage-fs-unix can also raise Sys_error,
         see https://github.com/mirage/mirage-fs-unix/issues/37 *)
      (function
        | Sys_error _ -> Lwt.return (Ok false)
        | exn -> Lwt.return (Error (`System (Printexc.to_string exn))))

  let is_dir t path =
    Log.debug (fun l -> l ~header:"is_dir" "Check if %a already exists as a directory." Fpath.pp path);

    Lwt.try_bind
      (fun () ->  FS.stat t (Fpath.to_string path))
      (function
        | Ok { Mirage_fs.directory; _ } -> Lwt.return (Ok directory)
        | Error _ ->
          Log.debug (fun l -> l ~header:"is_dir" "%a does not exists (as file or as directory)." Fpath.pp path);
          Lwt.return (Ok false))
      (* FIXME: see above. *)
      (function
        | Sys_error _ ->
          Lwt.return (Ok false)
        | exn ->
          Log.err (fun l -> l ~header:"is_dir" "Retrieve an exception: %s." (Printexc.to_string exn));
          Lwt.return (Error (`System (Printexc.to_string exn))))

  module Dir
    : Git.S.DIR
      with type path = path
       and type +'a io = 'a Lwt.t
       and type error = [ `Destroy of string
                        | `Listdir of string
                        | `Stat of string
                        | `Path of string
                        | Mirage_fs.write_error ]
  = struct
    type path = Fpath.t
    type +'a io = 'a Lwt.t

    type error =
      [ `Destroy of string
      | `Listdir of string
      | `Stat of string
      | `Path of string
      | Mirage_fs.write_error ]

    let pp_error ppf = function
      | `Destroy err -> Fmt.pf ppf "(`Destroy %s)" err
      | `Listdir err -> Fmt.pf ppf "(`Listdir %s)" err
      | `Stat err -> Fmt.pf ppf "(`Stat %s)" err
      | `Path err -> Fmt.pf ppf "(`Path %s)" err
      | #Mirage_fs.write_error as err -> Mirage_fs.pp_write_error ppf err

    let exists t path =
      let open Lwt.Infix in

      is_dir t path >>= function
      | Ok _ as v -> Lwt.return v
      | Error (`System err) -> Lwt.return (Error (`Stat err))

    let create t ?(path = true) ?mode:_ dir =
      let mkdir d =
        Log.debug (fun l -> l ~header:"create" "mkdir %a." Fpath.pp d);
        FS.mkdir t (Fpath.to_string d) in

      let open Lwt.Infix in

      is_dir t dir >>= function
      | Error (`System err) ->
        Log.err (fun l -> l ~header:"create" "Retrieve an error from [is_dir]: %s." err);
        Lwt.return (Error (`Stat err))
      | Ok true -> Lwt.return (Ok false)
      | Ok false ->
        Log.debug (fun l -> l ~header:"create" "Make the new directory %a." Fpath.pp dir);

        match path with
        | false -> Lwt_pool.use mkdir_pool (fun () -> mkdir dir) >>=
          (function Error #Mirage_fs.write_error as err -> Lwt.return err
                  | Ok _ -> Lwt.return (Ok false))
        | true ->
          let rec dirs_to_create p acc =
            is_dir t p >>= function
            | Error (`System err) -> Lwt.return (Error (`Stat err))
            | Ok true -> Lwt.return (Ok acc)
            | Ok false -> dirs_to_create (Fpath.parent p) (p :: acc)
          in

          let rec create_them dirs () = match dirs with
            | [] -> Lwt.return (Ok ())
            | dir :: dirs -> Lwt_pool.use mkdir_pool (fun () -> mkdir dir) >>= function
              | Error #Mirage_fs.write_error as err -> Lwt.return err
              | Ok () -> create_them dirs ()
          in

          dirs_to_create dir []
          >>= (function Ok dirs -> create_them dirs ()
                      | Error _ as err -> Lwt.return err)
          >|= result_bind (fun _ -> Ok true)

    let delete t ?(recurse = false) dir =
      let open Lwt.Infix in

      let rec delete_files to_rmdir dirs = match dirs with
        | [] -> Lwt.return (Ok to_rmdir)
        | dir :: todo ->
          let rec delete_dir_files files dirs = match files with
            | [] -> Lwt.return (Ok dirs)
            | (".." | ".") :: todo -> delete_dir_files todo dirs
            | file :: todo ->
              let try_unlink file =
                Lwt.try_bind
                  (fun () -> FS.destroy t (Fpath.to_string file))
                  (function
                    | Error `No_directory_entry
                    | Ok () -> Lwt.return (Ok dirs)
                    | Error `Is_a_directory ->
                      Lwt.return (Ok (file :: dirs))
                    | Error #Mirage_fs.write_error as err -> Lwt.return err)
                  (fun exn -> Lwt.return (Error (`Destroy (Printexc.to_string exn))))
              in

              try_unlink Fpath.(dir / file) >>= function
              | Ok dirs -> delete_dir_files todo dirs
              | Error _ as err -> Lwt.return err
          in

          Lwt.try_bind
            (fun () -> FS.listdir t (Fpath.to_string dir))
            (function
              | Error `No_directory_entry ->
                delete_files to_rmdir todo
              | Error #Mirage_fs.error as err -> Lwt.return err
              | Ok files -> delete_dir_files files [] >>= function
                | Ok dirs -> delete_files (dir :: to_rmdir) (List.rev_append dirs todo)
                | Error #error as err -> Lwt.return err)
            (fun exn -> Lwt.return (Error (`Listdir (Printexc.to_string exn))))
      in

      let rec delete_dirs = function
        | [] -> Lwt.return (Ok ())
        | dir :: dirs ->
          let rmdir dir =
            Lwt.try_bind
              (fun () -> FS.destroy t (Fpath.to_string dir))
              (function
                | Error `No_directory_entry
                | Ok () -> Lwt.return (Ok ())
                | Error #Mirage_fs.write_error as err -> Lwt.return err)
              (fun exn -> Lwt.return (Error (`Destroy (Printexc.to_string exn))))
          in
          rmdir dir >>= function
          | Ok () -> delete_dirs dirs
          | Error _ as err -> Lwt.return err
      in

      let delete recurse dir =
        if not recurse
        then let rmdir dir =
               Lwt.try_bind
                 (fun () -> FS.destroy t (Fpath.to_string dir))
                 (function
                   | Error `No_directory_entry
                   | Ok () -> Lwt.return (Ok ())
                   | Error #Mirage_fs.write_error as err -> Lwt.return err)
                 (fun exn -> Lwt.return (Error (`Destroy (Printexc.to_string exn))))
          in
          rmdir dir
        else
          delete_files [] [dir] >>= function
          | Ok rmdirs -> delete_dirs rmdirs
          | Error _ as err -> Lwt.return err
      in

      delete recurse dir >>= function
      | Ok () -> Lwt.return (Ok ())
      | Error (`Destroy msg) ->
        Lwt.return (Error (`Destroy (Fmt.strf "delete directory %s: %s" (Fpath.to_string dir) msg)))
      | Error _ as err -> Lwt.return err

    let contents t ?(dotfiles = false) ?(rel = false) dir =
      let rec readdir files acc =
        match files with
        | [] -> Lwt.return (Ok acc)
        | (".." | ".") :: rest -> readdir rest acc
        | f :: rest when dotfiles || not (String.get f 0 = '.') ->
          (match Fpath.of_string f with
           | Ok f -> readdir rest ((if rel then f else Fpath.(dir // f)) :: acc)
           | Error (`Msg _) ->
             Lwt.return (Error (`Path (Fmt.strf "directory conctents %s: cannot parse element to a path (%S)"
                                         (Fpath.to_string dir) f))))
        | _ :: rest -> readdir rest acc
      in

      Lwt.try_bind
        (fun () -> FS.listdir t (Fpath.to_string dir))
        (function
          | Ok files -> readdir files []
          | Error #Mirage_fs.error as err -> Lwt.return err)
        (fun exn -> Lwt.return (Error (`Listdir (Printexc.to_string exn))))

    let current () = Lwt.return (Ok Gamma.current)
    let temp () = Lwt.return Gamma.temp

    let contents ?dotfiles ?rel dir = connect @@ fun t -> contents t ?dotfiles ?rel dir
    let exists path = connect @@ fun t -> exists t path
    let create ?path ?mode dir = connect @@ fun t -> create t ?path ?mode dir
    let delete ?recurse path = connect @@ fun t -> delete t ?recurse path
  end

  module File
    : Git.S.FILE
      with type path = path
       and type +'a io = 'a Lwt.t
       and type lock = Lock.elt
       and type error = [ `Stat of string
                        | Mirage_fs.error
                        | Mirage_fs.write_error ]
       and type raw = Cstruct.t
  = struct
    type path = Fpath.t
    type lock = Lock.elt
    type raw = Cstruct.t
    type +'a io = 'a Lwt.t

    type error =
      [ `Stat of string
      | Mirage_fs.error
      | Mirage_fs.write_error ]

    let pp_error ppf = function
      | `Stat err -> Fmt.pf ppf "(`Stat %s)" err
      | #Mirage_fs.error as err -> Fmt.pf ppf "%a" Mirage_fs.pp_error err
      | #Mirage_fs.write_error as err -> Fmt.pf ppf "%a" Mirage_fs.pp_write_error err

    type 'a fd =
      { fd   : string
      ; mutable seek : int }
      constraint 'a = [< `Read | `Write ]

    let exists t path =
      let open Lwt.Infix in

      is_file t path >|= function
      | Ok _ as v -> v
      | Error (`System err) -> Error (`Stat err)

    let open' t ?lock path ~mode:_ =
      let open Lwt.Infix in

      Lock.with_lock lock
      @@ fun () ->
      let fd = Fpath.to_string path in
      FS.stat t fd >>= function
      | Ok { Mirage_fs.directory = false; _ } -> Lwt.return (Ok fd)
      | Ok _ -> Lwt.return (Error `Is_a_directory)
      | Error #Mirage_fs.error ->
        FS.create t (Fpath.to_string path) >>= function
        | Ok () -> Lwt.return (Ok fd)
        | Error (#Mirage_fs.write_error as err) ->
          Lwt.return (Error (err :> error))

    (* XXX(dinosaure): we can inform the size of the file and check at any time
       if [seek <= max]. *)

    let open_r t ?lock path ~mode =
      let open Lwt.Infix in
      open' t ?lock path ~mode >|= result_bind (fun fd -> Ok ({ fd; seek = 0 } :> [ `Read ] fd))

    let open_w t ?lock path ~mode =
      let open Lwt.Infix in
      open' t ?lock path ~mode >|= result_bind (fun fd -> Ok ({ fd; seek = 0 } :> [ `Write ] fd))

    let write t raw ?(off = 0) ?(len = Cstruct.len raw) fd =
      let open Lwt.Infix in

      FS.write t fd.fd fd.seek (Cstruct.sub raw off len) >|= function
      | Ok () -> fd.seek <- fd.seek + len; Ok len
      | Error (#Mirage_fs.write_error as err) -> Error (err :> error)

    let read t dst ?(off = 0) ?(len = Cstruct.len dst) fd =
      let open Lwt.Infix in

      FS.read t fd.fd fd.seek len >|= function
      | Error (#Mirage_fs.error as err) -> Error (err :> error)
      | Ok src ->
        let (len, _) = Cstruct.fillv ~src ~dst:(Cstruct.sub dst off len) in
        fd.seek <- fd.seek + len;
        Ok len

    let close _ = Lwt.return (Ok ())

    let delete t ?lock path =
      let open Lwt.Infix in

      Lock.with_lock lock
      @@ fun () ->
      FS.destroy t (Fpath.to_string path) >|= function
      | Ok _ as v -> v
      | Error (#Mirage_fs.write_error as err) -> Error (err :> error)

    let atomic_read t path =
      let open Lwt.Infix in

      FS.stat t (Fpath.to_string path) >>= function
      | Error _ -> Lwt.return None
      | Ok stat ->
        if stat.Mirage_fs.directory
        then Lwt.fail (Failure (Fmt.strf "%a is a directory" Fpath.pp path))
        else FS.read t (Fpath.to_string path) 0 (Int64.to_int stat.Mirage_fs.size) >|= function
          | Error _ -> None
          | Ok cs -> Some (Cstruct.of_string (Cstruct.copyv cs))

    let atomic_write t ?lock path content =
      let open Lwt.Infix in

      FS.mkdir t (Filename.dirname (Fpath.to_string path)) >>= fun _ ->
      Lock.with_lock lock
      @@ fun () ->
      FS.create t (Fpath.to_string path) >>= function
      | Error (#Mirage_fs.write_error as err) -> Lwt.return (Error (err :> error))
      | Ok () -> FS.write t (Fpath.to_string path) 0 content >|= function
        | Ok _ as v -> v
        | Error (#Mirage_fs.write_error as err) -> Error (err :> error)

    let test_and_set t ?lock ?temp:_ file ~test ~set =
      let open Lwt.Infix in

      FS.mkdir t (Filename.dirname (Fpath.to_string file)) >>= fun _ ->
      Lock.with_lock lock
      @@ fun () ->
      let set () =
        (match set with
         | None   -> delete t file
         | Some v -> atomic_write t file v)
        >|= function Ok () -> Ok true
                   | Error _ as err -> err
      in
      atomic_read t file >>= fun old ->
      match old, test with
      | None, None -> set ()
      | Some old, Some x when Cstruct.equal old x -> set ()
      | _ -> Lwt.return (Ok false)

    let move t ?lock patha pathb =
      let open Lwt_result in

      let ( >!= ) = Lwt_result.bind_lwt_err in

      Lock.with_lock lock
      @@ fun () ->
      (open' t patha ~mode:0o644 >!= fun err -> Lwt.return (err :> error)) >>= fun fda ->
      (open' t pathb ~mode:0o644 >!= fun err -> Lwt.return (err :> error)) >>= fun fdb ->
      (FS.size t fda >!= function #Mirage_fs.error as err -> Lwt.return (err :> error)) >>= fun size ->

      let stream, push = Lwt_stream.create () in

      let rec read pos rest = match rest with
        | 0L ->
          push None; Lwt.return (Ok ())
        | rest ->
          let len = Int64.to_int (min (Int64.of_int max_int) rest) in
          FS.read t fda pos len >>= fun cs ->
          push (Some (Cstruct.concat cs));
          read (pos + len) Int64.(sub rest (of_int len))
      in

      let rec write pos () =
        let open Lwt.Infix in

        Lwt_stream.get stream >>= function
        | Some cs ->
          let open Lwt_result in
          FS.write t fdb pos cs >>= write (pos + (Cstruct.len cs))
        | None ->
          Lwt.return (Ok ())
      in

      let open Lwt.Infix in
      let ( >!= ) = Lwt_result.bind_lwt_err in

      (read 0 size >!= function #Mirage_fs.error as err -> Lwt.return (err :> error))
      <?> (write 0 () >!= function (#Mirage_fs.error | #Mirage_fs.write_error) as err -> Lwt.return (err :> error))

    let exists path = connect @@ fun t -> exists t path
    let delete ?lock path = connect @@ fun t -> delete t ?lock path
    let move ?lock patha pathb = connect @@ fun t -> move t ?lock patha pathb

    let test_and_set ?lock ?temp path ~test ~set = connect @@ fun t -> test_and_set t ?lock ?temp path ~test ~set
    let open_w ?lock path ~mode = connect @@ fun t -> open_w t ?lock path ~mode
    let open_r ?lock path ~mode = connect @@ fun t -> open_r t ?lock path ~mode
    let write raw ?off ?len fd = connect @@ fun t -> write t raw ?off ?len fd
    let read raw ?off ?len fd = connect @@ fun t -> read t raw ?off ?len fd
  end

  module Mapper
    : Git.S.MAPPER
      with type raw = Cstruct.t
       and type +'a io = 'a Lwt.t
       and type path = path
       and type error = Mirage_fs.error
  = struct
    type fd = { fd : string
              ; sz : int64 }
    type raw = Cstruct.t
    type path = Fpath.t
    type error = Mirage_fs.error
    type +'a io = 'a Lwt.t

    let pp_error = Mirage_fs.pp_error

    let openfile t path =
      let open Lwt.Infix in

      let fd = Fpath.to_string path in
      FS.stat t fd >>= function
      | Ok stat -> Lwt.return (Ok { fd; sz = stat.Mirage_fs.size })
      | Error (#Mirage_fs.error as err) -> Lwt.return (Error (err :> error))

    let length { sz; _ } = Lwt.return (Ok sz)

    let map t { fd; _ } ?(pos = 0L) ~share:_ len =
      let open Lwt.Infix in

      FS.read t fd (Int64.to_int pos) len >|= function
      | Ok cs -> Ok (Cstruct.concat cs)
      | Error (#Mirage_fs.error as err) -> Error (err :> error)

    let close _ = Lwt.return (Ok ())

    let openfile path = connect @@ fun t -> openfile t path
    let map fd ?pos ~share len = connect @@ fun t -> map t fd ?pos ~share len
  end

  let is_dir path = connect @@ fun t -> is_dir t path
  let is_file path = connect @@ fun t -> is_file t path
end
