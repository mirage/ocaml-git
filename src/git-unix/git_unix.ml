(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Bigarray = Bigarray_compat
open Lwt.Infix

module Fold = struct
  let always x _ = x

  let contents ?(dotfiles = false) ?(rel = false) dir =
    let rec readdir dh acc =
      Lwt.catch
        (fun () -> Lwt_unix.readdir dh >>= Lwt.return_some)
        (fun _exn -> Lwt.return_none)
      >>= function
      | None -> Lwt.return acc
      | Some (".." | ".") -> readdir dh acc
      | Some f when dotfiles || not (f.[0] = '.') -> (
          match Fpath.of_string f with
          | Ok f -> readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
          | Error (`Msg _) -> (* ignore *) readdir dh acc)
      | Some _ -> readdir dh acc in
    Lwt.catch
      (fun () ->
        Lwt_unix.opendir (Fpath.to_string dir) >>= fun dh ->
        readdir dh [] >>= fun res ->
        Lwt_unix.closedir dh >>= fun () -> Lwt.return res)
      Lwt.fail

  let do_traverse_fun = function
    | `Any -> always true
    | `None -> always false
    | `Sat sat -> sat

  let exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun _ -> Lwt.return true)
      (fun _exn -> Lwt.return false)

  let file_exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_REG))
      (fun _exn -> Lwt.return false)

  let dir_exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_DIR))
      (fun _exn -> Lwt.return false)

  let is_element_fun = function
    | `Any -> exists
    | `Files -> file_exists
    | `Dirs -> dir_exists
    | `Sat sat -> sat

  let readdir_fun path =
    Lwt_unix.opendir (Fpath.to_string path) >>= fun dh ->
    let rec go acc =
      Lwt.catch
        (fun () -> Lwt_unix.readdir dh >>= fun entry -> go (entry :: acc))
        (fun _exn -> Lwt.return (List.rev acc)) in
    go [] >>= fun res ->
    Lwt_unix.closedir dh >>= fun () -> Lwt.return res

  let fold ?(dotfiles = false) ?(elements = `Any) ?(traverse = `Any) f acc paths
      =
    let process () =
      let do_traverse = do_traverse_fun traverse in
      let is_element = is_element_fun elements in
      let is_dir = dir_exists in
      let readdir = readdir_fun in
      let process_path p (acc, to_traverse) =
        Lwt.both (is_element p) (is_dir p) >>= function
        | false, true when do_traverse p -> Lwt.return (acc, p :: to_traverse)
        | true, true when do_traverse p ->
            Lwt.both (f p acc) (Lwt.return (p :: to_traverse))
        | true, _ -> Lwt.both (f p acc) (Lwt.return to_traverse)
        | _ -> Lwt.return (acc, to_traverse) in
      let dir_child d acc bname =
        if (not dotfiles) && bname.[0] = '.'
        then Lwt.return acc
        else process_path Fpath.(d / bname) acc in
      let rec loop acc = function
        | (d :: ds) :: up ->
            readdir d >>= fun childs ->
            Lwt_list.fold_left_s (dir_child d) (acc, []) childs
            >>= fun (acc, to_traverse) -> loop acc (to_traverse :: ds :: up)
        | [ [] ] -> Lwt.return acc
        | [] :: up -> loop acc up
        | _ -> assert false in
      let init acc p =
        let base = Fpath.(basename @@ normalize p) in
        if (not dotfiles) && base.[0] = '.'
        then Lwt.return acc
        else process_path p acc in
      Lwt_list.fold_left_s init (acc, []) paths >>= fun (acc, to_traverse) ->
      loop acc [ to_traverse ] in
    process ()

  let fold ?dotfiles ?elements ?traverse f acc d =
    contents d >>= fold ?dotfiles ?elements ?traverse f acc
end

module Minor_heap (Digestif : Digestif.S) = struct
  let src =
    Logs.Src.create "git-unix.minor" ~doc:"logs git-unix's minor heap event"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = Fpath.t (* [.git/objects] *)

  type uid = Digestif.t

  type error = [ `Not_found of Digestif.t ]

  let pp_error ppf = function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Digestif.pp uid

  type +'a fiber = 'a Lwt.t

  let split uid =
    let hex = Digestif.to_hex uid in
    (String.sub hex 0 2, String.sub hex 2 ((Digestif.digest_size * 2) - 2))

  let exists root uid =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process () =
      Lwt_unix.stat (Fpath.to_string path) >>= fun _ -> Lwt.return true in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return false
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let length root uid =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process () =
      Lwt_unix.LargeFile.stat (Fpath.to_string path) >>= fun stat ->
      Lwt.return_ok stat.Unix.LargeFile.st_size in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return_error (`Not_found uid)
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let map root uid ~pos len =
    if pos < 0L || len < 0 then invalid_arg "Minor_heap.map: invalid bounds" ;
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process () =
      Lwt_unix.LargeFile.stat (Fpath.to_string path) >>= fun stat ->
      let len =
        if Int64.add pos (Int64.of_int len) > stat.Lwt_unix.LargeFile.st_size
        then Int64.to_int (Int64.sub stat.Lwt_unix.LargeFile.st_size pos)
        else len in
      let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
      let rs =
        Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
      in
      Unix.close fd ;
      Lwt.return (Bigarray.array1_of_genarray rs) in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return Bigstringaf.empty
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let append root uid payload =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process0 () =
      let path = Fpath.(root / hd) in
      if Sys.file_exists (Fpath.to_string path)
         && Sys.is_directory (Fpath.to_string path)
      then (
        Log.debug (fun m -> m "%a already exists." Fpath.pp path) ;
        Lwt.return_unit)
      else (
        Log.debug (fun m -> m "Create %a." Fpath.pp path) ;
        Lwt_unix.mkdir (Fpath.to_string path) 0o755) in
    let process1 () =
      if Sys.file_exists (Fpath.to_string path)
      then (
        Log.debug (fun m -> m "%a already exists" Fpath.pp path) ;
        Lwt.return_ok ())
      else
        Lwt_unix.openfile (Fpath.to_string path)
          Unix.[ O_CREAT; O_WRONLY; O_TRUNC ]
          0o644
        >>= fun fd ->
        Log.debug (fun m -> m "Create %a with 0o644 permissions." Fpath.pp path) ;
        let rec go off len =
          Lwt_bytes.write fd payload off len >>= fun len' ->
          if len = len'
          then Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
          else go (off + len') (len - len') in
        go 0 (Bigstringaf.length payload) in
    Lwt.catch process0 Lwt.fail >>= fun () -> Lwt.catch process1 Lwt.fail

  let appendv root uid payloads =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let payloads = List.map (fun p -> (p, 0, Bigstringaf.length p)) payloads in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path)
        Unix.[ O_CREAT; O_WRONLY; O_TRUNC ]
        0o644
      >>= fun fd ->
      let rec go = function
        | [] -> Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
        | (p, off, len) :: r ->
            Lwt_bytes.write fd p off len >>= fun len' ->
            if len - len' = 0
            then go r
            else go ((p, off + len', len - len') :: r) in
      go payloads in
    Lwt.catch process Lwt.fail

  let list root =
    let f x r =
      match List.rev (Fpath.segs x) with
      | tl :: hd :: _ ->
          let uid = Digestif.of_hex (hd ^ tl) in
          Lwt.return (uid :: r)
      | _ -> Lwt.return r in
    let elements path =
      match List.rev (Fpath.segs path) with
      | tl :: hd :: _ -> (
          match Digestif.of_hex (hd ^ tl) with
          | _ -> Fold.file_exists path
          | exception _ -> Lwt.return false)
      | _ -> Lwt.return false in
    Fold.fold ~dotfiles:false ~elements:(`Sat elements) f [] root
end

module Major_heap = struct
  type t = Fpath.t (* [.git/objects/pack] *)

  type uid = Fpath.t

  and fd = Lwt_unix.file_descr

  type error = [ `Not_found of uid ]

  type +'a fiber = 'a Lwt.t

  let pp_error : error Fmt.t =
   fun ppf -> function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Fpath.pp uid

  (* XXX(dinosaure): currently, [Major_heap] has a read and a write
     access due to [append] which is only call by [Store.Sync]. We should
     provide 2 [Major_heap]:
     - one used by [Store] which is read-only
     - the second used by [Store.Sync] which is write-only *)

  let create root path =
    let path = Fpath.(root // path) in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path)
        Unix.[ O_RDWR; O_CREAT; O_APPEND ]
        0o644
      >>= fun fd -> Lwt.return_ok fd in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) ->
          Lwt.return_error (`Not_found path)
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let map : t -> fd -> pos:int64 -> int -> Bigstringaf.t fiber =
   fun _ fd ~pos len ->
    let fd = Lwt_unix.unix_file_descr fd in
    let payload =
      Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
    in
    Lwt.return (Bigarray.array1_of_genarray payload)

  let close _ fd = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()

  let length fd =
    Lwt_unix.LargeFile.fstat fd >>= fun st ->
    Lwt.return st.Unix.LargeFile.st_size

  let list root =
    Lwt_unix.opendir (Fpath.to_string root) >>= fun dh ->
    let rec go acc =
      Lwt.catch
        (fun () ->
          Lwt_unix.readdir dh >>= function
          | "." | ".." -> go acc
          | entry ->
          match Fpath.of_string entry with
          | Ok x -> if Fpath.has_ext "pack" x then go (x :: acc) else go acc
          | Error (`Msg _) -> (* ignore *) go acc)
        (function End_of_file -> Lwt.return acc | exn -> Lwt.fail exn) in
    go []

  let move root ~src ~dst =
    let src = Fpath.(root // src) in
    let dst = Fpath.(root // dst) in
    Lwt_unix.rename (Fpath.to_string src) (Fpath.to_string dst) >>= fun () ->
    Lwt.return_ok ()

  let append : t -> fd -> string -> unit fiber =
   fun _ fd str ->
    let rec go (off, len) =
      Lwt_unix.write_string fd str off len >>= fun len' ->
      if len = len' then Lwt.return () else go (off + len', len - len') in
    go (0, String.length str)
end

module Reference_heap = struct
  let exists dir =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string dir) >>= fun stat ->
        Lwt.return_ok (stat.Unix.st_kind = Unix.S_DIR))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok false
        | exn -> Lwt.fail exn)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> Lwt.return_error err

  let create ?(path = true) ?(mode = 0o755) dir =
    let mkdir d mode =
      Lwt.catch
        (fun () -> Lwt_unix.mkdir (Fpath.to_string d) mode >>= Lwt.return_ok)
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_ok ()
          | Unix.Unix_error (e, _, _) ->
              if d = dir
              then
                Lwt.return_error
                @@ Rresult.R.msgf "create directory %a: %s" Fpath.pp d
                     (Unix.error_message e)
              else
                Lwt.return_error
                @@ Rresult.R.msgf "create directory %a: %a: %s" Fpath.pp dir
                     Fpath.pp d (Unix.error_message e)
          | exn -> Lwt.fail exn) in
    exists dir >>? function
    | true -> Lwt.return_ok false
    | false ->
    match path with
    | false -> mkdir dir mode >>? fun () -> Lwt.return_ok false
    | true ->
        let rec dirs_to_create p acc =
          exists p >>? function
          | true -> Lwt.return_ok acc
          | false -> dirs_to_create (Fpath.parent p) (p :: acc) in
        let rec create_them dirs () =
          match dirs with
          | dir :: dirs -> mkdir dir mode >>? create_them dirs
          | [] -> Lwt.return_ok () in
        dirs_to_create dir [] >>? fun dirs ->
        create_them dirs () >>? fun () -> Lwt.return_ok true

  let src =
    Logs.Src.create "git-unix.reference"
      ~doc:"logs git-unix's reference heap event"

  module Log = (val Logs.src_log src : Logs.LOG)

  type +'a fiber = 'a Lwt.t

  type t = Fpath.t (* [.git] *)

  type error = [ `Not_found of Git.Reference.t | `Msg of string ]

  let pp_error ppf = function
    | `Not_found refname -> Fmt.pf ppf "%a not found" Git.Reference.pp refname
    | `Msg err -> Fmt.string ppf err

  let atomic_wr root refname str =
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    let base, _ = Fpath.split_base path in
    let process0 () = create ~path:true base in
    let process1 () =
      let fd =
        Unix.openfile (Fpath.to_string path)
          Unix.[ O_TRUNC; O_CREAT; O_WRONLY ]
          0o644 in
      let rec go (off, len) =
        let len' = Unix.single_write_substring fd str off len in
        if len = len'
        then (
          Unix.close fd ;
          Lwt.return_ok ())
        else
          (* TODO(dinosaure): replace by [open tmp / write / close tmp / rename tmp path]
           * to be really atomic. *)
          go (off + len', len - len') in
      go (0, String.length str) in
    Lwt.catch process0 Lwt.fail >>? fun _ -> Lwt.catch process1 Lwt.fail

  let atomic_rd root refname =
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    let process () =
      let fd =
        Unix.openfile (Fpath.to_string path) Unix.[ O_CREAT; O_RDONLY ] 0o644
      in
      let stat = Unix.fstat fd in
      let len = stat.Unix.st_size in
      let tmp = Bytes.create len in
      Log.debug (fun m -> m "Start to read %d byte(s) on %a." len Fpath.pp path) ;
      let rec go off len =
        let len' = Unix.read fd tmp off len in
        if len = len'
        then (
          Unix.close fd ;
          Lwt.return_ok (Bytes.unsafe_to_string tmp))
        else go (off + len') (len - len') in
      go 0 len in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) ->
          Lwt.return_error (`Not_found refname)
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let atomic_rm root refname =
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    let process () =
      Lwt_unix.unlink (Fpath.to_string path) >>= fun () -> Lwt.return_ok ()
    in
    Lwt.catch process Lwt.fail

  let list root =
    let f x r =
      match Git.Reference.of_string (Fpath.to_string x) with
      | Ok x -> Lwt.return (x :: r)
      | Error _ -> Lwt.return r in
    let elements path =
      match Fpath.segs path with
      | [ "HEAD" ] -> Fold.file_exists path
      | "refs" :: _ -> Fold.file_exists path
      | _ -> Lwt.return false in
    Fold.fold ~dotfiles:false ~elements:(`Sat elements) f [] root

  type fd = Stdlib.in_channel

  let input_line fd =
    match Stdlib.input_line fd with
    | line -> Lwt.return_some line
    | exception _ -> Lwt.return_none
end

module Make (Digestif : Digestif.S) = struct
  module Mn = Minor_heap (Digestif)
  include Git.Store.Make (Digestif) (Mn) (Major_heap) (Reference_heap)

  let v root =
    let minor = Fpath.(root / ".git" / "objects") in
    let major = Fpath.(root / ".git" / "objects" / "pack") in
    let idx = Fpath.set_ext "idx" in
    let refs = Fpath.(root / ".git") in
    v ~minor ~major ~idx ~refs root
end

module Sync
    (Digestif : Digestif.S)
    (Store : Git.S
               with type hash = Digestif.t
                and type pack = Fpath.t
                and type index = Fpath.t)
    (HTTP : Smart_git.HTTP) =
struct
  include Git.Sync.Make (Digestif) (Major_heap) (Major_heap) (Conduit_lwt)
            (Store)
            (HTTP)

  let random_gen = lazy (Random.State.make_self_init ())

  let random_path pat =
    let rand = Random.State.bits (Lazy.force random_gen) land 0xFFFFFF in
    Fpath.v (Fmt.strf pat (Fmt.strf "%06x" rand))

  let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

  let create_tmp_path mode dir pat =
    let rec loop count =
      if count < 0
      then
        failwithf "Create a temporary file %s in %a: too many failing attempts"
          (Fmt.strf pat "XXXXXX") Fpath.pp dir
      else
        let file = random_path pat in
        let sfile = Fpath.to_string Fpath.(dir // file) in
        let open_flags = Unix.[ O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE ] in
        let process () =
          Lwt_unix.openfile sfile open_flags mode >>= fun fd ->
          Lwt.return (file, fd) in
        let error = function
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (pred count)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
          | exn -> Lwt.fail exn in
        Lwt.catch process error in
    loop 10000

  let tmp ?(mode = 0o600) dir pat =
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    Lwt_unix.close fd >>= fun () -> Lwt.return file

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> Lwt.return_error err

  let fetch ~resolvers edn store ?version ?capabilities want =
    let root = Store.root store in
    let temp = Fpath.(root / ".git" / "tmp") in
    let major = Fpath.(root / ".git" / "objects" / "pack") in
    tmp temp "pack-%s.pack" >>= fun src ->
    tmp temp "pack-%s.pack" >>= fun dst ->
    tmp temp "pack-%s.idx" >>= fun idx ->
    fetch ~resolvers edn store ?version ?capabilities want ~src ~dst ~idx temp
      temp
    >>? function
    | `Empty -> Lwt.return_ok None
    | `Pack (hash, refs) ->
        let pack = Fpath.(major / Fmt.strf "pack-%a.pack" Digestif.pp hash) in
        let index = Fpath.(major / Fmt.strf "pack-%a.idx" Digestif.pp hash) in
        Lwt_unix.rename (Fpath.to_string dst) (Fpath.to_string pack)
        >>= fun () ->
        Lwt_unix.rename (Fpath.to_string idx) (Fpath.to_string index)
        >>= fun () ->
        Store.batch_write store ~index ~pack
        >|= Rresult.R.reword_error (fun err -> `Store err)
        >>? fun () -> Lwt.return_ok (Some (hash, refs))
end

module Store = Make (Digestif.SHA1)
