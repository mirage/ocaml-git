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
open Stdlib
open Lwt.Infix

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let ( <.> ) f g x = f (g x)

(* XXX(dinosaure): NOTE! [Git_unix] wants to provide an implementation
 * which can fit into required modules by [Git.Store] __and__ the usual
 * layout of a non-bare Git repository.
 *
 * Nothing was done about performances - and provided implementations 
 * are surely not the best. If someone wants a _fast_ implementation
 * of Git, this is the first entry-point. *)

module Fold = struct
  let src = Logs.Src.create "git-unix.fold" ~doc:"logs git-unix's fold event"

  module Log = (val Logs.src_log src : Logs.LOG)

  let always x _ = x

  let rec contents ?(dotfiles = false) ?(rel = false) dir =
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
          | Error (`Msg _) -> (* ignore *) readdir dh acc )
      | Some _ -> readdir dh acc
    in
    Lwt.catch
      (fun () ->
        Lwt_unix.opendir (Fpath.to_string dir) >>= fun dh ->
        readdir dh [] >>= fun res ->
        Lwt_unix.closedir dh >>= fun () -> Lwt.return res)
      (function
        | Unix.Unix_error (Unix.EINTR, _, _) -> contents ~dotfiles ~rel dir
        | Unix.Unix_error (err, _, _) ->
            let err =
              Fmt.strf "directory contents %a: %s" Fpath.pp dir
                (Unix.error_message err)
            in
            Log.err (fun m -> m "%s" err);
            Lwt.return []
        | exn -> Lwt.fail exn)

  let do_traverse_fun = function
    | `Any -> always true
    | `None -> always false
    | `Sat sat -> sat

  let rec exists path =
    Lwt.catch (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun _ -> Lwt.return true)
    @@ function
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Lwt.return false
    | Unix.Unix_error (Unix.EINTR, _, _) -> exists path
    | exn -> Lwt.fail exn

  let rec file_exists path =
    Lwt.catch (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_REG))
    @@ function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
    | Unix.Unix_error (Unix.EINTR, _, _) -> file_exists path
    | exn -> Lwt.fail exn

  let rec dir_exists path =
    Lwt.catch (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_DIR))
    @@ function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
    | Unix.Unix_error (Unix.EINTR, _, _) -> dir_exists path
    | exn -> Lwt.fail exn

  let is_element_fun = function
    | `Any -> exists
    | `Files -> file_exists
    | `Dirs -> dir_exists
    | `Sat sat -> sat

  let readdir_fun =
    let readdir d = try Sys.readdir (Fpath.to_string d) with _exn -> [||] in
    Lwt.return <.> Array.to_list <.> readdir

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
        | _ -> Lwt.return (acc, to_traverse)
      in
      let dir_child d acc bname =
        if (not dotfiles) && bname.[0] = '.' then Lwt.return acc
        else process_path Fpath.(d / bname) acc
      in
      let rec loop acc = function
        | (d :: ds) :: up ->
            readdir d >>= fun childs ->
            Lwt_list.fold_left_s (dir_child d) (acc, []) childs
            >>= fun (acc, to_traverse) -> loop acc (to_traverse :: ds :: up)
        | [ [] ] -> Lwt.return acc
        | [] :: up -> loop acc up
        | _ -> assert false
      in
      let init acc p =
        let base = Fpath.(basename @@ normalize p) in
        if (not dotfiles) && base.[0] = '.' then Lwt.return acc
        else process_path p acc
      in
      Lwt_list.fold_left_s init (acc, []) paths >>= fun (acc, to_traverse) ->
      loop acc [ to_traverse ]
    in
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
  type error = [ `Not_found of Digestif.t | `Msg of string ]

  let pp_error ppf = function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Digestif.pp uid
    | `Msg err -> Fmt.string ppf err

  type +'a fiber = 'a Lwt.t

  let split uid =
    let hex = Digestif.to_hex uid in
    String.sub hex 0 2, String.sub hex 2 ((Digestif.digest_size * 2) - 2)

  let rec exists root uid =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process () =
      Lwt_unix.stat (Fpath.to_string path) >>= fun _ -> Lwt.return true
    in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return false
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
      | Unix.Unix_error (Unix.EINTR, _, _) -> exists root uid
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let rec length root uid =
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let process () =
      Lwt_unix.LargeFile.stat (Fpath.to_string path) >>= fun stat ->
      Lwt.return_ok stat.Unix.LargeFile.st_size
    in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return_error (`Not_found uid)
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_error (`Not_found uid)
      | Unix.Unix_error (Unix.EINTR, _, _) -> length root uid
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let rec map root uid ~pos len =
    if pos < 0L || len < 0 then invalid_arg "Minor_heap.map: invalid bounds";
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let rec process () =
      Lwt_unix.LargeFile.stat (Fpath.to_string path) >>= fun stat ->
      try
        let len =
          if Int64.add pos (Int64.of_int len) > stat.Lwt_unix.LargeFile.st_size
          then Int64.to_int (Int64.sub stat.Lwt_unix.LargeFile.st_size pos)
          else len
        in
        let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o400 in
        let rs =
          Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false
            [| len |]
        in
        Unix.close fd;
        Lwt.return (Bigarray.array1_of_genarray rs)
      with
      | Unix.Unix_error (Unix.EACCES, _, _) | Unix.Unix_error (Unix.ENOENT, _, _)
        ->
          Lwt.return Bigstringaf.empty
      | Unix.Unix_error (Unix.EINTR, _, _) -> process ()
    in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return Bigstringaf.empty
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return Bigstringaf.empty
      | Unix.Unix_error (Unix.EINTR, _, _) -> map root uid ~pos len
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let append root uid payload =
    (* XXX(dinosaure): [irmin] expects an atomicity about the creation of [uid].
     * This does not mean an atomicty about the creation and the filling of [uid]!
     * This assumption requires an atomicity about [mkdir / openfile].
     *
     * A problem /can/ occur when we use [Lwt_unix.mkdir] which can /yield/. In such
     * case, [uid] still does not exist. However, [irmin] expects, at least, the existence
     * of it (whatever if we wrote entirely, partially or nothing).
     *
     * We should optimize this function but we need to keep this assumption -
     * which was not really clear.
     *
     * More precisely, a data-race condition exists when [irmin] wants to save 2 times the
     * same object from 2 different fibers. One can create the given object partially and
     * the other can try to read it, if such case appear, the second considers the object
     * as an non-existent object (but the second fiber is may be used to update a reference).
     * Finally, we assert the requirement of the atomicity about [append{v}]. However, the
     * bug discovered is really strange (replication of the bug can be done with [irmin-unix],
     * test [GIT.021]) *)
    Log.debug (fun m -> m "Minor.append %a" Digestif.pp uid);
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let fiber () =
      let open Rresult in
      Bos.OS.Dir.create Fpath.(root / hd) >>= fun _ ->
      Bos.OS.File.write path (Bigstringaf.to_string payload)
    in
    Lwt.return (fiber ())

  let f emitter (tmp, payloads) =
    let rec go pos = function
      | [] ->
          emitter None;
          Rresult.R.ok ()
      | src :: rest as payloads ->
          let len = min (Bytes.length tmp) (Bigstringaf.length src - pos) in
          Bigstringaf.blit_to_bytes src ~src_off:pos tmp ~dst_off:0 ~len;
          emitter (Some (tmp, 0, len));
          let pos = pos + len in
          if pos = Bigstringaf.length src then go 0 rest else go pos payloads
    in
    go 0 payloads

  let appendv root uid payloads =
    Log.debug (fun m -> m "Minor.appendv %a" Digestif.pp uid);
    let hd, tl = split uid in
    let path = Fpath.(root / hd / tl) in
    let fiber () =
      let open Rresult in
      Bos.OS.Dir.create Fpath.(root / hd) >>= fun _ ->
      Bos.OS.File.with_output path f (Bytes.create De.io_buffer_size, payloads)
    in
    Lwt.return (Rresult.R.join (fiber ()))

  let list root =
    let f x r =
      match List.rev (Fpath.segs x) with
      | tl :: hd :: _ ->
          let uid = Digestif.of_hex (hd ^ tl) in
          Lwt.return (uid :: r)
      | _ -> Lwt.return r
    in
    let elements path =
      match List.rev (Fpath.segs path) with
      | tl :: hd :: _ -> (
          match Digestif.of_hex (hd ^ tl) with
          | _ -> Fold.file_exists path
          | exception _ -> Lwt.return false )
      | _ -> Lwt.return false
    in
    Fold.fold ~dotfiles:false ~elements:(`Sat elements) f [] root

  let reset root =
    list root >>= fun lst ->
    let rec f uid =
      let hd, tl = split uid in
      let path = Fpath.(root / hd / tl) in
      Lwt.catch
        (fun () -> Lwt_unix.unlink (Fpath.to_string path))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> f uid
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
          | exn -> Lwt.fail exn)
    in
    Lwt_list.iter_p f lst >>= Lwt.return_ok
end

module Major_heap = struct
  let src =
    Logs.Src.create "git-unix.major" ~doc:"logs git-unix's major heap event"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = Fpath.t (* [.git/objects/pack] *)

  type uid = Fpath.t
  type 'a rd = < rd : unit ; .. > as 'a
  type 'a wr = < wr : unit ; .. > as 'a

  type 'a mode =
    | Rd : < rd : unit > mode
    | Wr : < wr : unit > mode
    | RdWr : < rd : unit ; wr : unit > mode

  type 'a fd = Lwt_unix.file_descr
  type error = [ `Not_found of uid ]
  type +'a fiber = 'a Lwt.t

  let pp_error : error Fmt.t =
   fun ppf -> function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Fpath.pp uid

  (* XXX(dinosaure): currently, [Major_heap] has a read and a write
     access due to [append] which is only call by [Store.Sync]. We should
     provide 2 [Major_heap]:
     - one used by [Store] which is read-only
     - the second used by [Store.Sync] which is write-only

     A [mode] is better (to avoid duplicate) and safe. *)

  let create : type a. mode:a mode -> t -> uid -> (a fd, error) result Lwt.t =
   fun ~mode root path ->
    let path = Fpath.(root // path) in
    let flags, perm =
      match mode with
      | Rd -> Unix.[ O_RDONLY ], 0o400
      | Wr -> Unix.[ O_WRONLY; O_CREAT; O_APPEND ], 0o600
      | RdWr -> Unix.[ O_RDWR; O_CREAT; O_APPEND ], 0o600
    in
    let rec process () =
      Lwt_unix.openfile (Fpath.to_string path) flags perm >>= fun fd ->
      Lwt.return_ok fd
    and error = function
      | Unix.Unix_error (Unix.ENOENT, _, _) | Unix.Unix_error (Unix.EACCES, _, _)
        ->
          Log.err (fun m -> m "%a does not exists." Fpath.pp path);
          Lwt.return_error (`Not_found path)
      | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let map : t -> [> `Rd ] fd -> pos:int64 -> int -> Bigstringaf.t fiber =
   fun _ fd ~pos len ->
    let fd = Lwt_unix.unix_file_descr fd in
    let payload =
      Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
    in
    Lwt.return (Bigarray.array1_of_genarray payload)

  let close _ fd =
    let rec process () = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
    and error = function
      | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let length fd =
    let rec process () =
      Lwt_unix.LargeFile.fstat fd >>= fun st ->
      Lwt.return st.Unix.LargeFile.st_size
    and error = function
      | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let list root =
    let res =
      let open Rresult in
      Bos.OS.Dir.contents ~dotfiles:false ~rel:true root
      >>| List.filter (Fpath.has_ext "pack")
    in
    match res with
    | Ok lst -> Lwt.return lst
    | Error (`Msg err) ->
        Log.warn (fun m -> m "Major.list: %s" err);
        Lwt.return []

  let reset root =
    list root >>= fun lst ->
    let rec f path =
      Lwt.catch
        (fun () ->
          Lwt_unix.unlink (Fpath.to_string path) >>= fun () ->
          Lwt_unix.unlink (Fpath.to_string (Fpath.set_ext "idx" path)))
        (function
          | Unix.Unix_error (Unix.EINTR, _, _) -> f path
          | _exn -> Lwt.return_unit)
    in
    Lwt_list.iter_p f lst >>= Lwt.return_ok

  let move root ~src ~dst =
    let src = Fpath.(root // src) in
    let dst = Fpath.(root // dst) in
    Lwt_unix.rename (Fpath.to_string src) (Fpath.to_string dst) >>= fun () ->
    Lwt.return_ok ()

  let append : t -> [> `Wr ] fd -> string -> unit fiber =
   fun _ fd str ->
    let rec go (off, len) =
      Lwt_unix.write_string fd str off len >>= fun len' ->
      if len = len' then Lwt.return () else go (off + len', len - len')
    in
    go (0, String.length str)
end

module Unix = struct
  include Unix

  let mkdir ?(path = true) ?(mode = 0o755) dir =
    let rec exists dir =
      Lwt.catch
        (fun () ->
          Lwt_unix.stat (Fpath.to_string dir) >>= fun stat ->
          Lwt.return_ok (stat.Unix.st_kind = Unix.S_DIR))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok false
          | Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
          | exn -> Lwt.fail exn)
    in
    let rec mkdir d mode =
      Lwt.catch
        (fun () -> Lwt_unix.mkdir (Fpath.to_string d) mode >>= Lwt.return_ok)
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_ok ()
          | Unix.Unix_error (Unix.EINTR, _, _) -> mkdir d mode
          | Unix.Unix_error (e, _, _) ->
              if d = dir then
                Lwt.return_error
                @@ Rresult.R.msgf "create directory %a: %s" Fpath.pp d
                     (Unix.error_message e)
              else
                Lwt.return_error
                @@ Rresult.R.msgf "create directory %a: %a: %s" Fpath.pp dir
                     Fpath.pp d (Unix.error_message e)
          | exn -> Lwt.fail exn)
    in
    exists dir >>? function
    | true -> Lwt.return_ok false
    | false -> (
        match path with
        | false -> mkdir dir mode >>? fun () -> Lwt.return_ok false
        | true ->
            let rec dirs_to_create p acc =
              exists p >>? function
              | true -> Lwt.return_ok acc
              | false -> dirs_to_create (Fpath.parent p) (p :: acc)
            in
            let rec create_them dirs () =
              match dirs with
              | dir :: dirs -> mkdir dir mode >>? create_them dirs
              | [] -> Lwt.return_ok ()
            in
            dirs_to_create dir [] >>? fun dirs ->
            create_them dirs () >>? fun () -> Lwt.return_ok true )
end

module Reference_heap = struct
  let src =
    Logs.Src.create "git-unix.reference"
      ~doc:"logs git-unix's reference heap event"

  module Log = (val Logs.src_log src : Logs.LOG)

  type +'a fiber = 'a

  (* XXX(dinosaure): ensure the atomicity. *)

  type t = Fpath.t (* [.git] *)

  type error = [ `Not_found of Git.Reference.t | `Msg of string ]

  let pp_error ppf = function
    | `Not_found refname -> Fmt.pf ppf "%a not found" Git.Reference.pp refname
    | `Msg err -> Fmt.string ppf err

  let atomic_wr root refname str =
    Log.debug (fun m -> m "Writing %a: %S." Git.Reference.pp refname str);
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    let base, _ = Fpath.split_base path in
    let open Rresult in
    Bos.OS.Dir.create ~path:true base >>= fun _ ->
    Bos.OS.Dir.exists path >>= fun res ->
    ( if res then Bos.OS.Dir.delete ~must_exist:false ~recurse:true path
    else R.ok () )
    >>= fun () ->
    Bos.OS.File.tmp "git-reference-%s" >>= fun src ->
    Bos.OS.File.write src str >>= fun () ->
    let fd = Unix.openfile (Fpath.to_string src) Unix.[ O_WRONLY ] 0o644 in
    Unix.close fd;
    Unix.rename (Fpath.to_string src) (Fpath.to_string path);
    R.ok ()

  let atomic_rd root refname =
    Log.debug (fun m -> m "Reading %a." Git.Reference.pp refname);
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    let open Rresult in
    Bos.OS.File.exists path >>= function
    | true ->
        let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
        let { Unix.st_size; _ } = Unix.fstat fd in
        let rs = Bytes.create st_size in
        let ln = Unix.read fd rs 0 st_size in
        assert (ln = st_size);
        Unix.close fd;
        R.ok (Bytes.unsafe_to_string rs)
    | false -> R.error (`Not_found refname)

  let atomic_rm root refname =
    Log.debug (fun m -> m "Deleting %a." Git.Reference.pp refname);
    let path = List.fold_left Fpath.add_seg root (Git.Reference.segs refname) in
    Bos.OS.File.delete path

  let list root =
    let f x r =
      match Fpath.rem_prefix root x with
      | Some x -> (
          Log.debug (fun l -> l "%a exists into the store." Fpath.pp x);
          match Git.Reference.of_string (Fpath.to_string x) with
          | Ok x -> x :: r
          | Error _ -> r )
      | None -> assert false
      (* XXX(dinosaure): see [elements]. *)
    in
    let elements path =
      match Option.map Fpath.segs (Fpath.rem_prefix root path) with
      | Some ("objects" :: _) -> Ok false
      | Some [ "HEAD" ] -> Bos.OS.File.exists path
      | Some ("refs" :: _) -> Bos.OS.File.exists path
      | _ -> Ok false
    in
    Log.debug (fun l -> l "Listing references into %a." Fpath.pp root);
    match
      Bos.OS.Dir.fold_contents ~dotfiles:false ~elements:(`Sat elements) f []
        root
    with
    | Ok lst -> lst
    | Error (`Msg err) ->
        Log.warn (fun m -> m "error when we listing references: %s" err);
        []

  let reset root =
    let open Rresult in
    let lst = list root in
    let f refname =
      let path =
        List.fold_left Fpath.add_seg root (Git.Reference.segs refname)
      in
      match Bos.OS.Path.delete path with
      | Ok () -> ()
      | Error (`Msg err) ->
          Log.warn (fun m ->
              m "error when we deleting %a: %s" Fpath.pp path err)
    in
    List.iter f lst;
    R.ok ()
end

module Make (Digestif : Digestif.S) = struct
  module Mn = Minor_heap (Digestif)
  include Git.Store.Make (Digestif) (Mn) (Major_heap) (Reference_heap)

  let major_uid =
    {
      Git.Store.pck_major_uid_of_uid =
        (fun root uid ->
          Fpath.(root / Fmt.strf "pack-%s.pack" (Digestif.to_hex uid)));
      Git.Store.idx_major_uid_of_uid =
        (fun root uid ->
          Fpath.(root / Fmt.strf "pack-%s.idx" (Digestif.to_hex uid)));
      Git.Store.uid_of_major_uid =
        (fun path ->
          let str = Fpath.basename (Fpath.rem_ext path) in
          match Astring.String.cut ~sep:"pack-" str with
          | Some ("", uid) -> Digestif.of_hex uid
          | _ -> Fmt.invalid_arg "Invalid major uniq ID: %a" Fpath.pp path);
    }

  let v ?dotgit root =
    let dotgit =
      match dotgit with Some v -> v | None -> Fpath.(root / ".git")
    in
    let packed = Packed_refs.load ~of_hex:Hash.of_hex dotgit in
    let minor = Fpath.(dotgit / "objects") in
    let major = Fpath.(dotgit / "objects" / "pack") in
    let temp = Fpath.(dotgit / "tmp") in
    let refs = dotgit in
    Bos.OS.Dir.set_default_tmp temp;
    Unix.mkdir ~path:true temp >>? fun _ ->
    Unix.mkdir ~path:true refs >>? fun _ ->
    Unix.mkdir ~path:true minor >>? fun _ ->
    Unix.mkdir ~path:true major >>? fun _ ->
    let open Lwt.Infix in
    (* TODO(dinosaure): [stat] directories. *)
    v ~dotgit ~minor ~major ~major_uid ~refs ~packed root >>= fun x ->
    Lwt.return_ok x
end

module Store = Make (Digestif.SHA1)

module Sync (Git_store : Git.S) (HTTP : Smart_git.HTTP) = struct
  let src = Logs.Src.create "git-unix.sync" ~doc:"logs git-unix's sync event"

  module Log = (val Logs.src_log src : Logs.LOG)

  include Git.Sync.Make (Git_store.Hash) (Major_heap) (Major_heap) (Conduit_lwt)
            (Git_store)
            (HTTP)

  let random_gen = lazy (Random.State.make_self_init ())

  let random_path pat =
    let rand = Random.State.bits (Lazy.force random_gen) land 0xFFFFFF in
    Fpath.v (Fmt.strf pat (Fmt.strf "%06x" rand))

  let failwithf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

  let create_tmp_path mode dir pat =
    let rec loop count =
      if count < 0 then
        failwithf "Create a temporary file %s in %a: too many failing attempts"
          (Fmt.strf pat "XXXXXX") Fpath.pp dir
      else
        let file = random_path pat in
        let sfile = Fpath.to_string Fpath.(dir // file) in
        let open_flags = Unix.[ O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE ] in
        let process () =
          Lwt_unix.openfile sfile open_flags mode >>= fun fd ->
          Lwt.return (file, fd)
        in
        let error = function
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (pred count)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
          | exn -> Lwt.fail exn
        in
        Lwt.catch process error
    in
    loop 10000

  let tmp ?(mode = 0o600) dir pat =
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    Lwt_unix.close fd >>= fun () -> Lwt.return file

  let stream_of_file ?(chunk = De.io_buffer_size) path =
    let stream, emitter = Lwt_stream.create () in
    let fill () =
      Lwt_unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644
      >>= fun fd ->
      let rec go () =
        let tmp = Bytes.create chunk in
        Lwt.catch
          (fun () ->
            Lwt_unix.read fd tmp 0 chunk >>= function
            | 0 ->
                emitter None;
                Lwt_unix.close fd
            | len ->
                emitter (Some (Bytes.sub_string tmp 0 len));
                go ())
          (fun _exn ->
            emitter None;
            Lwt_unix.close fd)
      in
      go ()
    in
    Lwt.async fill;
    fun () -> Lwt_stream.get stream

  let fetch ~resolvers edn store ?version ?capabilities want =
    let dotgit = Git_store.dotgit store in
    let temp = Fpath.(dotgit / "tmp") in
    tmp temp "pack-%s.pack" >>= fun src ->
    tmp temp "pack-%s.pack" >>= fun dst ->
    tmp temp "pack-%s.idx" >>= fun idx ->
    fetch ~resolvers edn store ?version ?capabilities want ~src ~dst ~idx temp
      temp
    >>? function
    | `Empty -> Lwt.return_ok None
    | `Pack (hash, refs) ->
        let pck = stream_of_file dst in
        let idx = stream_of_file idx in
        Git_store.batch_write store hash ~pck ~idx
        >|= Rresult.R.reword_error (fun err -> `Store err)
        >>? fun () ->
        let update (refname, hash) =
          Git_store.Ref.write store refname (Git.Reference.Uid hash)
          >>= function
          | Ok v -> Lwt.return v
          | Error err ->
              Log.warn (fun m ->
                  m "Impossible to update %a to %a: %a." Git.Reference.pp
                    refname Git_store.Hash.pp hash Git_store.pp_error err);
              Lwt.return_unit
        in
        Lwt_list.iter_p update refs >>= fun () ->
        Lwt.return_ok (Some (hash, refs))
end
