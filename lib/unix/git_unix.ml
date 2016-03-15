(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Git

module Log = Log.Make(struct let section = "unix" end)

(* Pool of opened files *)
let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

module IO_Sync = struct

  type ctx = unit

  type ic = Lwt_io.input_channel

  type oc = Lwt_io.output_channel

  let write oc s =
    Lwt_io.write oc s

  let flush oc =
    Lwt_io.flush oc

  let with_ssh_process ?init uri fn =
    let host = match Uri.host uri with
      | None   -> "localhost"
      | Some x -> x
    in
    let user = match Uri.userinfo uri with
      | None   -> ""
      | Some u -> u ^ "@"
    in
    let cmd = match init with
      | None   -> [| "ssh"; user ^ host; |]
      | Some x -> [| "ssh"; user ^ host; x |]
    in
    Log.info "Executing '%s'" (String.concat " " (Array.to_list cmd));
    let env = Unix.environment () in
    let p = Lwt_process.open_process_full ~env ("ssh", cmd) in
    Lwt.finalize
      (fun () -> fn (p#stdout, p#stdin))
      (fun () -> let _ = p#close in Lwt.return_unit)

  let with_conduit ?init uri fn =
    Log.debug "Connecting to %s" (Uri.to_string uri);
    let resolver = Resolver_lwt_unix.system in
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
    let ctx = Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
    Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, ic, oc) ->
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun ()  ->
         Lwt.catch
           (fun () -> Lwt_io.close ic)
           (function
             | Unix.Unix_error _ -> Lwt.return_unit
             | e -> Lwt.fail e))

  module IC = struct
    type t = Lwt_io.input_channel
    let make ?close perform_io =
      let perform_io buf = perform_io (Cstruct.of_bigarray buf) in
      Lwt_io.make ~mode:Lwt_io.Input ?close perform_io
  end

  module OC = struct
    type t = Lwt_io.output_channel
    let make ?close perform_io =
      let perform_io buf = perform_io (Cstruct.of_bigarray buf) in
      Lwt_io.make ~mode:Lwt_io.Output ?close perform_io
  end

  module Client = struct
    (* FIXME in cohttp *)
    module IO = Cohttp_lwt_unix_io
    let close_in x = Lwt.ignore_result (Lwt_io.close x)
    let close_out x = Lwt.ignore_result (Lwt_io.close x)
  end

  module HTTP = Git_http.Flow(Client)(IC)(OC)

  let with_connection ?ctx:_ uri ?init fn =
    match Sync.protocol uri with
    | `Ok `SSH -> with_ssh_process ?init uri fn
    | `Ok `Git -> with_conduit ?init uri fn
    | `Ok `Smart_HTTP -> HTTP.with_http ?init (with_conduit ?init:None) uri fn
    | `Not_supported x ->
      Lwt.fail (Failure ("Scheme " ^ x ^ " not supported yet"))
    | `Unknown ->
      Lwt.fail (Failure ("Unknown protocol. Must supply a scheme like git://"))

  let read_all ic =
    let len = 4 * 4096 in
    let return l = Lwt.return (List.rev l) in
    let rec aux acc =
      let buf = Bytes.create len in
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return acc
      | i ->
        let buf = Bytes.sub buf 0 i in
        if len = i then return (buf :: acc)
        else aux (buf :: acc)
    in
    aux []

  let read_exactly ic n =
    let res = Bytes.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    Lwt.return res

end

module IO_FS = struct

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
    | e -> Lwt.fail e

  let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn
  let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

  let remove_file f = safe Lwt_unix.unlink f

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            Log.debug "%s already exists but is a file, removing." dir;
            remove_file dir;
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug "mkdir %s" dir;
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let list_files kind dir =
    Lwt_pool.use openfile_pool (fun () ->
        if Sys.file_exists dir then (
          let s = Lwt_unix.files_of_directory dir in
          let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
          let s = Lwt_stream.map (Filename.concat dir) s in
          let s = Lwt_stream.filter kind s in
          Lwt_stream.to_list s >>= fun l ->
          Lwt.return l
        ) else
          Lwt.return_nil
      )

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let rename =
    if Sys.os_type <> "Win32" then Lwt_unix.rename
    else
      fun tmp file ->
        let delays = [| 0.; 1.; 10.; 20.; 40. |] in
        let rec aux i =
          Lwt.catch
            (fun () -> Lwt_unix.rename tmp file)
            (function
              | Unix.Unix_error (Unix.EACCES, _, _) as e ->
                if i >= Array.length delays then Lwt.fail e
                else Lwt_unix.sleep delays.(i) >>= fun () -> aux (i+1)
              | e -> Lwt.fail e)
        in
        aux 0

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Log.info "Writing %s (%s)" file tmp;
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644)
        >>= fun fd ->
        Lwt.finalize (fun () -> protect fn fd) (fun () -> Lwt_unix.close fd)
        >>= fun () ->
        rename tmp file
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

  (* [read_into ~chunk_size ~off buf ch] reads from [ch] into [buf] until
     either [buf] is full or [ch] is exhausted. It returns the
     subset of [buf] that was filled. *)
  let read_into ~chunk_size buf ch =
    let data = Bytes.create chunk_size in
    let rec aux off =
      match Cstruct.len buf - off with
      | 0     -> Lwt.return buf   (* Buffer full *)
      | avail ->
        Lwt_io.read_into ch data 0 (min chunk_size avail) >>= fun read ->
        Cstruct.blit_from_string data 0 buf off read;
        aux (off + read)
    in
    aux 0

  let read_file file =
    Lwt_pool.use openfile_pool (fun () ->
        Log.info "Reading %s" file;
        Lwt_unix.stat file >>= fun stats ->
        (* There are really too many buffers here. First we copy from the FS to the Lwt_io buffer,
           then from there into our own string buffer, then blit from there into a Cstruct. *)
        let chunk_size = max 4096 (min stats.Lwt_unix.st_size 0x100000) in
        let lwt_buffer = Lwt_bytes.create chunk_size in
        Lwt_io.(with_file ~buffer:lwt_buffer ~mode:input) ~flags:[Unix.O_RDONLY] file (fun ch ->
          let buf = Cstruct.create stats.Lwt_unix.st_size in
          read_into ~chunk_size buf ch
        )
      )

  let realdir dir =
    if Sys.file_exists dir && Sys.is_directory dir then (
      let d = Sys.getcwd () in
      Unix.chdir dir;
      let e = Sys.getcwd () in
      Sys.chdir d;
      e
    ) else dir

  (* FIXME: this is crazy *)
  let realpath file =
    let rec aux file =
      if Sys.file_exists file && Sys.is_directory file then
        realdir file
      else
        let dirname = Filename.dirname file in
        let basename = Filename.basename file in
        Filename.concat (aux dirname) basename
    in
    Lwt.return (aux file)

  let stat_info path =
    let open Index in
    let stats = Unix.stat path in
    let ctime = { lsb32 = Int32.of_float stats.Unix.st_ctime; nsec = 0l } in
    let mtime = { lsb32 = Int32.of_float stats.Unix.st_mtime; nsec = 0l } in
    let dev = Int32.of_int stats.Unix.st_dev in
    let inode = Int32.of_int stats.Unix.st_ino in
    let mode = match stats.Unix.st_kind, stats.Unix.st_perm with
      | Unix.S_REG, p -> if p land 0o100 = 0o100 then `Exec else `Normal
      | Unix.S_LNK, _ -> `Link
      | k, p ->
        let kind = match k with
          | Unix.S_REG -> "REG"
          | Unix.S_DIR -> "DIR"
          | Unix.S_CHR -> "CHR"
          | Unix.S_BLK -> "BLK"
          | Unix.S_LNK -> "LNK"
          | Unix.S_FIFO -> "FIFO"
          | Unix.S_SOCK -> "SOCK"
        in
        let perm = Printf.sprintf "%o" p in
        let error =
          Printf.sprintf "%s: not supported kind of file [%s, %s]."
            path kind perm
        in
        failwith error
    in
    let uid = Int32.of_int stats.Unix.st_uid in
    let gid = Int32.of_int stats.Unix.st_gid in
    let size = Int32.of_int stats.Unix.st_size in
    { ctime; mtime; dev; inode; uid; gid; mode; size }

  let file_exists f =
    Lwt.return (Sys.file_exists f)

  let rm_command =
    if Sys.os_type = "Win32" then
      "cmd /d /v:off /c rd /s /q"
    else
      "rm -rf"

  let remove f =
    if Sys.file_exists f && not (Sys.is_directory f) then remove_file f
    else if not (Sys.file_exists f) then Lwt.return_unit
    else
      (* FIXME: eeek *)
      let i = Sys.command (Printf.sprintf "%s %s" rm_command f) in
      if i = 0 then Lwt.return_unit else Lwt.fail (Failure ("Cannot remove " ^ f))

  let chmod f i =
    Lwt.return (Unix.chmod f i)

  let getcwd () =
    Lwt.return (Sys.getcwd ())

end

module Zlib = Git.Inflate.Make(Zlib)

module SHA1 = struct

  let cstruct buf =
    buf
    |> Nocrypto.Hash.SHA1.digest
    |> Cstruct.to_string
    |> fun x -> Hash.of_raw x

  let string str =
    Cstruct.of_string str
    |> cstruct

  let length = Nocrypto.Hash.SHA1.digest_size

end

module SHA256 = struct

  let cstruct buf =
    buf
    |> Nocrypto.Hash.SHA256.digest
    |> Cstruct.to_string
    |> fun x -> Hash.of_raw x

  let string str =
    Cstruct.of_string str
    |> cstruct

  let length = Nocrypto.Hash.SHA256.digest_size

end

module Make (D: Git.Hash.DIGEST) (I: Git.Inflate.S) = struct

  module Sync = struct
    module IO = IO_Sync
    module Result = Sync.Result
    module Make = Sync.Make(IO)
  end

  module FS = struct
    module IO = IO_FS
    include Git.FS.Make(IO_FS)(D)(I)
  end

  module Memory = Git.Memory.Make(D)(I)

  module Hash_IO = Git.Hash.IO(D)
  module Value_IO = Value.IO(D)(I)
  module Pack_IO = Git.Pack.IO(D)(I)
  module Index_IO = Git.Index.IO(D)

end

module M = Make(SHA1)(Zlib)
include M

module type S = sig
  module Sync: sig
    module IO: Git.Sync.IO
    module Result: (module type of Sync.Result
                     with type fetch = Sync.Result.fetch
                      and type push  = Sync.Result.push)
    module Make (S: Git.Store.S): Git.Sync.S with type t = S.t
  end
  module FS: sig
    module IO: Git.FS.IO
    include Git.FS.S
  end
  module Memory: Store.S

  module Hash_IO: Git.Hash.IO
  module Value_IO: Git.Value.IO
  module Pack_IO: Git.Pack.IO
  module Index_IO: Git.Index.IO

end
