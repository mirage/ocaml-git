(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Git
open Printf

module Log = Log.Make(struct let section = "unix" end)

(* Pool of opened files *)
let openfile_pool = Lwt_pool.create 200 (fun () -> return_unit)

let mkdir_pool = Lwt_pool.create 1 (fun () -> return_unit)

module M = struct
  type ic = Lwt_io.input_channel

  type oc = Lwt_io.output_channel

  let write oc s =
    Lwt_io.write oc s

  let flush oc =
    Lwt_io.flush oc

  let with_connection uri ?init fn =
    let host = match Uri.host uri with
      | None   -> "localhost"
      | Some x -> x in
    match Uri.scheme uri with
    | Some "git+ssh" ->
      let user = match Uri.userinfo uri with
        | None   -> ""
        | Some u -> u ^ "@" in
      let cmd = match init with
        | None   -> [| "ssh"; user ^ host; |]
        | Some x -> [| "ssh"; user ^ host; x |] in
      Log.debug "Executing %s" (String.concat " " (Array.to_list cmd));
      let env = Unix.environment () in
      let p = Lwt_process.open_process_full ~env ("ssh", cmd) in
      Lwt.finalize
        (fun () -> fn (p#stdout, p#stdin))
        (fun () -> let _ = p#close in return_unit)
    | Some "git" ->
      Log.debug "Connecting to %s" (Uri.to_string uri);
      let resolver = Resolver_lwt_unix.system in
      Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
      let ctx = Conduit_lwt_unix.default_ctx in
      Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
      Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, ic, oc) ->
      Lwt.finalize
        (fun () ->
           begin match init with
             | None   -> return_unit
             | Some s -> write oc s
           end >>= fun () ->
           fn (ic, oc))
        (fun ()  -> Lwt_io.close ic)
   | Some x ->
      (* XXX: make it work for smart-HTTP *)
      (* XXX: make it work over SSL *)
      fail (Failure ("Scheme " ^ x ^ " not supported yet"))
   | None -> fail (Failure ("Must supply a scheme like git://"))

  let read_all ic =
    let len = 64_1024 in
    let buf = Bytes.create len in
    let res = Buffer.create len in
    let rec aux () =
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return_unit
      | i -> Buffer.add_substring res buf 0 i;
        if len = i then return_unit
        else aux ()
    in
    aux () >>= fun () ->
    return (Buffer.contents res)

  let read_exactly ic n =
    let res = Bytes.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    return res

end

module D = struct

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir then return_unit
      else (
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug "mkdir %s" dir;
        Lwt_unix.mkdir dir 0o755
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let list_files kind dir =
    if Sys.file_exists dir then (
      let s = Lwt_unix.files_of_directory dir in
      let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
      let s = Lwt_stream.map (Filename.concat dir) s in
      let s = Lwt_stream.filter kind s in
      Lwt_stream.to_list s >>= fun l ->
      return l
    ) else
      return_nil

  let directories dir =
    list_files (fun f -> try Sys.is_directory f with _ -> false) dir

  let files dir =
    list_files (fun f -> try not (Sys.is_directory f) with _ -> false) dir

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else return_unit in
    match Cstruct.len b with
    | 0   -> return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let with_write_file file fn =
    let tmp = Filename.temp_file (Filename.basename file) "write" in
    Log.info "Writing %s (/tmp/%s)" file (Filename.basename tmp);
    mkdir (Filename.dirname file) >>= fun () ->
    Lwt_pool.use openfile_pool (fun () ->
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () -> fn fd >>= fun () -> Lwt_unix.rename tmp file)
          (fun _  -> Lwt_unix.close fd))

  let write_file file b =
    with_write_file file (fun fd -> write_cstruct fd b)

  let read_file file =
    Log.info "Reading %s" file;
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            return (Cstruct.of_bigarray ba)
          ))
      ()

  let realdir dir =
    if Sys.file_exists dir then (
      let d = Sys.getcwd () in
      Unix.chdir dir;
      let e = Sys.getcwd () in
      Sys.chdir d;
      e
    ) else dir

  let realpath file =
    let r =
      if Sys.is_directory file then realdir file
      else
        Filename.concat
          (realdir (Filename.dirname file))
          (Filename.basename file) in
    return r

  let stat_info path =
    let open Cache in
    let stats = Unix.stat path in
    let ctime = { lsb32 = Int32.of_float stats.Unix.st_ctime; nsec = 0l } in
    let mtime = { lsb32 = Int32.of_float stats.Unix.st_mtime; nsec = 0l } in
    let dev = Int32.of_int stats.Unix.st_dev in
    let inode = Int32.of_int stats.Unix.st_ino in
    let mode = match stats.Unix.st_kind, stats.Unix.st_perm with
      | Unix.S_REG, 0o755 -> `Exec
      | Unix.S_REG, 0o644 -> `Normal
      | Unix.S_LNK, _     -> `Link
      | _ -> failwith (path ^ ": not supported kind of file.") in
    let uid = Int32.of_int stats.Unix.st_uid in
    let gid = Int32.of_int stats.Unix.st_gid in
    let size = Int32.of_int stats.Unix.st_size in
    { ctime; mtime; dev; inode; uid; gid; mode; size }

  let file_exists f =
    return (Sys.file_exists f)

  let remove f =
    let _ = Sys.command (sprintf "rm -rf %s" f) in
    return_unit

  let chmod f i =
    return (Unix.chmod f i)

  let getcwd () =
    return (Sys.getcwd ())

end

module Sync = struct
  module IO = M
  module Result = Sync.Result
  module Make = Sync.Make(M)
end

module FS = struct
  module IO = D
  include Git.FS.Make(D)
end
