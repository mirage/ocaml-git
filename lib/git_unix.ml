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
      Log.debugf "Executing %s" (String.concat " " (Array.to_list cmd));
      let env = Unix.environment () in
      let p = Lwt_process.open_process_full ~env ("ssh", cmd) in
      Lwt.finalize
        (fun () -> fn (p#stdout, p#stdin))
        (fun () -> let _ = p#close in return_unit)
    | Some "git" ->
      Log.debugf "Connecting to %s" (Uri.to_string uri);
      let resolver = Resolver_lwt_unix.system in
      Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
      let ctx = Conduit_lwt_unix.default_ctx in
      Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
      Conduit_lwt_unix.connect ~ctx client >>= fun (flow, ic, oc) ->
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
    let len = 1024 in
    let buf = String.create len in
    let res = Buffer.create 1024 in
    let rec aux () =
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return (Buffer.contents res)
      | i -> Buffer.add_substring res buf 0 i; aux () in
    aux ()

  let read_exactly ic n =
    let res = String.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    return res

end

module D = struct

  module Read_file_cache : sig
    val find : string -> Cstruct.t option
    val add : string -> Cstruct.t -> unit
  end = struct

    (* Search key and value stored in the weak table.
       The path is used to find the file.
       When searching, file is a dummy empty value.
       This value should be alive as long as the file
       is alive, to ensure that, a finaliser is attached
       to the file referencing its key to maintain it alive.
       Notice that the key don't maintain the file alive to
       avoid making both values always reachable.
    *)
    type key =
      { path : string;
        file : Cstruct.t Weak.t }

    module WeakTbl = Weak.Make(struct
        type t = key
        let hash t = Hashtbl.hash t.path
        let equal t1 t2 = t1.path = t2.path
      end)

    let cache = WeakTbl.create 10

    let dummy = Weak.create 0 (* only used to create a search key *)

    let find path =
      try
        let search_key = { path; file = dummy } in
        let cached_value = WeakTbl.find cache search_key in
        match Weak.get cached_value.file 0 with
        | None -> WeakTbl.remove cache cached_value; None
        | Some f -> Some f
      with Not_found -> None

    let add path file =
      let w = Weak.create 1 in
      Weak.set w 0 (Some file);
      let v = { path; file = w } in
      Gc.finalise (fun _ -> Weak.set v.file 0 None) file;
      (* Maintain v alive while file is alive by forcing v to be
         present in the function closure. The effect is useless, but
         it ensures that the compiler won't optimise the refence to
         v away. This is guaranteed to work as long as the compiler
         don't have a deep knowledge of Weak.set behaviour.
         Maybe some kind of "ignore" external function would be better.
      *)
      WeakTbl.add cache v

  end

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir then return_unit
      else (
        aux (Filename.dirname dir) >>= fun () ->
        Log.debugf "mkdir %s" dir;
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

  let write_string fd b =
    let rec rwrite fd buf ofs len =
      Lwt_unix.write fd buf ofs len >>= fun n ->
      if len = 0 then fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else return_unit in
    match String.length b with
    | 0   -> return_unit
    | len -> rwrite fd b 0 len

  let with_write_file file fn =
    Log.infof "Writing %s" file;
    mkdir (Filename.dirname file) >>= fun () ->
    Lwt_pool.use openfile_pool (fun () ->
        Lwt_unix.(openfile file [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () -> fn fd)
          (fun _  -> Lwt_unix.close fd))

  let write_file file b =
    with_write_file file (fun fd -> write_cstruct fd b)

  let read_file file =
    Log.infof "Reading %s" file;
    match Read_file_cache.find file with
    | Some v -> Lwt.return v
    | None ->
      Unix.handle_unix_error (fun () ->
          Lwt_pool.use openfile_pool (fun () ->
              let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
              let ba = Lwt_bytes.map_file ~fd ~shared:false () in
              Unix.close fd;
              let cs = Cstruct.of_bigarray ba in
              Read_file_cache.add file cs;
              return cs
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
