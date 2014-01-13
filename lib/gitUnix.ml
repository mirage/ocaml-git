(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module M = struct
  type ic = Lwt_io.input_channel

  type oc = Lwt_io.output_channel

  let with_connection address port fn =
    let port = match port with
      | None   -> 9418
      | Some p -> p in
    Lwt_unix.gethostbyname address >>= fun host ->
    let inet_addr = host.Unix.h_addr_list.(0) in
    let sockaddr = Unix.ADDR_INET (inet_addr, port) in
    Lwt_io.with_connection sockaddr fn

  let read_all ic =
    let len = 1024 in
    let buf = String.create len in
    let res = Buffer.create 1024 in
    let rec aux () =
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return (Buffer.contents res)
      | i -> Buffer.add_substring res buf 0 i; aux () in
    aux ()

  let flush oc =
    Lwt_io.flush oc

  let read_exactly ic n =
    let res = String.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    return res

  let write oc s =
    Lwt_io.write oc s

  let mstruct_of_file file =
    let open Lwt in
    let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
    let ba = Lwt_bytes.map_file ~fd ~shared:false () in
    Unix.close fd;
    return (Mstruct.of_bigarray ba)

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir then return_unit
      else (
        aux (Filename.dirname dir) >>= fun () ->
        Log.debugf "mkdir %s" dir;
        catch
          (fun () -> Lwt_unix.mkdir dir 0o755)
          (fun _ -> return_unit)
      ) in
    aux dirname

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

end

module Remote = GitRemote.Make(M)

include M
