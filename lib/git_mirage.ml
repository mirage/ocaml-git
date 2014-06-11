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

open Core_kernel.Std
open Lwt
open Git

module Log = Log.Make(struct let section = "mirage" end)


module type FS = sig
  include V1_LWT.FS with type page_aligned_buffer = Cstruct.t
  val connect: unit -> [`Error of error | `Ok of t ] Lwt.t
  val string_of_error: error -> string
end


module FS (FS: FS) = struct

  let (>>|) x f =
    x >>= function
    | `Ok x    -> f x
    | `Error e ->
      let str = FS.string_of_error e in
      Log.errorf "%s" str;
      fail (Failure str)

  module M = struct

    let file_exists t f =
      Log.debugf "file_exists %S" f;
      FS.stat t f >>= function
      | `Ok _    -> return true
      | `Error e ->
        Log.errorf "%s" (FS.string_of_error e);
        return false

    let is_directory t dir =
      FS.stat t dir >>| fun s ->
      return s.FS.directory

    let parent_dir = function
      | "/"
      | "." -> None
      | s   -> Some (Filename.dirname s)

    let mkdir t dirname =
      Log.debugf "mkdir %s" dirname;
      let rec aux dir =
        file_exists t dir >>= function
        | true  -> return_unit
        | false ->
          match parent_dir dir with
          | None   -> return_unit
          | Some d ->
            aux d >>= fun () ->
            file_exists t dir >>= function
            | true  -> return_unit
            | false ->
              FS.mkdir t dir >>| fun () ->
              return_unit
      in
      aux dirname

    let list_files t kind dir =
      file_exists t dir >>= function
      | true ->
        FS.listdir t dir >>| fun l ->
        let l = List.filter ~f:(fun s -> s <> "." && s <> "..") l in
        let l = List.map ~f:(Filename.concat dir) l in
        Lwt_list.filter_s kind l
      | false ->
        return_nil

    let directories t dir =
      Log.debugf "directories %s" dir;
      list_files t (fun f -> catch
                       (fun () -> is_directory t f)
                       (fun _ -> return false)
                   ) dir

    let files t dir =
      Log.debugf "files %s" dir;
      list_files t (fun f -> catch
                       (fun () -> is_directory t f >>= fun b -> return (not b))
                       (fun _ -> return false)
                   ) dir

    let rec remove t dir =
      Log.debugf "remove %s" dir;
      let destroy dir =
        FS.destroy t dir >>| fun () ->
        return_unit in
      files t dir                   >>= fun ls ->
      Lwt_list.iter_s destroy ls    >>= fun () ->
      directories t dir             >>= fun ds ->
      Lwt_list.iter_s (remove t) ds >>= fun () ->
      destroy dir

    let rec_files t dir =
      Log.debugf "rec_files %s" dir;
      let rec aux accu dir =
        directories t dir >>= fun ds ->
        files t dir       >>= fun fs ->
        Lwt_list.fold_left_s aux (fs @ accu) ds in
      aux [] dir

    let read_file t file =
      Log.debugf "read_file %s" file;
      FS.stat t file >>| fun s ->
      FS.read t file 0 (Int64.to_int_exn s.FS.size) >>| fun bs ->
      let s = Cstruct.copyv bs in
      return (Bigstring.of_string s)

    let write_file t file b =
      Log.debugf "write_file %s %S" file (Bigstring.to_string b);
      mkdir t (Filename.dirname file) >>= fun () ->
      let c = Cstruct.of_bigarray b in
      FS.create t file    >>| fun () ->
      FS.write t file 0 c >>| fun () ->
      return_unit

    let getcwd () =
      return "/"

    let realdir dir =
      return dir

    let realpath file =
      realdir file

    let stat_info file =
      failwith "TODO"

    let chmod t file perm =
      return_unit

    let mutex = Lwt_mutex.create ()

    let connect fn =
      Lwt_mutex.with_lock mutex (fun () ->
          FS.connect () >>| fn
        )

    let mkdir dir =
      connect (fun t -> mkdir t dir)

    let remove file =
      connect (fun t -> remove t file)

    let file_exists file =
      connect (fun t -> file_exists t file)

    let directories dir =
      connect (fun t -> directories t dir)

    let files dir =
      connect (fun t -> files t dir)

    let rec_files dir =
      connect (fun t -> rec_files t dir)

    let read_file file =
      connect (fun t -> read_file t file)

    let write_file file buf =
      connect (fun t -> write_file t file buf)

    let chmod file perm =
      connect (fun t -> chmod t file perm)

  end

  include Git.FS.Make(M)

end
