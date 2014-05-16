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
open Misc.OP

type t = string

let root t = t

let create ?root () =
  match root with
  | None   -> return (Sys.getcwd ())
  | Some r ->
    Git_unix.mkdir r >>= fun () ->
    return (Git_unix.realpath r)

let clear t =
  Log.infof "clear %s" t;
  let cmd = Printf.sprintf "rm -rf %s/.git" t in
  let _ = Sys.command cmd in
  return ()

(* Loose objects *)
module Loose = struct

  module Log = Log.Make(struct let section = "fs-loose" end)

  let file root sha1 =
    let hex = SHA1.to_hex sha1 in
    let prefix = String.sub hex 0 2 in
    let suffix = String.sub hex 2 (String.length hex - 2) in
    root / ".git" / "objects" / prefix / suffix

  let mem root sha1 =
    return (Sys.file_exists (file root sha1))

  let read t sha1 =
    Log.debugf "read %s" (SHA1.to_hex sha1);
    let file = file t sha1 in
    if Sys.file_exists file then (
      let buf = Git_unix.read_file file in
      try
        let value = Value.input (Mstruct.of_bigarray buf) in
        return (Some value)
      with Zlib.Error _ ->
        fail (Zlib.Error (file, (Bigstring.to_string buf)))
    ) else
      return_none

  let write t value =
    Log.debugf "write";
    let inflated = Misc.with_buffer (fun buf -> Value.add_inflated buf value) in
    let sha1 = SHA1.create inflated in
    let file = file t sha1 in
    if Sys.file_exists file then
      return sha1
    else (
      let deflated = Misc.deflate_bigstring (Bigstring.of_string inflated) in
      Git_unix.write_file file deflated >>= fun () ->
      return sha1
    )

  let list root =
    Log.debugf "Loose.list %s" root;
    let objects = root / ".git" / "objects" in
    Git_unix.directories objects >>= fun objects ->
    let objects = List.map ~f:Filename.basename objects in
    let objects = List.filter ~f:(fun s -> (s <> "info") && (s <> "pack")) objects in
    Lwt_list.map_s (fun prefix ->
        let dir = root / ".git" / "objects" / prefix in
        Git_unix.files dir >>= fun suffixes ->
        let suffixes = List.map ~f:Filename.basename suffixes in
        let objects = List.map ~f:(fun suffix ->
            SHA1.of_hex (prefix ^ suffix)
          ) suffixes in
        return objects
      ) objects
    >>= fun files ->
    return (List.concat files)

end

module Packed = struct

  module Log = Log.Make(struct let section = "fs-packed" end)

  let file root sha1 =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let pack_file = "pack-" ^ (SHA1.to_hex sha1) ^ ".pack" in
    pack_dir / pack_file

  let list root =
    Log.debugf "list %s" root;
    let packs = root / ".git" / "objects" / "pack" in
    Git_unix.files packs >>= fun packs ->
    let packs = List.map ~f:Filename.basename packs in
    let packs = List.filter ~f:(fun f -> Filename.check_suffix f ".idx") packs in
    let packs = List.map ~f:(fun f ->
        let p = Filename.chop_suffix f ".idx" in
        let p = String.sub p 5 (String.length p - 5) in
        SHA1.of_hex p
      ) packs in
    return packs

  let index root sha1 =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let idx_file = "pack-" ^ (SHA1.to_hex sha1) ^ ".idx" in
    pack_dir / idx_file

  let indexes = SHA1.Table.create ()

  let write_index t sha1 idx =
    let file = index t sha1 in
    if not (Hashtbl.mem indexes sha1) then Hashtbl.add_exn indexes ~key:sha1 ~data:idx;
    if Sys.file_exists file then
      return_unit
    else (
      let buf = Bigbuffer.create 1024 in
      Pack_index.add buf idx;
      Git_unix.write_file file (Misc.buffer_contents buf)
    )

  let read_index t sha1 =
    Log.debugf "read_index %s" (SHA1.to_hex sha1);
    match Hashtbl.find indexes sha1 with
    | Some i -> return i
    | None   ->
      let file = index t sha1 in
      if Sys.file_exists file then
        let buf = Git_unix.read_file file in
        let buf = Mstruct.of_bigarray buf in
        let index = Pack_index.input buf in
        Hashtbl.add_exn indexes ~key:sha1 ~data:index;
        return index
      else (
        Printf.eprintf "%s does not exist." file;
        fail (Failure "read_index")
      )

  let keys = SHA1.Table.create ()

  let read_keys t sha1 =
    Log.debugf "read_keys %s" (SHA1.to_hex sha1);
    match Hashtbl.find keys sha1 with
    | Some k   -> k
    | None     ->
      let data = match Hashtbl.find indexes sha1 with
        | Some i -> SHA1.Set.of_list (SHA1.Map.keys i.Pack_index.offsets)
        | None   ->
          let file = index t sha1 in
          if Sys.file_exists file then
            let buf = Git_unix.read_file file in
            Pack_index.keys (Mstruct.of_bigarray buf)
          else
            failwith "Git_fs.Packed.read_keys"
      in
      Hashtbl.add_exn keys ~key:sha1 ~data;
      data

  let packs = SHA1.Table.create ()

  let read_pack t sha1 =
    match Hashtbl.find packs sha1 with
    | Some pack -> return pack
    | None      ->
      let file = file t sha1 in
      if Sys.file_exists file then (
        let buf = Git_unix.read_file file in
        read_index t sha1 >>= fun index ->
        let pack = Pack.Raw.input (Mstruct.of_bigarray buf) ~index:(Some index) in
        let pack = Pack.to_pic pack in
        Hashtbl.add_exn packs ~key:sha1 ~data:pack;
        return pack
      ) else (
        Printf.eprintf "No file associated with the pack object %s.\n" (SHA1.to_hex sha1);
        fail (Failure "read_file")
      )

  let write_pack t sha1 pack =
    let file = file t sha1 in
    if not (Hashtbl.mem packs sha1) then  (
      let pack = Pack.to_pic pack in
      Hashtbl.add_exn packs ~key:sha1 ~data:pack;
    );
    if Sys.file_exists file then
      return_unit
    else (
      let pack = Misc.with_bigbuffer (fun buf -> Pack.Raw.add buf pack) in
      Git_unix.write_file file pack
    )

  let mem_in_pack t pack_sha1 sha1 =
    Log.debugf "mem_in_pack %s:%s"
      (SHA1.to_hex pack_sha1) (SHA1.to_hex sha1);
    let keys = read_keys t pack_sha1 in
    Set.mem keys sha1

  let read_in_pack t pack_sha1 sha1 =
    Log.debugf "read_in_pack %s:%s"
      (SHA1.to_hex pack_sha1) (SHA1.to_hex sha1);
    if not (mem_in_pack t pack_sha1 sha1) then
      return_none
    else (
      read_pack t pack_sha1 >>= fun pack ->
      return (Pack.read pack sha1)
    )

  let values = SHA1.Table.create ()

  let read t sha1 =
    match SHA1.Table.find values sha1 with
    | Some v -> return (Some v)
    | None   ->
      begin match Value.Cache.find sha1 with
        | Some str -> return (Some (Value.input_inflated (Mstruct.of_string str)))
        | None     ->
          list t >>= fun packs ->
          Lwt_list.fold_left_s (fun acc pack ->
              match acc with
              | Some v -> return (Some v)
              | None   -> read_in_pack t pack sha1
            ) None packs
      end >>= function
      | None   -> return_none
      | Some v ->
        ignore (SHA1.Table.add values ~key:sha1 ~data:v);
        return (Some v)

  let mem t sha1 =
    list t >>= fun packs ->
    let mem =
      List.fold_left ~f:(fun acc pack ->
          acc || mem_in_pack t pack sha1
        ) ~init:false packs in
    return mem

end

module Log = Log.Make(struct let section = "fs" end)

let list t =
  Log.debugf "list";
  Loose.list t  >>= fun objects ->
  Packed.list t >>= fun packs   ->
  Misc.list_map_p (fun p -> return (Packed.read_keys t p)) packs >>= fun keys ->
  let keys = SHA1.Set.(union (of_list objects) (union_list keys)) in
  let keys = SHA1.Set.to_list keys in
  return keys

let read t sha1 =
  Log.debugf "read %s" (SHA1.to_hex sha1);
  Loose.read t sha1 >>= function
  | Some v -> return (Some v)
  | None   -> Packed.read t sha1

let read = Memo.general read

let read_exn t sha1 =
  read t sha1 >>= function
  | Some v -> return v
  | None   ->
    Log.debugf "read_exn: Cannot read %s" (SHA1.to_hex sha1);
    fail Not_found

let mem t sha1 =
  Loose.mem t sha1 >>= function
  | true  -> return true
  | false -> Packed.mem t sha1

let contents t =
  Log.debugf "contents";
  list t >>= fun sha1s ->
  Misc.list_map_p (fun sha1 ->
      read_exn t sha1 >>= fun value ->
      return (sha1, value)
    ) sha1s

let dump t =
  contents t >>= fun contents ->
  List.iter ~f:(fun (sha1, value) ->
      let typ = Value.type_of value in
      Printf.eprintf "%s %s\n" (SHA1.to_hex sha1) (Object_type.to_string typ);
    ) contents;
  return_unit

let references t =
  let refs = t / ".git" / "refs" in
  Git_unix.rec_files refs >>= fun files ->
  let n = String.length (t / ".git" / "") in
  let refs = List.map ~f:(fun file ->
      let ref = String.sub file n (String.length file - n) in
      Reference.of_string ref
    ) files in
  return refs

let file_of_ref t ref =
  t / ".git" / Reference.to_string ref

let mem_reference t ref =
  let file = file_of_ref t ref in
  return (Sys.file_exists file)

let remove_reference t ref =
  let file = file_of_ref t ref in
  catch
    (fun () -> Lwt_unix.unlink file)
    (fun _ -> return_unit)

let read_reference t ref =
  let file = file_of_ref t ref in
  Log.infof "Reading %s" file;
  if Sys.file_exists file then
    Lwt_io.(with_file ~mode:Input file read) >>= fun hex ->
    let hex = String.strip hex in
    return (Some (SHA1.Commit.of_hex hex))
  else
    return_none

let read_head t =
  let file = file_of_ref t Reference.head in
  Log.infof "Reading %s" file;
  if Sys.file_exists file then
    Lwt_io.(with_file ~mode:Input file read) >>= fun str ->
    let contents = match String.split ~on:' ' str with
      | [sha1]  -> Reference.SHA1 (SHA1.Commit.of_hex sha1)
      | [_;ref] -> Reference.Ref (Reference.of_string ref)
      | _       -> failwith (sprintf "read_head: %s is not a valid HEAD contents" str)
    in
    return (Some contents)
  else
    return None

let read_reference_exn t ref =
  read_reference t ref >>= function
  | Some s -> return s
  | None   ->
    Log.debugf "read_reference_exn: Cannot read %s" (Reference.to_string ref);
    fail Not_found

let write t value =
  Loose.write t value >>= fun sha1 ->
  Log.debugf "write -> %s" (SHA1.to_hex sha1);
  return sha1

let write_pack t pack =
  Log.debugf "write_pack";
  let sha1 = Pack.Raw.sha1 pack in
  let index = Pack.Raw.index pack in
  Packed.write_pack t sha1 pack   >>= fun () ->
  Packed.write_index t sha1 index >>= fun () ->
  return (Pack.Raw.keys pack)

let write_reference t ref sha1 =
  let file = t / ".git" / Reference.to_string ref in
  let contents = SHA1.Commit.to_hex sha1 in
  Git_unix.write_file_string file ~contents

let write_head t = function
  | Reference.SHA1 sha1 -> write_reference t Reference.head sha1
  | Reference.Ref ref   ->
    let file = t / ".git" / "HEAD" in
    let contents = sprintf "ref: %s" (Reference.to_string ref) in
    Git_unix.write_file_string file ~contents

(* XXX: do not load the blobs *)
let load_filesystem t commit =
  Log.debugf "load_filesystem %s" (SHA1.Commit.to_hex commit);
  let n = ref 0 in
  let rec aux (mode, sha1) =
    read_exn t sha1 >>= function
    | Value.Blob b   -> incr n; return (Lazy_trie.create ~value:(mode, b) ())
    | Value.Commit c -> aux (`Dir, SHA1.of_tree c.Commit.tree)
    | Value.Tag t    -> aux (mode, t.Tag.sha1)
    | Value.Tree t   ->
      Misc.list_map_p ~width:10 (fun e ->
          aux (e.Tree.perm, e.Tree.node) >>= fun t ->
          return (e.Tree.name, t)
        ) t
      >>= fun children ->
      let children = lazy children in
      return (Lazy_trie.create ~children ())
  in
  aux (`Dir, SHA1.of_commit commit) >>= fun t ->
  return (!n, t)

let iter_blobs t ~f ~init =
  load_filesystem t init >>= fun (n, trie) ->
  let i = ref 0 in
  Log.debugf "iter_blobs %s" (SHA1.Commit.to_hex init);
  Lazy_trie.fold (fun acc path (mode, blob) ->
      acc >>= fun () ->
      incr i;
      f (!i, n) (t :: path) mode blob
  ) trie return_unit

let create_file file mode blob =
  let blob = Blob.to_string blob in
  match mode with
  | `Link -> (* Lwt_unix.symlink file ??? *) failwith "TODO"
  | _     ->
    Git_unix.write_file_string file ~contents:blob >>= fun () ->
    match mode with
    | `Exec -> Lwt_unix.chmod file 0o755
    | _     -> return_unit

let cache_file t =
  t / ".git" / "index"

let read_cache t =
  let buf = Git_unix.read_file (cache_file t) in
  let buf = Mstruct.of_bigarray buf in
  return (Cache.input buf)

let stat_info_of_file path =
  let open Cache in
  let open Unix in
  let stats = Unix.stat path in
  let ctime = { lsb32 = Int32.of_float stats.st_ctime; nsec = 0l } in
  let mtime = { lsb32 = Int32.of_float stats.st_mtime; nsec = 0l } in
  let dev = Int32.of_int_exn stats.st_dev in
  let inode = Int32.of_int_exn stats.st_ino in
  let mode = match stats.st_kind, stats.st_perm with
    | Unix.S_REG, 0o755 -> `Exec
    | Unix.S_REG, 0o644 -> `Normal
    | Unix.S_LNK, _     -> `Link
    | _ -> failwith (path ^ ": not supported kind of file.") in
  let uid = Int32.of_int_exn stats.st_uid in
  let gid = Int32.of_int_exn stats.st_gid in
  let size = Int32.of_int_exn stats.st_size in
  { ctime; mtime; dev; inode; uid; gid; mode; size }

let entry_of_file ?root file mode blob =
  Log.debugf "entry_of_file %s" file;
  begin
    if Sys.file_exists file then return_unit
    else create_file file mode blob
  end >>= fun () ->
  let root = match root with
    | None   -> Sys.getcwd ()
    | Some r -> Git_unix.realpath r in
  let file = Git_unix.realpath file in
  try
    let id = Value.sha1 (Value.Blob blob) in
    let stats = stat_info_of_file file in
    let stage = 0 in
    let name = String.chop_prefix_exn ~prefix:(root / "") file in
    let entry = { Cache.stats; id; stage; name } in
    return (Some entry)
  with Failure _ ->
    return_none

let write_cache t head =
  Log.debugf "write_cache %s" (SHA1.Commit.to_hex head);
  let entries = ref [] in
  let all = ref 0 in
  iter_blobs t ~init:head ~f:(fun (i,n) path mode blob ->
      all := n;
      printf "\rChecking out files: %d%% (%d/%d), done.%!" Int.(100*i/n) i n;
      let file = String.concat ~sep:Filename.dir_sep path in
      Log.debugf "write_cache: blob:%s" file;
      entry_of_file ~root:t file mode blob >>= function
      | None   -> return_unit
      | Some e -> entries := e :: !entries; return_unit
    ) >>= fun () ->
  let cache = { Cache.entries = !entries; extensions = [] } in
  let buf = Bigbuffer.create 1024 in
  Cache.add buf cache;
  Git_unix.write_file (cache_file t) (Misc.buffer_contents buf) >>= fun () ->
  printf "\rChecking out files: 100%% (%d/%d), done.%!\n" !all !all;
  return_unit
