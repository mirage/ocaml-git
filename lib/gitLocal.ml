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

open Core_kernel.Std
open Lwt

open GitMisc.OP
open GitTypes

module Log = Log.Make(struct let section = "local" end)

type t = string

let root t = t

let create ?root () =
  let t = match root with
  | None   -> Sys.getcwd ()
  | Some r -> if Filename.is_relative r then Sys.getcwd () / r else r
  in
  return t

(* Loose objects *)
module Loose = struct

  let file root sha1 =
    let hex = SHA1.to_hex sha1 in
    let prefix = String.sub hex 0 2 in
    let suffix = String.sub hex 2 (String.length hex - 2) in
    root / ".git" / "objects" / prefix / suffix

  let read_inflated t sha1 =
    let file = file t sha1 in
    if Sys.file_exists file then (
      Log.infof "Reading %s" file;
      let buf = GitUnix.read_file file in
      let buf = Mstruct.of_bigarray buf in
      let buf = GitMisc.inflate_mstruct buf in
      let buf = Mstruct.to_bigarray buf in
      return (Some buf)
    ) else
      return_none

  let unsafe_write_inflated t sha1 ~inflated ~file =
    GitUnix.mkdir (Filename.dirname file) >>= fun () ->
    let deflated = GitMisc.deflate_bigstring inflated in
    Log.infof "Writing %s" file;
    GitUnix.write_file file deflated >>= fun () ->
    return_unit

  let write_inflated t inflated =
    let sha1 = SHA1.create inflated in
    let file = file t sha1 in
    if Sys.file_exists file then
      return sha1
    else
      unsafe_write_inflated t sha1 ~inflated ~file >>= fun () ->
      return sha1

  let write_and_check_inflated t sha1 inflated =
    let file = file t sha1 in
    if Sys.file_exists file then
      return_unit
    else (
      let new_sha1 = SHA1.create inflated in
      if sha1 = new_sha1 then
        unsafe_write_inflated t sha1 ~inflated ~file
      else (
        eprintf "%S\n" (Bigstring.to_string inflated);
        eprintf "Marshaling error: expected:%s actual:%s\n"
          (SHA1.to_hex sha1) (SHA1.to_hex new_sha1);
        fail (Failure "build_packed_object")
      )
    )

  let write t value =
    write_inflated t (Git.output_inflated value)

  let write_and_check t sha1 value =
    write_and_check_inflated t sha1 (Git.output_inflated value)

  let list root =
    Log.debugf "Loose.list %s" root;
    let objects = root / ".git" / "objects" in
    GitUnix.directories objects >>= fun objects ->
    let objects = List.map ~f:Filename.basename objects in
    let objects = List.filter ~f:(fun s -> (s <> "info") && (s <> "pack")) objects in
    Lwt_list.fold_left_s (fun acc prefix ->
      let dir = root / ".git" / "objects" / prefix in
      GitUnix.files dir >>= fun suffixes ->
      let suffixes = List.map ~f:Filename.basename suffixes in
      let objects = List.map ~f:(fun suffix ->
          SHA1.of_hex (prefix ^ suffix)
        ) suffixes in
      return (objects @ acc)
    ) [] objects

end

module Packed = struct

  let file root sha1 =
    let pack_dir = root / ".git" / "objects" / "pack" in
    let pack_file = "pack-" ^ (SHA1.to_hex sha1) ^ ".pack" in
    pack_dir / pack_file

  let list root =
    Log.debugf "Packed.list %s" root;
    let packs = root / ".git" / "objects" / "pack" in
    GitUnix.files packs >>= fun packs ->
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

  let read_index t sha1 =
    let file = index t sha1 in
    if Sys.file_exists file then
      let buf = GitUnix.read_file file in
      let buf = Mstruct.of_bigarray buf in
      let index = Git.Pack_index.input buf in
      return index
    else (
      Printf.eprintf "%s does not exist." file;
      fail (Failure "read_index")
    )

  let read t sha1 =
    let file = file t sha1 in
    read_index t sha1 >>= fun index ->
    if Sys.file_exists file then (
      let buf = GitUnix.read_file file in
      return buf
    ) else (
      Printf.eprintf "No file associated with the pack object %s.\n" (SHA1.to_hex sha1);
      fail (Failure "read_file")
    )

  let write t sha1 pack =
    let file = file t sha1 in
    GitUnix.write_file file pack

  let write_index t sha1 idx =
    let file = index t sha1 in
    let buffers = Git.Pack_index.output idx in
    GitUnix.writev_file file buffers

  let rec read_inflated t (pack, index) sha1 =
    let offsets = index.Pack_index.offsets in
    let error offset =
      let offsets = Map.data offsets in
      let offsets = String.concat ~sep:"," (List.map ~f:string_of_int offsets) in
      Printf.eprintf "%d: invalid offset.\nValid offsets are: {%s}\n" offset offsets;
      failwith "invalid offset" in
    if Map.mem offsets sha1 then (
      let offset = Map.find_exn offsets sha1 in
      let packed_value = Git.Pack.read_packed_value pack index sha1 in
      let read_inflated sha1 =
        Loose.read_inflated t sha1 >>= function
        | Some buf -> return buf
        | None     -> read_inflated t (pack, index) sha1 in
      Git.Pack.unpack_inflated ~read_inflated ~index ~offset packed_value
    ) else
      error (-1)

  let read_inflated t sha1 =
    let rec find = function
      | []    -> return_none
      | n::ns ->
        read_index t n >>= fun index ->
        if Map.mem index.Pack_index.offsets sha1 then
          read t n                           >>= fun pack ->
          read_inflated t (pack, index) sha1 >>= fun v ->
          return (Some v)
        else
          find ns in
    list t >>=
    find

end

let list t =
  Log.debugf "list";
  Loose.list t  >>= fun objects ->
  Packed.list t >>= fun packs   ->
  (* Add cached objects *)
  Lwt_list.fold_left_s (fun acc p ->
      Packed.read_index t p >>= fun { Pack_index.offsets } ->
      return ((Map.keys offsets) @ acc)
    ) objects packs

let read_inflated t sha1 =
  let file = Loose.file t sha1 in
  if Sys.file_exists file
  then Loose.read_inflated t sha1
  else Packed.read_inflated t sha1

let read_inflated_exn t sha1 =
  read_inflated t sha1 >>= function
  | None   -> fail Not_found
  | Some b -> return b

let read t sha1 =
  read_inflated t sha1 >>= function
  | None     -> return_none
  | Some buf ->
    let buf = Mstruct.of_bigarray buf in
    return (Some (Git.input_inflated buf))

let read_exn t sha1 =
  read_inflated_exn t sha1 >>= fun buf ->
  let buf = Mstruct.of_bigarray buf in
  return (Git.input_inflated buf)

let mem t sha1 =
  read_inflated t sha1 >>= function
  | None   -> return false
  | Some _ -> return true

let type_of t sha1 =
  read_inflated t sha1 >>= function
  | None     -> return_none
  | Some buf ->
    let buf = Mstruct.of_bigarray buf in
    return (Some (Git.type_of_inflated buf))

let string_of_type_opt = function
  | Some `Blob   -> "Blob"
  | Some `Commit -> "Commit"
  | Some `Tag    -> "Tag"
  | Some `Tree   -> "Tree"
  | None         -> "Unknown"

let dump t =
  Log.debugf "dump";
  list t >>= fun sha1s ->
  Lwt_list.iter_s (fun sha1 ->
      type_of t sha1 >>= fun typ ->
      Printf.eprintf "%s %s\n"
        (SHA1.to_hex sha1)
        (string_of_type_opt typ);
      return_unit
    ) sha1s

let references t =
  let refs = t / ".git" / "refs" in
  GitUnix.rec_files refs >>= fun files ->
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

let read_reference_exn t ref =
  read_reference t ref >>= function
  | None   -> fail Not_found
  | Some s -> return s

let contents t =
  references t >>= fun refs ->
  Lwt_list.map_p (fun r ->
      read_reference_exn t r >>= fun c ->
      return (r, c)
    ) refs

let succ root sha1 =
  let commit c = `Commit (SHA1.of_commit c) in
  let tree l s = `Tree (l, SHA1.of_tree s) in
  let tag t = `Tag (t.Tag.tag, t.Tag.sha1) in
  read root sha1 >>= function
  | None                  -> return_nil
  | Some (Value.Blob _)   -> return_nil
  | Some (Value.Commit c) -> return (tree "" c.Commit.tree :: List.map ~f:commit c.Commit.parents)
  | Some (Value.Tag t)    -> return [tag t]
  | Some (Value.Tree t)   ->
    let t = Tree.entries t in
    return (List.map ~f:(fun e -> `Tree (e.Tree.name, e.Tree.node)) t)

let write t value =
  Log.debugf "write %s" (Value.to_string value);
  Loose.write t value

let write_and_check t sha1 value =
  Loose.write_and_check t sha1 value

let write_inflated t inflated =
  Loose.write_inflated t inflated

let write_and_check_inflated t sha1 inflated =
  Loose.write_and_check_inflated t sha1 inflated

let write_raw_pack t pack =
  let index = Git.Pack_index.of_raw_pack pack in
  let name =
    Map.keys index.Pack_index.offsets
    |> List.map ~f:SHA1.to_hex
    |> List.sort ~cmp:String.compare
    |> String.concat ~sep:""
    |> Bigstring.of_string
    |> SHA1.create in
  Packed.write t name pack >>= fun () ->
  Packed.write_index t name index >>= fun () ->
  return index

let write_pack t pack =
  let buffers = Git.Pack.output pack in
  let buf = GitMisc.bigstring_concat buffers in
  write_raw_pack t buf

let write_reference t ref sha1 =
  let file = t / ".git" / Reference.to_string ref in
  GitUnix.mkdir (Filename.dirname file) >>= fun () ->
  Log.infof "Writing %s" file;
  begin match Reference.to_string ref with
    | "HEAD" ->
      contents t >>= fun refs ->
      let ref =
        match List.find ~f:(fun (r, k) -> not (Reference.is_head r) && k = sha1) refs with
        | None       -> SHA1.Commit.to_hex sha1
        | Some (r,_) -> Printf.sprintf "ref: %s" (Reference.to_string r)
      in
      return ref
    | _      ->
      return (SHA1.Commit.to_hex sha1)
  end >>= fun contents ->
  GitUnix.write_file_string file ~contents

(* XXX: do not load the blobs *)
let load_filesystem t commit =
  let rec aux (mode, sha1): ('a, perm * blob) Lazy_trie.t option Lwt.t =
    read t sha1 >>= function
    | Some (Value.Blob b) ->
      return (Some (Lazy_trie.create ~value:(mode, b) ()))
    | Some (Value.Tree t) ->
      Lwt_list.fold_left_s (fun acc e ->
          aux (e.Tree.perm, e.Tree.node) >>= function
          | None   -> return acc
          | Some t -> return ((e.Tree.name, t) :: acc)
        ) [] (Tree.entries t)
      >>= fun children ->
      let children = lazy children in
      return (Some (Lazy_trie.create ~children ()))
    | Some (Value.Commit c) -> aux (`Dir, SHA1.of_tree c.Commit.tree)
    | _ -> return_none
  in
  aux (`Dir, SHA1.of_commit commit) >>= function
  | None    -> fail (Failure "create")
  | Some fs -> return fs

let iter_blobs t ~f ~init =
  load_filesystem t init >>= fun trie ->
  Lazy_trie.fold (fun acc path (mode, blob) ->
      acc >>= fun () ->
      f (t :: path) mode blob
  ) trie return_unit

let create_file file mode blob =
  let blob = Blob.to_string blob in
  Log.infof "Writing %s" file;
  GitUnix.mkdir (Filename.dirname file) >>= fun () ->
  match mode with
  | `Link -> (* Lwt_unix.symlink file ??? *) failwith "TODO"
  | _     ->
    GitUnix.write_file_string file ~contents:blob >>= fun () ->
    match mode with
    | `Exec -> Lwt_unix.chmod file 0o755
    | _     -> return_unit

let cache_file t =
  t / ".git" / "index"

let read_cache t =
  let buf = GitUnix.read_file (cache_file t) in
  let buf = Mstruct.of_bigarray buf in
  return (Git.Cache.input buf)

let stat_info_of_file path =
  let open Cache.Entry in
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
    | _ -> failwith "stats_info_of_file" in
  let uid = Int32.of_int_exn stats.st_uid in
  let gid = Int32.of_int_exn stats.st_gid in
  let size = Int32.of_int_exn stats.st_size in
  { ctime; mtime; dev; inode; uid; gid; mode; size }

let write_cache t head =
  let entries = ref [] in
  iter_blobs t ~init:head ~f:(fun path mode blob ->
      let file = String.concat ~sep:Filename.dir_sep path in
      begin
        if not (Sys.file_exists file) then create_file file mode blob
        else return_unit
      end >>= fun () ->
      let id = Git.sha1 (Value.Blob blob) in
      let stats = stat_info_of_file file in
      let stage = 0 in
      let name = String.chop_prefix_exn ~prefix:(t / "") file in
      let entry = { Cache.Entry.stats; id; stage; name } in
      entries := entry :: !entries;
      return_unit
    ) >>= fun () ->
  let cache = { Cache.entries = !entries; extensions = [] } in
  let buffers = Git.Cache.output cache in
  GitUnix.writev_file (cache_file t) buffers
