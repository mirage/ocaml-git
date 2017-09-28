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

open Lwt.Infix
open Misc.OP
open Printf
open Astring

module PackedLog = (val Misc.src_log "fs-packed" : Logs.LOG)
module LooseLog = (val Misc.src_log "fs-loose" : Logs.LOG)

module ReferenceSet = Misc.Set(Reference)

let failf fmt = Fmt.kstrf failwith ("Git.FS." ^^ fmt)

let err_not_found n k = failf "%s: %s not found" n k

module Log = (val Misc.src_log "fs" : Logs.LOG)

module type IO = sig

  type path = string

  (* Reads *)

  val file_exists: path -> bool Lwt.t
  val directories: path -> string list Lwt.t
  val files: path -> string list Lwt.t
  val read_file: path -> Cstruct.t option Lwt.t
  val stat_info: path -> Index.stat_info option Lwt.t

  (* Updates *)

  val mkdir: path -> unit Lwt.t

  type lock
  val lock_file: path -> lock

  val write_file: ?temp_dir:path -> ?lock:lock -> path -> Cstruct.t -> unit Lwt.t
  val test_and_set_file: ?temp_dir:path -> lock:lock ->
    path -> test:Cstruct.t option -> set:Cstruct.t option -> bool Lwt.t
  val remove_file: ?lock:lock -> path -> unit Lwt.t
  val remove_dir: path -> unit Lwt.t
  val chmod: ?lock:lock -> path -> [`Exec] -> unit Lwt.t
end

module type S = sig
  include Store.S
  val create_file: t -> string -> Tree.perm -> Blob.t -> unit Lwt.t
  val entry_of_file: t -> Index.t ->
    string -> Tree.perm -> Hash.Blob.t -> Blob.t -> Index.entry option Lwt.t
  val reset: t -> unit Lwt.t
  val clear: unit -> unit
end

module Make (IO: IO) (D: Hash.DIGEST) (I: Inflate.S) = struct

  module Value_IO = Value.IO(D)(I)
  module Pack_IO = Pack.IO(D)(I)
  module Hash_IO = Hash.IO(D)
  module Pack_index = Pack_index.Make(D)
  module Packed_refs_IO = Packed_refs.IO(D)

  module File_cache : sig
    val read : string -> Cstruct.t option Lwt.t
    val clear: unit -> unit
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
    let clear () = WeakTbl.clear cache
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

    let read file =
      match find file with
      | Some _ as v -> Lwt.return v
      | None ->
        IO.read_file file >|= fun v ->
        (match v with None -> () | Some cs -> add file cs);
        v

  end

  type t = { root: string; dot_git: string; level: int; }

  let root t = t.root
  let dot_git t = t.dot_git
  let level t = t.level

  let temp_dir t = t.dot_git / "tmp"
  let lock_dir t = t.dot_git / "lock"

  let create ?(root=".") ?dot_git ?(level=6) () =
    if level < 0 || level > 9 then
      failf "create: level should be between 0 and 9";
    let dot_git = match dot_git with
      | None    -> root / ".git"
      | Some s -> s
    in
    IO.mkdir root    >>= fun () ->
    IO.mkdir dot_git >|= fun () ->
    { root; level; dot_git }

  let rec_files dir =
    let rec aux accu dir =
      IO.directories dir >>= fun ds ->
      IO.files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let reset t =
    Log.info (fun l -> l "remove %s" t.dot_git);
    let lock_file f = match String.cut ~sep:(t.dot_git / "") f with
      | Some (_, f) -> IO.lock_file (t.dot_git / "lock" / f)
      | None        -> assert false
    in
    rec_files t.dot_git >>= fun files ->
    (* remove all files with lock (in case someone else is writing
       some stuff). Slow, but we probably don't care *)
    Lwt_list.iter_s (fun f -> IO.remove_file ~lock:(lock_file f) f) files
    >>= fun () ->
    (* remove .git *)
    Log.warn (fun l -> l "removing %s" t.dot_git);
    IO.remove_dir t.dot_git

  (* Loose objects *)
  module Loose = struct

    module Log = LooseLog

    let file t h =
      let hex = Hash.to_hex h in
      let prefix = String.with_range hex ~len:2 in
      let suffix = String.with_range hex ~first:2 in
      t.dot_git / "objects" / prefix / suffix

    let mem t h = IO.file_exists (file t h)

    let ambiguous h = raise (Hash.Ambiguous (Hash.to_hex h))

    let get_file t h =
      IO.directories (t.dot_git / "objects") >>= fun dirs ->
      let hex = Hash.to_hex h in
      let len = String.length hex in
      let dcands =
        if len <= 2 then
          List.filter (fun d ->
              String.is_prefix ~affix:hex @@ Filename.basename d
            ) dirs
        else
          List.filter (fun d ->
              String.is_prefix ~affix:(Filename.basename d) hex
            ) dirs
      in
      match dcands with
      | []      -> Lwt.return_none
      | _::_::_ -> ambiguous h
      | [dir] ->
        Log.debug (fun l -> l "get_file: %s" dir);
        IO.files dir >>= fun files ->
        let fcands =
          if len <= 2 then files
          else
            let len' = len - 2 in
            let suffix = String.with_range hex ~first:2 in
            List.filter (fun f ->
                String.with_range (Filename.basename f) ~len:len' = suffix
              ) files
        in
        match fcands with
        | []     -> Lwt.return_none
        | [file] -> Lwt.return (Some file)
        | _      -> ambiguous h


    let some x = Lwt.return (Some x)

    let value_of_file file =
      File_cache.read file >|= function
      | None     -> None
      | Some buf -> Some (Value_IO.input (Mstruct.of_cstruct buf))

    let inflated_of_file file =
      File_cache.read file >>= function
      | None     -> Lwt.return None
      | Some buf ->
        match I.inflate (Mstruct.of_cstruct buf) with
        | None   -> failf "%s is not a valid compressed file." file;
        | Some s -> some (Mstruct.to_string s)

    let read_aux name read_file t h =
      Log.debug (fun l -> l "%s %a" name Hash.pp h);
      if Hash_IO.is_short h then (
        Log.debug (fun l -> l "read: short hash");
        get_file t h >>= function
        | Some file -> read_file file
        | None      -> Lwt.return_none
      ) else (
        read_file (file t h)
      )
    let read  = read_aux "read" value_of_file
    let read_inflated = read_aux "read_inflated" inflated_of_file

    let write_inflated t inflated =
      let h = D.string inflated in
      let file = file t h in
      IO.file_exists file >>= function
      | true  ->
        Log.debug (fun l -> l "write: file %s already exists!" file);
        Lwt.return h
      | false ->
        let level = t.level in
        let deflated = I.deflate ~level (Cstruct.of_string inflated) in
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir deflated >>= fun () ->
        Lwt.return h

    let write t value =
      Log.debug (fun l -> l "write");
      let inflated =
        Misc.with_buffer (fun buf -> Value_IO.add_inflated buf value)
      in
      write_inflated t inflated

    let list t =
      Log.debug (fun l -> l "Loose.list %s" t.dot_git);
      let objects = t.dot_git / "objects" in
      IO.directories objects >>= fun objects ->
      let objects = List.map Filename.basename objects in
      let objects =
        List.filter (fun s -> (s <> "info") && (s <> "pack")) objects
      in
      Lwt_list.map_s (fun prefix ->
          let dir = t.dot_git / "objects" / prefix in
          IO.files dir >>= fun suffixes ->
          let suffixes = List.map Filename.basename suffixes in
          let objects = List.map (fun suffix ->
              Hash_IO.of_hex (prefix ^ suffix)
            ) suffixes in
          Lwt.return objects
        ) objects
      >>= fun files ->
      Lwt.return (List.concat files)

  end

  module Packed = struct

    module Log = PackedLog

    let file t h =
      let pack_dir = t.dot_git / "objects" / "pack" in
      let pack_file = "pack-" ^ (Hash.to_hex h) ^ ".pack" in
      pack_dir / pack_file

    let list t =
      Log.debug (fun l -> l "list %s" t.dot_git);
      let packs = t.dot_git / "objects" / "pack" in
      IO.files packs >|= fun packs ->
      let parse_pack acc f =
        let hex = Hash_IO.of_hex in
        match String.cut ~rev:true ~sep:"." (Filename.basename f) with
        | None            -> acc
        | Some (f, "idx") -> hex (String.with_range ~first:5 f) :: acc
        | Some (_, _)     -> acc
      in
      List.(rev (fold_left parse_pack [] packs))

    let index t h =
      let pack_dir = t.dot_git / "objects" / "pack" in
      let idx_file = "pack-" ^ (Hash.to_hex h) ^ ".idx" in
      pack_dir / idx_file

    let index_lru = Lru.make 8
    let keys_lru = Lru.make (128 * 1024)

    let clear () =
      Lru.clear index_lru;
      Lru.clear keys_lru

    let read_pack_index t h =
      Log.debug (fun l -> l "read_pack_index %a" Hash.pp h);
      match Lru.find index_lru h with
      | Some i ->
        Log.debug (fun l -> l "read_pack_index cache hit!");
        Lwt.return i
      | None ->
        let file = index t h in
        File_cache.read file >>= function
        | None     -> failf "read_pack_index: %s does not exist" file
        | Some buf ->
          let index = Pack_index.input (Cstruct.to_bigarray buf) in
          Lru.add index_lru h index;
          Lwt.return index

    let write_pack_index t h idx =
      let file = index t h in
      IO.file_exists file >>= function
      | true  -> Lwt.return_unit
      | false ->
        let buf = Buffer.create 1024 in
        Pack_index.Raw.add buf idx;
        let temp_dir = temp_dir t in
        let buf = Cstruct.of_string (Buffer.contents buf) in
        IO.write_file file ~temp_dir buf

    let read_keys t h =
      Log.debug (fun l -> l "read_keys %a" Hash.pp h);
      match Lru.find keys_lru h with
      | Some ks -> Lwt.return ks
      | None    ->
        Log.debug (fun l -> l "read_keys: cache miss!");
        read_pack_index t h >>= fun index ->
        let keys = Pack_index.keys index |> Hash.Set.of_list in
        Lru.add keys_lru h keys;
        Lwt.return keys

    let write_pack t h pack =
      Log.debug (fun l -> l "write pack");
      let file = file t h in
      IO.file_exists file >>= function
      | true  -> Lwt.return_unit
      | false ->
        let pack = Pack.Raw.buffer pack in
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir pack

    let mem_in_pack t pack_hash h =
      Log.debug (fun l -> l "mem_in_pack %a:%a" Hash.pp pack_hash Hash.pp h);
      read_pack_index t pack_hash >>= fun idx ->
      Lwt.return (Pack_index.mem idx h)

    let read_in_pack name pack_read t pack_hash h =
      Log.debug (fun l ->
          l "read_in_pack(%s) %a:%a" name Hash.pp pack_hash Hash.pp h);
      read_pack_index t pack_hash >>= fun i ->
      let index = Pack_index.find_offset i in
      match index h with
      | None   ->
        Log.debug (fun l -> l "read_in_pack: not found");
        Lwt.return_none
      | Some _ ->
        let file = file t pack_hash in
        File_cache.read file >>= function
        | Some buf -> pack_read ~index (Mstruct.of_cstruct buf) h
        | None     ->
          failf "read_in_pack: cannot read the pack object %s"
            (Hash.to_hex pack_hash)


    let read_aux read_in_pack t h =
      list t >>= fun packs ->
      Lwt_list.fold_left_s (fun acc pack ->
          match acc with
          | Some v -> Lwt.return (Some v)
          | None   -> read_in_pack t pack h
        ) None packs

    let read t h ~read ~write =
      read_aux (read_in_pack "read" (Pack_IO.Raw.read ~read ~write)) t h

    let read_inflated t h ~read ~write =
      read_aux
        (read_in_pack "read_inflated"  (Pack_IO.Raw.read_inflated ~read ~write))
        t h

    let size = read_aux (read_in_pack "size" Pack_IO.Raw.size)

    let mem t h =
      list t >>= fun packs ->
      Lwt_list.fold_left_s (fun acc pack ->
          if acc then Lwt.return acc
          else mem_in_pack t pack h
        ) false packs
  end

  let list t =
    Log.debug (fun l -> l "list");
    Loose.list t  >>= fun objects ->
    Packed.list t >>= fun packs   ->
    Lwt_list.map_p (fun p -> Packed.read_keys t p) packs >>= fun keys ->
    let keys = List.fold_left Hash.Set.union (Hash.Set.of_list objects) keys in
    let keys = Hash.Set.to_list keys in
    Lwt.return keys

  let cache_add h = function
    | None   -> None
    | Some v -> Value.Cache.add h v; Some v

  let cache_add_inflated h = function
    | None   -> None
    | Some v -> Value.Cache.add_inflated h v; Some v

  let write t value =
    Loose.write t value >>= fun h ->
    Log.debug (fun l -> l "write -> %a" Hash.pp h);
    Value.Cache.add h value;
    Lwt.return h

  let write_inflated t value =
    Loose.write_inflated t value >>= fun h ->
    Log.debug (fun l -> l "write -> %a" Hash.pp h);
    Value.Cache.add_inflated h value;
    Lwt.return h

  let rec read t h =
    Log.debug (fun l -> l "read %a" Hash.pp h);
    match Value.Cache.find h with
    | Some v -> Lwt.return (Some v)
    | None   ->
      Log.debug (fun l -> l "read: cache miss!");
      begin
        Loose.read t h >>= function
        | Some v -> Lwt.return (Some v)
        | None   ->
          let read = read_inflated t in
          let write = write_inflated t in
          Packed.read ~read ~write t h
      end >|=
      cache_add h

  and read_inflated t h =
    Log.debug (fun l -> l "read_inflated %a" Hash.pp h);
    match Value.Cache.find_inflated h with
    | Some v -> Lwt.return (Some v)
    | None   ->
      Log.debug (fun l -> l "read_inflated: cache miss!");
      begin
        Loose.read_inflated t h >>= function
        | Some v -> Lwt.return (Some v)
        | None   ->
          let read = read_inflated t in
          let write = write_inflated t in
          Packed.read_inflated ~read ~write t h
      end >|=
      cache_add_inflated h

  let read_exn t h =
    read t h >>= function
    | Some v -> Lwt.return v
    | None   -> err_not_found "read_exn" (Hash.to_hex h)

  let size t h =
    let use = function
      | Value.Blob b -> Lwt.return (Some (String.length (Blob.to_raw b)))
      | _ -> Lwt.return None in
    match Value.Cache.find h with
    | Some v -> use v
    | None   ->
      Log.debug (fun l -> l "size: cache miss!");
      (* Could have a [Loose.size] here... *)
      Loose.read t h >>= function
      | Some v -> use v
      | None   -> Packed.size t h

  let mem t h =
    match Value.Cache.find h with
    | Some _ -> Lwt.return true
    | None   ->
      Log.debug (fun l -> l "mem: cache miss!");
      Loose.mem t h >>= function
      | true  -> Lwt.return true
      | false -> Packed.mem t h

  let contents t =
    Log.debug (fun l -> l "contents");
    list t >>= fun hs ->
    Lwt_list.map_p (fun h -> read_exn t h >|= fun value -> (h, value)) hs

  let dump t =
    contents t >>= fun contents ->
    List.iter (fun (h, value) ->
        let typ = Value.type_of value in
        Log.err (fun l -> l "%a %a" Hash.pp h Object_type.pp typ);
      ) contents;
    Lwt.return_unit

  let packed_refs t = t.dot_git / "packed-refs"

  let references t =
    let refs = t.dot_git / "refs" in
    rec_files refs >>= fun files ->
    let n = String.length (t.dot_git / "") in
    let refs = List.map (fun file ->
        let r = String.with_range file ~first:n in
        if Sys.os_type <> "Win32" then Reference.of_raw r
        else
          String.cuts ~sep:Filename.dir_sep r
          |> String.concat ~sep:"/"
          |> Reference.of_raw
      ) files in
    let packed_refs = packed_refs t in
    let packed_refs =
      IO.read_file packed_refs >|= function
      | None     -> []
      | Some buf ->
        let pr = Packed_refs_IO.input (Mstruct.of_cstruct buf) in
        Packed_refs.references pr
    in
    packed_refs >|= fun packed_refs ->
    ReferenceSet.(
      union (of_list refs) (of_list packed_refs)
      |> elements
    )

  let raw_ref r =
    let raw = Reference.to_raw r in
    if Sys.os_type <> "Win32" then raw
    else
      String.cuts ~sep:"/" raw
      |> String.concat ~sep:Filename.dir_sep

  let file_of_ref t r = t.dot_git / raw_ref r
  let mem_reference t r = IO.file_exists (file_of_ref t r)
  let lock_file t r = IO.lock_file (lock_dir t / raw_ref r)

  let remove_reference t r =
    let file = file_of_ref t r in
    let lock = lock_file t r in
    Lwt.catch
      (fun () -> IO.remove_file ~lock file)
      (fun _  -> Lwt.return_unit)

  let rec read_reference t r =
    let file = file_of_ref t r in
    (* We use `IO.read_file` here as the contents of the file might
       change. *)
    IO.read_file file >>= function
    | Some buf ->
      let str = Cstruct.to_string buf in
      let r = Reference.head_contents_of_string ~of_hex:Hash_IO.of_hex str in
      (match r with
       | Reference.Hash x -> Lwt.return (Some (Hash.of_commit x))
       | Reference.Ref r -> read_reference t r)
    | None ->
      let packed_refs = packed_refs t in
      (* We use `IO.read_file` here as the contents of the file
         might change. *)
      IO.read_file packed_refs >|= function
      | None -> None
      | Some buf ->
        let refs = Packed_refs_IO.input (Mstruct.of_cstruct buf) in
        Packed_refs.find refs r

  let read_head t =
    let file = file_of_ref t Reference.head in
    (* We use `IO.read_file` here as the contents of the file might
         change. *)
    IO.read_file file >|= function
    | None     -> None
    | Some buf ->
      let str = Cstruct.to_string buf in
      Some (Reference.head_contents_of_string ~of_hex:Hash_IO.of_hex str)

  let read_reference_exn t r =
    read_reference t r >>= function
    | Some s -> Lwt.return s
    | None   ->
      err_not_found "read_reference_exn" (Fmt.to_to_string Reference.pp r)

  let write_pack t pack =
    Log.debug (fun l -> l "write_pack");
    let name = Pack.Raw.name pack in
    let index = Pack.Raw.index pack in
    Packed.write_pack t name pack   >>= fun () ->
    Packed.write_pack_index t name index >>= fun () ->
    Lwt.return (Pack.Raw.keys pack)

  let write_reference t r hash =
    let file = file_of_ref t r in
    let contents = Hash.to_hex hash in
    let temp_dir = temp_dir t in
    let lock = lock_file t r in
    IO.write_file file ~temp_dir ~lock (Cstruct.of_string contents)

  let test_and_set_reference t r ~test ~set =
    let file = file_of_ref t r in
    let temp_dir = temp_dir t in
    let raw = function
      | None -> None
      | Some v -> Some (Cstruct.of_string (Hash.to_hex v))
    in
    let lock = lock_file t r in
    IO.test_and_set_file file ~temp_dir ~lock ~test:(raw test) ~set:(raw set)

  let write_head t = function
    | Reference.Hash h ->
      write_reference t Reference.head (Hash.of_commit h)
    | Reference.Ref  r ->
      let file = t.dot_git / "HEAD" in
      let contents = sprintf "ref: %s" (Reference.to_raw r) in
      let temp_dir = temp_dir t in
      IO.write_file file ~temp_dir (Cstruct.of_string contents)

  type 'a tree =
    | Leaf of 'a
    | Node of (string * 'a tree) list

  let iter fn t =
    let rec aux path = function
      | Leaf l -> fn (List.rev path) l
      | Node n ->
        Lwt_list.iter_p (fun (l, t) ->
            aux (l::path) t
          ) n in
    aux [] t

  (* XXX: do not load the blobs *)
  let id = let n = ref 0 in fun () -> incr n; !n

  let load_filesystem t head =
    Log.debug (fun l -> l "load_filesystem head=%a" Hash.Commit.pp head);
    let blobs_c = ref 0 in
    let id = id () in
    let error expected got =
      failf "load_filesystem: expecting a %s, got a %a"
        expected Object_type.pp (Value.type_of got)
    in
    let blob mode h k =
      Log.debug (fun l -> l "blob %d %a"id Hash.pp h);
      assert (mode <> `Dir);
      incr blobs_c;
      read_exn t h >>= function
      | Value.Blob b -> k (Leaf (mode, (Hash.to_blob h, b)))
      | obj          -> error "blob" obj
    in
    let rec tree mode h k =
      Log.debug (fun l -> l "tree %d %a" id Hash.pp h);
      assert (mode = `Dir);
      read_exn t h >>= function
      | Value.Tree t -> tree_entries t [] k
      | obj          -> error "tree" obj
    and tree_entries trees children k =
      match trees with
      | []   -> k (Node children)
      | e::t ->
        let k n = tree_entries t ((e.Tree.name, n)::children) k in
        match e.Tree.perm with
        | `Dir -> tree `Dir e.Tree.node k
        | mode -> blob mode e.Tree.node k
    in
    let commit h =
      Log.debug (fun l -> l "commit %d %a" id Hash.pp h);
      read_exn t h >>= function
      | Value.Commit c -> tree `Dir (Hash.of_tree c.Commit.tree) Lwt.return
      | obj            -> error "commit" obj
    in
    commit (Hash.of_commit head) >>= fun t ->
    Lwt.return (!blobs_c, t)

  let iter_blobs t ~f ~init =
    load_filesystem t init >>= fun (n, trie) ->
    let i = ref 0 in
    Log.debug (fun l -> l "iter_blobs %a" Hash.Commit.pp init);
    iter (fun path (mode, (h, blob)) ->
        incr i;
        f (!i, n) path mode h blob
      ) trie

  let create_file t file mode blob =
    Log.debug (fun l -> l "create_file %s" file);
    let blob = Blob.to_raw blob in
    match mode with
    | `Link -> (*q Lwt_unix.symlink file ??? *) failwith "TODO"
    | _     ->
      let temp_dir = temp_dir t in
      let contents = Cstruct.of_string blob in
      let rec write n =
        let one () =
          Log.debug (fun l -> l "one %S" file);
          IO.write_file file ~temp_dir contents
        in
        if n <= 1 then one () else
          Lwt.catch one (fun e ->
              Log.debug (fun l ->
                  l "write (%d/10): Got %S, retrying."
                    (11-n) (Printexc.to_string e));
              IO.remove_file file >>= fun () ->
              write (n-1))
      in
      write 10 >>= fun () ->
      match mode with
      | `Exec -> IO.chmod file `Exec
      | _     -> Lwt.return_unit

  let index_file t = t.dot_git / "index"

  module Index_IO = Index.IO(D)

  let read_index t =
    Log.debug (fun l -> l "read_index");
    let file = index_file t in
    (* We use `IO.read_file` here as the contents of the file might
       change. *)
    IO.read_file file >|= function
    | None     -> Index.empty
    | Some buf -> Index_IO.input (Mstruct.of_cstruct buf)

  let entry_of_file_aux t index file mode h blob =
    Log.debug (fun l -> l "entry_of_file %a %s" Hash.Blob.pp h file);
    begin
      IO.file_exists file >>= function
      | false ->
        Log.debug (fun l ->
            l "%s does not exist on the filesystem, creating!" file);
        create_file t file mode blob
      | true  ->
        let entry =
          try
            List.find (fun e ->
                t.root / e.Index.name = file
              ) index.Index.entries
            |> fun x -> Some x
          with Not_found ->
            None
        in
        match entry with
        | None  ->
          Log.debug (fun l -> l "%s does not exist in the index, adding!" file);
          (* in doubt, overide the current version -- git will just refuse
             to do anything in that case. *)
          create_file t file mode blob
        | Some e ->
          if not (Hash.Blob.equal e.Index.id h)then (
            Log.debug (fun l ->
                l "%s has an old version in the index, updating!" file);
            create_file t file mode blob
          ) else
            IO.stat_info file >>= function
            | None       -> Lwt.return_unit
            | Some stats ->
              if e.Index.stats <> stats then (
                (* same thing here, usually Git just stops in that case. *)
                Log.debug (fun l ->
                    l "%s has been modified on the filesystem, reversing!" file);
                create_file t file mode blob
              ) else (
                Log.debug (fun l ->
                    l "%s: %s unchanged!" (Hash.Blob.to_hex h) file);
                Lwt.return_unit
              )
    end >>= fun () ->
    IO.stat_info file >|= function
    | None       -> None
    | Some stats ->
      let id = h in
      let stage = 0 in
      let prefix = t.root / "" in
      if not (String.is_prefix file ~affix:prefix) then None
      else
        let name = String.with_range file ~first:(String.length prefix) in
        let entry = { Index.stats; id; stage; name } in
        Some entry

  let entry_of_file t index file mode h blob =
    Lwt.catch
      (fun () -> entry_of_file_aux t index file mode h blob)
      (function Failure _ | Sys_error _ -> Lwt.return_none | e -> Lwt.fail e)

  let write_index t ?index head =
    Log.debug (fun l -> l "write_index %a" Hash.Commit.pp head);
    let buf = Buffer.create 1024 in
    match index with
    | Some index ->
      Index_IO.add buf index;
      let temp_dir = temp_dir t in
      IO.write_file (index_file t) ~temp_dir
        (Cstruct.of_string (Buffer.contents buf)) >>= fun () ->
      let all = List.length index.Index.entries in
      Log.info (fun l -> l "Checking out files: 100%% (%d/%d), done." all all);
      Lwt.return_unit
    | None ->
      let entries = ref [] in
      let all = ref 0 in
      read_index t >>= fun index ->
      Log.info (fun l -> l "Checking out files...");
      iter_blobs t ~init:head ~f:(fun (i,n) path mode h blob ->
          all := n;
          let file = String.concat ~sep:Filename.dir_sep (t.root :: path) in
          Log.debug (fun l -> l "write_index: %d/%d blob:%s" i n file);
          entry_of_file t index file mode h blob >>= function
          | None   -> Lwt.return_unit
          | Some e -> entries := e :: !entries; Lwt.return_unit
        ) >>= fun () ->
      let index = Index.create !entries in
      let temp_dir = temp_dir t in
      Index_IO.add buf index;
      IO.write_file (index_file t) ~temp_dir
        (Cstruct.of_string (Buffer.contents buf)) >>= fun () ->
      Log.info (fun l -> l "Checking out files: 100%% (%d/%d), done." !all !all);
      Lwt.return_unit

  let kind = `Disk

  let clear () =
    File_cache.clear ();
    Packed.clear ()

  module Digest = D
  module Inflate = I

end
