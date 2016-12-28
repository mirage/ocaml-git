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
open Git_misc.OP
open Printf
open Astring

module PackedLog = (val Git_misc.src_log "fs-packed" : Logs.LOG)
module LooseLog = (val Git_misc.src_log "fs-loose" : Logs.LOG)

module ReferenceSet = Git_misc.Set(Git_reference)

let fail fmt = Fmt.kstrf failwith ("Git.FS." ^^ fmt)

let err_not_found n k = fail "%s: %s not found" n k

module Log = (val Git_misc.src_log "fs" : Logs.LOG)

module type IO = sig
  val getcwd: unit -> string Lwt.t
  val realpath: string -> string Lwt.t
  val mkdir: string -> unit Lwt.t
  val remove: string -> unit Lwt.t
  val file_exists: string -> bool Lwt.t
  val directories: string -> string list Lwt.t
  val files: string -> string list Lwt.t
  val rec_files: string -> string list Lwt.t
  val read_file: string -> Cstruct.t Lwt.t
  val write_file: string -> ?temp_dir:string -> Cstruct.t -> unit Lwt.t
  val chmod: string -> int -> unit Lwt.t
  val stat_info: string -> Git_index.stat_info
end

module type S = sig
  include Git_store.S
  val remove: t -> unit Lwt.t
  val create_file: t -> string -> Git_tree.perm -> Git_blob.t -> unit Lwt.t
  val entry_of_file: t -> Git_index.t ->
    string -> Git_tree.perm -> Git_hash.Blob.t -> Git_blob.t -> Git_index.entry option Lwt.t
  val clear: unit -> unit
end

module Make (IO: IO) (D: Git_hash.DIGEST) (I: Git_inflate.S) = struct

  module Value_IO = Git_value.IO(D)(I)
  module Pack_IO = Git_pack.IO(D)(I)
  module Hash_IO = Git_hash.IO(D)
  module Pack_index = Git_pack_index.Make(D)
  module Packed_refs_IO = Git_packed_refs.IO(D)

  module File_cache : sig
    val read : string -> Cstruct.t Lwt.t
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
      | Some v -> Lwt.return v
      | None ->
        IO.read_file file >>= fun cs ->
        add file cs;
        Lwt.return cs

  end

  type t = { root: string; dot_git: string; level: int; }

  let root t = t.root
  let dot_git t = t.dot_git
  let level t = t.level

  let temp_dir t = t.dot_git / "tmp"

  let create ?root ?dot_git ?(level=6) () =
    if level < 0 || level > 9 then
      fail "create: level should be between 0 and 9";
    begin match root with
      | None   -> IO.getcwd ()
      | Some r ->
        IO.mkdir r >>= fun () ->
        IO.realpath r
    end >>= fun root' ->
    let dot_git = match dot_git with
      | None    -> root' / ".git"
      | Some s -> s
    in
    Lwt.return { root = root'; level; dot_git }

  let remove t =
    Log.info (fun l -> l "remove %s" t.dot_git);
    IO.remove t.dot_git

  (* Loose objects *)
  module Loose = struct

    module Log = LooseLog

    let file t h =
      let hex = Git_hash.to_hex h in
      let prefix = String.with_range hex ~len:2 in
      let suffix = String.with_range hex ~first:2 in
      t.dot_git / "objects" / prefix / suffix

    let mem t h = IO.file_exists (file t h)

    let ambiguous h = raise (Git_hash.Ambiguous (Git_hash.to_hex h))

    let get_file t h =
      IO.directories (t.dot_git / "objects") >>= fun dirs ->
      let hex = Git_hash.to_hex h in
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
      File_cache.read file >>= fun buf ->
      Mstruct.of_cstruct buf
      |> Value_IO.input
      |> some

    let inflated_of_file file =
      File_cache.read file >>= fun buf ->
      match I.inflate (Mstruct.of_cstruct buf) with
      | None   -> fail "%s is not a valid compressed file." file;
      | Some s -> some (Mstruct.to_string s)

    let read_aux name read_file t h =
      Log.debug (fun l -> l "%s %a" name Git_hash.pp h);
      if Hash_IO.is_short h then (
        Log.debug (fun l -> l "read: short hash");
        get_file t h >>= function
        | Some file -> read_file file
        | None      -> Lwt.return_none
      ) else (
        let file = file t h in
        IO.file_exists file >>= function
        | false -> Lwt.return_none
        | true  -> read_file file
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
        Git_misc.with_buffer (fun buf -> Value_IO.add_inflated buf value)
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
      let pack_file = "pack-" ^ (Git_hash.to_hex h) ^ ".pack" in
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
      let idx_file = "pack-" ^ (Git_hash.to_hex h) ^ ".idx" in
      pack_dir / idx_file

    let index_lru = Git_lru.make 8
    let keys_lru = Git_lru.make (128 * 1024)

    let clear () =
      Git_lru.clear index_lru;
      Git_lru.clear keys_lru

    let read_pack_index t h =
      Log.debug (fun l -> l "read_pack_index %a" Git_hash.pp h);
      match Git_lru.find index_lru h with
      | Some i ->
        Log.debug (fun l -> l "read_pack_index cache hit!");
        Lwt.return i
      | None ->
        let file = index t h in
        IO.file_exists file >>= function
        | true ->
          File_cache.read file >>= fun buf ->
          let index = Pack_index.input (Cstruct.to_bigarray buf) in
          Git_lru.add index_lru h index;
          Lwt.return index
        | false -> fail "read_pack_index: %s does not exist" file

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
      Log.debug (fun l -> l "read_keys %a" Git_hash.pp h);
      match Git_lru.find keys_lru h with
      | Some ks -> Lwt.return ks
      | None    ->
        Log.debug (fun l -> l "read_keys: cache miss!");
        read_pack_index t h >>= fun index ->
        let keys = Pack_index.keys index |> Git_hash.Set.of_list in
        Git_lru.add keys_lru h keys;
        Lwt.return keys

    let write_pack t h pack =
      Log.debug (fun l -> l "write pack");
      let file = file t h in
      IO.file_exists file >>= function
      | true  -> Lwt.return_unit
      | false ->
        let pack = Git_pack.Raw.buffer pack in
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir pack

    let mem_in_pack t pack_hash h =
      Log.debug (fun l -> l "mem_in_pack %a:%a" Git_hash.pp pack_hash Git_hash.pp h);
      read_pack_index t pack_hash >>= fun idx ->
      Lwt.return (Pack_index.mem idx h)

    let read_in_pack name pack_read t pack_hash h =
      Log.debug (fun l ->
          l "read_in_pack(%s) %a:%a" name Git_hash.pp pack_hash Git_hash.pp h);
      read_pack_index t pack_hash >>= fun i ->
      let index = Pack_index.find_offset i in
      match index h with
      | None   ->
        Log.debug (fun l -> l "read_in_pack: not found");
        Lwt.return_none
      | Some _ ->
        let file = file t pack_hash in
        IO.file_exists file >>= function
        | true ->
          File_cache.read file >>= fun buf ->
          pack_read ~index (Mstruct.of_cstruct buf) h
        | false ->
          fail "read_in_pack: cannot read the pack object %s"
            (Git_hash.to_hex pack_hash)

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
    let keys = List.fold_left Git_hash.Set.union (Git_hash.Set.of_list objects) keys in
    let keys = Git_hash.Set.to_list keys in
    Lwt.return keys

  let cache_add h = function
    | None   -> None
    | Some v -> Git_value.Cache.add h v; Some v

  let cache_add_inflated h = function
    | None   -> None
    | Some v -> Git_value.Cache.add_inflated h v; Some v

  let write t value =
    Loose.write t value >>= fun h ->
    Log.debug (fun l -> l "write -> %a" Git_hash.pp h);
    Git_value.Cache.add h value;
    Lwt.return h

  let write_inflated t value =
    Loose.write_inflated t value >>= fun h ->
    Log.debug (fun l -> l "write -> %a" Git_hash.pp h);
    Git_value.Cache.add_inflated h value;
    Lwt.return h

  let rec read t h =
    Log.debug (fun l -> l "read %a" Git_hash.pp h);
    match Git_value.Cache.find h with
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
    Log.debug (fun l -> l "read_inflated %a" Git_hash.pp h);
    match Git_value.Cache.find_inflated h with
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
    | None   -> err_not_found "read_exn" (Git_hash.to_hex h)

  let size t h =
    let use = function
      | Git_value.Blob b -> Lwt.return (Some (String.length (Git_blob.to_raw b)))
      | _ -> Lwt.return None in
    match Git_value.Cache.find h with
    | Some v -> use v
    | None   ->
      Log.debug (fun l -> l "size: cache miss!");
      (* Could have a [Loose.size] here... *)
      Loose.read t h >>= function
      | Some v -> use v
      | None   -> Packed.size t h

  let mem t h =
    match Git_value.Cache.find h with
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
        let typ = Git_value.type_of value in
        Log.err (fun l -> l "%a %a" Git_hash.pp h Git_object_type.pp typ);
      ) contents;
    Lwt.return_unit

  let packed_refs t = t.dot_git / "packed-refs"

  let references t =
    let refs = t.dot_git / "refs" in
    IO.rec_files refs >>= fun files ->
    let n = String.length (t.dot_git / "") in
    let refs = List.map (fun file ->
        let r = String.with_range file ~first:n in
        Git_reference.of_raw r
      ) files in
    let packed_refs = packed_refs t in
    let packed_refs =
      IO.file_exists packed_refs >>= function
      | false -> Lwt.return_nil
      | true  ->
        IO.read_file packed_refs >>= fun buf ->
        let pr = Packed_refs_IO.input (Mstruct.of_cstruct buf) in
        Lwt.return (Git_packed_refs.references pr)
    in
    packed_refs >|= fun packed_refs ->
    ReferenceSet.(
      union (of_list refs) (of_list packed_refs)
      |> elements
    )

  let file_of_ref t r = t.dot_git / Git_reference.to_raw r

  let mem_reference t r =
    let file = file_of_ref t r in
    IO.file_exists file

  let remove_reference t r =
    let file = file_of_ref t r in
    Lwt.catch
      (fun () -> IO.remove file)
      (fun _ -> Lwt.return_unit)

  let rec read_reference t r =
    let file = file_of_ref t r in
    IO.file_exists file >>= fun exists ->
    if exists then
      (* We use `IO.read_file` here as the contents of the file might
         change. *)
      IO.read_file file >>= fun buf ->
      let str = Cstruct.to_string buf in
      match Git_reference.head_contents_of_string ~of_hex:Hash_IO.of_hex str with
      | Git_reference.Hash x -> Lwt.return (Some (Git_hash.of_commit x))
      | Git_reference.Ref r -> read_reference t r
    else
      let packed_refs = packed_refs t in
      IO.file_exists packed_refs >>= function
      | false -> Lwt.return_none
      | true  ->
        (* We use `IO.read_file` here as the contents of the file
           might change. *)
        IO.read_file packed_refs >>= fun buf ->
        let refs = Packed_refs_IO.input (Mstruct.of_cstruct buf) in
        let h = Git_packed_refs.find refs r in
        Lwt.return h

  let read_head t =
    let file = file_of_ref t Git_reference.head in
    IO.file_exists file >>= function
    | true ->
      (* We use `IO.read_file` here as the contents of the file might
         change. *)
      IO.read_file file >|= fun buf ->
      let str = Cstruct.to_string buf in
      Some (Git_reference.head_contents_of_string ~of_hex:Hash_IO.of_hex str)
    | false ->
      Lwt.return None

  let read_reference_exn t r =
    read_reference t r >>= function
    | Some s -> Lwt.return s
    | None   ->
      err_not_found "read_reference_exn" (Fmt.to_to_string Git_reference.pp r)

  let write_pack t pack =
    Log.debug (fun l -> l "write_pack");
    let name = Git_pack.Raw.name pack in
    let index = Git_pack.Raw.index pack in
    Packed.write_pack t name pack   >>= fun () ->
    Packed.write_pack_index t name index >>= fun () ->
    Lwt.return (Git_pack.Raw.keys pack)

  let write_reference t r hash =
    let file = t.dot_git / Git_reference.to_raw r in
    let contents = Git_hash.to_hex hash in
    let temp_dir = temp_dir t in
    IO.write_file file ~temp_dir (Cstruct.of_string contents)

  let write_head t = function
    | Git_reference.Hash h -> write_reference t Git_reference.head (Git_hash.of_commit h)
    | Git_reference.Ref  r ->
      let file = t.dot_git / "HEAD" in
      let contents = sprintf "ref: %s" (Git_reference.to_raw r) in
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
    Log.debug (fun l -> l "load_filesystem head=%a" Git_hash.Commit.pp head);
    let blobs_c = ref 0 in
    let id = id () in
    let error expected got =
      fail "load_filesystem: expecting a %s, got a %a"
        expected Git_object_type.pp (Git_value.type_of got)
    in
    let blob mode h k =
      Log.debug (fun l -> l "blob %d %a"id Git_hash.pp h);
      assert (mode <> `Dir);
      incr blobs_c;
      read_exn t h >>= function
      | Git_value.Blob b -> k (Leaf (mode, (Git_hash.to_blob h, b)))
      | obj          -> error "blob" obj
    in
    let rec tree mode h k =
      Log.debug (fun l -> l "tree %d %a" id Git_hash.pp h);
      assert (mode = `Dir);
      read_exn t h >>= function
      | Git_value.Tree t -> tree_entries t [] k
      | obj          -> error "tree" obj
    and tree_entries trees children k =
      match trees with
      | []   -> k (Node children)
      | e::t ->
        let k n = tree_entries t ((e.Git_tree.name, n)::children) k in
        match e.Git_tree.perm with
        | `Dir -> tree `Dir e.Git_tree.node k
        | mode -> blob mode e.Git_tree.node k
    in
    let commit h =
      Log.debug (fun l -> l "commit %d %a" id Git_hash.pp h);
      read_exn t h >>= function
      | Git_value.Commit c -> tree `Dir (Git_hash.of_tree c.Git_commit.tree) Lwt.return
      | obj            -> error "commit" obj
    in
    commit (Git_hash.of_commit head) >>= fun t ->
    Lwt.return (!blobs_c, t)

  let iter_blobs t ~f ~init =
    load_filesystem t init >>= fun (n, trie) ->
    let i = ref 0 in
    Log.debug (fun l -> l "iter_blobs %a" Git_hash.Commit.pp init);
    iter (fun path (mode, (h, blob)) ->
        incr i;
        f (!i, n) (t.root :: path) mode h blob
      ) trie

  let create_file t file mode blob =
    Log.debug (fun l -> l "create_file %s" file);
    let blob = Git_blob.to_raw blob in
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
              IO.remove file >>= fun () ->
              write (n-1))
      in
      write 10 >>= fun () ->
      match mode with
      | `Exec -> IO.chmod file 0o755
      | _     -> Lwt.return_unit

  let index_file t = t.dot_git / "index"

  module Index_IO = Git_index.IO(D)

  let read_index t =
    Log.debug (fun l -> l "read_index");
    let file = index_file t in
    IO.file_exists file >>= function
    | false -> Lwt.return Git_index.empty
    | true  ->
      (* We use `IO.read_file` here as the contents of the file might
         change. *)
      IO.read_file file >>= fun buf ->
      let buf = Mstruct.of_cstruct buf in
      Lwt.return (Index_IO.input buf)

  let entry_of_file_aux t index file mode h blob =
    IO.realpath file >>= fun file ->
    Log.debug (fun l -> l "entry_of_file %a %s" Git_hash.Blob.pp h file);
    begin
      IO.file_exists file >>= function
      | false ->
        Log.debug (fun l ->
            l "%s does not exist on the filesystem, creating!" file);
        create_file t file mode blob
      | true  ->
        let entry =
          try
            List.find (fun e -> t.root / e.Git_index.name = file) index.Git_index.entries
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
          if not (Git_hash.Blob.equal e.Git_index.id h)then (
            Log.debug (fun l ->
                l "%s has an old version in the index, updating!" file);
            create_file t file mode blob
          ) else
            let stats = IO.stat_info file in
            if e.Git_index.stats <> stats then (
              (* same thing here, usually Git just stops in that case. *)
              Log.debug (fun l ->
                  l "%s has been modified on the filesystem, reversing!" file);
              create_file t file mode blob
            ) else (
              Log.debug (fun l ->
                  l "%s: %s unchanged!" (Git_hash.Blob.to_hex h) file);
              Lwt.return_unit
            )
    end >|= fun () ->
    let id = h in
    let stats = IO.stat_info file in
    let stage = 0 in
    let prefix = t.root / "" in
    if not (String.is_prefix file ~affix:prefix) then None
    else
      let name = String.with_range file ~first:(String.length prefix) in
      let entry = { Git_index.stats; id; stage; name } in
      Some entry

  let entry_of_file t index file mode h blob =
    Lwt.catch
      (fun () -> entry_of_file_aux t index file mode h blob)
      (function Failure _ | Sys_error _ -> Lwt.return_none | e -> Lwt.fail e)

  let write_index t ?index head =
    Log.debug (fun l -> l "write_index %a" Git_hash.Commit.pp head);
    let buf = Buffer.create 1024 in
    match index with
    | Some index ->
      Index_IO.add buf index;
      let temp_dir = temp_dir t in
      IO.write_file (index_file t) ~temp_dir
        (Cstruct.of_string (Buffer.contents buf)) >>= fun () ->
      let all = List.length index.Git_index.entries in
      Log.info (fun l -> l "Checking out files: 100%% (%d/%d), done." all all);
      Lwt.return_unit
    | None ->
      let entries = ref [] in
      let all = ref 0 in
      read_index t >>= fun index ->
      Log.info (fun l -> l "Checking out files...");
      iter_blobs t ~init:head ~f:(fun (i,n) path mode h blob ->
          all := n;
          let file = String.concat ~sep:Filename.dir_sep path in
          Log.debug (fun l -> l "write_index: %d/%d blob:%s" i n file);
          entry_of_file t index file mode h blob >>= function
          | None   -> Lwt.return_unit
          | Some e -> entries := e :: !entries; Lwt.return_unit
        ) >>= fun () ->
      let index = Git_index.create !entries in
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
