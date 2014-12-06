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

open Misc.OP
open Printf

let (>>=) = Lwt.bind

module LogMake = Log.Make

module Log = LogMake(struct let section = "fs" end)

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
  val stat_info: string -> Index.stat_info
end

module type S = sig
  include Store.S
  val create_file: t -> string -> Tree.perm -> Blob.t -> unit Lwt.t
  val entry_of_file: t -> Index.t ->
    string -> Tree.perm -> SHA.Blob.t -> Blob.t -> Index.entry option Lwt.t
end

module Make (IO: IO) = struct

  module File_cache : sig
    val read : string -> Cstruct.t Lwt.t
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

    let read file =
      match find file with
      | Some v -> Lwt.return v
      | None ->
        IO.read_file file >>= fun cs ->
        add file cs;
        Lwt.return cs

  end

  type t = {
    root: string;
    level: int;
  }

  let root t = t.root
  let level t = t.level

  let temp_dir { root; _ } = root / ".git" / "tmp"

  let create ?root ?(level=6) () =
    if level < 0 || level > 9 then failwith "level should be between 0 and 9";
    begin match root with
    | None   -> IO.getcwd ()
    | Some r ->
      IO.mkdir r >>= fun () ->
      IO.realpath r
    end >>= fun root ->
    Lwt.return { root; level }

  let clear t =
    Log.info "clear %s" t.root;
    IO.remove (sprintf "%s/.git" t.root)

  (* Loose objects *)
  module Loose = struct

    module Log = LogMake(struct let section = "fs-loose" end)

    let file { root; _ } sha1 =
      let hex = SHA.to_hex sha1 in
      let prefix = String.sub hex 0 2 in
      let suffix = String.sub hex 2 (String.length hex - 2) in
      root / ".git" / "objects" / prefix / suffix

    let mem t sha1 =
      IO.file_exists (file t sha1)

    let read t sha1 =
      Log.debug "read %s" (SHA.to_hex sha1);
      let file = file t sha1 in
      IO.file_exists file >>= function
      | false -> Lwt.return_none
      | true  ->
        File_cache.read file >>= fun buf ->
        try
          let value = Value.input (Mstruct.of_cstruct buf) in
          Lwt.return (Some value)
        with Zlib.Error _ ->
          Lwt.fail (Zlib.Error (file, (Cstruct.to_string buf)))

    let write t value =
      Log.debug "write";
      let inflated = Misc.with_buffer (fun buf -> Value.add_inflated buf value) in
      let sha1 = SHA.of_string inflated in
      let file = file t sha1 in
      IO.file_exists file >>= function
      | true  -> Log.debug "write: file %s already exists!" file; Lwt.return sha1
      | false ->
        let level = t.level in
        let deflated =
          Misc.deflate_cstruct ~level (Cstruct.of_string inflated)
        in
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir deflated >>= fun () ->
        Lwt.return sha1

    let list { root; _ } =
      Log.debug "Loose.list %s" root;
      let objects = root / ".git" / "objects" in
      IO.directories objects >>= fun objects ->
      let objects = List.map Filename.basename objects in
      let objects = List.filter (fun s -> (s <> "info") && (s <> "pack")) objects in
      Lwt_list.map_s (fun prefix ->
          let dir = root / ".git" / "objects" / prefix in
          IO.files dir >>= fun suffixes ->
          let suffixes = List.map Filename.basename suffixes in
          let objects = List.map (fun suffix ->
              SHA.of_hex (prefix ^ suffix)
            ) suffixes in
          Lwt.return objects
        ) objects
      >>= fun files ->
      Lwt.return (List.concat files)

  end

  module Packed = struct

    module Log = LogMake(struct let section = "fs-packed" end)

    let file { root; _ } sha1 =
      let pack_dir = root / ".git" / "objects" / "pack" in
      let pack_file = "pack-" ^ (SHA.to_hex sha1) ^ ".pack" in
      pack_dir / pack_file

    let packs_opt = ref None

    let list { root; _ } =
      Log.debug "list %s" root;
(*
      let packs = root / ".git" / "objects" / "pack" in
      IO.files packs >>= fun packs ->
      let packs = List.map Filename.basename packs in
      let packs = List.filter (fun f -> Filename.check_suffix f ".idx") packs in
      let packs = List.map (fun f ->
          let p = Filename.chop_suffix f ".idx" in
          let p = String.sub p 5 (String.length p - 5) in
          SHA.of_hex p
        ) packs in
      Lwt.return packs
*)
    match !packs_opt with
    | Some ps -> 
        Log.debug "list cache hit!";
        Lwt.return ps
    | None ->
        let packs = root / ".git" / "objects" / "pack" in
        IO.files packs >>= fun packs ->
          let packs = List.map Filename.basename packs in
          let packs = List.filter (fun f -> Filename.check_suffix f ".idx") packs in
          let packs = List.map (fun f ->
              let p = Filename.chop_suffix f ".idx" in
              let p = String.sub p 5 (String.length p - 5) in
              SHA.of_hex p
            ) packs in
          packs_opt := Some packs;
          Lwt.return packs

    let index { root; _ } sha1 =
      let pack_dir = root / ".git" / "objects" / "pack" in
      let idx_file = "pack-" ^ (SHA.to_hex sha1) ^ ".idx" in
      pack_dir / idx_file

    let index_lru: Pack_index.t LRU.t = LRU.make 8

    let indexes_c = Hashtbl.create 1024

    let read_index_c t sha1 =
      try
        let i = Hashtbl.find indexes_c sha1 in
        Log.debug "read_index_c cache hit!";
        Lwt.return i
      with
        Not_found ->
          let file = index t sha1 in
          IO.file_exists file >>= function
            | true ->
                IO.read_file file >>= fun buf ->
                  let index = new Pack_index.c (Cstruct.to_bigarray buf) in
                  Hashtbl.add indexes_c sha1 index;
                  Lwt.return index
            | false ->
                Log.error "%s does not exist." file;
	        Lwt.fail (Failure "read_index_c")

    let write_pack_index t sha1 idx =
      LRU.add index_lru sha1 idx;
      let file = index t sha1 in
      IO.file_exists file >>= function
      | true  -> Lwt.return_unit
      | false ->
        let buf = Buffer.create 1024 in
        Pack_index.add buf idx;
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir (Cstruct.of_string (Buffer.contents buf))

    let read_pack_index t sha1 =
      Log.debug "read_pack_index %s" (SHA.to_hex sha1);
      match LRU.find index_lru sha1 with
      | Some i -> Lwt.return i
      | None   ->
        Log.debug "read_pack_index: cache miss!";
        let file = index t sha1 in
        IO.file_exists file >>= function
        | true ->
          File_cache.read file >>= fun buf ->
          let buf = Mstruct.of_cstruct buf in
          let index = Pack_index.input buf in
          LRU.add index_lru sha1 index;
          Lwt.return index
        | false ->
          Log.error "%s does not exist." file;
          Lwt.fail (Failure "read_index")

    let keys_lru = LRU.make (128 * 1024)

    let read_keys t sha1 =
      Log.debug "read_keys %s" (SHA.to_hex sha1);
      match LRU.find keys_lru sha1 with
      | Some ks -> Lwt.return ks
      | None    ->
        Log.debug "read_keys: cache miss!";
        let file = index t sha1 in
        IO.file_exists file >>= function
        | true ->
          File_cache.read file >>= fun buf ->
          let keys = Pack_index.keys (Mstruct.of_cstruct buf) in
          LRU.add keys_lru sha1 keys;
          Lwt.return keys
        | false ->
          Lwt.fail (Failure "Git_fs.Packed.read_keys")

    let pack_lru = LRU.make 2

    let read_pack t sha1 =
      Log.debug "read_pack";
      match LRU.find pack_lru sha1 with
      | Some p -> Lwt.return p
      | None   ->
        Log.debug "read_pack: cache miss";
        let file = file t sha1 in
        IO.file_exists file >>= function
        | true ->
          File_cache.read file >>= fun buf ->
          read_pack_index t sha1 >>= fun index ->
          let pack = Pack.Raw.input (Mstruct.of_cstruct buf) ~index:(Some index) in
          let pack = Pack.to_pic pack in
          LRU.add pack_lru sha1 pack;
          Lwt.return pack
        | false ->
          Log.error "No file associated with the pack object %s." (SHA.to_hex sha1);
          Lwt.fail (Failure "read_pack")

    let write_pack t sha1 pack =
      Log.debug "write pack";
      let file = file t sha1 in
      IO.file_exists file >>= function
      | true  -> Lwt.return_unit
      | false ->
        let pack = Misc.with_buffer' (fun buf -> Pack.Raw.add buf pack) in
        let temp_dir = temp_dir t in
        IO.write_file file ~temp_dir pack

    let mem_in_pack t pack_sha1 sha1 =
      Log.debug "mem_in_pack %s:%s" (SHA.to_hex pack_sha1) (SHA.to_hex sha1);
(*
      read_keys t pack_sha1 >>= fun keys ->
      Lwt.return (SHA.Set.mem sha1 keys)
*)
      read_index_c t pack_sha1 >>= fun idx -> 
        Lwt.return (idx#mem sha1)

    let pack_size_thresh = 10000000

    let pack_ba_cache = Hashtbl.create 8

    let cache_pack sha buf =
      let ba = Cstruct.to_bigarray buf in
      let sz = Bigarray.Array1.dim ba in
      if sz < pack_size_thresh then
        Hashtbl.add pack_ba_cache sha ba

    let read_in_pack t pack_sha1 sha1 =
      Log.debug "read_in_pack %s:%s"
        (SHA.to_hex pack_sha1) (SHA.to_hex sha1);
(*
      mem_in_pack t pack_sha1 sha1 >>= function
      | false -> Lwt.return_none
      | true  ->
        read_pack t pack_sha1 >>= fun pack ->
        Lwt.return (Pack.read pack sha1)
*)
    read_index_c t pack_sha1 >>= fun index ->
      if index#mem sha1 then begin
        try
          let pack = Hashtbl.find packs sha1 in
	  Log.debug "read_in_pack pack cache hit!";
	  return (Pack.read pack sha1)
        with
          Not_found -> begin
            try
              let ba = Hashtbl.find pack_ba_cache pack_sha1 in
	      Log.debug "read_in_pack ba cache hit!";
	      let v_opt = Pack.Raw.read (Mstruct.of_bigarray ba) index sha1 in
	      return v_opt
            with
              Not_found -> begin
	        let file = file t pack_sha1 in
                IO.file_exists file >>= function
                  | true ->
	              IO.read_file file >>= fun buf ->
                        cache_pack pack_sha1 buf;
	                let v_opt = Pack.Raw.read (Mstruct.of_cstruct buf) index sha1 in
	                return v_opt
                  | false ->
	              Log.error
	                "No file associated with the pack object %s.\n" (SHA.to_hex pack_sha1);
	              Lwt.fail (Failure "read_in_pack")
              end
          end
      end
      else begin
        Log.debug "read_in_pack: not found";
        return_none
      end

    let read t sha1 =
      list t >>= fun packs ->
      Lwt_list.fold_left_s (fun acc pack ->
          match acc with
          | Some v -> Lwt.return (Some v)
          | None   -> read_in_pack t pack sha1
        ) None packs

    let mem t sha1 =
      list t >>= fun packs ->
      Lwt_list.fold_left_s (fun acc pack ->
          if acc then Lwt.return acc
          else mem_in_pack t pack sha1
        ) false packs

  end

  let list t =
    Log.debug "list";
    Loose.list t  >>= fun objects ->
    Packed.list t >>= fun packs   ->
    Lwt_list.map_p (fun p -> Packed.read_keys t p) packs >>= fun keys ->
    let keys = List.fold_left SHA.Set.union (SHA.Set.of_list objects) keys in
    let keys = SHA.Set.to_list keys in
    Lwt.return keys

  let read t sha1 =
    Log.debug "read %s" (SHA.to_hex sha1);
    match Value.Cache.find sha1 with
    | Some v -> Lwt.return (Some v)
    | None   ->
      Log.debug "read: cache miss!";
      Loose.read t sha1 >>= function
      | Some v -> Value.Cache.add sha1 v; Lwt.return (Some v)
      | None   -> Packed.read t sha1

  let read_exn t sha1 =
    read t sha1 >>= function
    | Some v -> Lwt.return v
    | None   ->
      Log.debug "read_exn: Cannot read %s" (SHA.to_hex sha1);
      Lwt.fail Not_found

  let mem t sha1 =
    match Value.Cache.find sha1 with
    | Some _ -> Lwt.return true
    | None   ->
      Log.debug "mem: cache miss!";
      Loose.mem t sha1 >>= function
      | true  -> Lwt.return true
      | false -> Packed.mem t sha1

  let contents t =
    Log.debug "contents";
    list t >>= fun sha1s ->
    Lwt_list.map_p (fun sha1 ->
        read_exn t sha1 >>= fun value ->
        Lwt.return (sha1, value)
      ) sha1s

  let dump t =
    contents t >>= fun contents ->
    List.iter (fun (sha1, value) ->
        let typ = Value.type_of value in
        Log.error "%s %s" (SHA.to_hex sha1) (Object_type.to_string typ);
      ) contents;
    Lwt.return_unit

  let references t =
    let refs = t.root / ".git" / "refs" in
    IO.rec_files refs >>= fun files ->
    let n = String.length (t.root / ".git" / "") in
    let refs = List.map (fun file ->
        let ref = String.sub file n (String.length file - n) in
        Reference.of_raw ref
      ) files in
    Lwt.return refs

  let file_of_ref t ref = t.root / ".git" / Reference.to_raw ref

  let mem_reference t ref =
    let file = file_of_ref t ref in
    IO.file_exists file

  let remove_reference t ref =
    let file = file_of_ref t ref in
    Lwt.catch
      (fun () -> IO.remove file)
      (fun _ -> Lwt.return_unit)

  let read_reference t ref =
    let file = file_of_ref t ref in
    IO.file_exists file >>= function
    | true ->
      IO.read_file file >>= fun hex ->
      let hex = String.trim (Cstruct.to_string hex) in
      Lwt.return (Some (SHA.Commit.of_hex hex))
    | false ->
      let packed_refs = t.root / ".git" / "packed-refs" in
      IO.file_exists packed_refs >>= function
      | false -> Lwt.return_none
      | true  ->
        IO.read_file packed_refs >>= fun buf ->
        let refs = Packed_refs.input (Mstruct.of_cstruct buf) in
        let sha1 = Packed_refs.find refs ref in
        Lwt.return sha1

  let read_head t =
    let file = file_of_ref t Reference.head in
    IO.file_exists file >>= function
    | true ->
      IO.read_file file >>= fun str ->
      let str = Cstruct.to_string str in
      let contents = match Misc.string_split ~on:' ' str with
        | [sha1]  -> Reference.SHA (SHA.Commit.of_hex sha1)
        | [_;ref] -> Reference.Ref (Reference.of_raw ref)
        | _       ->
          failwith (sprintf "read_head: %s is not a valid HEAD contents" str)
      in
      Lwt.return (Some contents)
    | false ->
      Lwt.return None

  let read_reference_exn t ref =
    read_reference t ref >>= function
    | Some s -> Lwt.return s
    | None   ->
      Log.debug "read_reference_exn: Cannot read %s" (Reference.pretty ref);
      Lwt.fail Not_found

  let write t value =
    Loose.write t value >>= fun sha1 ->
    Log.debug "write -> %s" (SHA.to_hex sha1);
    Value.Cache.add sha1 value;
    Lwt.return sha1

  let write_pack t pack =
    Log.debug "write_pack";
    let sha1 = Pack.Raw.sha1 pack in
    let index = Pack.Raw.index pack in
    Packed.write_pack t sha1 pack   >>= fun () ->
    Packed.write_pack_index t sha1 index >>= fun () ->
    Lwt.return (Pack.Raw.keys pack)

  let write_reference t ref sha1 =
    let file = t.root / ".git" / Reference.to_raw ref in
    let contents = SHA.Commit.to_hex sha1 in
    let temp_dir = temp_dir t in
    IO.write_file file ~temp_dir (Cstruct.of_string contents)

  let write_head t = function
    | Reference.SHA sha1 -> write_reference t Reference.head sha1
    | Reference.Ref ref   ->
      let file = t.root / ".git" / "HEAD" in
      let contents = sprintf "ref: %s" (Reference.to_raw ref) in
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
    Log.debug "load_filesystem head=%s" (SHA.Commit.to_hex head);
    let blobs_c = ref 0 in
    let id = id () in
    let error expected got =
      let str = Printf.sprintf
          "Expecting a %s, got a %s" expected
          (Object_type.pretty (Value.type_of got))
      in
      Log.error "load-filesystem: %s" str;
      Lwt.fail (Failure str)
    in
    let blob mode sha1 k =
      Log.debug "blob %d %s" id (SHA.to_hex sha1);
      assert (mode <> `Dir);
      incr blobs_c;
      read_exn t sha1 >>= function
      | Value.Blob b -> k (Leaf (mode, (SHA.to_blob sha1, b)))
      | obj -> error "blob" obj
    in
    let rec tree mode sha1 k =
      Log.debug "tree %d %s" id (SHA.to_hex sha1);
      assert (mode = `Dir);
      read_exn t sha1 >>= function
      | Value.Tree t -> tree_entries t [] k
      | obj -> error "tree" obj
    and tree_entries trees children k =
      match trees with
      | []   -> k (Node children)
      | e::t ->
        let k n = tree_entries t ((e.Tree.name, n)::children) k in
        match e.Tree.perm with
        | `Dir -> tree `Dir e.Tree.node k
        | mode -> blob mode e.Tree.node k
    in
    let commit sha1 =
      Log.debug "commit %d %s" id (SHA.to_hex sha1);
      read_exn t sha1 >>= function
      | Value.Commit c -> tree `Dir (SHA.of_tree c.Commit.tree) Lwt.return
      | obj -> error "commit" obj
    in
    commit (SHA.of_commit head) >>= fun t ->
    Lwt.return (!blobs_c, t)

  let iter_blobs t ~f ~init =
    load_filesystem t init >>= fun (n, trie) ->
    let i = ref 0 in
    Log.debug "iter_blobs %s" (SHA.Commit.to_hex init);
    iter (fun path (mode, (sha1, blob)) ->
        incr i;
        f (!i, n) (t.root :: path) mode sha1 blob
      ) trie

  let create_file t file mode blob =
    Log.debug "create_file %s" file;
    let blob = Blob.to_raw blob in
    match mode with
    | `Link -> (*q Lwt_unix.symlink file ??? *) failwith "TODO"
    | _     ->
      let temp_dir = temp_dir t in
      IO.write_file file ~temp_dir (Cstruct.of_string blob) >>= fun () ->
      match mode with
      | `Exec -> IO.chmod file 0o755
      | _     -> Lwt.return_unit

  let index_file t = t.root / ".git" / "index"

  let read_index t =
    Log.debug "read_index";
    let file = index_file t in
    IO.file_exists file >>= function
    | false -> Lwt.return Index.empty
    | true  ->
      IO.read_file file >>= fun buf ->
      let buf = Mstruct.of_cstruct buf in
      Lwt.return (Index.input buf)

  let entry_of_file_aux t index file mode sha1 blob =
    IO.realpath file >>= fun file ->
    Log.debug "entry_of_file %s %s" (SHA.Blob.to_hex sha1) file;
    begin
      IO.file_exists file >>= function
      | false ->
        Log.debug "%s does not exist on the filesystem, creating!" file;
        create_file t file mode blob
      | true  ->
        let entry =
          try
            List.find (fun e -> t.root / e.Index.name = file) index.Index.entries
            |> fun x -> Some x
          with Not_found ->
            None
        in
        match entry with
        | None  ->
          Log.debug "%s does not exist in the index, adding!" file;
          (* in doubt, overide the current version -- git will just refuse
             to do anything in that case. *)
          create_file t file mode blob
        | Some e ->
          if e.Index.id <> sha1 then (
            Log.debug "%s has an old version in the index, updating!" file;
            create_file t file mode blob
          ) else
            let stats = IO.stat_info file in
            if e.Index.stats <> stats then (
              (* same thing here, usually Git just stops in that case. *)
              Log.debug "%s has been modified on the filesystem, reversing!" file;
              create_file t file mode blob
            ) else (
              Log.debug "%s: %s unchanged!" (SHA.Blob.to_hex sha1) file;
              Lwt.return_unit
            )
    end >>= fun () ->
    let id = sha1 in
    let stats = IO.stat_info file in
    let stage = 0 in
    match Misc.string_chop_prefix ~prefix:(t.root / "") file with
    | None      -> Lwt.fail (Failure ("entry_of_file: " ^ file))
    | Some name ->
      let entry = { Index.stats; id; stage; name } in
      Lwt.return (Some entry)

  let entry_of_file t index file mode sha1 blob =
    Lwt.catch
      (fun () -> entry_of_file_aux t index file mode sha1 blob)
      (function Failure _ | Sys_error _ -> Lwt.return_none | e -> Lwt.fail e)

  let write_index t ?index head =
    Log.debug "write_index %s" (SHA.Commit.to_hex head);
    let buf = Buffer.create 1024 in
    match index with
    | Some index ->
      Index.add buf index;
      let temp_dir = temp_dir t in
      IO.write_file (index_file t) ~temp_dir
        (Cstruct.of_string (Buffer.contents buf)) >>= fun () ->
      let all = List.length index.Index.entries in
      Log.info "Checking out files: 100%% (%d/%d), done." all all;
      Lwt.return_unit
    | None ->
      let entries = ref [] in
      let all = ref 0 in
      read_index t >>= fun index ->
      Log.info "Checking out files...";
      iter_blobs t ~init:head ~f:(fun (i,n) path mode sha1 blob ->
          all := n;
          let file = String.concat Filename.dir_sep path in
          Log.debug "write_index: %d/%d blob:%s" i n file;
          entry_of_file t index file mode sha1 blob >>= function
          | None   -> Lwt.return_unit
          | Some e -> entries := e :: !entries; Lwt.return_unit
        ) >>= fun () ->
      let index = Index.create !entries in
      let temp_dir = temp_dir t in
      Index.add buf index;
      IO.write_file (index_file t) ~temp_dir
        (Cstruct.of_string (Buffer.contents buf)) >>= fun () ->
      Log.info "Checking out files: 100%% (%d/%d), done." !all !all;
      Lwt.return_unit

  let kind = `Disk

end
