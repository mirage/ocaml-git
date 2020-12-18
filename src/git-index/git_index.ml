[@@@warning "-32"]

open Stdlib
module Bigarray = Bigarray_compat

let io_buffer_size = 65536

type _ hash = SHA1 : Digestif.SHA1.t hash

let module_of : type oid. oid hash -> (module Digestif.S with type t = oid) =
  function
  | SHA1 -> (module Digestif.SHA1)

module Varint = struct
  let decode buffer ~off =
    let p = ref off in
    let c = ref (Char.code (Bigstringaf.get buffer !p)) in
    let r = ref (!c land 127) in

    incr p;

    while !c land 128 <> 0 do
      incr r;
      c := Char.code (Bigstringaf.get buffer !p);
      incr p;
      r := (!r lsl 7) + (!c land 127)
    done;

    !r, !p

  let encode buffer ~off value =
    let res = Bytes.create 16 in
    let p = ref 15 in
    let v = ref value in
    Bytes.set res !p (Char.chr (!v land 127));
    while
      v := !v asr 7;
      !v <> 0
    do
      decr p;
      decr v;
      Bytes.set res !p (Char.chr (128 lor (!v land 127)))
    done;
    Bigstringaf.blit_from_string ~src_off:!p
      (Bytes.unsafe_to_string res)
      ~dst_off:off buffer ~len:(16 - !p);
    16 - !p
end

module Entry = struct
  type time = { sec : int32; nsec : int32 }

  let pp_time ppf t =
    Fmt.pf ppf "{ @[<hov>sec= %ld;@ nsec= %ld;@] }" t.sec t.nsec

  type stat = {
    sd_ctime : time;
    sd_mtime : time;
    sd_dev : int;
    sd_ino : int;
    sd_uid : int;
    sd_gid : int;
    sd_size : int;
  }

  let pp_stat ppf stat =
    Fmt.pf ppf
      "{ @[<hov>sd_ctime= %a;@ sd_mtime= %a;@ sd_dev= %d;@ sd_ino= %d;@ \
       sd_uid= %d;@ sd_gid= %d;@ sd_size= %d;@] }"
      pp_time stat.sd_ctime pp_time stat.sd_mtime stat.sd_dev stat.sd_ino
      stat.sd_uid stat.sd_gid stat.sd_size

  let _ce_stagemask = 0x3000
  let _ce_extended = 0x4000
  let _ce_valid = 0x8000
  let _ce_stageshift = 12
  let _ce_update = 1 lsl 16
  let _ce_remove = 1 lsl 17
  let _ce_uptodate = 1 lsl 18
  let _ce_added = 1 lsl 19
  let _ce_hashed = 1 lsl 20
  let _ce_fsmonitor_valid = 1 lsl 21
  let _ce_wt_remove = 1 lsl 22
  let _ce_conflicted = 1 lsl 23
  let _ce_unpacked = 1 lsl 24
  let _ce_new_skip_worktree = 1 lsl 25
  let _ce_matched = 1 lsl 26
  let _ce_update_in_base = 1 lsl 27
  let _ce_strip_name = 1 lsl 28
  let _ce_intent_to_add = 1 lsl 29
  let _ce_skip_worktree = 1 lsl 30
  let _ce_extended_flags = _ce_intent_to_add lor _ce_skip_worktree
  let _ce_namemask = 0x0fff
  let _ce_extended = 0x4000

  type 'oid t = {
    ce_stat : stat;
    ce_mode : int;
    ce_length : int;
    mutable ce_flags : int;
    oid : 'oid;
    name : Fpath.t;
  }

  let path { name; _ } = name
  let oid { oid; _ } = oid
  let mode { ce_mode; _ } = ce_mode

  let compare a b =
    let res = Fpath.compare a.name b.name in
    if res = 0 then
      let a = (a.ce_flags land _ce_stagemask) lsr _ce_stageshift in
      let b = (b.ce_flags land _ce_stagemask) lsr _ce_stageshift in
      if a < b then -1 else if a > b then 1 else 0
    else res

  let mode_of_unix_kind kind perm =
    match kind with
    | Unix.S_REG ->
        0x8000 lor if perm land 0o100 <> 0 then 0o755 else 0o644
        (* XXX(dinosaure): it seems that Git is able to store
           100600 as a permission of an entry (which update the
           hash of the tree object).

           However, I did not see any usual way to do that with
           Git - and Internet tells me that it's not possible where
           Git handles only two permissions: 755 and 644.

           May be a bug or not, I don't care. *)
    | Unix.S_DIR (* | S_GITLINK *) -> 0o160000
    | Unix.S_CHR -> 0x2000
    | Unix.S_BLK -> 0x3000
    | Unix.S_FIFO -> 0x1000
    | Unix.S_LNK -> 0o120000
    | Unix.S_SOCK -> Fmt.invalid_arg "Git does not handle socket"

  let oid_of_blob :
      type oid. hash:oid hash -> Fpath.t -> (oid, [> Rresult.R.msg ]) result =
   fun ~hash path ->
    let module Hash = (val module_of hash) in
    try
      let ic = open_in_bin (Fpath.to_string path) in
      let tmp = Bytes.create io_buffer_size in
      let ctx = Hash.empty in
      let rec go ctx =
        let len = input ic tmp 0 (Bytes.length tmp) in
        if len = 0 then Hash.get ctx
        else
          let ctx = Hash.feed_bytes ctx tmp ~off:0 ~len in
          go ctx
      in
      let len = in_channel_length ic in
      let ctx = Hash.feed_string ctx (Fmt.str "blob %d\000" len) in
      let hash = go ctx in
      close_in ic;
      Rresult.R.ok hash
    with exn ->
      Rresult.R.error_msgf "%a: %s" Fpath.pp path (Printexc.to_string exn)

  let oid_of_link :
      type oid. hash:oid hash -> Fpath.t -> (oid, [> Rresult.R.msg ]) result =
   fun ~hash path ->
    let module Hash = (val module_of hash) in
    let contents = Unix.readlink (Fpath.to_string path) in
    let contents =
      Astring.String.map (function '\\' -> '/' | c -> c) contents
    in
    let ctx = Hash.empty in
    let ctx =
      Hash.feed_string ctx
        (Fmt.str "blob %d\000%s" (String.length contents) contents)
    in
    Rresult.R.ok (Hash.get ctx)

  let make ~hash path =
    try
      let stat = Unix.lstat (Fpath.to_string path) in
      let ctime_nsec, ctime_sec = Float.modf stat.Unix.st_ctime in
      let ctime_sec = Float.to_int ctime_sec in
      let ctime_nsec = Float.to_int (ctime_nsec *. 1_000_000_000.) in
      let mtime_nsec, mtime_sec = Float.modf stat.Unix.st_mtime in
      let mtime_sec = Float.to_int mtime_sec in
      let mtime_nsec = Float.to_int (mtime_nsec *. 1_000_000_000.) in
      let open Rresult in
      (match stat.Unix.st_kind with
      | Unix.S_DIR -> Fmt.invalid_arg "Git sub-module are not implemented"
      | Unix.S_REG -> oid_of_blob ~hash path
      | Unix.S_LNK -> oid_of_link ~hash path
      | _kind -> Fmt.invalid_arg "Invalid kind")
      >>| fun oid ->
      {
        ce_stat =
          {
            sd_ctime =
              { sec = Int32.of_int ctime_sec; nsec = Int32.of_int ctime_nsec };
            sd_mtime =
              { sec = Int32.of_int mtime_sec; nsec = Int32.of_int mtime_nsec };
            sd_dev = stat.Unix.st_dev;
            sd_ino = stat.Unix.st_ino;
            sd_uid = stat.Unix.st_uid;
            sd_gid = stat.Unix.st_gid;
            sd_size = stat.Unix.st_size;
          };
        ce_mode = mode_of_unix_kind stat.Unix.st_kind stat.Unix.st_perm;
        ce_length = String.length (Fpath.to_string path);
        ce_flags = 0;
        oid;
        name = path;
      }
    with Unix.Unix_error (err, _, _) ->
      Rresult.R.error_msgf "%a: %s" Fpath.pp path (Unix.error_message err)

  let pp ~pp_oid ppf t =
    Fmt.pf ppf
      "{ @[<hov>ce_stat= %a;@ ce_mode= 0o%03o;@ ce_length= %d;@ ce_flags= \
       %08x;@ oid= %a;@ name= %a;@] }"
      pp_stat t.ce_stat t.ce_mode t.ce_length t.ce_flags pp_oid t.oid Fpath.pp
      t.name

  let stage t = (_ce_stagemask land t.ce_flags) asr _ce_stageshift
  let uptodate t = t.ce_flags land _ce_uptodate
  let skip_worktree t = t.ce_flags land _ce_skip_worktree
  let mark_uptodate t = t.ce_flags <- t.ce_flags lor _ce_uptodate
  let intent_to_add t = t.ce_flags land _ce_intent_to_add
  let permissions t = if t.ce_mode land 0o100 <> 0 then 0o755 else 0o644

  let strlen buffer ~off =
    let res = ref off in
    while Bigstringaf.get buffer !res <> '\000' do
      incr res
    done;
    !res

  let _offset_of_data =
    8 (* ctime *)
    + 8 (* mtime *)
    + 4 (* dev *)
    + 4 (* ino *)
    + 4 (* mode *)
    + 4 (* uid *)
    + 4 (* gid *)
    + 4

  (* size *)

  let align_padding_size size len = ((size + len + 8) land lnot 7) - (size + len)
  let align_flex_name len = (_offset_of_data + len + 8) land lnot 7
  let ondisk_cache_entry_size len = align_flex_name len

  let ondisk_data_size ~uid_ln flags len =
    uid_ln + (if flags land _ce_extended <> 0 then 4 else 2) + len

  let ondisk_ce_size ~uid_ln ce =
    ondisk_cache_entry_size (ondisk_data_size ~uid_ln ce.ce_flags ce.ce_length)

  let load :
      type oid.
      version:int ->
      hash:oid hash ->
      ?previous:oid t ->
      off:int ->
      Bigstringaf.t ->
      oid t * int =
   fun ~version ~hash ?previous ~off buffer ->
    let module Hash = (val module_of hash) in
    let ctime =
      {
        sec = Bigstringaf.get_int32_be buffer off;
        nsec = Bigstringaf.get_int32_be buffer (off + 4);
      }
    in
    let mtime =
      {
        sec = Bigstringaf.get_int32_be buffer (off + 8);
        nsec = Bigstringaf.get_int32_be buffer (off + 12);
      }
    in
    let dev = Bigstringaf.get_int32_be buffer (off + 16) in
    let ino = Bigstringaf.get_int32_be buffer (off + 20) in
    let mode = Bigstringaf.get_int32_be buffer (off + 24) in
    let uid = Bigstringaf.get_int32_be buffer (off + 28) in
    let gid = Bigstringaf.get_int32_be buffer (off + 32) in
    let size = Bigstringaf.get_int32_be buffer (off + 36) in

    let flags = Bigstringaf.get_int16_be buffer (off + 40 + Hash.digest_size) in
    let expand_name_field = version = 4 in
    let name_length = flags land _ce_namemask in
    let flags, name_offset =
      if flags land _ce_extended <> 0 then
        ( flags
          lor Bigstringaf.get_int16_be buffer (off + 40 + Hash.digest_size + 2)
              lsl 16,
          off + 40 + Hash.digest_size + 4 )
      else flags, off + 40 + Hash.digest_size + 2
    in

    let name =
      let name_offset, prefix =
        if expand_name_field then
          let strip_length, name_offset =
            Varint.decode buffer ~off:name_offset
          in
          match previous with
          | Some { ce_length; name = previous_name; _ } ->
              if ce_length < strip_length then
                Fmt.invalid_arg
                  "Malformed name field in the index, near path %a" Fpath.pp
                  previous_name;
              ( name_offset,
                String.sub
                  (Fpath.to_string previous_name)
                  0 (ce_length - strip_length) )
          | None -> name_offset, ""
        else name_offset, ""
      in
      let name_length =
        if name_length = _ce_namemask then strlen buffer ~off:name_offset
        else name_length
      in
      prefix ^ Bigstringaf.substring buffer ~off:name_offset ~len:name_length
    in
    let oid =
      Bigstringaf.substring buffer ~off:(off + 40) ~len:Hash.digest_size
    in
    let oid = Hash.of_raw_string oid in

    let ce =
      {
        ce_stat =
          {
            sd_ctime = ctime;
            sd_mtime = mtime;
            sd_dev = Int32.to_int dev;
            sd_ino = Int32.to_int ino;
            sd_uid = Int32.to_int uid;
            sd_gid = Int32.to_int gid;
            sd_size = Int32.to_int size;
          };
        ce_mode = Int32.to_int mode;
        ce_flags = flags land lnot _ce_namemask;
        ce_length = String.length name;
        oid;
        name = Fpath.v name;
      }
    in
    let entry_size = ondisk_ce_size ~uid_ln:Hash.digest_size ce in
    ce, off + entry_size

  let load ~version ~hash ?previous ~off buffer =
    let open Rresult in
    try R.ok (load ~version ~hash ?previous ~off buffer) with
    | Invalid_argument err -> R.error_msg err
    | exn -> raise exn

  let padding = Bigstringaf.of_string ~off:0 ~len:1 "\x00"

  let store :
      type oid.
      version:int ->
      hash:oid hash ->
      ?previous:oid t ->
      oid t ->
      Bigstringaf.t list =
   fun ~version:_ ~hash ?previous ce ->
    let module Hash = (val module_of hash) in
    let size =
      _offset_of_data + ondisk_data_size ~uid_ln:Hash.digest_size ce.ce_flags 0
    in
    let ce_payload = Bigstringaf.create (40 + Hash.digest_size + 4) in
    Bigstringaf.set_int32_be ce_payload 0 ce.ce_stat.sd_ctime.sec;
    Bigstringaf.set_int32_be ce_payload 4 ce.ce_stat.sd_ctime.nsec;
    Bigstringaf.set_int32_be ce_payload 8 ce.ce_stat.sd_mtime.sec;
    Bigstringaf.set_int32_be ce_payload 12 ce.ce_stat.sd_mtime.nsec;
    Bigstringaf.set_int32_be ce_payload 16 (Int32.of_int ce.ce_stat.sd_dev);
    Bigstringaf.set_int32_be ce_payload 20 (Int32.of_int ce.ce_stat.sd_ino);
    Bigstringaf.set_int32_be ce_payload 24 (Int32.of_int ce.ce_mode);
    Bigstringaf.set_int32_be ce_payload 28 (Int32.of_int ce.ce_stat.sd_uid);
    Bigstringaf.set_int32_be ce_payload 32 (Int32.of_int ce.ce_stat.sd_gid);
    Bigstringaf.set_int32_be ce_payload 36 (Int32.of_int ce.ce_stat.sd_size);
    Bigstringaf.blit_from_string
      (Hash.to_raw_string ce.oid)
      ~src_off:0 ce_payload ~dst_off:40 ~len:Hash.digest_size;
    let flags = ce.ce_flags land lnot _ce_namemask in
    let flags =
      flags
      lor if ce.ce_length >= _ce_namemask then _ce_namemask else ce.ce_length
    in
    Bigstringaf.set_int16_be ce_payload (40 + Hash.digest_size)
      (flags land 0xffff);
    let prelude =
      if ce.ce_flags land _ce_extended <> 0 then (
        Bigstringaf.set_int16_be ce_payload
          (40 + Hash.digest_size + 2)
          (flags asr 16);
        ce_payload)
      else Bigstringaf.sub ce_payload ~off:0 ~len:(40 + Hash.digest_size + 2)
    in

    match previous with
    | None ->
        let name = Fpath.to_string ce.name in
        let name =
          Bigstringaf.of_string ~off:0 ~len:(String.length name) name
        in
        let padding =
          String.make (align_padding_size size ce.ce_length) '\x00'
        in
        let padding =
          Bigstringaf.of_string ~off:0 ~len:(String.length padding) padding
        in
        [ prelude; name; padding ]
    | Some previous ->
        let common = ref 0 in
        let a = Fpath.to_string ce.name in
        let b = Fpath.to_string previous.name in
        while
          !common < ce.ce_length
          && !common < previous.ce_length
          && a.[!common] = b.[!common]
        do
          incr common
        done;
        let common = !common in
        let to_remove = previous.ce_length - common in
        let prefix_size = Bigstringaf.create 16 in
        let len = Varint.encode prefix_size ~off:0 to_remove in
        let prefix_size = Bigstringaf.sub prefix_size ~off:0 ~len in
        [
          prelude; prefix_size;
          Bigstringaf.of_string ~off:common ~len:(ce.ce_length - common) a;
          padding;
        ]
end

type 'oid t = { mutable entries : 'oid Entry.t array; version : int }

let empty_index_file :
    type oid. version:int -> hash:oid hash -> Bigstringaf.t * oid =
 fun ~version ~hash ->
  let module Hash = (val module_of hash) in
  let res = Bigstringaf.create (12 + Hash.digest_size) in
  Bigstringaf.set_int32_be res 0 0x44495243l;
  Bigstringaf.set_int32_be res 4 (Int32.of_int version);
  Bigstringaf.set_int32_be res 8 0l;
  let hash = Hash.digest_bigstring (Bigstringaf.sub ~off:0 ~len:12 res) in
  let hash = Hash.to_raw_string hash in
  Bigstringaf.blit_from_string hash ~src_off:0 res ~dst_off:12
    ~len:Hash.digest_size;
  res, Hash.of_raw_string hash

let make : type oid. ?version:int -> oid hash -> oid t =
 fun ?(version = 2) _ -> { entries = [||]; version }

let exists t path =
  let rec go n =
    if n >= Array.length t.entries then false
    else if Fpath.equal t.entries.(n).name path then true
    else go (succ n)
  in
  go 0

let find t path =
  let rec go n =
    if n >= Array.length t.entries then None
    else if Fpath.equal t.entries.(n).name path then Some t.entries.(n)
    else go (succ n)
  in
  go 0

let pos_of_entry t path =
  let rec go first last =
    if last > first then
      let next = first + ((last - first) lsr 1) in
      let cmp =
        let a = Fpath.to_string path
        and b = Fpath.to_string t.entries.(next).name in
        let alen = String.length a and blen = String.length b in
        let m = min alen blen in
        let v = String.(compare (sub a 0 m) (sub b 0 m)) in
        if v <> 0 then v
        else if alen < blen then -1
        else if alen > blen then 1
        else 0
      in
      if cmp = 0 then next
      else if cmp < 0 then go first next
      else go (next + 1) last
    else -first - 1
  in
  go 0 (Array.length t.entries)

let replace t entry =
  if not (exists t entry.Entry.name) then Fmt.invalid_arg "Git_index.replace";
  let rec go n =
    if Fpath.equal t.entries.(n).Entry.name entry.Entry.name then
      t.entries.(n) <- entry
    else go (succ n)
  in
  go 0

let add :
    type oid.
    hash:oid hash -> Fpath.t -> oid t -> (unit, [> Rresult.R.msg ]) result =
 fun ~hash path t ->
  let open Rresult in
  Entry.make ~hash path >>= fun entry ->
  (* entry.ce_flags <- entry.ce_flags lor Entry._ce_intent_to_add ;
     XXX(dinosaure): [CE_INTENT_TO_ADD] adds [M] into [git status --porcelain] *)
  match find t path with
  | Some _ ->
      replace t entry;
      R.ok ()
  | None ->
      let pos = pos_of_entry t path in
      let pos =
        if pos >= 0 then
          (* XXX(dinosaure): when [pos >= 0], it means that [path] already exists.
             See [add_index_entry_with_check]. *)
          pos
        else -pos - 1
      in
      let init entry i =
        if i < pos then t.entries.(i)
        else if i = pos then entry
        else t.entries.(i - 1)
      in
      let res = Array.init (Array.length t.entries + 1) (init entry) in
      t.entries <- res;
      R.ok ()

let rem : Fpath.t -> 'oid t -> unit =
 fun path t ->
  let pos = pos_of_entry t path in
  let pos = if pos < 0 then -pos - 1 else pos in
  let rec go pos =
    if
      pos < Array.length t.entries
      && Fpath.compare t.entries.(pos).name path = 0
    then (
      let res =
        Array.init
          (Array.length t.entries - 1)
          (fun i -> if i < pos then t.entries.(i) else t.entries.(i + 1))
      in
      t.entries <- res;
      go pos)
  in
  go pos

(* XXX(dinosaure): not sure about [-1]. *)

let ( <.> ) f g x = f (g x)

type 'oid elt = [ `Tree of Fpath.t | `Blob of 'oid Entry.t ]

let create_graph : 'oid t -> ([ 'oid elt | `Root ], 'oid elt list) Hashtbl.t =
 fun t ->
  let tbl = Hashtbl.create ~random:false (Array.length t.entries) in
  let put tbl k v =
    try
      let vs = Hashtbl.find tbl k in
      if
        not
          (List.exists
             (fun v' ->
               match v, v' with
               | `Tree p, `Tree p' -> Fpath.equal p p'
               | `Blob e, `Blob e' -> Fpath.equal e.Entry.name e'.Entry.name
               | _ -> false)
             vs)
      then Hashtbl.replace tbl k (v :: vs)
    with Not_found -> Hashtbl.add tbl k [ v ]
  in
  Hashtbl.add tbl `Root [];
  let insert ({ Entry.name; _ } as entry) =
    match Fpath.segs name with
    | [] -> () (* XXX(dinosaure): or [assert false]. *)
    | [ _ ] ->
        Hashtbl.add tbl (`Blob entry) [];
        put tbl `Root (`Blob entry)
    | segs ->
        let segs = (List.rev <.> List.tl <.> List.rev) segs in
        let bottom =
          List.fold_left
            (function
              | `Root ->
                  fun seg ->
                    put tbl `Root (`Tree (Fpath.v seg));
                    `Tree (Fpath.v seg)
              | `Tree tree as bottom ->
                  fun seg ->
                    put tbl bottom (`Tree Fpath.(tree / seg));
                    `Tree Fpath.(tree / seg))
            `Root segs
        in
        put tbl
          (bottom :> [ `Blob of _ Entry.t | `Tree of Fpath.t | `Root ])
          (`Blob entry);
        Hashtbl.add tbl (`Blob entry) []
  in
  Array.iter insert t.entries;
  tbl

let sort :
    ([ `Root | 'oid elt ], 'oid elt list) Hashtbl.t ->
    ([ `Root | 'oid elt ] list, [> Rresult.R.msg ]) result =
 fun tbl ->
  let find_nodes tbl =
    let fold k vs acc = match vs with [] -> k :: acc | _ -> acc in
    Hashtbl.fold fold tbl []
  in
  let remove_nodes nodes tbl = List.iter (Hashtbl.remove tbl) nodes in
  let remove_dep tbl dep =
    let go dep tbl k =
      let deps = Hashtbl.find tbl k in
      let deps =
        if List.exists (( = ) dep) deps then List.filter (( <> ) dep) deps
        else deps
      in
      Hashtbl.remove tbl k;
      Hashtbl.add tbl k deps
    in
    let ks = Hashtbl.fold (fun k _ a -> k :: a) tbl [] in
    List.iter (go dep tbl) ks
  in
  let rec go deps tbl acc =
    match deps with
    | [] -> acc
    | `Root :: deps ->
        let nodes = find_nodes tbl in
        remove_nodes nodes tbl;
        go (List.append deps nodes) tbl (List.append acc nodes)
    | (#elt as dep) :: deps ->
        remove_dep tbl dep;
        let nodes = find_nodes tbl in
        remove_nodes nodes tbl;
        go (List.append deps nodes) tbl (List.append acc nodes)
  in
  let bases = find_nodes tbl in
  remove_nodes bases tbl;
  let sorted_ks = go bases tbl [] in
  let sorted_ks = List.append bases sorted_ks in
  let remaining = Hashtbl.fold (fun k _ a -> k :: a) tbl [] in
  let pp_v ppf = function
    | `Root -> Fmt.pf ppf "<root>"
    | `Tree p -> Fmt.pf ppf "<tree:%a>" Fpath.pp p
    | `Blob { Entry.name = p; _ } -> Fmt.pf ppf "<blob:%a>" Fpath.pp p
  in
  match remaining with
  | [] -> Ok sorted_ks
  | _ ->
      Rresult.R.error_msgf "Git_index.sort: cycle (remaining; @[<hov>%a@])"
        Fmt.(Dump.hashtbl pp_v (Dump.list pp_v))
        tbl

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let fold :
    type oid.
    f:
      ([ oid elt | `Root ] ->
      oid elt list ->
      'a ->
      ('a, ([> Rresult.R.msg ] as 'err)) result Lwt.t) ->
    'a ->
    oid t ->
    ('a, 'err) result Lwt.t =
 fun ~f acc t ->
  let graph = create_graph t in

  let rec go acc = function
    | [] -> Lwt.return_ok acc
    | `Root :: rest ->
        let vs = Option.value (Hashtbl.find_opt graph `Root) ~default:[] in
        f `Root vs acc >>? fun acc -> go acc rest
    | (`Tree _ as tree) :: rest ->
        let vs = Option.value (Hashtbl.find_opt graph tree) ~default:[] in
        f tree vs acc >>? fun acc -> go acc rest
    | (`Blob _ as blob) :: rest -> f blob [] acc >>? fun acc -> go acc rest
  in
  match sort (Hashtbl.copy graph) with
  | Ok lst -> go acc lst
  | Error err -> Lwt.return_error err

let load_header buffer =
  let open Rresult in
  if Bigstringaf.get_int32_be buffer 0 <> 0x44495243l then
    R.error_msgf "Invalid DIRC file (%08lx)" (Bigstringaf.get_int32_be buffer 0)
  else
    let version = Int32.to_int (Bigstringaf.get_int32_be buffer 4) in
    if version < 2 || version > 4 then R.error_msgf "Invalid version of DIRC"
    else
      let entries = Int32.to_int (Bigstringaf.get_int32_be buffer 8) in
      R.ok (version, entries)

let ( .%[] ) : string -> int -> int32 =
 fun str off -> Int32.of_int (Char.code str.[off])

let load_extension ~off buffer =
  let _size = Int32.to_int (Bigstringaf.get_int32_be buffer (off + 4)) in
  let ext =
    let str = Bigstringaf.substring buffer ~off ~len:4 in
    let ( lsl ) = Int32.shift_left and ( lor ) = Int32.logor in
    (str.%[0] lsl 24) lor (str.%[1] lsl 16) lor (str.%[2] lsl 8) lor str.%[3]
  in
  match ext with
  | 0x54524545l -> assert false
  | ext -> Fmt.invalid_arg "Invalid or unsupported extension: %08lx" ext

let load :
    type oid. hash:oid hash -> Fpath.t -> (oid t, [> Rresult.R.msg ]) result =
 fun ~hash path ->
  let open Rresult in
  let module Hash = (val module_of hash) in
  let load path =
    try
      let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o600 in
      let stat = Unix.fstat fd in
      let mmap =
        Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout false
          [| stat.Unix.st_size |]
      in
      Unix.close fd;
      Ok mmap
    with
    | Unix.Unix_error (err, _, _) ->
        R.error_msgf "Git_index.load: %s" (Unix.error_message err)
    | exn -> raise exn
  in
  load path >>= fun mmap ->
  let mmap = Bigarray.array1_of_genarray mmap in
  load_header mmap >>= fun (version, nr) ->
  let rec go i off acc =
    if i <= 0 then R.ok (List.rev acc, off)
    else
      match acc with
      | previous :: _ ->
          Entry.load ~version ~hash ~previous ~off mmap >>= fun (entry, off) ->
          go (pred i) off (entry :: acc)
      | [] ->
          Entry.load ~version ~hash ~off mmap >>= fun (entry, off) ->
          go (pred i) off (entry :: acc)
  in
  go nr 12 [] >>= fun (entries, _off) ->
  let oid0 =
    Hash.digest_bigstring ~off:0
      ~len:(Bigstringaf.length mmap - Hash.digest_size)
      mmap
  in
  let oid1 =
    let off = Bigstringaf.length mmap - Hash.digest_size in
    let len = Hash.digest_size in
    Hash.of_raw_string (Bigstringaf.substring mmap ~off ~len)
  in
  if Hash.equal oid0 oid1 then R.ok { entries = Array.of_list entries; version }
  else R.error_msgf "Invalid hash (%a <> %a)" Hash.pp oid0 Hash.pp oid1

let store :
    type fd oid.
    hash:oid hash -> append:(fd -> Bigstringaf.t -> fd) -> fd -> oid t -> fd =
 fun ~hash ~append fd t ->
  let module Hash = (val module_of hash) in
  let extended =
    let v = ref false in
    for i = 0 to Array.length t.entries - 1 do
      t.entries.(i).ce_flags <-
        t.entries.(i).ce_flags land lnot Entry._ce_extended;

      if t.entries.(i).ce_flags land Entry._ce_extended_flags <> 0 then (
        v := true;
        t.entries.(i).ce_flags <- t.entries.(i).ce_flags lor Entry._ce_extended)
    done;
    !v
  in
  let version = if extended then 3 else 2 in
  let header = Bigstringaf.create 12 in
  Bigstringaf.set_int32_be header 0 0x44495243l;
  Bigstringaf.set_int32_be header 4 (Int32.of_int version);
  Bigstringaf.set_int32_be header 8 (Int32.of_int (Array.length t.entries));
  let rec go fd ctx i =
    if i >= Array.length t.entries then fd, ctx
    else
      let entry = t.entries.(i) in
      let previous =
        if version < 4 || i = 0 then None else Some t.entries.(i - 1)
      in
      let payloads = Entry.store ~version ~hash ?previous entry in
      let fd = List.fold_left append fd payloads in
      let ctx = Hash.feedi_bigstring ctx (fun f -> List.iter f payloads) in
      go fd ctx (succ i)
  in
  let ctx = Hash.empty in
  let fd = append fd header in
  let ctx = Hash.feed_bigstring ctx header in
  let fd, ctx = go fd ctx 0 in
  let hash = Hash.get ctx in
  let hash = Hash.to_raw_string hash in
  append fd (Bigstringaf.of_string ~off:0 ~len:Hash.digest_size hash)

let store_to_path :
    type oid.
    hash:oid hash -> Fpath.t -> oid t -> (unit, [> Rresult.R.msg ]) result =
 fun ~hash path t ->
  try
    let fd =
      Unix.openfile (Fpath.to_string path)
        Unix.[ O_WRONLY; O_CREAT; O_TRUNC ]
        0o600
    in
    let append fd buf =
      let rec go fd buf off len =
        if len > 0 then
          let str = Bigstringaf.substring ~off ~len buf in
          let len' = Unix.write fd (Bytes.unsafe_of_string str) 0 len in
          go fd buf (off + len') (len - len')
      in
      go fd buf 0 (Bigstringaf.length buf);
      fd
    in
    let _ = store ~hash ~append fd t in
    Unix.close fd;
    Rresult.R.ok ()
  with Unix.Unix_error (err, _, _) ->
    Rresult.R.error_msgf "Git_index.store_to_path %a: %s" Fpath.pp path
      (Unix.error_message err)
