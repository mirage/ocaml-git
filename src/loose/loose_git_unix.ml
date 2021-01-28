open Lwt.Infix

type error = |

module type UID = sig
  include Loose.UID

  val of_hex : string -> t
end

module Make (Uid : UID) = struct
  let split uid =
    let hex = Uid.to_hex uid in
    String.sub hex 0 2, String.sub hex 2 ((Uid.length * 2) - 2)

  let unix_store_map root uid ~pos len =
    if pos < 0L || len < 0 then invalid_arg "unix_store_map: invalid bounds";
    let hd, tl = split uid in
    let path = Fpath.(root / "objects" / hd / tl) in

    let process () =
      Lwt_unix.LargeFile.stat (Fpath.to_string path) >>= fun stat ->
      let len =
        if Int64.add pos (Int64.of_int len) > stat.Lwt_unix.LargeFile.st_size
        then Int64.to_int (Int64.sub stat.Lwt_unix.LargeFile.st_size pos)
        else len
      in
      let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
      let rs =
        Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
      in
      Unix.close fd;
      Lwt.return (Bigarray.array1_of_genarray rs)
    in

    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return Bigstringaf.empty
      | exn -> Lwt.fail exn
    in

    (* TODO(dinosaure): [EINTR]? *)
    Lwt.catch process error

  let unix_store_mem root uid =
    let hd, tl = split uid in
    let path = Fpath.(root / "objects" / hd / tl) in
    let process () =
      let _ = Lwt_unix.stat (Fpath.to_string path) in
      Lwt.return true
    in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return false
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error

  let unix_store_append root uid payload =
    let hd, tl = split uid in
    let path = Fpath.(root / "objects" / hd / tl) in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path) Unix.[ O_WRONLY ] 0o644
      >>= fun fd ->
      let rec go off len =
        Lwt_bytes.write fd payload off len >>= fun len' ->
        if len - len' = 0 then Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
        else go (off + len') (len - len')
      in
      go 0 (Bigstringaf.length payload)
    in
    let error = Lwt.fail in
    Lwt.catch process error

  let unix_store_appendv root uid payloads =
    let hd, tl = split uid in
    let path = Fpath.(root / "objects" / hd / tl) in
    let payloads =
      List.map (fun payload -> payload, 0, Bigstringaf.length payload) payloads
    in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path) Unix.[ O_WRONLY ] 0o644
      >>= fun fd ->
      let rec go = function
        | [] -> Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
        | (payload, off, len) :: rest ->
            Lwt_bytes.write fd payload off len >>= fun len' ->
            if len - len' = 0 then go rest
            else go ((payload, off + len', len - len') :: rest)
      in
      go payloads
    in
    let error = Lwt.fail in
    Lwt.catch process error

  (* fold *)

  let always x _ = x
  let failwithf fmt = Fmt.kstr Lwt.fail_with fmt

  let contents ?(dotfiles = false) ?(rel = false) dir =
    let rec readdir dh acc =
      Lwt.catch
        (fun () -> Lwt_unix.readdir dh >>= Lwt.return_some)
        (fun _exn -> Lwt.return_none)
      >>= function
      | None -> Lwt.return acc
      | Some (".." | ".") -> readdir dh acc
      | Some f when dotfiles || not (f.[0] = '.') -> (
          match Fpath.of_string f with
          | Ok f -> readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
          | Error (`Msg _) ->
              failwithf
                "Directory contents %a: cannot parse element to a path (%S)"
                Fpath.pp dir f)
      | Some _ -> readdir dh acc
    in
    Lwt.catch
      (fun () ->
        Lwt_unix.opendir (Fpath.to_string dir) >>= fun dh ->
        readdir dh [] >>= fun res ->
        Lwt_unix.closedir dh >>= fun () -> Lwt.return res)
      (function
        | Unix.Unix_error (err, _, _) ->
            failwithf "Directory contents %a: %s" Fpath.pp dir
              (Unix.error_message err)
        | exn -> Lwt.fail exn)

  let do_traverse_fun = function
    | `Any -> always true
    | `None -> always false
    | `Sat sat -> sat

  let exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun _ -> Lwt.return_true)
      (fun _exn -> Lwt.return_false)

  let file_exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_REG))
      (fun _exn -> Lwt.return_false)

  let dir_exists path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat (Fpath.to_string path) >>= fun stat ->
        Lwt.return (stat.Unix.st_kind = Unix.S_DIR))
      (fun _exn -> Lwt.return_false)

  let is_element_fun = function
    | `Any -> exists
    | `Files -> file_exists
    | `Dirs -> dir_exists
    | `Sat sat -> sat

  let readdir_fun path =
    Lwt_unix.opendir (Fpath.to_string path) >>= fun dh ->
    let rec go acc =
      Lwt.catch
        (fun () -> Lwt_unix.readdir dh >>= fun entry -> go (entry :: acc))
        (fun _exn -> Lwt.return (List.rev acc))
    in
    go [] >>= fun res ->
    Lwt_unix.closedir dh >>= fun () -> Lwt.return res

  let fold ?(dotfiles = false) ?(elements = `Any) ?(traverse = `Any) f acc paths
      =
    let process () =
      let do_traverse = do_traverse_fun traverse in
      let is_element = is_element_fun elements in
      let is_dir = dir_exists in
      let readdir = readdir_fun in
      let process_path p (acc, to_traverse) =
        Lwt.both (is_element p) (is_dir p) >>= function
        | false, true when do_traverse p -> Lwt.return (acc, p :: to_traverse)
        | true, true when do_traverse p ->
            Lwt.both (f p acc) (Lwt.return (p :: to_traverse))
        | true, _ -> Lwt.both (f p acc) (Lwt.return to_traverse)
        | _ -> Lwt.return (acc, to_traverse)
      in
      let dir_child d acc bname =
        if (not dotfiles) && bname.[0] = '.' then Lwt.return acc
        else process_path Fpath.(d / bname) acc
      in
      let rec loop acc = function
        | (d :: ds) :: up ->
            readdir d >>= fun childs ->
            Lwt_list.fold_left_s (dir_child d) (acc, []) childs
            >>= fun (acc, to_traverse) -> loop acc (to_traverse :: ds :: up)
        | [ [] ] -> Lwt.return acc
        | [] :: up -> loop acc up
        | _ -> assert false
      in
      let init acc p =
        let base = Fpath.(basename @@ normalize p) in
        if (not dotfiles) && base.[0] = '.' then Lwt.return acc
        else process_path p acc
      in
      Lwt_list.fold_left_s init (acc, []) paths >>= fun (acc, to_traverse) ->
      loop acc [ to_traverse ]
    in
    process ()

  let fold ?dotfiles ?elements ?traverse f acc d =
    contents d >>= fold ?dotfiles ?elements ?traverse f acc

  let elements path =
    match List.rev (Fpath.segs path) with
    | tl :: hd :: _ -> (
        match Uid.of_hex (hd ^ tl) with
        | _ -> file_exists path
        | exception _ -> Lwt.return_false)
    | _ -> Lwt.return_false

  (* fold *)

  let unix_store_list root =
    let f x r =
      match List.rev (Fpath.segs x) with
      | tl :: hd :: _ ->
          let uid = Uid.of_hex (hd ^ tl) in
          Lwt.return (uid :: r)
      | _ -> assert false
    in
    fold ~dotfiles:false ~elements:(`Sat elements) f [] root

  let unix_store : (_, _, error, _) Loose.store =
    {
      Loose.map =
        (fun t brk ~pos len -> Carton_lwt.inj (unix_store_map t brk ~pos len));
      Loose.mem = (fun t brk -> Carton_lwt.inj (unix_store_mem t brk));
      Loose.append = (fun t brk v -> Carton_lwt.inj (unix_store_append t brk v));
      Loose.appendv =
        (fun t brk vs -> Carton_lwt.inj (unix_store_appendv t brk vs));
      Loose.list = (fun t -> Carton_lwt.inj (unix_store_list t));
    }

  let space = Cstruct.of_string " "
  let zero = Cstruct.of_string "\000"

  let cut ~sep:({ Cstruct.len = sep_len; _ } as sep) ({ Cstruct.len; _ } as t) =
    if sep_len = 0 then invalid_arg "cut: empty separator";
    let max_sep_zidx = sep_len - 1 in
    let max_t_zidx = len - sep_len in
    let rec check_sep i k =
      if k > max_sep_zidx then
        Some (Cstruct.sub t 0 i, Cstruct.sub t (i + sep_len) (len - sep_len - i))
      else if Cstruct.get_char t (i + k) = Cstruct.get_char sep k then
        check_sep i (succ k)
      else scan (succ i)
    and scan i =
      if i > max_t_zidx then None
      else if Cstruct.get_char t i = Cstruct.get_char sep 0 then check_sep i 1
      else scan (succ i)
    in
    scan 0

  let hdr_get raw =
    match cut ~sep:space raw with
    | None -> failwith "Invalid Git header"
    | Some (kind, rest) -> (
        match cut ~sep:zero rest with
        | Some (length, contents) ->
            let length = Int64.of_string (Cstruct.to_string length) in
            let kind =
              match Cstruct.to_string kind with
              | "commit" -> `A
              | "blob" -> `C
              | "tag" -> `D
              | "tree" -> `B
              | v -> Fmt.failwith "Invalid type of Git object: %s" v
            in
            contents, kind, length
        | None -> failwith "Invalid Git header")

  let hdr_set ~buffer (kind, length) =
    let kind =
      match kind with
      | `Commit -> "commit"
      | `Tree -> "tree"
      | `Blob -> "blob"
      | `Tag -> "tag"
    in
    Cstruct.blit_from_string kind 0 buffer 0 (String.length kind);
    Cstruct.set_char buffer (String.length kind) ' ';
    let length = Int64.to_string length in
    Cstruct.blit_from_string length 0 buffer
      (String.length kind + 1)
      (String.length length);
    Cstruct.set_char buffer
      (String.length kind + 1 + String.length length)
      '\000';
    Cstruct.sub buffer 0 (String.length kind + 1 + String.length length + 1)

  include Loose_lwt.Make (Uid)

  let exists t uid = exists t unix_store uid

  let atomic_add t buffers v =
    let hdr_set ~buffer v =
      let kind =
        match Carton.Dec.kind v with
        | `A -> `Commit
        | `B -> `Tree
        | `C -> `Blob
        | `D -> `Tag
      in
      let length = Int64.of_int (Carton.Dec.len v) in
      hdr_set ~buffer (kind, length)
    in
    atomic_add t buffers unix_store ~hdr:hdr_set v

  let add t buffers (kind, length) stream =
    let hdr = hdr_set ~buffer:(Cstruct.create 30) (kind, length) in
    add t buffers unix_store ~hdr stream

  let atomic_get t buffer uid = atomic_get t buffer unix_store ~hdr:hdr_get uid

  let size_and_kind t buffers uid =
    size_and_kind t buffers unix_store ~hdr:hdr_get uid

  let get t buffer uid = get t buffer unix_store ~hdr:hdr_get uid
end
