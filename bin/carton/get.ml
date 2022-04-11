let failwith fmt = Fmt.kstr failwith fmt

module SHA1 = struct
  include Digestif.SHA1

  let feed ctx ?off ?len bs = feed_bigstring ctx ?off ?len bs
  let null = digest_string ""
  let length = digest_size
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

let load_idx fpath =
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let ln = (Unix.fstat fd).Unix.st_size in
  let mp =
    Unix.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout false [| ln |]
  in
  let mp = Bigarray.array1_of_genarray mp in
  Unix.close fd;
  Carton.Dec.Idx.make mp ~uid_ln:SHA1.length ~uid_rw:SHA1.to_raw_string
    ~uid_wr:SHA1.of_raw_string

let z = De.bigstring_create De.io_buffer_size
let allocate bits = De.make_window ~bits

let map fd ~pos len =
  let max = (Unix.LargeFile.fstat fd).Unix.LargeFile.st_size in
  let len = min (Int64.sub max pos) (Int64.of_int len) in
  let len = Int64.to_int len in
  let res =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray res

type cfg = Hex of { hxd : Hxd.cfg; path : bool; info : bool } | Raw

let show_path path =
  let lst = Carton.Dec.path_to_list path in
  let arr = Array.of_list lst in
  Format.printf "path:   ";
  for i = 0 to Array.length arr - 1 do
    if i > 0 then Format.printf "        ";
    Format.printf "%8Lx\n" arr.(i)
  done

let pp_kind ppf = function
  | `A -> Format.pp_print_string ppf "a"
  | `B -> Format.pp_print_string ppf "b"
  | `C -> Format.pp_print_string ppf "c"
  | `D -> Format.pp_print_string ppf "d"

let show_info v =
  Format.printf "depth:  %8d\n%!" (Carton.Dec.depth v);
  Format.printf "length: %8d\n%!" (Carton.Dec.len v);
  Format.printf "kind:          %a\n%!" pp_kind (Carton.Dec.kind v)

let show cfg path v =
  match cfg with
  | Raw ->
      let len = Carton.Dec.len v in
      let raw = Bigstringaf.substring (Carton.Dec.raw v) ~off:0 ~len in
      print_string raw
  | Hex { hxd; path = p; info = i } ->
      let len = Carton.Dec.len v in
      let raw = Bigstringaf.substring (Carton.Dec.raw v) ~off:0 ~len in
      if p then show_path path;
      if i then show_info v;
      if p || i then Format.printf "\n%!";
      Format.printf "%a%!" (Hxd_string.pp hxd) raw;
      Format.printf "%!"

let get_object_from_uid cfg fpath uid =
  let open Rresult in
  let open Bos in
  OS.File.must_exist (Fpath.set_ext "idx" fpath) >>| load_idx >>= fun idx ->
  let find uid =
    match Carton.Dec.Idx.find idx uid with
    | Some (_crc, offset) -> offset
    | None -> failwith "object %a not found" SHA1.pp uid
  in
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let pack =
    Carton.Dec.make fd ~z ~allocate ~uid_ln:SHA1.length
      ~uid_rw:SHA1.of_raw_string find
  in
  let path = Carton.Dec.path_of_uid ~map pack uid in
  let weight = Carton.Dec.weight_of_uid ~map pack ~weight:Carton.Dec.null uid in
  let raw = Carton.Dec.make_raw ~weight in
  let v = Carton.Dec.of_uid ~map pack raw uid in
  show cfg path v;
  Ok ()

let never _ = assert false

let get_object_from_offset cfg fpath offset =
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let pack =
    Carton.Dec.make fd ~z ~allocate ~uid_ln:SHA1.length
      ~uid_rw:SHA1.of_raw_string never
  in
  let path = Carton.Dec.path_of_offset ~map pack ~cursor:offset in
  let weight =
    Carton.Dec.weight_of_offset ~map pack ~weight:Carton.Dec.null offset
  in
  let raw = Carton.Dec.make_raw ~weight in
  let v = Carton.Dec.of_offset ~map pack raw ~cursor:offset in
  show cfg path v;
  Ok ()

let run hxd path info raw pack uid_or_offset =
  match raw, uid_or_offset with
  | true, `Uid uid -> get_object_from_uid Raw pack uid
  | true, `Offset ofs -> get_object_from_offset Raw pack ofs
  | false, `Uid uid -> get_object_from_uid (Hex { hxd; path; info }) pack uid
  | false, `Offset ofs ->
      get_object_from_offset (Hex { hxd; path; info }) pack ofs

let run hxd path info raw pack uid_or_offset =
  match run hxd path info raw pack uid_or_offset with
  | Ok () -> Ok ()
  | Error (`Msg err) -> Error (Fmt.str "%s." err)

open Cmdliner

let existing_file =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok v
    | Ok v -> Rresult.R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err
  in
  Arg.conv (parser, Fpath.pp)

let uid_or_offset =
  let parser str =
    match Int64.of_string_opt str, SHA1.of_hex_opt str with
    | Some v, _ -> Ok (`Offset v)
    | _, Some uid -> Ok (`Uid uid)
    | _ -> Rresult.R.error_msgf "Invalid value: %S" str
  in
  let pp ppf = function
    | `Uid v -> SHA1.pp ppf v
    | `Offset v -> Format.fprintf ppf "0x%Lx" v
  in
  Arg.conv (parser, pp)

let with_path =
  let doc = "Show the delta-path." in
  Arg.(value & flag & info [ "with-path" ] ~doc)

let with_info =
  let doc = "Show $(i,metadata) of the given object." in
  Arg.(value & flag & info [ "with-info" ] ~doc)

let raw =
  let doc =
    "Show as is the given object without extra-informations or post-treatments."
  in
  Arg.(value & flag & info [ "r"; "raw" ] ~doc)

let pack =
  Arg.(required & pos 0 (some existing_file) None & info [] ~docv:"<pack>")

let uid_or_offset =
  Arg.(required & pos 1 (some uid_or_offset) None & info [] ~docv:"<object>")

let cmd =
  let doc = "Extract an object from a PACK file." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Extract an object from the given PACK file and its SHA-1 or its \
         offset.";
    ]
  in
  let info = Cmd.info "get" ~doc ~man in
  Cmd.v info
    Term.(
      const run
      $ Hxd_cmdliner.cmd
      $ with_path
      $ with_info
      $ raw
      $ pack
      $ uid_or_offset)

let () = exit @@ Cmd.eval_result cmd
