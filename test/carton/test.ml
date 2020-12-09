open Stdlib

let weights =
  Alcotest.test_case "weight" `Quick @@ fun () ->
  Alcotest.(check int) "0" (Carton.Dec.null :> int) 0;
  Alcotest.(check int) "1" (Carton.Dec.weight_of_int_exn 1 :> int) 1;
  Alcotest.(check int) "2" (Carton.Dec.weight_of_int_exn 2 :> int) 2;
  Alcotest.(check int) "3" (Carton.Dec.weight_of_int_exn 3 :> int) 3;
  let weight_of_int_exn = Invalid_argument "weight_of_int_exn" in
  Alcotest.check_raises "-1" weight_of_int_exn (fun () ->
      ignore @@ Carton.Dec.weight_of_int_exn (-1));
  Alcotest.check_raises "-2" weight_of_int_exn (fun () ->
      ignore @@ Carton.Dec.weight_of_int_exn (-2));
  Alcotest.check_raises "-3" weight_of_int_exn (fun () ->
      ignore @@ Carton.Dec.weight_of_int_exn (-3))

let randomize payload =
  for i = 0 to Bigstringaf.length payload - 1 do
    Bigstringaf.set payload i (Char.chr (Random.bits () land 0xff))
  done

let seed = "OYfrfVoWdfZgHS18ubCo4ChABW+SstWbVXUya2moM2Y="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () = Random.full_init seed

open Prelude

let failf fmt = Fmt.kstr Alcotest.fail fmt

let bigstringaf =
  Alcotest.testable
    Fmt.(using Bigstringaf.to_string string)
    (fun a b ->
      String.equal (Bigstringaf.to_string a) (Bigstringaf.to_string b))

let physical_equal =
  Alcotest.testable (fun ppf _ -> Fmt.string ppf "#ptr") (fun a b -> a == b)

let loads =
  Alcotest.test_case "load" `Quick @@ fun () ->
  let chunk = Int64.to_int Carton.Dec.W.length in
  let payload = Bigstringaf.create (chunk * 2) in
  randomize payload;
  let do_mmap = ref false in
  let map payload ~pos len =
    if pos < 0L then failf "mmap: index out of bounds";
    if pos > Int64.of_int (Bigstringaf.length payload) then
      failf "mmap: index out of bounds";
    let max = Int64.sub (Int64.of_int (Bigstringaf.length payload)) pos in
    let len = min max (Int64.of_int len) in
    let len = Int64.to_int len in
    do_mmap := true;
    Us.inj (Bigstringaf.sub payload ~off:(Int64.to_int pos) ~len)
  in
  let w = Carton.Dec.W.make payload in
  let slice0 = Us.prj (Carton.Dec.W.load unix ~map w 0L) in
  Alcotest.(check bool) "first load" (Option.is_some slice0) true;
  let slice0 = Option.get slice0 in
  Alcotest.(check bool) "first load" !do_mmap true;
  Alcotest.(check int64) "first load: offset" slice0.Carton.Dec.W.offset 0L;
  Alcotest.(check int)
    "first load: length" slice0.Carton.Dec.W.length
    (min (Bigstringaf.length payload) chunk);
  Alcotest.(check bigstringaf)
    "first load: contents"
    (Bigstringaf.sub payload ~off:0
       ~len:(min (Bigstringaf.length payload) chunk))
    slice0.Carton.Dec.W.payload;
  do_mmap := false;
  (* reset *)
  let slice1 = Us.prj (Carton.Dec.W.load unix ~map w 0L) in
  Alcotest.(check bool) "second load" (Option.is_some slice1) true;
  let slice1 = Option.get slice1 in
  Alcotest.(check bool) "second load" !do_mmap false;
  Alcotest.(check int64) "second load: offset" slice1.Carton.Dec.W.offset 0L;
  Alcotest.(check int)
    "second load: length" slice1.Carton.Dec.W.length
    (min (Bigstringaf.length payload) chunk);
  Alcotest.(check bigstringaf)
    "second load: contents"
    (Bigstringaf.sub payload ~off:0
       ~len:(min (Bigstringaf.length payload) chunk))
    slice1.Carton.Dec.W.payload;
  Alcotest.(check physical_equal) "no allocation" slice0 slice1;
  let slice2 = Us.prj (Carton.Dec.W.load unix ~map w 100L) in
  Alcotest.(check bool) "third load" (Option.is_some slice2) true;
  let slice2 = Option.get slice2 in
  Alcotest.(check bool) "third load" !do_mmap false;
  Alcotest.(check physical_equal) "no allocation" slice0 slice2;
  do_mmap := false;
  (* reset *)
  let slice3 = Us.prj (Carton.Dec.W.load unix ~map w (Int64.of_int chunk)) in
  Alcotest.(check bool) "four load" (Option.is_some slice3) true;
  let slice3 = Option.get slice3 in
  Alcotest.(check bool) "four load" !do_mmap true;
  Alcotest.(check int64)
    "four load: offset" slice3.Carton.Dec.W.offset (Int64.of_int chunk);
  Alcotest.(check int)
    "four load: length" slice3.Carton.Dec.W.length
    (min (Bigstringaf.length payload) chunk);
  Alcotest.(check bigstringaf)
    "four load: contents"
    (Bigstringaf.sub payload ~off:chunk
       ~len:(min (Bigstringaf.length payload) chunk))
    slice3.Carton.Dec.W.payload

let pp_kind ppf = function
  | `A -> Fmt.string ppf "a"
  | `B -> Fmt.string ppf "b"
  | `C -> Fmt.string ppf "c"
  | `D -> Fmt.string ppf "d"

let equal_kind a b =
  match a, b with `A, `A | `B, `B | `C, `C | `D, `D -> true | _ -> false

let kind = Alcotest.testable pp_kind equal_kind
let optint = Alcotest.testable Optint.pp Optint.equal
let sha1 = Alcotest.testable Digestif.SHA1.pp Digestif.SHA1.equal
let s = Alcotest.testable (fun ppf x -> Fmt.pf ppf "%S" x) String.equal
let z = Bigstringaf.create De.io_buffer_size
let allocate bits = De.make_window ~bits
let o = Bigstringaf.create De.io_buffer_size

let empty_pack, uid_empty_pack =
  let () =
    let cmd = Bos.Cmd.(v "git" % "pack-objects" % "-q" % "--stdout") in
    let out =
      let open Rresult in
      Bos.OS.Dir.current () >>= fun current ->
      let dst = Fpath.(current / "pack-null") in
      ( Bos.OS.Dir.with_tmp "git-%s" @@ fun path ->
        Bos.OS.Dir.with_current path @@ fun () ->
        Bos.OS.Cmd.run_status Bos.Cmd.(v "git" % "init") >>| fun _ ->
        let out = Bos.OS.Cmd.(run_io cmd in_null) in
        Bos.OS.Cmd.out_file dst out )
        ()
    in
    match Rresult.R.(join (join (join out))) with
    | Ok ((), (_, `Exited 0)) -> ()
    | _ -> Alcotest.fail "Error while executing 'git pack-objects'"
  in
  let ic = open_in_bin "pack-null" in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  close_in ic;
  ( Bytes.unsafe_to_string rs,
    Digestif.SHA1.of_raw_string (Bytes.sub_string rs (Bytes.length rs - 20) 20)
  )

let test_empty_pack () =
  Alcotest.test_case "empty pack" `Quick @@ fun () ->
  let contents_expected = empty_pack in
  let buf = Bigstringaf.create 12 in
  let ctx = Digestif.SHA1.empty in
  Carton.Enc.header_of_pack ~length:0 buf 0 12;
  let ctx = Digestif.SHA1.feed_bigstring ctx buf ~off:0 ~len:12 in
  let sha = Digestif.SHA1.get ctx in
  Alcotest.(check sha1) "hash" sha uid_empty_pack;
  let res = Bigstringaf.to_string buf ^ Digestif.SHA1.to_raw_string sha in
  Alcotest.(check s) "contents" res contents_expected

module Fp = Carton.Dec.Fp (Uid)

type fake_file_descriptor = {
  mutable pos : int;
  mutable lst : Bigstringaf.t list;
}

let fd_and_read_of_bigstring_list lst =
  let fd = { pos = 0; lst } in
  let read fd buf ~off ~len =
    match fd.lst with
    | [] -> Us.inj 0
    | x :: r ->
        let len = min len (Bigstringaf.length x - fd.pos) in
        Bigstringaf.blit_to_bytes x ~src_off:fd.pos buf ~dst_off:off ~len;
        fd.pos <- fd.pos + len;
        if fd.pos = Bigstringaf.length x then fd.lst <- r;
        Us.inj len
  in
  fd, read

let valid_empty_pack () =
  Alcotest.test_case "valid empty pack" `Quick @@ fun () ->
  let fd, read =
    fd_and_read_of_bigstring_list
      [
        Bigstringaf.of_string ~off:0 ~len:(String.length empty_pack) empty_pack;
      ]
  in
  let max, buf, _ = Us.prj (Fp.check_header unix read fd) in
  let tmp0 = Bytes.create De.io_buffer_size in
  let tmp1 = Bigstringaf.create De.io_buffer_size in

  let decoder = Fp.decoder ~o ~allocate `Manual in
  let decoder =
    Fp.src decoder
      (Bigstringaf.of_string buf ~off:0 ~len:(String.length buf))
      0 (String.length buf)
  in

  Alcotest.(check int) "number" max 0;

  let rec go decoder =
    match Fp.decode decoder with
    | `End uid -> Alcotest.(check sha1) "hash" uid uid_empty_pack
    | `Entry _ -> Alcotest.fail "Unexpected entry"
    | `Malformed err -> Alcotest.fail err
    | `Await decoder ->
        let fiber = read fd tmp0 ~off:0 ~len:(Bytes.length tmp0) in
        let len = Us.prj fiber in
        Bigstringaf.blit_from_bytes tmp0 ~src_off:0 tmp1 ~dst_off:0 ~len;
        let decoder = Fp.src decoder tmp1 0 len in
        go decoder
    | `Peek _ -> Alcotest.fail "Unexpected `Peek"
  in

  go decoder

module Verify = Carton.Dec.Verify (Uid) (Us) (IO)

let digest_like_git ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigstringaf.length buf - off
  in
  let ctx = Digestif.SHA1.empty in

  let ctx =
    match kind with
    | `A -> Digestif.SHA1.feed_string ctx (Fmt.str "commit %d\000" len)
    | `B -> Digestif.SHA1.feed_string ctx (Fmt.str "tree %d\000" len)
    | `C -> Digestif.SHA1.feed_string ctx (Fmt.str "blob %d\000" len)
    | `D -> Digestif.SHA1.feed_string ctx (Fmt.str "tag %d\000" len)
  in
  let ctx = Digestif.SHA1.feed_bigstring ctx ~off ~len buf in
  Digestif.SHA1.get ctx

let verify_empty_pack () =
  Alcotest.test_case "verify empty pack" `Quick @@ fun () ->
  let t =
    Carton.Dec.make () ~z ~allocate ~uid_ln:Uid.length ~uid_rw:Uid.of_raw_string
      (fun _ -> Alcotest.fail "Invalid call to IDX")
  in
  let map () ~pos length =
    let len = min length (Int64.to_int pos - String.length empty_pack) in
    Us.inj (Bigstringaf.of_string empty_pack ~off:(Int64.to_int pos) ~len)
  in
  let oracle =
    {
      Carton.Dec.digest = digest_like_git;
      children = (fun ~cursor:_ ~uid:_ -> []);
      where = (fun ~cursor:_ -> Alcotest.fail "Invalid call to [where]");
      weight = (fun ~cursor:_ -> Alcotest.fail "Invalid call to [weight]");
    }
  in
  Verify.verify ~threads:1 ~map ~oracle t ~matrix:[||]

module Idx = Carton.Dec.Idx.N (Uid)

let empty_index, uid_empty_index =
  let res =
    let open Rresult in
    Bos.OS.Dir.current () >>= fun current ->
    let dst = Fpath.(current / "index-null") in
    ( Bos.OS.Dir.with_tmp "git-%s" @@ fun path ->
      Bos.OS.Dir.with_current path @@ fun () ->
      Bos.OS.Cmd.run_status Bos.Cmd.(v "git" % "init") >>= fun _ ->
      let cmd = Bos.Cmd.(v "git" % "pack-objects" % "-q" % "--stdout") in
      let out = Bos.OS.Cmd.(run_io cmd in_null) in
      Bos.OS.Cmd.out_run_in out >>= fun in_cmd ->
      let cmd =
        Bos.Cmd.(
          v "git" % "index-pack" % "--stdin" % "-o" % Fpath.to_string dst)
      in
      Bos.OS.Cmd.run_in cmd in_cmd )
      ()
  in
  let () =
    match Rresult.R.(join (join res)) with
    | Ok () -> ()
    | Error (`Msg err) -> Alcotest.fail err
  in
  let ic = open_in_bin "index-null" in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  close_in ic;
  ( Bytes.unsafe_to_string rs,
    Digestif.SHA1.of_raw_string (Bytes.sub_string rs (Bytes.length rs - 20) 20)
  )

let index_of_empty_pack () =
  Alcotest.test_case "index of empty pack" `Quick @@ fun () ->
  let p = ref 0 and c = ref 0 in
  let encoder = Idx.encoder `Manual ~pack:uid_empty_pack [||] in
  Idx.dst encoder o 0 (Bigstringaf.length o);

  let rec go () =
    match Idx.encode encoder `Await with
    | `Partial ->
        Alcotest.(check bool) "`Partial" (!c < 3) true;
        incr c;
        let pos = Bigstringaf.length o - !p - Idx.dst_rem encoder in
        Idx.dst encoder o pos (Bigstringaf.length o - pos);
        p := !p + pos;
        go ()
    | `Ok ->
        let raw = Bigstringaf.substring o ~off:0 ~len:!p in
        Alcotest.(check s) "index" raw empty_index
  in
  go ();
  let uid = Bigstringaf.substring o ~off:(!p - Uid.length) ~len:Uid.length in
  let uid = Uid.of_raw_string uid in
  Alcotest.(check sha1) "hash" uid uid_empty_index

let check_empty_index () =
  Alcotest.test_case "check empty index" `Quick @@ fun () ->
  let map =
    Bigstringaf.of_string empty_index ~off:0 ~len:(String.length empty_index)
  in
  let idx =
    Carton.Dec.Idx.make map ~uid_ln:Uid.length ~uid_rw:Uid.to_raw_string
      ~uid_wr:Uid.of_raw_string
  in
  Alcotest.(check int) "number of entries" (Carton.Dec.Idx.max idx) 0

let index_of_one_entry () =
  Alcotest.test_case "index of one entry" `Quick @@ fun () ->
  let p = ref 0 and c = ref 0 in
  let encoder =
    Idx.encoder `Manual ~pack:(Uid.of_hex "")
      [|
        {
          Carton.Dec.Idx.crc = Checkseum.Crc32.default;
          offset = 0L;
          uid = Uid.null;
        };
      |]
  in
  Idx.dst encoder o 0 (Bigstringaf.length o);

  let rec go () =
    match Idx.encode encoder `Await with
    | `Partial ->
        Alcotest.(check bool) "`Partial" (!c < 3) true;
        incr c;
        let pos = Bigstringaf.length o - !p - Idx.dst_rem encoder in
        Idx.dst encoder o pos (Bigstringaf.length o - pos);
        p := !p + pos;
        go ()
    | `Ok -> Bigstringaf.sub o ~off:0 ~len:!p
  in
  let idx =
    Carton.Dec.Idx.make (go ()) ~uid_ln:Uid.length ~uid_rw:Uid.to_raw_string
      ~uid_wr:Uid.of_raw_string
  in
  Alcotest.(check int) "number of entries" (Carton.Dec.Idx.max idx) 1;
  Alcotest.(check (option (pair optint int64)))
    "entry"
    (Carton.Dec.Idx.find idx Uid.null)
    (Some (Checkseum.Crc32.default, 0L))

let file =
  let compare a b =
    let ic_a = open_in_bin a in
    let ic_b = open_in_bin b in
    let ln_a = in_channel_length ic_a and ln_b = in_channel_length ic_b in
    if ln_a <> ln_b then (
      close_in ic_a;
      close_in ic_b;
      false )
    else
      let bf_a = Bytes.create 0x1000 and bf_b = Bytes.create 0x1000 in
      let rec go () =
        match input ic_a bf_a 0 0x1000, input ic_b bf_b 0 0x1000 with
        | 0, 0 -> true
        | rs_a, rs_b when rs_a = rs_b ->
            if not (Bytes.sub_string bf_a 0 rs_a = Bytes.sub_string bf_b 0 rs_b)
            then false
            else go ()
        | _ -> false
        | exception End_of_file -> true
      in
      let rs = go () in
      close_in ic_a;
      close_in ic_b;
      rs
  in
  Alcotest.testable Fmt.string compare

let zip a b =
  if Array.length a <> Array.length b then Fmt.invalid_arg "Array.zip";
  Array.init (Array.length a) (fun i -> a.(i), b.(i))

type fd_with_length = { fd : Unix.file_descr; mx : int64 }

let map { fd; mx } ~pos len =
  let len = min Int64.(sub mx pos) (Int64.of_int len) in
  let mp =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false
      [| Int64.to_int len |]
  in
  let mp = Bigarray.array1_of_genarray mp in
  Us.inj mp

let verify_bomb_pack () =
  Alcotest.test_case "verify & generate index of bomb pack" `Quick @@ fun () ->
  let o = Bigstringaf.create 0x1000 in
  let allocate bits = De.make_window ~bits in
  let decoder = Fp.decoder ~o ~allocate `Manual in
  let tmp0 = Bytes.create 0x1000 in
  let tmp1 = Bigstringaf.create 0x1000 in

  let ic = open_in_bin "bomb.pack" in
  let hash_expected =
    let len = in_channel_length ic in
    seek_in ic (len - 20);
    let res = really_input_string ic 20 in
    let res = Digestif.SHA1.of_raw_string res in
    seek_in ic 0;
    res
  in

  let max, buf, _ =
    Fp.check_header unix
      (fun ic buf ~off ~len -> Us.inj (input ic buf off len))
      ic
    |> Us.prj
  in
  let decoder =
    Fp.src decoder (Bigstringaf.of_string buf ~off:0 ~len:12) 0 12
  in

  let weight = Hashtbl.create max in
  let checks = Hashtbl.create max in
  let children = Hashtbl.create max in
  let where = Hashtbl.create max in
  let matrix = Array.make max Verify.unresolved_node in

  let rec go decoder =
    match Fp.decode decoder with
    | `Await decoder ->
        let len = input ic tmp0 0 0x1000 in
        Bigstringaf.blit_from_bytes tmp0 ~src_off:0 tmp1 ~dst_off:0 ~len;
        let decoder = Fp.src decoder tmp1 0 len in
        go decoder
    | `Peek decoder ->
        let keep = Fp.src_rem decoder in
        let len = input ic tmp0 0 (0x1000 - keep) in
        Bigstringaf.blit_from_bytes tmp0 ~src_off:0 tmp1 ~dst_off:keep ~len;
        let decoder = Fp.src decoder tmp1 0 (keep + len) in
        go decoder
    | `Entry ({ Fp.kind = Base _; offset; size; crc; _ }, decoder) ->
        let n = Fp.count decoder - 1 in
        Hashtbl.add checks offset crc;
        Hashtbl.add weight offset size;
        Hashtbl.add where offset n;
        matrix.(n) <- Verify.unresolved_base ~cursor:offset;
        go decoder
    | `Entry
        ({ Fp.kind = Ofs { sub = s; source; target }; offset; crc; _ }, decoder)
      ->
        let n = Fp.count decoder - 1 in
        let base = Int64.(sub offset (of_int s)) in

        Hashtbl.add checks offset crc;
        Hashtbl.add weight base source;
        Hashtbl.add weight offset target;
        Hashtbl.add where offset n;

        ( try
            let v = Hashtbl.find children (`Ofs base) in
            Hashtbl.add children (`Ofs base) (offset :: v)
          with Not_found -> Hashtbl.add children (`Ofs base) [ offset ] );
        go decoder
    | `Entry _ -> (* OBJ_REF *) Alcotest.fail "Unexpected OBJ_REF"
    | `Malformed err -> Alcotest.fail err
    | `End uid -> Alcotest.(check sha1) "hash" uid hash_expected
  in

  go decoder;
  close_in ic;
  let fd = Unix.openfile "bomb.pack" Unix.[ O_RDONLY ] 0o644 in
  let mx =
    let st = Unix.LargeFile.fstat fd in
    st.Unix.LargeFile.st_size
  in

  let oracle =
    {
      Carton.Dec.where = (fun ~cursor -> Hashtbl.find where cursor);
      children =
        (fun ~cursor ~uid ->
          match
            ( Hashtbl.find_opt children (`Ofs cursor),
              Hashtbl.find_opt children (`Ref uid) )
          with
          | Some a, Some b -> List.sort_uniq compare (a @ b)
          | Some x, None | None, Some x -> x
          | None, None -> []);
      digest = digest_like_git;
      weight = (fun ~cursor -> Hashtbl.find weight cursor);
    }
  in

  let z = Bigstringaf.create 0x1000 in
  let t =
    Carton.Dec.make { fd; mx } ~z ~allocate ~uid_ln:Uid.length
      ~uid_rw:Uid.of_raw_string (fun _ -> Alcotest.fail "Invalid call to IDX")
  in
  Verify.verify ~threads:1 ~map ~oracle t ~matrix;
  Unix.close fd;

  let offsets =
    Hashtbl.fold (fun k _ a -> k :: a) where []
    |> List.sort Stdlib.compare
    |> Array.of_list
  in
  let matrix = zip offsets matrix in
  let entries =
    Array.map
      (fun (offset, s) ->
        let uid = Verify.uid_of_status s in
        let crc = Hashtbl.find checks offset in
        { Carton.Dec.Idx.crc; offset; uid })
      matrix
  in
  let oc = open_out_bin "bomb-test.idx" in
  let encoder = Idx.encoder (`Channel oc) ~pack:hash_expected entries in
  let go () =
    match Idx.encode encoder `Await with `Partial -> assert false | `Ok -> ()
  in
  go ();
  close_out oc;

  let () =
    let cmd =
      Bos.Cmd.(v "git" % "index-pack" % "-o" % "git-bomb.idx" % "bomb.pack")
    in
    match Bos.OS.Cmd.run cmd with
    | Ok () -> ()
    | Error (`Msg err) -> Alcotest.fail err
  in
  Alcotest.(check file) "index" "git-bomb.idx" "bomb-test.idx"

let first_entry_of_bomb_pack () =
  Alcotest.test_case "first entry of bomb pack" `Quick @@ fun () ->
  let fd = Unix.openfile "bomb.pack" Unix.[ O_RDONLY ] 0o644 in
  let mx =
    let st = Unix.LargeFile.fstat fd in
    st.Unix.LargeFile.st_size
  in
  let pack =
    Carton.Dec.make { fd; mx } ~z ~allocate ~uid_ln:Uid.length
      ~uid_rw:Uid.of_raw_string (fun _ -> Alcotest.fail "Invalid call to IDX")
  in
  let fiber () =
    let ( >>= ) = unix.Carton.bind in

    Carton.Dec.weight_of_offset unix ~map pack ~weight:Carton.Dec.null 12L
    >>= fun weight ->
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_offset unix ~map pack raw ~cursor:12L >>= fun v ->
    unix.Carton.return v
  in
  let v = Us.prj (fiber ()) in
  Alcotest.(check kind) "kind" (Carton.Dec.kind v) `A;
  Alcotest.(check int) "length" (Carton.Dec.len v) 218;
  Alcotest.(check int) "depth" (Carton.Dec.depth v) 1

let bomb_matrix = ref [||]
let bomb_index = Hashtbl.create 0x10

(* XXX(dinosaure): to avoid systematic unpack of bomb.pack, [unpack_bomb_pack]
   sets [bomb_matrix] and fills [bomb_index]. Any use of them should be after
   [unpack_bomb_pack]. *)

module Verbose = struct
  type 'a fiber = 'a

  let succ () = ()
  let print () = ()
  let flush () = ()
end

let unpack_bomb_pack () =
  Alcotest.test_case "unpack bomb pack" `Quick @@ fun () ->
  let fd = Unix.openfile "bomb.pack" Unix.[ O_RDONLY ] 0o644 in
  let mx =
    let st = Unix.LargeFile.fstat fd in
    st.Unix.LargeFile.st_size
  in
  let pack =
    Carton.Dec.make { fd; mx } ~z ~allocate ~uid_ln:Uid.length
      ~uid_rw:Uid.of_raw_string (fun _ -> Alcotest.fail "Invalid call to IDX")
  in

  let first_pass () =
    let ic = open_in_bin "bomb.pack" in
    let max, _, _ = Us.prj (Fp.check_header unix unix_read ic) in
    seek_in ic 0;
    let decoder = Fp.decoder ~o:z ~allocate (`Channel ic) in
    let matrix = Array.make max Verify.unresolved_node in

    let where = Hashtbl.create 0x10 in
    let children = Hashtbl.create 0x10 in
    let weight = Hashtbl.create 0x10 in

    let rec go decoder =
      match Fp.decode decoder with
      | `Await _ | `Peek _ -> assert false
      | `Entry ({ Fp.kind = Base _; offset; size; _ }, decoder) ->
          let n = Fp.count decoder - 1 in
          Hashtbl.add weight offset size;
          Hashtbl.add where offset n;
          matrix.(n) <- Verify.unresolved_base ~cursor:offset;
          go decoder
      | `Entry
          ({ Fp.kind = Ofs { sub = s; source; target; _ }; offset; _ }, decoder)
        ->
          let n = Fp.count decoder - 1 in
          Hashtbl.add weight Int64.(sub offset (Int64.of_int s)) source;
          Hashtbl.add weight offset target;
          Hashtbl.add where offset n;
          ( try
              let v =
                Hashtbl.find children (`Ofs Int64.(sub offset (of_int s)))
              in
              Hashtbl.add children
                (`Ofs Int64.(sub offset (of_int s)))
                (offset :: v)
            with _exn ->
              Hashtbl.add children
                (`Ofs Int64.(sub offset (of_int s)))
                [ offset ] );
          go decoder
      | `Entry _ -> assert false
      | `End _ ->
          close_in ic;
          ( {
              Carton.Dec.digest = digest_like_git;
              children =
                (fun ~cursor ~uid ->
                  match
                    ( Hashtbl.find_opt children (`Ofs cursor),
                      Hashtbl.find_opt children (`Ref uid) )
                  with
                  | Some a, Some b -> List.sort_uniq compare (a @ b)
                  | Some x, None | None, Some x -> x
                  | None, None -> []);
              where = (fun ~cursor -> Hashtbl.find where cursor);
              weight = (fun ~cursor -> Hashtbl.find weight cursor);
            },
            matrix )
      | `Malformed err -> Alcotest.fail err
    in
    go decoder
  in

  let oracle, matrix = first_pass () in
  Verify.verify ~threads:1 ~map ~oracle pack ~matrix;
  Alcotest.(check pass) "verify" () ();
  let unpack status =
    let return = unix.Carton.return in
    let ( >>= ) = unix.Carton.bind in
    let cursor = Verify.offset_of_status status in
    Carton.Dec.weight_of_offset unix ~map pack ~weight:Carton.Dec.null cursor
    >>= fun weight ->
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_offset unix ~map pack raw ~cursor >>= fun _ -> return ()
  in
  Array.iter
    (fun s ->
      let _ = Us.prj (unpack s) in
      Alcotest.(check pass) (Uid.to_hex (Verify.uid_of_status s)) () ())
    matrix;
  Alcotest.(check pass) "unpack" () ();
  bomb_matrix := matrix;
  Array.iter
    (fun s ->
      Hashtbl.add bomb_index (Verify.uid_of_status s)
        (Verify.offset_of_status s))
    matrix

let pack_bomb_pack () =
  Alcotest.test_case "pack bomb pack" `Quick @@ fun () ->
  let fd = Unix.openfile "bomb.pack" Unix.[ O_RDONLY ] 0o644 in
  let mx =
    let st = Unix.LargeFile.fstat fd in
    st.Unix.LargeFile.st_size
  in
  let pack =
    Carton.Dec.make { fd; mx } ~z ~allocate ~uid_ln:Uid.length
      ~uid_rw:Uid.of_raw_string (fun _ -> Alcotest.fail "Invalid call to IDX")
  in

  let load uid =
    let ( >>= ) = unix.Carton.bind in
    match Hashtbl.find bomb_index uid with
    | cursor ->
        Carton.Dec.weight_of_offset unix ~map pack ~weight:Carton.Dec.null
          cursor
        >>= fun weight ->
        let raw = Carton.Dec.make_raw ~weight in
        Carton.Dec.of_offset unix ~map pack raw ~cursor
    | exception Not_found -> Alcotest.failf "Invalid UID %a" Uid.pp uid
  in
  let entries =
    Array.map
      (fun s ->
        let uid = Verify.uid_of_status s in
        let cursor = Hashtbl.find bomb_index uid in
        let length =
          let return = unix.Carton.return in
          let ( >>= ) = unix.Carton.bind in
          Carton.Dec.weight_of_offset unix ~map pack ~weight:Carton.Dec.null
            cursor
          >>= fun weight -> return (weight :> int)
        in
        Carton.Enc.make_entry ~kind:(Verify.kind_of_status s)
          ~length:(Us.prj length) (Verify.uid_of_status s))
      !bomb_matrix
  in
  let module D = Carton.Enc.Delta (Us) (IO) (Uid) (Verbose) in
  let targets =
    D.delta ~threads:[ load ] ~weight:10 ~uid_ln:Uid.length entries
  in

  let offsets = Hashtbl.create 0x10 in
  let find uid =
    match Hashtbl.find offsets uid with
    | v -> Us.inj (Some v)
    | exception Not_found -> Us.inj None
  in

  let uid =
    { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }
  in

  let b =
    {
      Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.q = De.Queue.create 0x10000;
      Carton.Enc.w = De.make_window ~bits:15;
    }
  in

  let ctx = ref Uid.empty in
  let output_bigstring oc buf ~off ~len =
    ctx := Uid.feed !ctx buf ~off ~len;
    let s = Bigstringaf.substring buf ~off ~len in
    output_string oc s
  in

  let oc = open_out_bin "new.pack" in

  let cursor = ref 12 in
  let iter target =
    let return = unix.Carton.return in
    let ( >>= ) = unix.Carton.bind in

    Hashtbl.add offsets (Carton.Enc.target_uid target) !cursor;
    Carton.Enc.encode_target unix ~b ~find ~load ~uid target ~cursor:!cursor
    >>= fun (len, encoder) ->
    let rec go encoder =
      match Carton.Enc.N.encode ~o:b.o encoder with
      | `Flush (encoder, len) ->
          output_bigstring oc b.o ~off:0 ~len;
          cursor := !cursor + len;
          let encoder =
            Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o)
          in
          go encoder
      | `End -> ()
    in
    output_bigstring oc b.o ~off:0 ~len;
    cursor := !cursor + len;
    let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
    go encoder;
    return ()
  in
  let iter target = Us.prj (iter target) in

  let header = Bigstringaf.create 12 in
  Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12;
  output_bigstring oc header ~off:0 ~len:12;
  Array.iter iter targets;
  let hash = Uid.get !ctx in
  output_string oc (Uid.to_raw_string hash);
  close_out oc;

  Alcotest.(check pass) "new.pack" () ();
  let res =
    let cmd = Bos.Cmd.(v "git" % "index-pack" % "new.pack") in
    let out = Bos.OS.Cmd.(run_out cmd) in
    match Bos.OS.Cmd.out_lines out with
    | Ok ([ hash ], (_, `Exited 0)) -> Uid.of_hex hash
    | _ -> Alcotest.fail "Error while executing 'git index-pack'"
  in
  let uid = Alcotest.testable Uid.pp Uid.equal in
  Alcotest.(check uid) "hash" res hash

let cycle () =
  Alcotest.test_case "cycle" `Quick @@ fun () ->
  let a = Bigstringaf.create 0x100 in
  let b = Bigstringaf.create 0x100 in
  randomize a;
  randomize b;
  let ea =
    Carton.Enc.make_entry ~kind:`A ~length:(Bigstringaf.length a)
      ~delta:(Carton.Enc.From `B)
      `A
  in
  let eb =
    Carton.Enc.make_entry ~kind:`A ~length:(Bigstringaf.length b)
      ~delta:(Carton.Enc.From `A)
      `B
  in

  let load = function
    | `A -> Us.inj (Carton.Dec.v ~kind:`A a)
    | `B -> Us.inj (Carton.Dec.v ~kind:`A b)
  in
  let ta = Us.prj (Carton.Enc.entry_to_target unix ~load ea) in
  let tb = Us.prj (Carton.Enc.entry_to_target unix ~load eb) in

  let offsets = Hashtbl.create 0x10 in
  let find uid =
    match Hashtbl.find offsets uid with
    | v -> Us.inj (Some v)
    | exception Not_found -> Us.inj None
  in

  let uid =
    {
      Carton.Enc.uid_ln = 1;
      Carton.Enc.uid_rw = (function `A -> "a" | `B -> "b");
    }
  in

  let b =
    {
      Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.q = De.Queue.create 0x10000;
      Carton.Enc.w = De.make_window ~bits:15;
    }
  in

  let ctx = ref Uid.empty in
  let output_bigstring oc buf ~off ~len =
    ctx := Uid.feed !ctx buf ~off ~len;
    let s = Bigstringaf.substring buf ~off ~len in
    output_string oc s
  in

  let oc = open_out_bin "cycle.pack" in
  let targets = [| ta; tb |] in

  let cursor = ref 12 in
  let iter target =
    let return = unix.Carton.return in
    let ( >>= ) = unix.Carton.bind in

    Hashtbl.add offsets (Carton.Enc.target_uid target) !cursor;
    Carton.Enc.encode_target unix ~b ~find ~load ~uid target ~cursor:!cursor
    >>= fun (len, encoder) ->
    let rec go encoder =
      match Carton.Enc.N.encode ~o:b.o encoder with
      | `Flush (encoder, len) ->
          output_bigstring oc b.o ~off:0 ~len;
          cursor := !cursor + len;
          let encoder =
            Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o)
          in
          go encoder
      | `End -> ()
    in
    output_bigstring oc b.o ~off:0 ~len;
    cursor := !cursor + len;
    let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
    go encoder;
    return ()
  in
  let iter target = Us.prj (iter target) in

  let header = Bigstringaf.create 12 in
  Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12;
  output_bigstring oc header ~off:0 ~len:12;
  Array.iter iter targets;
  let hash = Uid.get !ctx in
  output_string oc (Uid.to_raw_string hash);
  close_out oc;

  Alcotest.(check pass) "cycle.pack" () ();

  let fd = Unix.openfile "cycle.pack" Unix.[ O_RDONLY ] 0o644 in
  let mx =
    let st = Unix.LargeFile.fstat fd in
    st.Unix.LargeFile.st_size
  in
  let pack =
    Carton.Dec.make { fd; mx } ~z ~allocate ~uid_ln:1
      ~uid_rw:(function
        | "a" -> `A | "b" -> `B | v -> Fmt.invalid_arg "invalid uid: %S" v)
      (fun uid -> Int64.of_int (Hashtbl.find offsets uid))
  in

  (* XXX(dinosaure): must fail! *)
  try
    let fiber =
      let cursor = Int64.of_int (Hashtbl.find offsets `B) in
      let ( >>= ) = unix.Carton.bind in
      Carton.Dec.weight_of_offset unix ~map pack ~weight:Carton.Dec.null cursor
      >>= fun weight ->
      let raw = Carton.Dec.make_raw ~weight in
      Carton.Dec.of_offset unix ~map pack raw ~cursor
    in
    let _ = Us.prj fiber in
    Alcotest.failf "We did not discovered our cycle."
  with Carton.Dec.Cycle -> Alcotest.(check pass) "cycle" () ()

module Index_stream_decoder = Carton.Dec.Idx.M (IO) (Uid)

let decode_index_stream () =
  Alcotest.test_case "decode index stream" `Quick @@ fun () ->
  let ic = open_in_bin "git-bomb.idx" in
  let device = Carton.Dec.Idx.Device.device () in
  let uid = Carton.Dec.Idx.Device.create device in
  let tp = Bytes.create 0x1000 in
  let fiber =
    let open Rresult in
    Index_stream_decoder.create device uid >>= fun fd ->
    let rec go () =
      let len = 1 + Random.int (Bytes.length tp - 1) in
      let len = input ic tp 0 len in
      if len = 0 then (
        Gc.minor ();
        Gc.full_major ()
        (* XXX(dinosaure): we must ensure that underlying value kept by [device]
           is available as long as we keep [uid].
           TODO(dinosaure): we should add test to ensure that [device] did not
           keep our value if we don't keep [uid]. *);
        close_in ic;
        Index_stream_decoder.close device fd )
      else (
        Index_stream_decoder.append device fd (Bytes.sub_string tp 0 len);
        go () )
    in
    Gc.minor ();
    Gc.full_major ();
    go ()
  in
  match fiber with
  | Ok () ->
      Alcotest.(check pass) "index decoder" () ();
      let ic = Unix.openfile "git-bomb.idx" Unix.[ O_RDONLY ] 0o644 in
      let ln = (Unix.fstat ic).Unix.st_size in
      let mp =
        Mmap.V1.map_file ic ~pos:0L Bigarray.char Bigarray.c_layout false
          [| ln |]
      in
      Unix.close ic;

      let payload = Carton.Dec.Idx.Device.project device uid in
      let index0 =
        Carton.Dec.Idx.make
          (Bigarray.array1_of_genarray mp)
          ~uid_ln:Uid.length ~uid_rw:Uid.to_raw_string ~uid_wr:Uid.of_raw_string
      in
      let index1 =
        Carton.Dec.Idx.make payload ~uid_ln:Uid.length ~uid_rw:Uid.to_raw_string
          ~uid_wr:Uid.of_raw_string
      in
      Carton.Dec.Idx.iter
        ~f:(fun ~uid ~offset ~crc:_ ->
          match Carton.Dec.Idx.find index0 uid with
          | Some (_, offset') ->
              Alcotest.(check int64) (Fmt.str "%a" Uid.pp uid) offset offset'
          | None -> Alcotest.failf "%a not found" Uid.pp uid)
        index1
  | Error err ->
      Alcotest.failf "Error while decoding IDX file: %a"
        Index_stream_decoder.pp_error err

let empty_stream () =
  Alcotest.test_case "empty stream" `Quick @@ fun () ->
  let decoder = Fp.decoder ~o ~allocate `Manual in
  let decoder =
    match Fp.decode decoder with
    | `Await decoder -> Fp.src decoder Bigstringaf.empty 0 0
    | _ -> Alcotest.fail "Unexpected result of [decode]"
  in
  match Fp.decode decoder with
  | `Malformed _ -> Alcotest.(check pass) "no infinite loop" () ()
  | _ -> Alcotest.fail "Unexpected result of [decode]"

let () =
  Alcotest.run "carton"
    [
      "weights", [ weights ]; "loads", [ loads ];
      ( "encoder",
        [
          test_empty_pack (); index_of_empty_pack (); index_of_one_entry ();
          pack_bomb_pack (); cycle ();
        ] );
      ( "decoder",
        [
          valid_empty_pack (); verify_empty_pack (); check_empty_index ();
          verify_bomb_pack (); first_entry_of_bomb_pack (); unpack_bomb_pack ();
          decode_index_stream (); empty_stream ();
        ] ); "lwt", [ Test_lwt.test_map_yield ];
    ]
