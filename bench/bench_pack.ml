module Unix_scheduler = Carton.Make (struct type 'a t = 'a end)
open Unix_scheduler

let bomb_pack = "../test/carton/bomb.pack"
let bomb_idx = "../test/carton/bomb.idx"

let scheduler =
  { Carton.bind = (fun x f -> f (prj x)); Carton.return = (fun x -> inj x) }

let map fd ~pos len =
  let { Unix.LargeFile.st_size; _ } = Unix.LargeFile.fstat fd in
  let len =
    if Int64.of_int len <= Int64.sub st_size pos then len
    else Int64.(to_int (sub st_size pos))
  in
  let res =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray res

let fd = Unix.openfile bomb_pack Unix.[ O_RDONLY ] 0o644
let () = at_exit (fun () -> Unix.close fd)

let index =
  let tbl = Hashtbl.create 0x100 in

  let fd = Unix.openfile bomb_idx Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.fstat fd in
  let payload = map fd ~pos:0L st.Unix.st_size in
  Unix.close fd;

  let idx =
    Carton.Dec.Idx.make payload ~uid_ln:Digestif.SHA1.digest_size
      ~uid_rw:Digestif.SHA1.to_raw_string ~uid_wr:Digestif.SHA1.of_raw_string
  in
  let f ~uid ~offset ~crc:_ = Hashtbl.add tbl uid offset in
  Carton.Dec.Idx.iter ~f idx;
  tbl

let z = De.bigstring_create De.io_buffer_size
let w = De.make_window ~bits:15
let allocate _ = w

let pack =
  Carton.Dec.make fd ~z ~allocate ~uid_ln:Digestif.SHA1.digest_size
    ~uid_rw:Digestif.SHA1.of_raw_string (fun uid -> Hashtbl.find index uid)

let ( >>= ) = scheduler.Carton.bind
let return = scheduler.Carton.return
let uid_0 = Digestif.SHA1.of_hex "7af99c9e7d4768fa681f4fe4ff61259794cf719b"
let uid_1 = Digestif.SHA1.of_hex "d9513477b01825130c48c4bebed114c4b2d50401"

let load uid =
  let weight = Carton.Dec.weight_of_uid ~map pack ~weight:Carton.Dec.null uid in
  let raw = Carton.Dec.make_raw ~weight in
  let _ = Carton.Dec.of_uid ~map pack raw uid in
  return ()

let fn_map = Benchmark.V (fun () -> ignore (map fd ~pos:10L (1024 * 1024)))
let fn_load_0 = Benchmark.V (fun () -> ignore (prj (load uid_0)))
let fn_load_1 = Benchmark.V (fun () -> ignore (prj (load uid_1)))
let s x = Mtime.Span.of_uint64_ns (Int64.mul (Int64.of_int x) 1_000_000_000L)

let run fn_load title =
  let (Benchmark.V fn) = fn_load in
  let _ = fn () in
  let samples_map = Benchmark.run (s 8) fn_map in
  let samples_load = Benchmark.run (s 8) fn_load in

  match
    ( Linear_algebra.ols
        (fun m -> m.(1))
        [| (fun m -> m.(0)); (fun _ -> 1.) |]
        samples_map,
      Linear_algebra.ols
        (fun m -> m.(1))
        [| (fun m -> m.(0)); (fun _ -> 1.) |]
        samples_load )
  with
  | Ok (estimate_map, r_map), Ok (estimate_load, r_load) ->
      Fmt.pr "%15.2fns (r²: %f) [map syscall].\n%!" estimate_map.(0) r_map;
      Fmt.pr "%15.2fns (r²: %f) [load %s].\n%!" estimate_load.(0) r_load title
  | Error (`Msg err), _ | _, Error (`Msg err) ->
      Fmt.epr "%s: %s.\n%!" Sys.argv.(0) err

let () =
  match Sys.argv with
  | [| _; "0" |] -> run fn_load_0 "commit (0 delta)"
  | [| _; "1" |] -> run fn_load_1 "tree (1 delta)"
  | _ ->
      Fmt.epr "%s [0|1].\n" Sys.argv.(0);
      Fmt.epr "0    commit (0 delta).\n%!";
      Fmt.epr "1    tree (1 delta).\n%!"
