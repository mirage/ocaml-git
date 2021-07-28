open Rresult

let ( <.> ) f g x = f (g x)

let verbosef max ppf fmt =
  let counter = ref 0 in
  let mutex = Mutex.create () in
  fun () ->
    Mutex.lock mutex;
    Format.fprintf ppf fmt (!counter * 100 / max) !counter max;
    incr counter;
    Mutex.unlock mutex

module Scheduler = Carton.Make (Fiber)
open Scheduler

module SHA1 = struct
  include Digestif.SHA1

  let feed ctx ?off ?len bs = feed_bigstring ctx ?off ?len bs
  let null = digest_string ""
  let length = digest_size
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

module Verify = Carton.Dec.Verify (SHA1) (Scheduler) (Fiber)
module First_pass = Carton.Dec.Fp (SHA1)
module Encoder = Carton.Dec.Idx.N (SHA1)
open Fiber

let sched =
  let open Scheduler in
  {
    Carton.bind = (fun x f -> inj (bind (prj x) (fun x -> prj (f x))));
    return = (fun x -> inj (return x));
  }

let z = De.bigstring_create De.io_buffer_size
let allocate bits = De.make_window ~bits

let replace hashtbl k v =
  try
    let v' = Hashtbl.find hashtbl k in
    if v < v' then Hashtbl.replace hashtbl k v'
  with _ -> Hashtbl.add hashtbl k v

let never _ = assert false

let zip a b =
  if Array.length a <> Array.length b then invalid_arg "zip: lengths mismatch";
  Array.init (Array.length a) (fun i -> a.(i), b.(i))

exception Exists

let share l0 l1 =
  try
    List.iter
      (fun (v, _) -> if List.exists (Int64.equal v) l1 then raise Exists)
      l0;
    false
  with Exists -> true

let digest ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigstringaf.length buf - off
  in
  let ctx = SHA1.empty in
  let ctx =
    match kind with
    | `A -> SHA1.feed_string ctx (Fmt.str "commit %d\000" len)
    | `B -> SHA1.feed_string ctx (Fmt.str "tree %d\000" len)
    | `C -> SHA1.feed_string ctx (Fmt.str "blob %d\000" len)
    | `D -> SHA1.feed_string ctx (Fmt.str "tag %d\000" len)
  in
  let ctx = SHA1.feed_bigstring ctx ~off ~len buf in
  SHA1.get ctx

let ( >>= ) = sched.bind
let return = sched.return

let blit_from_bytes src src_off dst dst_off len =
  Bigstringaf.blit_from_bytes src ~src_off dst ~dst_off ~len

let read ?save ic =
  let tp = Bytes.create 0x1000 in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let rec go filled inputs =
    match Ke.Rke.N.peek ke with
    | [] ->
        let len = input ic tp 0 0x1000 in
        Option.iter (fun save -> Buffer.add_subbytes save tp 0 len) save;
        if len > 0 then (
          Ke.Rke.N.push ke ~blit:blit_from_bytes ~length:Bytes.length ~off:0
            ~len tp;
          go filled inputs)
        else return filled
    | src :: _ ->
        let src = Cstruct.of_bigarray src in
        let len = min (Cstruct.length inputs) (Cstruct.length src) in
        Cstruct.blit src 0 inputs 0 len;
        Ke.Rke.N.shift_exn ke len;
        if len < Cstruct.length inputs then
          go (filled + len) (Cstruct.shift inputs len)
        else return (filled + len)
  in
  fun filled inputs -> go filled inputs

let first_pass ?(verbose = false) ?save ic =
  let fl_buffer = Cstruct.create De.io_buffer_size in
  let zw = De.make_window ~bits:15 in
  let allocate _ = zw in

  let read_cstruct = read ?save ic in
  let read_bytes () buf ~off ~len =
    let rec go rest raw =
      if rest <= 0 then (
        Cstruct.blit_to_bytes fl_buffer 0 buf off len;
        return (abs rest + len))
      else
        read_cstruct 0 raw >>= function
        | 0 -> return (len - rest)
        | filled -> go (rest - filled) (Cstruct.shift raw filled)
    in
    go len fl_buffer
  in

  First_pass.check_header sched read_bytes () >>= fun (max, _, len) ->
  let decoder = First_pass.decoder ~o:z ~allocate `Manual in
  let decoder = First_pass.src decoder (Cstruct.to_bigarray fl_buffer) 0 len in

  let children = Hashtbl.create 0x100 in
  let where = Hashtbl.create 0x100 in
  let weight = Hashtbl.create 0x100 in
  let length = Hashtbl.create 0x100 in
  let checks = Hashtbl.create 0x100 in
  let matrix = Array.make max Verify.unresolved_node in

  let rec go decoder =
    match First_pass.decode decoder with
    | `Await decoder ->
        read_cstruct 0 fl_buffer >>= fun len ->
        go (First_pass.src decoder (Cstruct.to_bigarray fl_buffer) 0 len)
    | `Peek decoder ->
        let keep = First_pass.src_rem decoder in
        read_cstruct 0 (Cstruct.shift fl_buffer keep) >>= fun len ->
        go
          (First_pass.src decoder
             (Cstruct.to_bigarray fl_buffer)
             0 (keep + len))
    | `Entry ({ First_pass.kind = Base _; offset; crc; size; _ }, decoder) ->
        let n = First_pass.count decoder - 1 in
        if verbose then
          Fmt.pr "\rIndexing objects: %3d%% (%d/%d)%!" (n * 100 / max) n max;
        Hashtbl.add weight offset size;
        Hashtbl.add length offset size;
        Hashtbl.add checks offset crc;
        Hashtbl.add where offset n;
        matrix.(n) <- Verify.unresolved_base ~cursor:offset;
        go decoder
    | `Entry
        ( {
            First_pass.kind = Ofs { sub = s; source; target };
            offset;
            crc;
            size;
            _;
          },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        if verbose then
          Fmt.pr "\rIndexing objects: %3d%% (%d/%d)%!" (n * 100 / max) n max;
        replace weight Int64.(sub offset (Int64.of_int s)) source;
        replace weight offset target;
        Hashtbl.add length offset size;
        Hashtbl.add checks offset crc;
        Hashtbl.add where offset n;
        (try
           let vs =
             Hashtbl.find children (`Ofs Int64.(sub offset (of_int s)))
           in
           Hashtbl.replace children
             (`Ofs Int64.(sub offset (of_int s)))
             (offset :: vs)
         with _ ->
           Hashtbl.add children (`Ofs Int64.(sub offset (of_int s))) [ offset ]);
        go decoder
    | `Entry
        ( { First_pass.kind = Ref { ptr; target; source }; offset; crc; size; _ },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        if verbose then
          Fmt.pr "\rIndexing objects: %3d%% (%d/%d)%!" (n * 100 / max) n max;
        replace weight offset (Stdlib.max target source);
        Hashtbl.add length offset size;
        Hashtbl.add checks offset crc;
        Hashtbl.add where offset n;
        (try
           let vs = Hashtbl.find children (`Ref ptr) in
           Hashtbl.replace children (`Ref ptr) (offset :: vs)
         with _ -> Hashtbl.add children (`Ref ptr) [ offset ]);
        go decoder
    | `End hash ->
        if verbose then
          Fmt.pr "\rIndexing objects: 100%% (%d/%d), done.\n%!" max max;
        close_in ic;
        return (Ok hash)
    | `Malformed err -> return (Error (`Msg err))
  in
  go decoder >>= function
  | Error _ as err -> return err
  | Ok hash ->
      let weight ~cursor = Hashtbl.find weight cursor in
      let oracle =
        let where ~cursor = Hashtbl.find where cursor in
        let children ~cursor ~uid =
          match
            ( Hashtbl.find_opt children (`Ofs cursor),
              Hashtbl.find_opt children (`Ref uid) )
          with
          | Some a, Some b -> List.sort_uniq compare (a @ b)
          | Some x, None | None, Some x -> x
          | None, None -> []
        in
        { Carton.Dec.where; children; digest; weight }
      in
      return (Ok (hash, oracle, matrix, where, checks))

let ( >>? ) x f = x >>= function Ok x -> f x | Error err -> return (Error err)
let ( let* ) x f = x >>= f
let ( let+ ) x f = x >>? f

let second_pass ?(verbose = false) ~map oracle matrix =
  let pack =
    Carton.Dec.make () ~allocate ~z ~uid_ln:SHA1.length
      ~uid_rw:SHA1.of_raw_string never
  in
  let max_unresolveds =
    Array.fold_left (fun a x -> if Verify.is_base x then a else succ a) 0 matrix
  in
  let verbose_deltas =
    match verbose with
    | false -> ignore
    | true ->
        verbosef max_unresolveds Fmt.stdout
          "\rResolving deltas: %3d%% (%d/%d).%!"
  in
  let* () =
    Verify.verify ~threads:(Fiber.get_concurrency ()) pack ~map ~oracle
      ~verbose:verbose_deltas ~matrix
    |> inj
  in
  if verbose then
    Fmt.pr "\rResolving deltas: 100%% (%d/%d), done.\n%!" max_unresolveds
      max_unresolveds;
  return ()

type fd = Unix : Unix.file_descr -> fd | Stdin : fd

let close_fd = function
  | Unix fd ->
      Unix.close fd;
      return ()
  | Stdin -> return ()

module Set = Set.Make (SHA1)

let close_in ic =
  close_in ic;
  return ()

let ignore_ic _ = return ()

let values_from_src = function
  | `Stdin ->
      let buf = Buffer.create 0x1000 in
      let map () ~pos len =
        let max = Buffer.length buf in
        let pos = Int64.to_int pos in
        let len = Stdlib.min len (max - pos) in
        Bigstringaf.of_string ~off:pos ~len (Buffer.contents buf)
      in
      Some buf, stdin, ignore_ic, Stdin, map
  | `File fpath ->
      let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
      let max = (Unix.LargeFile.fstat fd).Unix.LargeFile.st_size in
      let map () ~pos len =
        let len = Stdlib.min len (Int64.to_int (Int64.sub max pos)) in
        let res =
          Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false
            [| len |]
        in
        Bigarray.array1_of_genarray res
      in
      None, open_in (Fpath.to_string fpath), close_in, Unix fd, map

let split where checks matrix =
  let offsets =
    Hashtbl.fold (fun k _ a -> k :: a) where []
    |> List.sort Int64.compare
    |> Array.of_list
  in
  let fold (unresolveds, resolveds) (offset, status) =
    if Verify.is_resolved status then
      let uid = Verify.uid_of_status status in
      let crc = Hashtbl.find checks offset in
      unresolveds, { Carton.Dec.Idx.crc; offset; uid } :: resolveds
    else
      let crc = Hashtbl.find checks offset in
      (offset, crc) :: unresolveds, resolveds
  in
  Array.fold_left fold ([], []) (zip offsets matrix)

let make_index_pack hash src dst resolveds =
  let oc, close =
    match src, dst with
    | `File fpath, None ->
        (* XXX(dinosaure): infinite loop on [Fiber] if [open_out] raises an exception. *)
        open_out Fpath.(to_string (set_ext "idx" fpath)), close_out
    | _, Some fpath -> open_out Fpath.(to_string fpath), close_out
    | `Stdin, None -> stdout, ignore
  in
  let encoder =
    Encoder.encoder (`Channel oc) ~pack:hash (Array.of_list resolveds)
  in
  let[@warning "-8"] (`Ok : [ `Ok | `Partial ]) =
    Encoder.encode encoder `Await
  in
  close oc;
  Option.iter (fun _ -> Fmt.pr "%a\n%!" SHA1.pp hash) dst;
  return (Ok ())

let index_pack ?(verbose = false) src dst =
  let verbose = match dst with None -> false | Some _ -> verbose in
  (* XXX(dinosaure): if the user wants to /stream/ the index file, we must
     set verbose to [false] - otherwise, we produce a bad index file/output. *)
  let save, ic, close_ic, fd, map = values_from_src src in
  let+ hash, oracle, matrix, where, checks = first_pass ~verbose ?save ic in
  let* () = close_ic ic in
  let* _ = second_pass ~verbose ~map oracle matrix in
  let* () = close_fd fd in
  match split where checks matrix with
  | [], resolveds -> make_index_pack hash src dst resolveds
  | _ ->
      let err =
        R.error_msgf
          "Carton is not able to canonicalize a PACK file without the Git \
           layer."
      in
      return err

let run verbose src dst =
  match (Fiber.run <.> prj) (index_pack ~verbose src dst) with
  | Ok () -> `Ok 0
  | Error (`Msg err) -> `Error (false, Fmt.str "%s." err)

open Cmdliner

let src =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> Ok (`File v)
    | Ok v -> Rresult.R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err
  in
  let pp ppf = function
    | `File v -> Fpath.pp ppf v
    | `Stdin -> Fmt.string ppf "<stdin>"
  in
  Arg.conv (parser, pp)

let dst = Arg.conv (Fpath.of_string, Fpath.pp)

let src =
  let doc = "PACK file." in
  Arg.(value & pos ~rev:true 0 src `Stdin & info [] ~doc)

let dst =
  let doc =
    "Write the generated index into the specified file. Without this option \
     the name of pack index file is constructed from the name of packed \
     archive file by replacing .pack with .idx."
  in
  Arg.(value & opt (some dst) None & info [ "o" ] ~doc ~docv:"<index-file>")

let verbose =
  let doc = "Be verbose about what is going on, including progress status." in
  Arg.(value & flag & info [ "v" ] ~doc)

let cmd =
  let doc = "Build pack index for an packed archive." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Readss a packed archive (.pack) from a specified file (or standard \
         input), and builds a pack index file (.idx) for it.";
    ]
  in
  ( Term.(ret (const run $ verbose $ src $ dst)),
    Term.info "index-pack" ~doc ~exits ~man )

let () = Term.(exit_status @@ eval cmd)
