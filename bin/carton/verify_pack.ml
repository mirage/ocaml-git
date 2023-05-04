open Rresult

let ( <.> ) f g x = f (g x)

module Scheduler = Carton.Make (Fiber)

module SHA1 = struct
  include Digestif.SHA1

  let feed ctx ?off ?len bs = feed_bigstring ctx ?off ?len bs
  let null = digest_string ""
  let length = digest_size
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

module Verify = Carton.Dec.Verify (SHA1) (Scheduler) (Fiber)
module First_pass = Carton.Dec.Fp (SHA1)
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
  match Hashtbl.find_opt hashtbl k with
  | Some v' -> if v' < v then Hashtbl.replace hashtbl k v
  | None -> Hashtbl.add hashtbl k v

let never _ = assert false

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

let read fd buf ~off ~len =
  let len = input fd buf off len in
  return len

let first_pass fpath =
  let ic = open_in (Fpath.to_string fpath) in
  let zw = De.make_window ~bits:15 in
  let allocate _ = zw in

  First_pass.check_header sched read ic >>= fun (max, _, _) ->
  seek_in ic 0;

  let decoder = First_pass.decoder ~o:z ~allocate (`Channel ic) in
  let children = Hashtbl.create 0x100 in
  let where = Hashtbl.create 0x100 in
  let weight = Hashtbl.create 0x100 in
  let length = Hashtbl.create 0x100 in
  let carbon = Hashtbl.create 0x100 in
  let matrix = Array.make max Verify.unresolved_node in

  let rec go decoder =
    match First_pass.decode decoder with
    | `Await _ | `Peek _ -> assert false
    | `Entry ({ First_pass.kind = Base _; offset; size; consumed; _ }, decoder)
      ->
        let n = First_pass.count decoder - 1 in
        Hashtbl.add weight offset size;
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
        Hashtbl.add where offset n;
        matrix.(n) <- Verify.unresolved_base ~cursor:offset;
        go decoder
    | `Entry
        ( {
            First_pass.kind = Ofs { sub = s; source; target };
            offset;
            size;
            consumed;
            _;
          },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        replace weight Int64.(sub offset (Int64.of_int s)) source;
        replace weight offset target;
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
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
        ( {
            First_pass.kind = Ref { ptr; target; source };
            offset;
            size;
            consumed;
            _;
          },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        replace weight offset (Stdlib.max target source);
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
        Hashtbl.add where offset n;
        (try
           let vs = Hashtbl.find children (`Ref ptr) in
           Hashtbl.replace children (`Ref ptr) (offset :: vs)
         with _ -> Hashtbl.add children (`Ref ptr) [ offset ]);
        go decoder
    | `End hash ->
        close_in ic;
        return (Ok hash)
    | `Malformed err -> return (Error (`Msg err))
  in
  go decoder >>= function
  | Error _ as err -> return err
  | Ok hash ->
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
      let weight ~cursor = Hashtbl.find weight cursor in
      let oracle = { Carton.Dec.where; children; digest; weight } in
      return (Ok (hash, oracle, matrix, length, carbon))

let map ~max fd ~pos len =
  let len = Stdlib.min len (Int64.to_int (Int64.sub max pos)) in
  let res =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray res

let second_pass fpath (hash, oracle, matrix) =
  let open Fiber in
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let max = (Unix.LargeFile.fstat fd).Unix.LargeFile.st_size in
  let map fd ~pos len = map ~max fd ~pos len in
  let pack =
    Carton.Dec.make fd ~allocate ~z ~uid_ln:SHA1.length
      ~uid_rw:SHA1.of_raw_string never
  in
  Verify.verify ~threads:(Fiber.get_concurrency ()) pack ~map ~oracle
    ~verbose:ignore ~matrix
  >>= fun () ->
  match Array.for_all Verify.is_resolved matrix with
  | false -> return (R.error_msgf "Thin PACK file")
  | true -> return (Ok (hash, matrix))

let pp_kind ppf = function
  | `A -> Fmt.string ppf "commit"
  | `B -> Fmt.string ppf "tree  "
  | `C -> Fmt.string ppf "blob  "
  | `D -> Fmt.string ppf "tag   "

let pp_delta ppf status =
  match Verify.source_of_status status with
  | Some uid -> Fmt.pf ppf " %d %a" (Verify.depth_of_status status) SHA1.pp uid
  | None -> ()

let verify_hash ~memory hash =
  let max = Bigstringaf.length memory in
  let hash' =
    SHA1.of_raw_string
      (Bigstringaf.substring memory
         ~off:(max - (2 * SHA1.length))
         ~len:SHA1.length)
  in
  SHA1.equal hash hash'

let verify ~verbose fpath hash length carbon matrix =
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let len = (Unix.fstat fd).Unix.st_size in
  let memory =
    Unix.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout false [| len |]
  in
  let memory = Bigarray.array1_of_genarray memory in
  Unix.close fd;
  let idx =
    Carton.Dec.Idx.make memory ~uid_ln:SHA1.length ~uid_rw:SHA1.to_raw_string
      ~uid_wr:SHA1.of_raw_string
  in
  if not (verify_hash ~memory hash) then
    return (R.error_msgf "Invalid PACK hash")
  else
    match verbose with
    | false ->
        if
          Array.for_all
            (Carton.Dec.Idx.exists idx <.> Verify.uid_of_status)
            matrix
        then return (Ok ())
        else return (R.error_msgf "Invalid PACK file")
    | true -> (
        let chains = Hashtbl.create 0x10 in

        let f status =
          let uid = Verify.uid_of_status status in
          let kind = Verify.kind_of_status status in
          let offset = Verify.offset_of_status status in
          let (size : Carton.Dec.weight) = Hashtbl.find length offset in
          let size_in_pack = Hashtbl.find carbon offset in
          let depth = Verify.depth_of_status status in
          (match Hashtbl.find chains depth with
          | v -> Hashtbl.replace chains depth (succ v)
          | exception _ -> Hashtbl.replace chains depth 1);
          match Carton.Dec.Idx.find idx uid with
          | Some (_crc, offset') when offset = offset' ->
              Fmt.pr "%a %a %d %d %Ld%a\n%!" SHA1.pp uid pp_kind kind
                (size :> int)
                size_in_pack offset pp_delta status
          | _ -> Fmt.failwith "Invalid PACK file"
        in

        let pp_chain ppf (depth, n) =
          match depth with
          | 0 -> Fmt.pf ppf "non delta: %d objects\n%!" n
          | _ -> Fmt.pf ppf "chain length = %d: %d objects\n%!" depth n
        in

        try
          Array.iter f matrix;
          let chains =
            List.sort_uniq
              (fun (a, _) (b, _) -> compare a b)
              ((List.of_seq <.> Hashtbl.to_seq) chains)
          in
          Fmt.pr "%a%!" (Fmt.list ~sep:Fmt.nop pp_chain) chains;
          Fmt.pr "%a: ok\n%!" Fpath.pp Fpath.(set_ext "pack" (base fpath));
          return (Ok ())
        with _exn -> return (R.error_msgf "Invalid PACK file"))

let ( >>? ) x f =
  let open Fiber in
  x >>= function Ok x -> f x | Error err -> return (Error err)

let run ~verbose fpath =
  let pack = Fpath.set_ext "pack" fpath in
  Bos.OS.File.must_exist pack |> Fiber.return >>? fun pack ->
  first_pass pack |> Scheduler.prj
  >>? fun (hash, oracle, matrix, length, carbon) ->
  second_pass pack (hash, oracle, matrix) >>? fun (hash, matrix) ->
  verify ~verbose fpath hash length carbon matrix |> Scheduler.prj

let run verbose fpath =
  match Fiber.run (run ~verbose fpath) with
  | Ok () -> Ok ()
  | Error (`Msg err) -> Error (Fmt.str "%s." err)

open Cmdliner

let verbose =
  let doc =
    "After verifying the pack, show list of objects contained in the pack and \
     histogram of delta chain length."
  in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let fpath =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.file_exists x -> Ok v
    | Ok v -> R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err
  in
  Arg.conv (parser, Fpath.pp)

let fpath =
  let doc = "The idx files to verify." in
  Arg.(required & pos ~rev:true 0 (some fpath) None & info [] ~doc)

let cmd =
  let doc = "Validate packed Git archive files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Reads given idx file for packed Git archive created with the $(i,git) \
         $(i,pack-objets) command and verifies idx file and the corresponding \
         pack file.";
    ]
  in
  let info = Cmd.info "verify-pack" ~doc ~man in
  Cmd.v info Term.(const run $ verbose $ fpath)

let () = exit @@ Cmd.eval_result cmd
