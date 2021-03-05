open Carton

type ('uid, 's) light_load = 'uid -> (kind * int, 's) io
type ('uid, 's) heavy_load = 'uid -> (Dec.v, 's) io
type optint = Optint.t

let blit_from_string src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len
  [@@inline]

let src = Logs.Src.create "thin"

module Log = (val Logs.src_log src : Logs.LOG)

exception Exists

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Uid : UID) =
struct
  let ( >>= ) x f = IO.bind x f
  let return x = IO.return x
  let ( >>? ) x f = x >>= function Ok x -> f x | Error _ as err -> return err

  let sched =
    let open Scheduler in
    {
      Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Carton.return = (fun x -> inj (return x));
    }

  let read stream =
    let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in

    let rec go filled inputs =
      match Ke.Rke.N.peek ke with
      | [] -> (
          stream () >>= function
          | Some (src, off, len) ->
              Ke.Rke.N.push ke ~blit:blit_from_string ~length:String.length ~off
                ~len src;
              go filled inputs
          | None -> return filled)
      | src :: _ ->
          let src = Cstruct.of_bigarray src in
          let len = min (Cstruct.len inputs) (Cstruct.len src) in
          Cstruct.blit src 0 inputs 0 len;
          Ke.Rke.N.shift_exn ke len;
          if len < Cstruct.len inputs then
            go (filled + len) (Cstruct.shift inputs len)
          else return (filled + len)
    in
    fun filled inputs -> go filled inputs

  module Verify = Carton.Dec.Verify (Uid) (Scheduler) (IO)
  module Fp = Carton.Dec.Fp (Uid)

  let first_pass ~zl_buffer ~digest stream =
    let fl_buffer = Cstruct.create De.io_buffer_size in
    let zl_window = De.make_window ~bits:15 in

    let allocate _ = zl_window in

    let read_cstruct = read stream in
    let read_bytes () buf ~off ~len =
      let rec go rest raw =
        if rest <= 0 then (
          Cstruct.blit_to_bytes fl_buffer 0 buf off len;
          return (abs rest + len))
        else
          read_cstruct 0 raw >>= function
          | 0 ->
              (* TODO(dinosaure): end of flow, add a test. *)
              return (len - rest)
          | filled -> go (rest - filled) (Cstruct.shift raw filled)
      in
      go len fl_buffer
    in
    let read_bytes () buf ~off ~len =
      Scheduler.inj (read_bytes () buf ~off ~len)
    in

    Fp.check_header sched read_bytes () |> Scheduler.prj
    >>= fun (max, _, len) ->
    let decoder = Fp.decoder ~o:zl_buffer ~allocate `Manual in
    let decoder = Fp.src decoder (Cstruct.to_bigarray fl_buffer) 0 len in

    let children = Hashtbl.create 0x100 in
    let where = Hashtbl.create 0x100 in
    let weight = Hashtbl.create 0x100 in
    let checks = Hashtbl.create 0x100 in
    let matrix = Array.make max Verify.unresolved_node in

    let replace hashtbl k v =
      try
        let v' = Hashtbl.find hashtbl k in
        if v < v' then Hashtbl.replace hashtbl k v'
      with Not_found -> Hashtbl.add hashtbl k v
    in

    let rec go decoder =
      match Fp.decode decoder with
      | `Await decoder ->
          read_cstruct 0 fl_buffer >>= fun len ->
          Log.debug (fun m ->
              m "Refill the first-pass state with %d byte(s)." len);
          go (Fp.src decoder (Cstruct.to_bigarray fl_buffer) 0 len)
      | `Peek decoder ->
          (* XXX(dinosaure): [Fp] does the compression. *)
          let keep = Fp.src_rem decoder in
          read_cstruct 0 (Cstruct.shift fl_buffer keep) >>= fun len ->
          go (Fp.src decoder (Cstruct.to_bigarray fl_buffer) 0 (keep + len))
      | `Entry ({ Fp.kind = Base _; offset; size; crc; _ }, decoder) ->
          let n = Fp.count decoder - 1 in
          Log.debug (fun m -> m "[+] base object (%d) (%Ld)." n offset);
          replace weight offset size;
          Hashtbl.add where offset n;
          Hashtbl.add checks offset crc;
          matrix.(n) <- Verify.unresolved_base ~cursor:offset;
          go decoder
      | `Entry
          ( { Fp.kind = Ofs { sub = s; source; target }; offset; crc; _ },
            decoder ) ->
          let n = Fp.count decoder - 1 in
          Log.debug (fun m -> m "[+] ofs object (%d) (%Ld)." n offset);
          replace weight Int64.(sub offset (Int64.of_int s)) source;
          replace weight offset target;
          Hashtbl.add where offset n;
          Hashtbl.add checks offset crc;

          (try
             let vs =
               Hashtbl.find children (`Ofs Int64.(sub offset (of_int s)))
             in
             Hashtbl.replace children
               (`Ofs Int64.(sub offset (of_int s)))
               (offset :: vs)
           with Not_found ->
             Hashtbl.add children
               (`Ofs Int64.(sub offset (of_int s)))
               [ offset ]);
          go decoder
      | `Entry
          ({ Fp.kind = Ref { ptr; target; source }; offset; crc; _ }, decoder)
        ->
          let n = Fp.count decoder - 1 in
          Log.debug (fun m ->
              m "[+] ref object (%d) (%Ld) (weight: %d)." n offset
                (Stdlib.max target source :> int));
          replace weight offset (Stdlib.max target source);
          Hashtbl.add where offset n;
          Hashtbl.add checks offset crc;

          (try
             let vs = Hashtbl.find children (`Ref ptr) in
             Hashtbl.replace children (`Ref ptr) (offset :: vs)
           with Not_found -> Hashtbl.add children (`Ref ptr) [ offset ]);
          go decoder
      | `End uid -> return (Ok uid)
      | `Malformed err ->
          Log.err (fun m -> m "Got an error: %s." err);
          return (Error (`Msg err))
    in
    go decoder >>? fun uid ->
    Log.debug (fun m -> m "First pass on incoming PACK file is done.");
    return
      (Ok
         ( {
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
             digest;
             weight = (fun ~cursor -> Hashtbl.find weight cursor);
           },
           matrix,
           where,
           checks,
           children,
           uid ))

  type ('t, 'path, 'fd, 'error) fs = {
    create : 't -> 'path -> ('fd, 'error) result IO.t;
    append : 't -> 'fd -> string -> unit IO.t;
    map : 't -> 'fd -> pos:int64 -> int -> Bigstringaf.t;
    close : 't -> 'fd -> (unit, 'error) result IO.t;
  }

  module Set = Set.Make (Uid)

  let zip a b =
    if Array.length a <> Array.length b then invalid_arg "zip: lengths mismatch";
    Array.init (Array.length a) (fun i -> a.(i), b.(i))

  let share l0 l1 =
    try
      List.iter
        (fun (v, _) -> if List.exists (Int64.equal v) l1 then raise Exists)
        l0;
      false
    with Exists -> true

  let verify ?(threads = 4) ~digest t path { create; append; map; close } stream
      =
    let zl_buffer = De.bigstring_create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    let weight = ref 0L in
    create t path >>? fun fd ->
    let stream () =
      stream () >>= function
      | Some (buf, off, len) as res ->
          append t fd (String.sub buf off len) >>= fun () ->
          weight := Int64.add !weight (Int64.of_int len);
          return res
      | none -> return none
    in
    Log.debug (fun m -> m "Start to analyse the PACK file.");
    first_pass ~zl_buffer ~digest stream
    >>? fun (oracle, matrix, where, checks, children, uid) ->
    let weight = !weight in
    let pack =
      Carton.Dec.make fd ~allocate ~z:zl_buffer ~uid_ln:Uid.length
        ~uid_rw:Uid.of_raw_string (fun _ -> assert false)
    in
    let map fd ~pos len =
      let len = min len Int64.(to_int (sub weight pos)) in
      map t fd ~pos len
    in
    Log.debug (fun m -> m "Start to verify incoming PACK file (second pass).");
    Verify.verify ~threads pack ~map ~oracle ~matrix ~verbose:ignore
    >>= fun () ->
    Log.debug (fun m -> m "Second pass on incoming PACK file is done.");
    let offsets =
      Hashtbl.fold (fun k _ a -> k :: a) where []
      |> List.sort Int64.compare
      |> Array.of_list
    in
    let unresolveds, resolveds =
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
    in
    let requireds =
      Hashtbl.fold
        (fun k vs a ->
          match k with
          | `Ofs _ -> a
          | `Ref uid -> if share unresolveds vs then Set.add uid a else a)
        children Set.empty
    in
    close t fd >>? fun () ->
    Log.debug (fun m ->
        m "PACK file verified (%d resolved object(s), %d unresolved object(s))"
          (List.length resolveds) (List.length unresolveds));
    return
      (Ok
         ( Hashtbl.length where,
           Set.elements requireds,
           unresolveds,
           resolveds,
           weight,
           uid ))

  let find _ = assert false

  let vuid =
    { Carton.Enc.uid_ln = Uid.length; Carton.Enc.uid_rw = Uid.to_raw_string }

  type nonrec light_load = (Uid.t, Scheduler.t) light_load
  type nonrec heavy_load = (Uid.t, Scheduler.t) heavy_load

  let canonicalize ~light_load ~heavy_load ~src ~dst t
      { create; append; close; map; _ } n uids weight =
    let b =
      {
        Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
        Carton.Enc.q = De.Queue.create 0x10000;
        Carton.Enc.w = De.Lz77.make_window ~bits:15;
      }
    in
    let ctx = ref Uid.empty in
    let cursor = ref 0L in
    let light_load uid = Scheduler.prj (light_load uid) in
    create t dst >>? fun fd ->
    let header = Bigstringaf.create 12 in
    Carton.Enc.header_of_pack ~length:(n + List.length uids) header 0 12;
    let hdr = Bigstringaf.to_string header in
    append t fd hdr >>= fun () ->
    ctx := Uid.feed !ctx header;
    cursor := Int64.add !cursor 12L;
    let encode_base uid =
      light_load uid >>= fun (kind, length) ->
      let entry = Carton.Enc.make_entry ~kind ~length uid in
      let anchor = !cursor in
      let crc = ref Checkseum.Crc32.default in
      Carton.Enc.entry_to_target sched ~load:heavy_load entry |> Scheduler.prj
      >>= fun target ->
      Carton.Enc.encode_target sched ~b ~find ~load:heavy_load ~uid:vuid target
        ~cursor:(Int64.to_int anchor)
      |> Scheduler.prj
      >>= fun (len, encoder) ->
      let rec go encoder =
        match Carton.Enc.N.encode ~o:b.o encoder with
        | `Flush (encoder, len) ->
            append t fd (Bigstringaf.substring b.o ~off:0 ~len) >>= fun () ->
            ctx := Uid.feed !ctx ~off:0 ~len b.o;
            crc := Checkseum.Crc32.digest_bigstring b.o 0 len !crc;
            cursor := Int64.add !cursor (Int64.of_int len);
            let encoder =
              Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o)
            in
            go encoder
        | `End -> return { Carton.Dec.Idx.crc = !crc; offset = anchor; uid }
      in
      append t fd (Bigstringaf.substring b.o ~off:0 ~len) >>= fun () ->
      ctx := Uid.feed !ctx ~off:0 ~len b.o;
      crc := Checkseum.Crc32.digest_bigstring b.o 0 len !crc;
      cursor := Int64.add !cursor (Int64.of_int len);
      let encoder = Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o) in
      go encoder
    in
    let rec go acc = function
      | [] -> return (List.rev acc)
      | uid :: uids -> encode_base uid >>= fun entry -> go (entry :: acc) uids
    in
    go [] uids >>= fun entries ->
    let shift = Int64.sub !cursor 12L in
    let top = Int64.sub weight (Int64.of_int Uid.length) in
    let rec go src pos =
      let max = Int64.sub top pos in
      let len = min max (Int64.mul 1024L 1024L) in
      let len = Int64.to_int len in
      let raw = map t src ~pos len in
      append t fd (Bigstringaf.to_string raw) >>= fun () ->
      ctx := Uid.feed !ctx raw;
      cursor := Int64.add !cursor (Int64.of_int len);
      if Int64.add pos (Int64.of_int len) < top then
        go src (Int64.add pos (Int64.of_int len))
      else
        let uid = Uid.get !ctx in
        append t fd (Uid.to_raw_string uid) >>= fun () ->
        return (Ok (Int64.(add !cursor (of_int Uid.length)), uid))
    in
    create t src >>? fun src ->
    go src 12L >>? fun (weight, uid) ->
    close t fd >>? fun () -> return (Ok (shift, weight, uid, entries))
end
