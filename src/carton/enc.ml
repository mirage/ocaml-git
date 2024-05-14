open Sigs

type 'uid entry = {
  uid : 'uid;
  kind : kind;
  length : int;
  preferred : bool;
  delta : 'uid delta;
}

and 'uid delta = From of 'uid | Zero

let make_entry ~kind ~length ?(preferred = false) ?(delta = Zero) uid =
  { uid; kind; length; preferred; delta }

let length { length; _ } = length

module Utils = struct
  let length_of_variable_length n =
    let rec go r = function 0 -> r | n -> go (succ r) (n lsr 7) in
    go 1 (n lsr 7)

  let cmd off len =
    let cmd = ref 0 in

    if off land 0x000000ff <> 0 then cmd := !cmd lor 0x01;
    if off land 0x0000ff00 <> 0 then cmd := !cmd lor 0x02;
    if off land 0x00ff0000 <> 0 then cmd := !cmd lor 0x04;
    if off land 0x7f000000 <> 0 then cmd := !cmd lor 0x08;

    if len land 0x0000ff <> 0 then cmd := !cmd lor 0x10;
    if len land 0x00ff00 <> 0 then cmd := !cmd lor 0x20;
    if len land 0xff0000 <> 0 then cmd := !cmd lor 0x40;

    !cmd
  [@@inline]

  let length_of_copy_code ~off ~len =
    let required =
      let a = [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4 |] in
      fun x -> a.(x land 0xf) + a.(x lsr 4)
    in
    let cmd = cmd off len in
    required cmd

  let length ~source ~target hunks =
    length_of_variable_length source
    + length_of_variable_length target
    + List.fold_left
        (fun acc -> function
          | Duff.Insert (_, len) -> 1 + len + acc
          | Duff.Copy (off, len) -> 1 + length_of_copy_code ~off ~len + acc)
        0 hunks
end

module W = struct
  type 'a t = 'a Weak.t

  let create () = Weak.create 1

  let create_with v =
    let t = Weak.create 1 in
    Weak.set t 0 (Some v);
    t

  let set t v = Weak.set t 0 (Some v)
  let get t = Weak.get t 0
end

type 'uid p = {
  index : Duff.index W.t;
  entry : 'uid entry;
  depth : int;
  v : Dec.v W.t;
}

type 'uid patch = {
  hunks : Duff.hunk list;
  depth : int;
  source : 'uid;
  source_length : int;
}

type 'uid q = {
  mutable patch : 'uid patch option;
  entry : 'uid entry;
  v : Dec.v W.t;
}

let target_uid { entry; _ } = entry.uid
let target_length { entry; _ } = entry.length
let target_patch { patch; _ } = patch
let source_of_patch { source; _ } = source

let pp_patch target_length pp_uid ppf patch =
  Fmt.pf ppf
    "{ @[<hov>hunks= %d;@ depth= %d;@ source= %a;@ source_length= %d;@] }"
    (Utils.length ~source:patch.source_length ~target:target_length patch.hunks)
    patch.depth pp_uid patch.source patch.source_length

[@@@warning "-32"] (* XXX(dinosaure): pretty-printers. *)

let pp_kind ppf = function
  | `A -> Fmt.string ppf "a"
  | `B -> Fmt.string ppf "b"
  | `C -> Fmt.string ppf "c"
  | `D -> Fmt.string ppf "d"

let pp_delta pp_uid ppf = function
  | Zero -> Fmt.string ppf "<none>"
  | From uid -> Fmt.pf ppf "@[<1>(From %a)@]" pp_uid uid

let pp_entry pp_uid ppf entry =
  Fmt.pf ppf
    "{ @[<hov>uid= %a;@ kind= %a;@ length= %d;@ preferred= %b;@ delta= \
     @[<hov>%a@];@] }"
    pp_uid entry.uid pp_kind entry.kind entry.length entry.preferred
    (pp_delta pp_uid) entry.delta

let pp_q pp_uid ppf q =
  Fmt.pf ppf "{ @[<hov>patch= @[<hov>%a@]; entry= @[<hov>%a@]; v= %s@] }"
    Fmt.(Dump.option (pp_patch q.entry.length pp_uid))
    q.patch (pp_entry pp_uid) q.entry
    (if Weak.check q.v 0 then "#raw" else "NULL")

[@@@warning "+32"]

type ('uid, 's) load = 'uid -> (Dec.v, 's) io

let depth_of_source : 'uid p -> int = fun { depth; _ } -> depth

let depth_of_target : 'uid q -> int =
 fun { patch; _ } -> match patch with None -> 1 | Some { depth; _ } -> depth

let target_to_source : 'uid q -> 'uid p =
 fun target ->
  {
    index = W.create ();
    entry = target.entry;
    depth = depth_of_target target;
    v = target.v (* XXX(dinosaure): dragoon here! *);
  }

let entry_to_target :
    type s. s scheduler -> load:('uid, s) load -> 'uid entry -> ('uid q, s) io =
 fun { bind; return } ~load entry ->
  let ( >>= ) = bind in

  load entry.uid >>= fun v ->
  (match entry.delta with
  | From uid ->
      load uid >>= fun s ->
      let source = Bigstringaf.sub ~off:0 ~len:(Dec.len s) (Dec.raw s) in
      let target = Bigstringaf.sub ~off:0 ~len:(Dec.len v) (Dec.raw v) in
      let index =
        Duff.make (Bigstringaf.sub ~off:0 ~len:(Dec.len s) (Dec.raw s))
      in
      let hunks = Duff.delta index ~source ~target in
      return
        (Some
           {
             hunks;
             depth = Dec.depth v;
             source = uid;
             source_length = Dec.len s;
           })
  | Zero -> return None)
  >>= fun patch -> return { patch; entry; v = W.create_with v }

let length_of_delta ~source ~target hunks = Utils.length ~source ~target hunks

exception Break
exception Next

(* XXX(dinosaure): [apply] tries to generate a patch between [source] and [target].
   If the resulted patch is good enough, we set [target.patch] to it. [apply] can raise
   two exceptions:

   - [Break] where it is not able to generate a patch (different kinds)
   - [Next] when it reaches the depth limit or resulted patch is not good enough

   NOTE: [load] must create a new [Bigstringaf.t]! No cache are expected at this
   layer where we already handle it with [W.t] (weak reference). *)
let apply :
    type s uid.
    s scheduler ->
    load:(uid, s) load ->
    uid_ln:int ->
    source:uid p ->
    target:uid q ->
    (unit, s) io =
 fun { bind; return } ~load ~uid_ln ~source ~target ->
  let ( >>= ) = bind in

  (* Don't bother doing diffs between different types. *)
  if source.entry.kind <> target.entry.kind then raise_notrace Break;

  (* Let's not bust the allowed depth. *)
  if depth_of_source source >= _max_depth then raise_notrace Next;

  (* Now some size filtering heuristics. *)
  let max_length, ref_depth =
    match target.patch with
    | Some { hunks; source_length; depth; _ } ->
        ( length_of_delta ~source:source_length ~target:target.entry.length hunks,
          depth )
    | None -> (target.entry.length / 2) - uid_ln, 1
  in

  let max_length =
    max_length
    * (_max_depth - depth_of_source source)
    / (_max_depth - ref_depth + 1)
  in

  if max_length == 0 then raise_notrace Next;

  let diff =
    if source.entry.length < target.entry.length then
      target.entry.length - source.entry.length
    else 0
  in

  if diff >= max_length then raise_notrace Next;
  if target.entry.length < source.entry.length / 32 then raise_notrace Next;

  (* Load data if not already done. *)
  let load_if weak uid =
    match W.get weak with
    | Some v -> return v
    | None ->
        load uid >>= fun v ->
        W.set weak v;
        return v
  in
  (* Load index if not already done (TODO: check it!). *)
  let index_if weak v =
    match W.get weak with
    | Some index -> index
    | None ->
        let index =
          Duff.make (Bigstringaf.sub ~off:0 ~len:(Dec.len v) (Dec.raw v))
        in
        W.set weak index;
        index
  in

  load_if source.v source.entry.uid >>= fun source_v ->
  load_if target.v target.entry.uid >>= fun target_v ->
  index_if source.index source_v |> fun source_index ->
  let target_r =
    Bigstringaf.sub ~off:0 ~len:(Dec.len target_v) (Dec.raw target_v)
  in
  let source_r =
    Bigstringaf.sub ~off:0 ~len:(Dec.len source_v) (Dec.raw source_v)
  in
  let hunks = Duff.delta source_index ~source:source_r ~target:target_r in

  target.patch <-
    Some
      {
        hunks;
        source = source.entry.uid;
        source_length = source.entry.length;
        depth = source.depth + 1;
      };
  return ()

module type VERBOSE = sig
  type 'a fiber

  val succ : unit -> unit fiber
  val print : unit -> unit fiber
end

module type UID = sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
end

module Delta
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Uid : UID)
    (Verbose : VERBOSE with type 'a fiber = 'a IO.t) =
struct
  let ( >>= ) = IO.bind
  let return = IO.return

  let s =
    let open Scheduler in
    {
      bind = (fun x f -> inj (IO.bind (prj x) (fun x -> prj (f x))));
      return = (fun x -> inj (IO.return x));
    }

  let delta :
      load:(Uid.t, Scheduler.t) load ->
      weight:int ->
      uid_ln:int ->
      Uid.t q array ->
      unit IO.t =
   fun ~load ~weight ~uid_ln targets ->
    let window = Array.make weight None in

    let find_delta idx target =
      let best : int ref = ref (-1) in
      let try_delta j source =
        let other_idx = idx + j in
        let other_idx =
          if other_idx >= weight then other_idx - weight else other_idx
        in
        try
          apply s ~load ~uid_ln ~source ~target |> Scheduler.prj >>= fun () ->
          best := other_idx;
          return ()
        with
        | Next -> return ()
        | Break as exn -> raise_notrace exn
      in
      let rec go j =
        if j < 0 then return ()
        else
          match window.(j) with
          | Some (m : Uid.t p) ->
              if m.entry.uid <> target.entry.uid then
                try try_delta j m >>= fun () -> (go [@tailcall]) (pred j)
                with Break -> return ()
              else return ()
          | None -> return ()
        (* TODO: check it! *)
      in
      go (Array.length window - 1) >>= fun () ->
      (if !best >= 0 then Verbose.succ () else return ()) >>= fun () ->
      return !best
    in
    (* XXX(dinosaure): [git] does something a bit complex between the iteration
       over [targets] and the [window]. [n] is the current [target] where we will try
       to apply a patch and [idx] seems a lower-bound of the LRU-cache [window]. *)
    let rec iter n idx =
      if n < Array.length targets then (
        find_delta idx targets.(n) >>= fun best ->
        (* [git] does this update __before__ to try to find a patch. However, it seems fine
           to do that after when an object can not be patched with itself. *)
        window.(idx) <- Some (target_to_source targets.(n));
        Verbose.print () >>= fun () ->
        (* [git] wants to deflate and cache the delta data. Should we do the same? TODO *)
        if
          depth_of_target targets.(n) > 1
          && depth_of_target targets.(n) < _max_depth
        then (
          (* XXX(dinosaure): a slightly assumption, if [target] has a patch,
             [!best] (into [go]) was properly set to a valid source. Of course, that
             means that given [targets] contains non-delta-ified objects. *)
          let swap = window.(best) in

          (* Move the best delta base up in the window, after the currently deltified object, to
             keep it longer. It will be the first base object to be attempted next. *)
          let v = ref best in
          for _ = (weight + idx - best) mod weight to 0 do
            window.(!v) <- window.((!v + 1) mod weight);
            v := (!v + 1) mod weight
          done;

          window.(!v) <- swap);

        if depth_of_target targets.(n) < _max_depth then
          (iter [@tailcall]) (succ n) (if idx + 1 >= weight then 0 else idx + 1)
        else (iter [@tailcall]) (succ n) idx)
      else return ()
    in
    iter 0 0

  type m = { mutable v : int; m : IO.Mutex.t }

  let dispatcher :
      load:(Uid.t, Scheduler.t) load ->
      mutex:m ->
      entries:Uid.t entry array ->
      targets:Uid.t q option array ->
      unit IO.t =
   fun ~load ~mutex ~entries ~targets ->
    let rec go () =
      IO.Mutex.lock mutex.m >>= fun () ->
      let v = mutex.v in
      mutex.v <- mutex.v + 1;
      if v >= Array.length entries then (
        IO.Mutex.unlock mutex.m;
        IO.return ())
      else (
        IO.Mutex.unlock mutex.m;
        entry_to_target s ~load entries.(v) |> Scheduler.prj >>= fun target ->
        targets.(v) <- Some target;
        go ())
    in
    go ()

  let get = function Some x -> x | None -> assert false

  let delta ~threads ~weight ~uid_ln entries =
    let mutex = { v = 0; m = IO.Mutex.create () } in
    let targets = Array.make (Array.length entries) None in
    IO.parallel_iter
      ~f:(fun load -> dispatcher ~load ~mutex ~entries ~targets)
      threads
    >>= fun () ->
    let targets = Array.map get targets in
    delta ~load:(List.hd threads) ~weight ~uid_ln targets >>= fun () ->
    return targets
end

module N : sig
  type encoder
  type b = { i : Bigstringaf.t; q : De.Queue.t; w : De.Lz77.window }

  val encoder :
    's scheduler ->
    ?level:int ->
    b:b ->
    load:('uid, 's) load ->
    'uid q ->
    (encoder, 's) io

  val encode : o:Bigstringaf.t -> encoder -> [ `Flush of encoder * int | `End ]
  val dst : encoder -> Bigstringaf.t -> int -> int -> encoder
end = struct
  type b = { i : Bigstringaf.t; q : De.Queue.t; w : De.Lz77.window }
  type encoder = H of Zh.N.encoder | Z of Zl.Def.encoder

  let rec encode_zlib ~o encoder =
    match Zl.Def.encode encoder with
    | `Await encoder ->
        encode_zlib ~o (Zl.Def.src encoder Bigstringaf.empty 0 0)
    | `Flush encoder ->
        let len = Bigstringaf.length o - Zl.Def.dst_rem encoder in
        `Flush (encoder, len)
    | `End encoder ->
        let len = Bigstringaf.length o - Zl.Def.dst_rem encoder in
        if len > 0 then `Flush (encoder, len) else `End

  let encode_hunk ~o encoder =
    match Zh.N.encode encoder with
    | `Flush encoder ->
        let len = Bigstringaf.length o - Zh.N.dst_rem encoder in
        `Flush (encoder, len)
    | `End -> `End

  let encode ~o = function
    | Z encoder -> (
        match encode_zlib ~o encoder with
        | `Flush (encoder, len) -> `Flush (Z encoder, len)
        | `End -> `End)
    | H encoder -> (
        match encode_hunk ~o encoder with
        | `Flush (encoder, len) -> `Flush (H encoder, len)
        | `End -> `End)

  let dst encoder s j l =
    match encoder with
    | Z encoder ->
        let encoder = Zl.Def.dst encoder s j l in
        Z encoder
    | H encoder ->
        let encoder = Zh.N.dst encoder s j l in
        H encoder

  let encoder :
      type s.
      s scheduler ->
      ?level:int ->
      b:b ->
      load:('uid, s) load ->
      'uid q ->
      (encoder, s) io =
   fun { bind; return } ?(level = 4) ~b ~load target ->
    let ( >>= ) = bind in

    let load_if weak uid =
      match W.get weak with
      | Some v -> return v
      | None ->
          load uid >>= fun v ->
          W.set weak v;
          return v
    in

    match target.patch with
    | Some { hunks; source_length; _ } ->
        load_if target.v target.entry.uid >>= fun v ->
        let raw = Bigstringaf.sub ~off:0 ~len:(Dec.len v) (Dec.raw v) in
        let encoder =
          Zh.N.encoder ~level ~i:b.i ~q:b.q ~w:b.w ~source:source_length raw
            `Manual hunks
        in
        return (H encoder)
    | None ->
        load_if target.v target.entry.uid >>= fun v ->
        let encoder = Zl.Def.encoder `Manual `Manual ~q:b.q ~w:b.w ~level in
        let encoder = Zl.Def.src encoder (Dec.raw v) 0 (Dec.len v) in

        return (Z encoder)
end

type ('uid, 's) find = 'uid -> (int option, 's) io

type b = {
  i : Bigstringaf.t;
  q : De.Queue.t;
  w : De.Lz77.window;
  o : Bigstringaf.t;
}

let encode_header ~o kind length =
  if length < 0 then invalid_arg "encode_header: length must be positive";
  let c = ref ((kind lsl 4) lor (length land 15)) in
  let l = ref (length asr 4) in
  let p = ref 0 in
  let n = ref 1 in

  while !l != 0 do
    Bigstringaf.set o !p (Char.chr (!c lor 0x80 land 0xff));
    incr p;
    c := !l land 0x7f;
    l := !l asr 7;
    incr n
  done;

  Bigstringaf.set o !p (Char.unsafe_chr !c);
  !n

type 'uid uid = { uid_ln : int; uid_rw : 'uid -> string }

let kind_to_int = function
  | `A -> 0b001
  | `B -> 0b010
  | `C -> 0b011
  | `D -> 0b100

let header_of_pack ~length buf off len =
  if off < 0 || len < 0 || off + len > Bigstringaf.length buf || len < 4 + 4 + 4
  then Fmt.invalid_arg "header_of_pack";
  Bigstringaf.set_int32_be buf (off + 0) 0x5041434bl;
  Bigstringaf.set_int32_be buf (off + 4) 0x2l;
  Bigstringaf.set_int32_be buf (off + 8) (Int32.of_int length)

let encode_target :
    type s.
    s scheduler ->
    ?level:int ->
    b:b ->
    find:('uid, s) find ->
    load:('uid, s) load ->
    uid:'uid uid ->
    'uid q ->
    cursor:int ->
    (int * N.encoder, s) io =
 fun ({ bind; return } as s) ?level ~b ~find ~load ~uid target ~cursor ->
  let ( >>= ) = bind in

  match target.patch with
  | None ->
      let off =
        encode_header ~o:b.o (kind_to_int target.entry.kind) target.entry.length
      in
      N.encoder s ?level ~b:{ i = b.i; q = b.q; w = b.w } ~load target
      >>= fun encoder ->
      return (off, N.dst encoder b.o off (Bigstringaf.length b.o - off))
  | Some { source; source_length; hunks; _ } -> (
      find source >>= function
      | Some offset ->
          let off =
            encode_header ~o:b.o 0b110
              (Utils.length ~source:source_length ~target:target.entry.length
                 hunks)
          in
          let buf = Bytes.create 10 in

          let p = ref (10 - 1) in
          let n = ref (cursor - offset) in

          Bytes.set buf !p (Char.unsafe_chr (!n land 127));
          while !n asr 7 <> 0 do
            n := !n asr 7;
            decr p;
            Bytes.set buf !p (Char.unsafe_chr (128 lor ((!n - 1) land 127)));
            decr n
          done;

          Bigstringaf.blit_from_bytes buf ~src_off:!p b.o ~dst_off:off
            ~len:(10 - !p);
          N.encoder s ~b:{ i = b.i; q = b.q; w = b.w } ~load target
          >>= fun encoder ->
          let off = off + (10 - !p) in
          let len = Bigstringaf.length b.o - off in
          return (off, N.dst encoder b.o off len)
      | None ->
          let off =
            encode_header ~o:b.o 0b111
              (Utils.length ~source:source_length ~target:target.entry.length
                 hunks)
          in
          let raw = uid.uid_rw source in
          Bigstringaf.blit_from_string raw ~src_off:0 b.o ~dst_off:off
            ~len:uid.uid_ln;
          N.encoder s ~b:{ i = b.i; q = b.q; w = b.w } ~load target
          >>= fun encoder ->
          let off = off + uid.uid_ln in
          let len = Bigstringaf.length b.o - off in
          return (off, N.dst encoder b.o off len))
