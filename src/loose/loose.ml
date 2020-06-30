let src = Logs.Src.create "git.loose" ~doc:"logs git's loose event"

module Log = (val Logs.src_log src : Logs.LOG)

type ('a, 's) io = ('a, 's) Carton.io

let ( <.> ) f g x = f (g x)

module type UID = sig
  include Carton.UID

  val to_hex : t -> string
end

type kind = [ `A | `B | `C | `D ]

type ('t, 'brk, 'error, 's) store = {
  map : 't -> 'brk -> pos:int64 -> int -> (Bigstringaf.t, 's) io;
  mem : 't -> 'brk -> (bool, 's) io;
  list : 't -> ('brk list, 's) io;
  append : 't -> 'brk -> Bigstringaf.t -> ((unit, 'error) result, 's) io;
  appendv : 't -> 'brk -> Bigstringaf.t list -> ((unit, 'error) result, 's) io;
}

type buffers = {
  window : De.window;
  queue : De.Queue.t;
  i : De.bigstring;
  o : De.bigstring;
  hdr : Cstruct.t;
}

let reword_error f x = match x with Ok x -> Ok x | Error err -> Error (f err)

module Make (Uid : UID) = struct
  let list t store = store.list t

  let exists t store uid = store.mem t uid

  let atomic_add { Carton.return; bind } t buffers store ~hdr v =
    let ( >>= ) = bind in
    let ( >>| ) x f = x >>= fun x -> return (f x) in
    let ( >>? ) x f =
      x >>= function Ok x -> f x | Error _ as err -> return err in

    let hdr = hdr ~buffer:buffers.hdr v in
    let encoder =
      Zl.Def.encoder `Manual `Manual ~q:buffers.queue ~w:buffers.window ~level:2
    in
    (* TODO(dinosaure): delete [rec], we should have only one [`Flush] and [`End]. *)
    let rec go encoder srcs dst =
      match (srcs, Zl.Def.encode encoder) with
      | src :: srcs, `Await encoder ->
          let encoder =
            Zl.Def.src encoder (Cstruct.to_bigarray src) 0 (Cstruct.len src)
          in
          go encoder srcs dst
      | [], `Await encoder ->
          let encoder = Zl.Def.src encoder Bigstringaf.empty 0 0 in
          go encoder [] dst
      | _, `Flush encoder ->
          let len = Cstruct.len dst - Zl.Def.dst_rem encoder in
          if len = Cstruct.len dst
          then Error `Non_atomic
          else
            let dst = Cstruct.shift dst len in
            go
              (Zl.Def.dst encoder (Cstruct.to_bigarray dst) 0 (Cstruct.len dst))
              srcs dst
      | _, `End encoder ->
          let len = Cstruct.len dst - Zl.Def.dst_rem encoder in
          Rresult.R.ok (Cstruct.len (Cstruct.shift dst len)) in
    let encoder =
      Zl.Def.dst encoder buffers.o 0 (Bigstringaf.length buffers.o) in
    let contents =
      Cstruct.of_bigarray (Carton.Dec.raw v) ~off:0 ~len:(Carton.Dec.len v)
    in
    let uid =
      let fold ctx payload = Uid.feed ctx (Cstruct.to_bigarray payload) in
      let ctx = List.fold_left fold Uid.empty [ hdr; contents ] in
      Uid.get ctx in
    match go encoder [ hdr; contents ] (Cstruct.of_bigarray buffers.o) with
    | Ok rest ->
        let len = Bigstringaf.length buffers.o - rest in
        Log.debug (fun m -> m "Atomic write of %a." Uid.pp uid) ;
        store.append t uid (Bigstringaf.sub buffers.o ~off:0 ~len)
        >>| reword_error (fun err -> `Store err)
        >>? fun () -> return (Ok (uid, len))
    | Error _ as err -> return err

  let add { Carton.return; bind } t buffers store ~hdr stream =
    let ( >>= ) = bind in
    let ( >>| ) x f = x >>= fun x -> return (f x) in
    let ( >>? ) x f =
      x >>= function Ok x -> f x | Error _ as err -> return err in

    let encoder =
      Zl.Def.encoder `Manual `Manual ~q:buffers.queue ~w:buffers.window ~level:2
    in
    let ctx = ref Uid.empty in
    let rec go ((src, off, len) as payload) dsts encoder =
      match Zl.Def.encode encoder with
      | `Await encoder -> (
          if len > 0
          then (
            let max = min len (Bigstringaf.length buffers.i) in
            Bigstringaf.blit_from_string src ~src_off:off buffers.i ~dst_off:0
              ~len:max ;
            ctx := Uid.feed !ctx buffers.i ~off:0 ~len:max ;
            let encoder = Zl.Def.src encoder buffers.i 0 len in
            go (src, off + len, len - max) dsts encoder)
          else
            stream () >>= function
            | Some src -> go (src, 0, String.length src) dsts encoder
            | None ->
                let encoder = Zl.Def.src encoder Bigstringaf.empty 0 0 in
                go payload dsts encoder)
      | `Flush encoder ->
          let len = Bigstringaf.length buffers.o - Zl.Def.dst_rem encoder in
          let raw = Bigstringaf.copy buffers.o ~off:0 ~len in
          let encoder =
            Zl.Def.dst encoder buffers.o 0 (Bigstringaf.length buffers.o) in
          go payload (raw :: dsts) encoder
      | `End encoder ->
          let len = Bigstringaf.length buffers.o - Zl.Def.dst_rem encoder in
          let raw = Bigstringaf.copy buffers.o ~off:0 ~len in
          return (List.rev (raw :: dsts)) in
    let encoder =
      Zl.Def.dst encoder buffers.o 0 (Bigstringaf.length buffers.o) in
    ctx := Uid.feed !ctx (Cstruct.to_bigarray hdr) ~off:0 ~len:(Cstruct.len hdr) ;
    go (Cstruct.to_string hdr, 0, Cstruct.len hdr) [] encoder >>= fun vs ->
    let uid = Uid.get !ctx in
    let len = List.fold_right (( + ) <.> Bigstringaf.length) vs 0 in
    (* XXX(dinosaure): shame! *)
    store.appendv t uid vs >>| reword_error (fun err -> `Store err)
    >>? fun () -> return (Ok (uid, len))

  let atomic_get { Carton.return; bind } t buffers store ~hdr uid =
    let ( >>= ) = bind in

    store.map t uid ~pos:0L (Bigstringaf.length buffers.i) >>= fun i ->
    let decoder =
      Zl.Inf.decoder `Manual ~allocate:(fun _ -> buffers.window) ~o:buffers.o
    in
    let decoder = Zl.Inf.src decoder i 0 (Bigstringaf.length i) in
    let res =
      match Zl.Inf.decode decoder with
      | `Await _ -> Error `Non_atomic
      | `Malformed _ -> Error `Non_atomic
      | `Flush decoder ->
          let len = Bigstringaf.length buffers.o - Zl.Inf.dst_rem decoder in
          Ok (Zl.Inf.flush decoder, len)
      | `End _ -> Ok (Zl.Inf.flush decoder, 0) in
    let open Rresult in
    match res with
    | Ok (_, len) ->
        let raw = Cstruct.of_bigarray buffers.o ~off:0 ~len in
        let contents, kind, length = hdr raw in
        if Int64.of_int (Cstruct.len contents) <> length
        then return (Error `Non_atomic)
        else return (Ok (Carton.Dec.v ~kind (Cstruct.to_bigarray contents)))
    | Error _ as err -> return err

  let size_and_kind { Carton.return; bind } t buffers store ~hdr uid =
    let ( >>= ) = bind in

    store.map t uid ~pos:0L (2 + 286 + 1 + 20 + 1) >>= fun i ->
    let decoder =
      Zl.Inf.decoder `Manual ~allocate:(fun _ -> buffers.window) ~o:buffers.o
    in
    let decoder = Zl.Inf.src decoder i 0 (Bigstringaf.length i) in
    let res =
      match Zl.Inf.decode decoder with
      | `Await _ ->
          let len = Bigstringaf.length buffers.o - Zl.Inf.dst_rem decoder in
          Ok len
      | `Malformed _ -> Error `Malformed
      | `Flush decoder ->
          let len = Bigstringaf.length buffers.o - Zl.Inf.dst_rem decoder in
          Ok len
      | `End _ -> Error `Malformed in
    let open Rresult in
    match res with
    | Ok len ->
        let raw = Cstruct.of_bigarray buffers.o ~off:0 ~len in
        let _contents, kind, length = hdr raw in
        return (Ok (length, kind))
    | Error _ as err -> return err

  let get { Carton.return; bind } t buffers store ~hdr uid =
    let ( >>= ) = bind in
    let ( >>? ) x f =
      x >>= function Ok x -> f x | Error _ as err -> return err in

    let decoder =
      Zl.Inf.decoder `Manual ~allocate:(fun _ -> buffers.window) ~o:buffers.o
    in
    let rec go pos dsts decoder =
      match Zl.Inf.decode decoder with
      | `Await decoder ->
          store.map t uid ~pos (Bigstringaf.length buffers.i) >>= fun i ->
          let len = Bigstringaf.length i in
          let decoder = Zl.Inf.src decoder i 0 len in
          go Int64.(add pos (of_int len)) dsts decoder
      | `Flush decoder ->
          let len = Bigstringaf.length buffers.o - Zl.Inf.dst_rem decoder in
          go pos
            (Bigstringaf.copy buffers.o ~off:0 ~len :: dsts)
            (Zl.Inf.flush decoder)
      | `Malformed err -> return (Rresult.R.error_msg err)
      | `End decoder ->
          let len = Bigstringaf.length buffers.o - Zl.Inf.dst_rem decoder in
          return
            (Ok (List.rev (Bigstringaf.copy buffers.o ~off:0 ~len :: dsts)))
    in
    go 0L [] decoder >>? fun vs ->
    let raw = Cstruct.concat (List.map Cstruct.of_bigarray vs) in
    (* XXX(dinosaure): ropes? *)
    let contents, kind, _length = hdr raw in
    (* assert (_length = len); *)
    return (Ok (Carton.Dec.v ~kind (Cstruct.to_bigarray contents)))
end
