module Decoder = struct
  type 'a decoder = {
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.Weighted.t;
    buffer : Bigstringaf.t;
    mutable state : 'a Angstrom.Unbuffered.state;
    mutable closed : bool;
  }

  let decoder parser buffer =
    {
      queue = Ke.Rke.Weighted.from buffer;
      buffer;
      state = Angstrom.Unbuffered.parse parser;
      closed = false;
    }

  let rec decode decoder =
    let open Angstrom.Unbuffered in
    match decoder.state with
    | Partial { committed; continue } ->
        Ke.Rke.Weighted.N.shift_exn decoder.queue committed ;
        Ke.Rke.Weighted.compress decoder.queue ;
        let more = if decoder.closed then Complete else Incomplete in
        let off = 0 and len = Ke.Rke.Weighted.length decoder.queue in
        if len > 0 || decoder.closed
        then (
          decoder.state <- continue decoder.buffer ~off ~len more ;
          protect decoder)
        else `Await
    | Fail (committed, _, err) ->
        Ke.Rke.Weighted.N.shift_exn decoder.queue committed ;
        `Malformed err
    | Done (committed, v) -> (
        Ke.Rke.Weighted.N.shift_exn decoder.queue committed ;
        Ke.Rke.Weighted.compress decoder.queue ;
        match Ke.Rke.Weighted.N.peek decoder.queue with
        | rest :: _ -> `End (Some rest, v)
        | [] -> `End (None, v))

  and protect decoder =
    match decoder.state with
    | Partial { committed = 0; _ } -> `Await
    | _ -> decode decoder

  let blit_from_string src src_off dst dst_off len =
    Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

  let src decoder src off len =
    if off < 0 || len < 0 || off + len > String.length src
    then invalid_arg "Invalid bounds" ;
    match
      Ke.Rke.Weighted.N.push decoder.queue ~blit:blit_from_string
        ~length:String.length ~off ~len src
    with
    | Some _ ->
        if len = 0 then decoder.closed <- true ;
        Ok ()
    | None -> Error `Internal_queue_is_full

  let blit src src_off dst dst_off len =
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let push decoder payload ~off ~len =
    match
      Ke.Rke.Weighted.N.push decoder.queue ~blit ~length:Bigstringaf.length ~off
        ~len payload
    with
    | Some _ -> Ok ()
    | None -> Error `Internal_queue_is_full

  let close decoder = decoder.closed <- true
end

module Encoder = struct
  type encoder = {
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.Weighted.t;
    buffer : Bigstringaf.t;
    mutable state : Encore.Lavoisier.state;
    mutable closed : bool;
  }

  let encoder serializer v buffer =
    {
      queue = Ke.Rke.Weighted.from buffer;
      buffer;
      state = Encore.Lavoisier.emit v serializer;
      closed = false;
    }

  let blit src src_off dst dst_off len =
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let encode encoder `Await =
    match encoder.state with
    | Done ->
        encoder.closed <- true ;
        if Ke.Rke.Weighted.is_empty encoder.queue then `Ok else `Partial
    | Fail -> `Malformed
    | Partial { buffer; off; len; continue } ->
    match
      Ke.Rke.Weighted.N.push encoder.queue ~blit ~length:Bigstringaf.length ~off
        ~len buffer
    with
    | Some _ ->
        encoder.state <- continue ~committed:len ;
        `Partial
    | None -> `Internal_queue_is_full

  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

  let dst encoder dst off len =
    if off < 0 || len < 0 || off + len > Bytes.length dst
    then invalid_arg "Invalid bounds" ;
    if len = 0 || Ke.Rke.Weighted.length encoder.queue = 0
    then if encoder.closed then None else Some 0
    else
      let len = min (Ke.Rke.Weighted.length encoder.queue) len in
      Ke.Rke.Weighted.N.keep_exn encoder.queue ~blit ~length:Bytes.length ~off
        ~len dst ;
      Ke.Rke.Weighted.N.shift_exn encoder.queue len ;
      Some len

  let peek encoder =
    if encoder.closed && Ke.Rke.Weighted.length encoder.queue = 0
    then None
    else Some (Ke.Rke.Weighted.N.peek encoder.queue)

  let shift encoder len = Ke.Rke.Weighted.N.shift_exn encoder.queue len
end

let hdr = function
  | `Blob -> Fmt.strf "blob %Ld\000"
  | `Tree -> Fmt.strf "tree %Ld\000"
  | `Tag -> Fmt.strf "tag %Ld\000"
  | `Commit -> Fmt.strf "commit %Ld\000"

type ('uid, 't) digest = {
  empty : 't;
  feed_string : string -> 't -> 't;
  feed_bigstring : Bigstringaf.t -> 't -> 't;
  get : 't -> 'uid;
}

let digest digest buffer kind length serializer v =
  let encoder = Encoder.encoder serializer v buffer in
  let rec go ctx =
    match Encoder.encode encoder `Await with
    | `Malformed -> failwith "digest: value is malformed"
    | `Internal_queue_is_full -> failwith "digest: internal queue is full"
    | `Ok -> digest.get ctx
    | `Partial ->
    match Encoder.peek encoder with
    | Some payloads ->
        let ctx, len =
          let fold (ctx, len) payload =
            (digest.feed_bigstring payload ctx, len + Bigstringaf.length payload)
          in
          List.fold_left fold (ctx, 0) payloads in
        Encoder.shift encoder len ;
        go ctx
    | None -> digest.get ctx in
  let ctx = digest.empty in
  let hdr = hdr kind (length v) in
  go (digest.feed_string hdr ctx)

let compress serializer v buffer0 buffer1 window queue flush =
  let encoder = Encoder.encoder serializer v buffer0 in
  let rec compress zl =
    match Zl.Def.encode zl with
    | `Await zl -> refill zl
    | `Flush zl ->
        let len = Bigstringaf.length buffer1 - Zl.Def.dst_rem zl in
        flush (Some (buffer1, 0, len)) ;
        let zl = Zl.Def.dst zl buffer1 0 (Bigstringaf.length buffer1) in
        compress zl
    | `End _ -> flush None
  and refill zl =
    match Encoder.encode encoder `Await with
    | `Malformed -> failwith "compress: malformed OCaml value"
    | `Internal_queue_is_full -> failwith "compress: internal queue is full"
    | `Ok ->
        let zl = Zl.Def.src zl Bigstringaf.empty 0 0 in
        compress zl
    | `Partial ->
    match Encoder.peek encoder with
    | Some (payload :: _) ->
        let len = Bigstringaf.length payload in
        let zl = Zl.Def.src zl payload 0 len in
        Encoder.shift encoder len ;
        compress zl
    | Some [] -> refill zl
    | None ->
        let zl = Zl.Def.src zl Bigstringaf.empty 0 0 in
        compress zl in
  let zl = Zl.Def.encoder `Manual `Manual ~q:queue ~w:window ~level:2 in
  compress zl

let uncompress { Carton.bind; Carton.return } parser buffer0 buffer1 buffer2
    allocate refill =
  let ( >>= ) = bind in

  let decoder = Decoder.decoder parser buffer0 in
  let rec parse zl =
    match Decoder.decode decoder with
    | `Await -> uncompress zl
    | `End (_, v) -> return (Ok v)
    | `Malformed err -> return (Error (`Msg err))
  and uncompress zl =
    match Zl.Inf.decode zl with
    | `Malformed err -> return (Error (`Msg err))
    | `Await zl ->
        refill buffer2 >>= fun len ->
        let zl = Zl.Inf.src zl buffer2 0 len in
        uncompress zl
    | `Flush zl -> (
        let len = Bigstringaf.length buffer1 - Zl.Inf.dst_rem zl in
        let zl = Zl.Inf.flush zl in
        match Decoder.push decoder buffer1 ~off:0 ~len with
        | Ok () -> parse (Zl.Inf.flush zl)
        | Error err -> return (Error err))
    | `End zl ->
        Decoder.close decoder ;
        parse zl in
  let zl = Zl.Inf.decoder `Manual ~o:buffer1 ~allocate in
  parse zl
