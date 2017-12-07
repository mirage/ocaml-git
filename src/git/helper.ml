(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "git.helper" ~doc:"logs git's internal helper"
module Log = (val Logs.src_log src : Logs.LOG)

let ppe ~name ppv =
  Fmt.braces (fun ppf -> Fmt.pf ppf "%s %a" name (Fmt.hvbox ppv))

module BaseBytes = struct
  [@@@warning "-44"] (* XXX(dinosaure): shadowing of [compare] and [equal]. *)

  open Bytes

  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = Hashtbl.hash

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)

  let pp = Fmt.iter Bytes.iter (Fmt.using Char.code (fun ppf -> Fmt.pf ppf "%02x"))
  let to_hex x = let `Hex v = Hex.of_string (Bytes.unsafe_to_string x) in v
  let of_hex x = Hex.to_string (`Hex x) |> Bytes.unsafe_of_string
end

module MakeDecoder (A: S.ANGSTROM) = struct
  (* XXX(dinosaure): This decoder is on top of some assertion about
     the decoding of a Git object. [Angstrom] can not consume all
     input (when we have an alteration specifically) and the client
     need to keep a part of the previous input but continue to feed
     the parser with a new input (to determine the choice of the
     alteration). In the Git format, we can have this problem but for
     few bytes (something like ten bytes). So, we consider to use an
     internal buffer (than the size is bigger than what is needed) and
     we ensure than is not possible to keep more than the size of the
     internal buffer when we parse a Git object.

     However, this case appear in the code (when we don't have enough
     trailing space and writable space). This case appear when we
     don't parse a Git object but something else (which is not respect
     our assertions). And if this case appear, we can consider that
     someone try to cheat you. So we prefer to return an error.

     Obviously, in a better world with unicorn and pineapple pizza, we
     can just avoid this case. *)

  let src = Logs.Src.create "git.decoder" ~doc:"logs git's internal decoder"
  module Log = (val Logs.src_log src : Logs.LOG)

  type error = [ `Decoder of string ]
  type init = Cstruct.t
  type t = A.t

  let pp_error ppf (`Decoder err) = ppe ~name:"`Decoder" Fmt.string ppf err

  type decoder =
    { state    : Angstrom.bigstring -> Angstrom.Unbuffered.more -> A.t Angstrom.Unbuffered.state
    ; final    : Angstrom.Unbuffered.more
    ; internal : Cstruct.t
    ; max      : int }

  let kdone (consumed, value) =
    fun _ _ -> Angstrom.Unbuffered.Done (consumed, value)

  let kfail (consumed, path, err) =
    fun _ _ -> Angstrom.Unbuffered.Fail (consumed, path, err)

  let default raw =
    if Cstruct.len raw < 25 * 2
    then raise (Invalid_argument "The internal buffer is not enough to parse");
    (* XXX(dinosaure): So, if you read the comment before, you
       understand than the internal buffer need to be bigger than an
       alteration. I decide (because I'm the God) to accept only a
       buffer bigger than 25 bytes. Otherwise, we raise an
       [Invalid_argument]. *)
    let len = Cstruct.len raw in
    Log.debug (fun l ->
        l "Starting to decode a Git object with a internal buffer (%d)." len);
    let open Angstrom.Unbuffered in
    { state = (match parse A.decoder with
          | Done (committed, value)         -> kdone (committed, value)
          | Fail (committed, path, err)     -> kfail (committed, path, err)
          | Partial { committed; continue } ->
            Log.debug (fun l ->
                l "Retrieving Partial's angstrom result which  parsed %d \
                   byte(s) (= 0)." committed);
            assert (committed = 0);
            continue)
    ; final    = Incomplete
    ; internal = Cstruct.sub raw 0 0
    (* XXX(dinosaure): first, we consider than the internal buffer is
       empty. But keep calm, the physical buffer will not be
       free-ed. *)
    ; max      = len }

  let eval decoder =
    Log.debug (fun l ->
        l "Start to decode a partial chunk of the flow (%d) (final:%b)."
          (Cstruct.len decoder.internal)
          (decoder.final = Complete));
    match decoder.state (Cstruct.to_bigarray decoder.internal) decoder.final with
    | Done (consumed, value) ->
      Log.debug (fun l -> l "End of the decoding.");
      `End (Cstruct.shift decoder.internal consumed, value)
    | Fail (consumed, path, err) ->
      let err_path = String.concat " > " path in
      Log.err (fun l -> l "Error while decoding: %s (%s)." err err_path);
      `Error (Cstruct.shift decoder.internal consumed,
              `Decoder (err_path ^ ": " ^ err))
    | Partial { committed; continue } ->
      Log.debug (fun l ->
          l "Current decoding waits more input (committed: %d)." committed);
      let decoder =
        { decoder with internal = Cstruct.shift decoder.internal committed
                     ; state    = continue }
      in
      `Await decoder

  let compress _ decoder =
    let off, len = 0, Cstruct.len decoder.internal in
    let buffer = Cstruct.of_bigarray ~off ~len decoder.internal.Cstruct.buffer in
    Log.debug (fun l ->
        l "Compressing the internal buffer of the current decoding.");
    Cstruct.blit decoder.internal 0 buffer 0 len;
    { decoder with internal = buffer }

  let refill input decoder =
    let len = Cstruct.len input in
    Log.debug (fun l ->
        l "Starting to refill the internal buffer of the current decoding \
           (len: %d)." (Cstruct.len input));
    if len > decoder.max
    then (
      (* XXX(dinosaure): it's to avoid to grow the internal buffer. *)
      Log.err (fun l -> l ~header:"refill"
                  "The client want to refill the internal buffer by a bigger input.");

      Error (`Decoder (Fmt.strf "Input is too huge: we authorized only an \
                                 input lower or equal than %d" decoder.max))
    ) else
      let trailing_space =
        let { Cstruct.buffer; off; len } = decoder.internal in
        Bigarray.Array1.dim buffer - (off + len)
      in
      let writable_space =
        let { Cstruct.buffer; len; _ } = decoder.internal in
        Bigarray.Array1.dim buffer - len
      in
      (if trailing_space >= len
       then Ok decoder
       else if writable_space >= len
       then Ok (compress input decoder)
       else (
         Log.err (fun l -> l ~header:"refill"
                     "trailing space:%d and writable space:%d, \
                      the alteration is not done, the error could \
                      be the size of the internal buffer (%d) or the input \
                      is malicious."
                     trailing_space
                     writable_space
                     (Bigarray.Array1.dim decoder.internal.Cstruct.buffer));
         Log.err (fun l -> l ~header:"refill"
                     "Production of the error in Helper.MakeDecoder.refill.");
         Error (`Decoder "Input does not respect assertion, it may be malicious");
       ))
      |> function
      | Error err  -> Error err
      | Ok decoder ->
        let internal = Cstruct.add_len decoder.internal len in
        (* XXX(dinosaure): see the function store.ml:buffer
           (). C'est à cette endroit où si on considère
           [decoder.internal] comme un [Cstruct.t]
           appartennant à un [Bigarray] plus grand, il peut
           arriver que celui ci empiète sur le buffer
           [io]. *)

        let off = Cstruct.len internal - len in
        let ()  = Cstruct.blit input 0 internal off len in
        Log.debug (fun l ->
            l "Refill the internal buffer in the current decoding.");
        Ok { decoder with internal = internal }

  let to_result input =
    Angstrom.parse_bigstring A.decoder (Cstruct.to_bigarray input)
    |> function
    | Ok _ as v -> v
    | Error err -> Error (`Decoder err)

  let finish decoder =
    { decoder with final = Angstrom.Unbuffered.Complete }
end

module MakeInflater (Z: S.INFLATE) (A: S.ANGSTROM) = struct

  let src = Logs.Src.create "git.inflater.decoder"
      ~doc:"logs git's internal inflater/decoder"
  module Log = (val Logs.src_log src : Logs.LOG)

  module D = MakeDecoder(A)

  type t = A.t
  type init = Z.window * Cstruct.t * Cstruct.t

  type error =
    [ `Decoder of string
    | `Inflate of Z.error ]

  type decoder =
    { cur : Cstruct.t
    ; tmp : Cstruct.t
    ; inf : Z.t
    ; dec : D.decoder }

  let pp_error ppf = function
    | `Decoder err -> ppe ~name:"`Decoder" Fmt.string ppf err
    | `Inflate err -> ppe ~name:"`Inflate" (Fmt.hvbox Z.pp_error) ppf err

  let empty = Cstruct.create 0

  let default (window, raw0, raw1) =
    Log.debug (fun l -> l "Starting to inflate and decode a Git object.");
    { cur = empty
    ; tmp = raw0
    ; inf = Z.flush 0 (Cstruct.len raw0) @@ Z.default (Z.window_reset window)
    ; dec = D.default raw1 }

  let rec eval decoder =
    Log.debug (fun l -> l "Starting to inflate a partial chunk of the flow.");
    match D.eval decoder.dec with
    | `Error (unconsumed, `Decoder err) -> `Error (unconsumed, `Decoder err)
    | `End (unconsumed, value)          -> `End (unconsumed, value)
    | `Await dec                        ->
      Log.debug (fun l -> l "Decoder waits.");
      match Z.eval ~src:decoder.cur ~dst:decoder.tmp decoder.inf with
      | `Await inf ->
        Log.debug (fun l -> l "Inflator waits.");
        `Await { decoder with cur = Cstruct.shift decoder.cur (Z.used_in inf)
                            ; inf = inf
                            ; dec = dec }
      | `Flush inf ->
        Log.debug (fun l -> l "Inflator flushes: %a." (Fmt.hvbox Z.pp) inf);
        (match D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec with
         | Error (`Decoder err) -> `Error (decoder.cur, `Decoder err)
         | Ok dec               ->
           let inf = Z.flush 0 (Cstruct.len decoder.tmp) inf in
           Log.debug (fun l ->
               l "Internal buffer of the current decoding refilled: %a."
                 (Fmt.hvbox Z.pp) inf);
           eval { decoder with dec = dec ; inf = inf })
      | `Error (inf, err) ->
        Log.err (fun l -> l "Inflate error: %a." (Fmt.hvbox Z.pp_error) err);
        `Error (Cstruct.shift decoder.cur (Z.used_in inf), `Inflate err)
      | `End inf ->
        Log.debug (fun l ->
            l "Inflator finished with the current decoding flow.");
        match D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec with
        | Error (`Decoder err) -> `Error (decoder.cur, `Decoder err)
        | Ok dec               ->
          eval { decoder with cur = Cstruct.shift decoder.cur (Z.used_in inf)
                            ; inf = Z.flush 0 0 (Z.refill 0 0 inf)
                            ; dec = D.finish dec }

  let refill input decoder =
    Ok { decoder with cur = input
                    ; inf = Z.refill 0 (Cstruct.len input) decoder.inf }

  let finish decoder = { decoder with dec = D.finish decoder.dec }

  let to_result deflated =
    let window = Z.window () in
    let i = Cstruct.create 0x800 in
    let o = Cstruct.create 0x800 in
    let t = default (window, i, o) in
    let rec go t = match eval t with
      | `Error (_, err) -> Error err
      | `End (_, value) -> Ok value
      | `Await t        ->
        refill (Cstruct.sub deflated 0 0x800) t |> function
        | Ok t           -> go t
        | Error _ as err -> err
    in
    go t
end

module MakeEncoder (M: S.MINIENC) = struct

  let src = Logs.Src.create "git.encoder" ~doc:"logs git's internal encoder"
  module Log = (val Logs.src_log src : Logs.LOG)

  type t = M.t
  type init = int * t
  type error = [ `Never ]

  let pp_error ppf `Never = Fmt.string ppf "`Never"

  type encoder =
    { o_off : int
    ; o_pos : int
    ; o_len : int
    ; w_acc : int
    ; state : Minienc.encoder Minienc.state }

  let default (capacity, x) =
    Log.debug (fun l ->
        l "Starting to encode a Git object with a capacity = %d." capacity);

    let encoder = Minienc.create capacity in
    let state   = M.encoder x (fun encoder -> Minienc.End encoder) encoder in
    { o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; w_acc = 0
    ; state }

  let cstruct_blit_iovec iovec cs cs_off limit =
    let max = fun len -> match limit with
      | Some limit -> min limit len
      | None       -> len
    in
    match iovec with
    | { Minienc.IOVec.buffer = `Bigstring x; off; len; } ->
      Cstruct.blit (Cstruct.of_bigarray ~off ~len x) 0 cs cs_off (max len); max len
    | { Minienc.IOVec.buffer = `Bytes x; off; len; } ->
      Cstruct.blit_from_bytes x off cs cs_off (max len); max len
    | { Minienc.IOVec.buffer = `String x; off; len; } ->
      Cstruct.blit_from_string x off cs cs_off (max len); max len

  exception Drain of int

  let rec eval current e =
    match e.state with
    | Minienc.End minienc ->
      let shift  = min (e.o_len - e.o_pos) (Minienc.has minienc) in
      let iovecs, shifted = Minienc.shift shift minienc in
      let shift' = List.fold_left (fun acc iovec ->
          let write = cstruct_blit_iovec iovec current acc None in
          acc + write)
          (e.o_off + e.o_pos) iovecs
      in
      Log.debug (fun l ->
          l "Ensure than we wrote exactly [shift = %d] byte(s)." shift);
      assert (e.o_off + e.o_pos + shift = shift');
      if Minienc.has shifted > 0
      then `Flush { e with o_pos = e.o_pos + shift
                         ; w_acc = e.w_acc + shift
                         ; state = Minienc.End shifted }
      else `End ({ e with o_pos = e.o_pos + shift
                        ; w_acc = e.w_acc + shift
                        ; state = Minienc.End shifted }, e.w_acc + shift)
    | Minienc.Continue { encoder; continue; } ->
      (* XXX(dinosaure): we can shift the minienc at this time, but
         it's very useful? *)
      eval current { e with state = continue encoder }
    | Minienc.Flush { continue; iovecs; } ->
      let max = min (e.o_len - e.o_pos) (Minienc.IOVec.lengthv iovecs) in
      try
        (* XXX(dinosaure): wtf?! pourquoi j'ai fait ce code (peut être
           pour ne pas utiliser [fold_left]. It's an optimization to
           jump directly when we fill entirely the buffer - but the
           code is not safe. *)
        let pos = ref 0 in
        List.iter (fun iovec ->
            let write =
              cstruct_blit_iovec
                iovec current (e.o_off + e.o_pos + !pos) (Some (max - !pos))
            in
            if !pos + write = max
            then raise (Drain (!pos + write))
            else pos := !pos + write)
          iovecs;
        raise (Drain !pos)
      with Drain drain ->
        `Flush { e with o_pos = e.o_pos + drain
                      ; w_acc = e.w_acc + drain
                      ; state = continue drain }

  let flush off len e =
    { e with o_off = off
           ; o_pos = 0
           ; o_len = len }

  let used { o_pos; _ } = o_pos
end

module MakeDeflater (Z: S.DEFLATE) (M: S.MINIENC) = struct

  let src = Logs.Src.create "git.deflater.encoder"
      ~doc:"logs git's internal deflater/encoder"
  module Log = (val Logs.src_log src : Logs.LOG)

  type t = M.t
  type init = int * t * int * Cstruct.t
  type error = [ `Deflate of Z.error ]

  let pp_error ppf (`Deflate err) =
    ppe ~name:"`Deflate" (Fmt.hvbox Z.pp_error) ppf err

  module E = MakeEncoder(M)

  type encoder =
    { e        : E.encoder
    ; z        : Z.t
    ; internal : Cstruct.t
    ; used_in  : int }

  let default (capacity, value, level, internal) =
    Log.debug (fun l ->
        l "Starting to deflate and encode a Git object \
           (level of compression: %d, internal buffer: %d)."
          level (Cstruct.len internal));
    { e = E.default (capacity, value)
    ; z = Z.default level
    ; internal
    ; used_in = 0 }

  let rec eval dst encoder =
    match Z.eval ~src:encoder.internal ~dst encoder.z with
    (* XXX(dinosaure): [`Never] is a trick just to constraint the type
       error to be a polymorphic variant. But this case never
       happens. *)
    | `Flush z        -> `Flush { encoder with z; }
    | `End z          -> `End ({ encoder with z; }, 0)
    | `Error (_, exn) ->
      Log.err (fun l -> l "Deflate error: %a." (Fmt.hvbox Z.pp_error) exn);
      `Error (`Deflate exn)
    | `Await z        ->
      match E.eval encoder.internal encoder.e with
      | `Flush e ->
        let used_in' = encoder.used_in + Z.used_in z in
        let z, e, used_in =
          if used_in' = E.used e
          then Z.no_flush 0 0 z, E.flush 0 (Cstruct.len encoder.internal) e, 0
          else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
        in
        eval dst { encoder with e; z; used_in; }
      | `End (e, _) ->
        let used_in' = encoder.used_in + Z.used_in z in
        let z, e, used_in =
          if used_in' = E.used e
          then Z.finish z, e, used_in'
          else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
        in
        eval dst { encoder with e; z; used_in; }
      | `Error `Never -> assert false

  let flush off len encoder =
    { encoder with z = Z.flush off len encoder.z }

  let used { z; _ } =
    Z.used_out z
end

(* XXX(dinosaure): The purpose between [fdigest] and [digest] is why
   we create the module {!Minienc}. In fact, if we take care about the
   memory consumption of this library, we need to control how much we
   exactly need to compute the hash of an object.

   In fact, with Faraday, the common way to serialize a data is to
   have 2 threads, one to fill and one other to flush the shared state
   of Faraday. This case oes not appear when we want to compute the
   hash of the Git object - it could be appear but we don't want to
   constraint the client on this pattern.

   So, we decide to create a new encoder which limit the memory
   consumption. When we fill entirely the internal buffer, we ask to
   the client to flush it and we don't continue to fill it until the
   client does not free some spaces.

   Then, we control the memory consumption when we compute the hash of
   the Git object and we never grow the internal buffer. *)
let fdigest: type t hash.
     (module S.IDIGEST with type t = hash)
  -> (module S.ENCODER
       with type t = t
        and type init = (int * t)
        and type error = [ `Never ])
  -> ?capacity:int
  -> tmp:Cstruct.t
  -> kind:string
  -> length:(t -> int64)
  -> t
  -> hash
  = fun digest encoder ?(capacity = 0x100) ~tmp ~kind ~length value ->
    let module Digest = (val digest) in
    let module M      = (val encoder) in
    let hdr = Fmt.strf "%s %Ld\000" kind (length value) in
    let ctx = Digest.init () in
    Digest.feed ctx (Cstruct.of_string hdr);
    let encoder = M.default (capacity, value) in
    let rec loop encoder = match M.eval tmp encoder with
      | `Flush encoder ->
        if M.used encoder > 0 then
          Digest.feed ctx (Cstruct.sub tmp 0 (M.used encoder));
        loop (M.flush 0 (Cstruct.len tmp) encoder)
      | `End (encoder, _) ->
        if M.used encoder > 0 then
          Digest.feed ctx (Cstruct.sub tmp 0 (M.used encoder));
        Digest.get ctx
      | `Error `Never -> assert false
    in
    loop encoder

let digest
  : type t hash. (module S.IDIGEST with type t = hash)
    -> (module S.FARADAY with type t = t)
    -> kind:string
  -> t
  -> hash
  = fun digest faraday ~kind value ->
    let module Digest = (val digest) in
    let module F      = (val faraday) in
    let hdr = Fmt.strf "%s %Ld\000" kind (F.length value) in
    let ctx = Digest.init () in
    let raw = Cstruct.create (Int64.to_int (F.length value)) in (* XXX(dinosaure): to digest. *)
    let enc = Faraday.of_bigstring (Cstruct.to_bigarray raw) in
    Digest.feed ctx (Cstruct.of_string hdr);
    F.encoder enc value;
    Faraday.close enc;
    Faraday.serialize enc (fun iovecs ->
        let len = List.fold_left (fun acc -> function
            | { Faraday.buffer = buf; off; len; } ->
              Digest.feed ctx (Cstruct.of_bigarray ~off ~len buf); acc + len)
            0 iovecs
        in `Ok len)
    |> function
    | `Close -> Digest.get ctx
    | `Yield -> assert false

module type ENCODER = sig
  type state
  type raw
  type result
  type error

  val raw_length : raw -> int
  val raw_blit   : raw -> int -> raw -> int -> int -> unit

  val eval  : raw -> state -> [ `Flush of state | `End of (state * result) | `Error of (state * error) ] Lwt.t
  val used  : state -> int
  val flush : int -> int -> state -> state
end

type ('state, 'raw, 'result, 'error) encoder =
  (module ENCODER with type state  = 'state
                   and type raw    = 'raw
                   and type result = 'result
                   and type error  = 'error)
and ('fd, 'raw, 'error) writer = 'raw -> ?off:int -> ?len:int -> 'fd -> (int, 'error) result Lwt.t

(* XXX(dinosaure): this function takes care about how many byte(s) we
   write to the file-descriptor and retry to write while the limit is
   not retrieved. In some case (but not common), the writer does not
   write all bytes available by the encoder. In this case, we compress
   the internal buffer [raw] and retry to write (and, at the same
   time, feed the rest of the internal buffer).

   If the internal buffer is full, we start to count how many call we
   need to write something and if we arrive to the limit before
   writing would be only one byte, we stop (but not close the
   file-descriptor) the process and return [`Stack].

   If we arrive at the end and the buffer is not empty, we count how
   many call we need to flush the buffer, and, as below, if we arrive
   to the limit, we stop the process and return [`Stack].

   That means, if the client receives [`Stack], he can consider than
   the file is incomplete and he needs to deal with this partial
   writing in other way than the [writer] - abort and use an other way
   to write what he wants.

   * If the [writer] returns an error, we stop the process and return
   [`Writer error].
   * If the [encoder] returns an error, we stop the process and return
   [`Encoder error].

   In all case, we don't close the file-descriptor.

   Otherwise, we return [Ok result] with the result of the encoder. *)

let src = Logs.Src.create "git.encoder.io" ~doc:"logs git's internal I/O encoder"
module ELog = (val Logs.src_log src : Logs.LOG)

open Lwt.Infix

let safe_encoder_to_file
    (type state) (type raw) (type res) (type err_encoder) (type err_writer)
    ~limit
    (encoder : (state, raw, res, err_encoder) encoder)
    (writer : ('fd, raw, err_writer) writer)
    (fd : 'fd)
    (raw : raw)
    (state : state) : (res, [ `Stack | `Encoder of err_encoder | `Writer of err_writer ]) result Lwt.t
  =
  let module E =
    (val encoder : ENCODER with type state = state
                            and type raw = raw
                            and type result = res
                            and type error = err_encoder)
  in
  let rec go ~stack ?(rest = 0) state =
    if stack < limit
    then E.eval raw state >>= function
      | `Error (_, err) ->
        ELog.err (fun l ->
            l ~header:"go" "Retrieving an encoder error when we serialize \
                            the value to a file-descriptor.");
        Lwt.return (Error (`Encoder err))
      | `End (state, res) ->
        if E.used state + rest > 0
        then begin
          ELog.debug (fun l ->
              l ~header:"go" "Final step to write entirely the file \
                              (rest: %d)." (E.used state + rest));
          writer raw ~off:0 ~len:(rest + E.used state) fd >>= function
          | Ok n ->
            if n = E.used state + rest
            then Lwt.return (Ok res)
            else begin
              ELog.warn (fun l ->
                  l ~header:"go" "Loop back to writing the rest (%d) \
                                  of the encoding (stack: %d)."
                    (E.used state + rest - n) stack);
              go ~stack:(stack + 1) (E.flush n ((E.used state + rest) - n) state)
            end
          | Error err -> Lwt.return (Error (`Writer err))
        end else Lwt.return (Ok res)
      | `Flush state ->
        writer raw ~off:0 ~len:(rest + E.used state) fd >>= function
        | Ok n ->
          if n = rest + E.used state
          then go ~stack:0 (E.flush 0 (E.raw_length raw) state)
          else begin
            let rest = (rest + E.used state) - n in
            E.raw_blit raw n raw 0 rest;
            ELog.debug (fun l ->
                l ~header:"go" "The I/O encoder writes %d (rest: %d, loop back: %b)."
                  n (E.raw_length raw - rest) (E.raw_length raw - rest = 0));
            go ~stack:(if E.raw_length raw - rest = 0 then stack + 1 else 0)
              ~rest
              (E.flush rest (E.raw_length raw - rest) state)
          end
        | Error err -> Lwt.return (Error (`Writer err))
    else (
      ELog.err (fun l ->
          l "Got a [`Stack] error: impossible to serialize and write the Git \
             object.");
      Lwt.return (Error `Stack)
    )
  in
  go ~stack:0 state
