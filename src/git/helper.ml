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

module Pair = struct let flip (a, b) = b, a
                     let fst (a, _) = a
                     let snd (_, b) = b
end

module Option = struct
  let map f v = match v with Some v -> Some (f v) | None -> None
  let mem v x ~equal = match v with Some x' -> equal x x' | None -> false
  let value ~default = function Some a -> a | None -> default
  let ( >>= ) v f = map f v
end

module BaseIso = struct
  open Encore.Bijection

  let flip (a, b) = b, a

  let int64 =
    let tag = "string", "int64" in
    make_exn ~tag
      ~fwd:(Exn.safe_exn tag Int64.of_string)
      ~bwd:(Exn.safe_exn (flip tag) Int64.to_string)

  let cstruct =
    Encore.Bijection.make_exn ~tag:("cstruct", "bigarray")
      ~fwd:Cstruct.of_bigarray ~bwd:Cstruct.to_bigarray

  let char_elt chr =
    Exn.element
      ~tag:(Fmt.strf "char:%02x" (Char.code chr))
      ~compare:Char.equal chr

  let string_elt str = Exn.element ~tag:str ~compare:String.equal str
end

module MakeDecoder (A : S.DESC with type 'a t = 'a Angstrom.t) = struct
  (* XXX(dinosaure): This decoder is on top of some assertion about the
     decoding of a Git object. [Angstrom] can not consume all input (when we
     have an alteration specifically) and the client need to keep a part of the
     previous input but continue to feed the parser with a new input (to
     determine the choice of the alteration). In the Git format, we can have
     this problem but for few bytes (something like ten bytes). So, we consider
     to use an internal buffer (than the size is bigger than what is needed)
     and we ensure than is not possible to keep more than the size of the
     internal buffer when we parse a Git object.

     However, this case appear in the code (when we don't have enough trailing
     space and writable space). This case appear when we don't parse a Git
     object but something else (which is not respect our assertions). And if
     this case appear, we can consider that someone try to cheat you. So we
     prefer to return an error.

     Obviously, in a better world with unicorn and pineapple pizza, we can just
     avoid this case. *)

  let src = Logs.Src.create "git.decoder" ~doc:"logs git's internal decoder"

  module Log = (val Logs.src_log src : Logs.LOG)
  module Q = Ke.Rke.Weighted

  type error = Error.Decoder.t
  type init = Cstruct.t
  type t = A.e

  let pp_error ppf = function
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err

  type decoder =
    { state:
           Angstrom.bigstring
        -> off:int
        -> len:int
        -> Angstrom.Unbuffered.more
        -> A.e Angstrom.Unbuffered.state
    ; final: Angstrom.Unbuffered.more
    ; queue: (char, Bigarray.int8_unsigned_elt) Q.t
    ; max: int }

  let kdone (consumed, value) _ba ~off:_ ~len:_ _more =
    Angstrom.Unbuffered.Done (consumed, value)

  let kfail (consumed, path, err) _ba ~off:_ ~len:_ _more =
    Angstrom.Unbuffered.Fail (consumed, path, err)

  let default raw =
    if Cstruct.len raw < 25 * 2 then
      raise (Invalid_argument "The internal buffer is not enough to parse") ;
    (* XXX(dinosaure): So, if you read the comment before, you understand than
       the internal buffer need to be bigger than an alteration. I decide
       (because I'm the God) to accept only a buffer bigger than 25 bytes.
       Otherwise, we raise an [Invalid_argument]. *)
    let len = Cstruct.len raw in
    Log.debug (fun l ->
        l "Starting to decode a Git object with a internal buffer (%d)." len ) ;
    let open Angstrom.Unbuffered in
    { state=
        ( match parse A.p with
        | Done (committed, value) -> kdone (committed, value)
        | Fail (committed, path, err) -> kfail (committed, path, err)
        | Partial {committed; continue} ->
            Log.debug (fun l ->
                l
                  "Retrieving Partial's angstrom result which  parsed %d \
                   byte(s) (= 0)."
                  committed ) ;
            assert (committed = 0) ;
            continue )
    ; final= Incomplete
    ; queue= Q.from (Cstruct.to_bigarray raw)
    ; max= len }

  let empty = Cstruct.create 0

  let eval decoder =
    let open Angstrom.Unbuffered in
    let raw = match Q.N.peek decoder.queue with
      | [ x ] -> Cstruct.of_bigarray x | [] -> empty
      | _ :: _ -> assert false in
    Log.debug (fun l ->
        l "Start to decode a partial chunk of the flow (%d) (final:%b)."
          (Cstruct.len raw)
          (decoder.final = Complete) ) ;
    let off, len = 0, Cstruct.len raw in
    match decoder.state (Cstruct.to_bigarray raw) ~off ~len decoder.final with
    | Done (consumed, value) ->
        Log.debug (fun l -> l "End of the decoding.") ;
        Q.N.shift_exn decoder.queue consumed ;
        `End (empty (* TODO *), value)
    | Fail (consumed, path, err) ->
        let err_path = String.concat " > " path in
        Log.err (fun l -> l "Error while decoding: %s (%s)." err err_path) ;
        Q.N.shift_exn decoder.queue consumed ;
        `Error
          ( empty (* TODO *)
          , Error.Decoder.err_decode (consumed, path, err) )
    | Partial {committed; continue} ->
        Q.N.shift_exn decoder.queue committed ;
        Log.debug (fun l ->
            l "Current decoding waits more input (committed: %d)." committed ) ;
        `Await { decoder with state= continue }

  let refill input decoder =
    let len = Cstruct.len input in
    Log.debug (fun l ->
        l
          "Starting to refill the internal buffer of the current decoding \
           (len: %d)."
          (Cstruct.len input) ) ;
    Q.compress decoder.queue ;
    let raw = Cstruct.to_bigarray input in
    let blit src src_off dst dst_off len = Bigstringaf.blit src ~src_off dst ~dst_off ~len in
    match Q.N.push decoder.queue ~blit:blit ~length:Bigstringaf.length ~off:0 ~len raw with
    | Some [ _ ] ->
      Log.debug (fun l ->
          l "Refill the internal buffer in the current decoding." ) ;
      Ok decoder
    | Some _ -> assert false (* XXX(dinosaure): see [ke] assumptions. *)
    | None ->
      Log.err (fun l -> l "The client wants to refill the internal buffer by a bigger input." ) ;
      Error.(v @@ Decoder.err_too_big len decoder.max)

  let to_result input =
    Angstrom.parse_bigstring A.p (Cstruct.to_bigarray input)
    |> function
    | Ok _ as v -> v | Error err -> Error.(v @@ Decoder.err_result input err)

  let finish decoder = {decoder with final= Angstrom.Unbuffered.Complete}
end

module MakeInflater (Z : S.INFLATE) (A : S.DESC with type 'a t = 'a Angstrom.t) =
struct
  let src =
    Logs.Src.create "git.inflater.decoder"
      ~doc:"logs git's internal inflater/decoder"

  module Log = (val Logs.src_log src : Logs.LOG)
  module D = MakeDecoder (A)

  type t = A.e
  type init = Z.window * Cstruct.t * Cstruct.t
  type error = [Error.Decoder.t | `Inflate of Z.error]
  type decoder = {cur: Cstruct.t; tmp: Cstruct.t; inf: Z.t; dec: D.decoder}

  let pp_error : error Fmt.t =
   fun ppf err ->
    match err with
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | `Inflate _ as err -> Error.Inf.pp_error Z.pp_error ppf err

  let empty = Cstruct.create 0

  let default (window, raw0, raw1) =
    Log.debug (fun l -> l "Starting to inflate and decode a Git object.") ;
    { cur= empty
    ; tmp= raw0
    ; inf= Z.flush 0 (Cstruct.len raw0) @@ Z.default (Z.window_reset window)
    ; dec= D.default raw1 }

  let rec eval decoder =
    Log.debug (fun l -> l "Starting to inflate a partial chunk of the flow.") ;
    match D.eval decoder.dec with
    | `Error (unconsumed, `Decoder err) -> `Error (unconsumed, `Decoder err)
    | `End (unconsumed, value) -> `End (unconsumed, value)
    | `Await dec -> (
        Log.debug (fun l -> l "Decoder waits.") ;
        match Z.eval ~src:decoder.cur ~dst:decoder.tmp decoder.inf with
        | `Await inf ->
            Log.debug (fun l -> l "Inflator waits.") ;
            `Await
              { decoder with
                cur= Cstruct.shift decoder.cur (Z.used_in inf); inf; dec }
        | `Flush inf -> (
            Log.debug (fun l -> l "Inflator flushes: %a." (Fmt.hvbox Z.pp) inf) ;
            match
              D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec
            with
            | Error err -> `Error (decoder.cur, err)
            | Ok dec ->
                let inf = Z.flush 0 (Cstruct.len decoder.tmp) inf in
                Log.debug (fun l ->
                    l "Internal buffer of the current decoding refilled: %a."
                      (Fmt.hvbox Z.pp) inf ) ;
                eval {decoder with dec; inf} )
        | `Error (inf, err) ->
            Log.err (fun l -> l "Inflate error: %a." (Fmt.hvbox Z.pp_error) err) ;
            `Error (Cstruct.shift decoder.cur (Z.used_in inf), `Inflate err)
        | `End inf -> (
            Log.debug (fun l ->
                l "Inflator finished with the current decoding flow." ) ;
            match
              D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec
            with
            | Error err -> `Error (decoder.cur, err)
            | Ok dec ->
                eval
                  { decoder with
                    cur= Cstruct.shift decoder.cur (Z.used_in inf)
                  ; inf= Z.flush 0 0 (Z.refill 0 0 inf)
                  ; dec= D.finish dec } ) )

  let refill input decoder =
    Ok
      {decoder with cur= input; inf= Z.refill 0 (Cstruct.len input) decoder.inf}

  let finish decoder = {decoder with dec= D.finish decoder.dec}

  let to_result deflated =
    let window = Z.window () in
    let i = Cstruct.create 0x800 in
    let o = Cstruct.create 0x800 in
    let t = default (window, i, o) in
    let rec go t =
      match eval t with
      | `Error (_, err) -> Error err
      | `End (_, value) -> Ok value
      | `Await t -> (
          refill (Cstruct.sub deflated 0 0x800) t
          |> function Ok t -> go t | Error _ as err -> err )
    in
    go t
end

module MakeEncoder (M : S.DESC with type 'a t = 'a Encore.Encoder.t) = struct
  let src = Logs.Src.create "git.encoder" ~doc:"logs git's internal encoder"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = M.e
  type init = Cstruct.t * t
  type error = Error.never

  let pp_error ppf `Never = Fmt.string ppf "`Never"

  open Encore

  type encoder =
    { o_off: int
    ; o_pos: int
    ; o_len: int
    ; w_acc: int
    ; state: Lole.encoder Lole.state }

  let default (etmp, x) =
    Log.debug (fun l ->
        l "Starting to encode a Git object with a buffer.") ;
    let encoder = Lole.from (Cstruct.len etmp) (Cstruct.to_bigarray etmp) in
    let state = Encoder.run M.p (fun encoder -> Lole.End encoder) encoder x in
    {o_off= 0; o_pos= 0; o_len= 0; w_acc= 0; state}

  let cstruct_blit_iovec iovec cs cs_off limit =
    let max len =
      match limit with Some limit -> min limit len | None -> len
    in
    match iovec with
    | {Lole.IOVec.buffer= Lole.Buffer.Bigstring x; off; len} ->
        Cstruct.blit (Cstruct.of_bigarray ~off ~len x) 0 cs cs_off (max len) ;
        max len
    | {Lole.IOVec.buffer= Lole.Buffer.Bytes x; off; len} ->
        Cstruct.blit_from_bytes x off cs cs_off (max len) ;
        max len
    | {Lole.IOVec.buffer= Lole.Buffer.String x; off; len} ->
        Cstruct.blit_from_string x off cs cs_off (max len) ;
        max len

  exception Drain of int

  let rec eval current e =
    match e.state with
    | Lole.End minienc ->
        let shift = min (e.o_len - e.o_pos) (Lole.has minienc) in
        let iovecs, shifted = Lole.shift shift minienc in
        let shift' =
          List.fold_left
            (fun acc iovec ->
              let write = cstruct_blit_iovec iovec current acc None in
              acc + write )
            (e.o_off + e.o_pos) iovecs
        in
        Log.debug (fun l ->
            l "Ensure than we wrote exactly [shift = %d] byte(s)." shift ) ;
        assert (e.o_off + e.o_pos + shift = shift') ;
        if Lole.has shifted > 0 then
          `Flush
            { e with
              o_pos= e.o_pos + shift
            ; w_acc= e.w_acc + shift
            ; state= Lole.End shifted }
        else
          `End
            ( { e with
                o_pos= e.o_pos + shift
              ; w_acc= e.w_acc + shift
              ; state= Lole.End shifted }
            , e.w_acc + shift )
    | Lole.Continue {encoder; continue} ->
        (* XXX(dinosaure): we can shift the minienc at this time, but it's very
           useful? *)
        eval current {e with state= continue encoder}
    | Lole.Flush {continue; iovecs} -> (
        let max = min (e.o_len - e.o_pos) (Lole.IOVec.lengthv iovecs) in
        try
          (* XXX(dinosaure): wtf?! pourquoi j'ai fait ce code (peut être pour
             ne pas utiliser [fold_left]. It's an optimization to jump directly
             when we fill entirely the buffer - but the code is not safe. *)
          let pos = ref 0 in
          List.iter
            (fun iovec ->
              let write =
                cstruct_blit_iovec iovec current
                  (e.o_off + e.o_pos + !pos)
                  (Some (max - !pos))
              in
              if !pos + write = max then raise (Drain (!pos + write))
              else pos := !pos + write )
            iovecs ;
          raise (Drain !pos)
        with Drain drain ->
          `Flush
            { e with
              o_pos= e.o_pos + drain
            ; w_acc= e.w_acc + drain
            ; state= continue drain } )

  let flush off len e = {e with o_off= off; o_pos= 0; o_len= len}
  let used {o_pos; _} = o_pos
end

module MakeDeflater
    (Z : S.DEFLATE)
    (M : S.DESC with type 'a t = 'a Encore.Encoder.t) =
struct
  let src =
    Logs.Src.create "git.deflater.encoder"
      ~doc:"logs git's internal deflater/encoder"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = M.e
  type init = Cstruct.t * t * int * Cstruct.t
  type error = [`Deflate of Z.error]

  let pp_error ppf err = Error.Def.pp_error Z.pp_error ppf err

  module E = MakeEncoder (M)

  type encoder = {e: E.encoder; z: Z.t; internal: Cstruct.t; used_in: int}

  let default (etmp, value, level, internal) =
    Log.debug (fun l ->
        l
          "Starting to deflate and encode a Git object (level of compression: \
           %d, internal buffer: %d)."
          level (Cstruct.len internal) ) ;
    {e= E.default (etmp, value); z= Z.default level; internal; used_in= 0}

  let rec eval dst encoder =
    match Z.eval ~src:encoder.internal ~dst encoder.z with
    (* XXX(dinosaure): [`Never] is a trick just to constraint the type error to
       be a polymorphic variant. But this case never happens. *)
    | `Flush z -> `Flush {encoder with z}
    | `End z -> `End ({encoder with z}, 0)
    | `Error (_, exn) ->
        Log.err (fun l -> l "Deflate error: %a." (Fmt.hvbox Z.pp_error) exn) ;
        `Error (`Deflate exn)
    | `Await z -> (
      match E.eval encoder.internal encoder.e with
      | `Flush e ->
          let used_in' = encoder.used_in + Z.used_in z in
          let z, e, used_in =
            if used_in' = E.used e then
              Z.no_flush 0 0 z, E.flush 0 (Cstruct.len encoder.internal) e, 0
            else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
          in
          eval dst {encoder with e; z; used_in}
      | `End (e, _) ->
          let used_in' = encoder.used_in + Z.used_in z in
          let z, e, used_in =
            if used_in' = E.used e then Z.finish z, e, used_in'
            else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
          in
          eval dst {encoder with e; z; used_in}
      | `Error `Never -> assert false )

  let flush off len encoder = {encoder with z= Z.flush off len encoder.z}
  let used {z; _} = Z.used_out z
end


let digest : type t hash.
       (module S.HASH with type t = hash)
    -> (module
        S.ENCODER
          with type t = t and type init = Cstruct.t * t and type error = Error.never)
    -> etmp:Cstruct.t
    -> tmp:Cstruct.t
    -> kind:string
    -> length:(t -> int64)
    -> t
    -> hash =
 fun digest encoder ~etmp ~tmp ~kind ~length value ->
  let module Digest = (val digest) in
  let module M = (val encoder) in
  let hdr = Fmt.strf "%s %Ld\000" kind (length value) in
  let ctx = Digest.init () in
  let ctx = Digest.feed_string ctx hdr in
  let encoder = M.default (etmp, value) in
  let rec loop ctx encoder =
    match M.eval tmp encoder with
    | `Flush encoder ->
        let ctx =
          if M.used encoder > 0 then
            Digest.feed_bigstring ctx
              Cstruct.(to_bigarray @@ sub tmp 0 (M.used encoder))
          else ctx
        in
        loop ctx (M.flush 0 (Cstruct.len tmp) encoder)
    | `End (encoder, _) ->
        let ctx =
          if M.used encoder > 0 then
            Digest.feed_bigstring ctx
              Cstruct.(to_bigarray @@ sub tmp 0 (M.used encoder))
          else ctx
        in
        Digest.get ctx
    | `Error `Never -> assert false
  in
  loop ctx encoder

module type ENCODER = sig
  type state
  type result
  type error

  val eval :
       Cstruct.t
    -> state
    -> [`Flush of state | `End of state * result | `Error of state * error]
       Lwt.t

  val used : state -> int
  val flush : int -> int -> state -> state
end

module type DECODER = sig
  type decoder
  type error
  type t

  val eval :
       decoder
    -> [`Await of decoder | `End of Cstruct.t * t | `Error of Cstruct.t * error]

  val refill : Cstruct.t -> decoder -> (decoder, error) result
  val finish : decoder -> decoder
end

(* XXX(dinosaure): this function takes care about how many byte(s) we write to
   the file-descriptor and retry to write while the limit is not retrieved. In
   some case (but not common), the writer does not write all bytes available by
   the encoder. In this case, we compress the internal buffer [raw] and retry
   to write (and, at the same time, feed the rest of the internal buffer).

   If the internal buffer is full, we start to count how many call we need to
   write something and if we arrive to the limit before writing would be only
   one byte, we stop (but not close the file-descriptor) the process and return
   [`Stack].

   If we arrive at the end and the buffer is not empty, we count how many call
   we need to flush the buffer, and, as below, if we arrive to the limit, we
   stop the process and return [`Stack].

   That means, if the client receives [`Stack], he can consider than the file
   is incomplete and he needs to deal with this partial writing in other way
   than the [writer] - abort and use an other way to write what he wants.

   * If the [writer] returns an error, we stop the process and return [`FS
   error]. * If the [encoder] returns an error, we stop the process and return
   [`Encoder error].

   In all case, we don't close the file-descriptor.

   Otherwise, we return [Ok result] with the result of the encoder. *)

open Lwt.Infix

module FS (FS : S.FS) = struct
  include FS

  let with_f open_f err path f =
    open_f path
    >>= function
    | Error e -> Lwt.return Error.(v @@ FS.err_open path e)
    | Ok fd ->
        Lwt.finalize
          (fun () -> f fd)
          (fun () ->
            FS.File.close fd
            >>= function
            | Ok () -> Lwt.return ()
            | Error e -> err (Error.FS.err_close path e) )

  let no_err e =
    Log.warn (fun l -> l "%a, ignoring." (Error.FS.pp_error FS.pp_error) e) ;
    Lwt.return ()

  let with_open_r t path f = with_f FS.File.(open_r t) no_err path f
  let prng = lazy (Random.State.make_self_init ())

  let temp_file_name temp_dir file =
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    Fpath.(temp_dir / Fmt.strf "%s.%06x" file rnd)

  let temp_file t temp_dir file =
    let rec aux counter =
      let name = temp_file_name temp_dir file in
      FS.File.exists t name
      >>= function
      | Ok false -> Lwt.return name
      | _ ->
          if counter >= 1000 then
            failwith "cannot create a unique temporary file"
          else aux (counter + 1)
    in
    aux 0

  let with_open_w ?(atomic = true) t ~temp_dir path f =
    if not atomic then with_f FS.File.(open_w t) no_err path f
    else
      temp_file t temp_dir Fpath.(basename path)
      >>= fun temp ->
      let err (`Close (_, e)) =
        Log.debug (fun l ->
            l "Got %a while writing in the temporary file %a" FS.pp_error e
              Fpath.pp path ) ;
        FS.File.delete t temp >|= fun _ -> ()
      in
      with_f FS.File.(open_w t) err temp f
      >>= function
      | Error _ as err -> Lwt.return err
      | Ok x -> (
          FS.File.move t temp path
          >|= function
          | Error err -> Error.(v @@ FS.err_move temp path err) | Ok () -> Ok x
          )
end

module Decoder (D : DECODER) (X : S.FS) = struct
  let src =
    Logs.Src.create "git.decoder" ~doc:"logs git's internal I/O decoder"

  module Log = (val Logs.src_log src : Logs.LOG)
  module FS = FS (X)

  type error = [FS.error Error.FS.t | `Decoder of D.error]

  let of_file fs file raw state =
    FS.with_open_r fs file
    @@ fun fd ->
    let rec go state =
      match D.eval state with
      | `Error (_, err) -> Lwt.return (Error (`Decoder err))
      | `End (_, value) -> Lwt.return (Ok value)
      | `Await state -> (
          FS.File.read raw fd
          >>= function
          | Error err -> Lwt.return Error.(v @@ FS.err_read file err)
          | Ok 0 -> go (D.finish state)
          | Ok n -> (
            match D.refill (Cstruct.sub raw 0 n) state with
            | Ok state -> go state
            | Error err -> Lwt.return (Error (`Decoder err)) ) )
    in
    go state
end

module Encoder (E : ENCODER) (X : S.FS) = struct
  let src =
    Logs.Src.create "git.encoder" ~doc:"logs git's internal I/O encoder"

  module Log = (val Logs.src_log src : Logs.LOG)
  module FS = FS (X)

  type error = [FS.error Error.FS.t | `Encoder of E.error]

  let to_file ?(limit = 50) ?atomic fs ~temp_dir file raw state =
    FS.with_open_w ?atomic fs ~temp_dir file
    @@ fun fd ->
    let rec go ~stack ?(rest = 0) state =
      if stack >= limit then Lwt.return Error.(v @@ FS.err_stack file)
      else
        E.eval raw state
        >>= function
        | `Error (_, err) -> Lwt.return (Error (`Encoder err))
        | `End (state, res) ->
            if E.used state + rest <= 0 then Lwt.return (Ok res)
            else (
              Log.debug (fun l -> l "Final step: rest=%d" (E.used state + rest)) ;
              FS.File.write raw ~off:0 ~len:(rest + E.used state) fd
              >>= function
              | Error err -> Lwt.return Error.(v @@ FS.err_write file err)
              | Ok n ->
                  if n = E.used state + rest then Lwt.return (Ok res)
                  else (
                    Log.debug (fun l ->
                        l "Writing the rest (%d) of the encoding (stack: %d)."
                          (E.used state + rest - n)
                          stack ) ;
                    let state = E.flush n (E.used state + rest - n) state in
                    go ~stack:(stack + 1) state ) )
        | `Flush state -> (
            FS.File.write raw ~off:0 ~len:(rest + E.used state) fd
            >>= function
            | Error err -> Lwt.return Error.(v @@ FS.err_write file err)
            | Ok n ->
                if n = rest + E.used state then
                  go ~stack:0 (E.flush 0 (Cstruct.len raw) state)
                else
                  let rest = rest + E.used state - n in
                  Cstruct.blit raw n raw 0 rest ;
                  Log.debug (fun l ->
                      l "The I/O encoder writes %d (rest: %d, loop back: %b)" n
                        (Cstruct.len raw - rest)
                        (Cstruct.len raw - rest = 0) ) ;
                  let stack =
                    if Cstruct.len raw - rest = 0 then stack + 1 else 0
                  in
                  let state = E.flush rest (Cstruct.len raw - rest) state in
                  go ~stack ~rest state )
    in
    go ~stack:0 state
end
