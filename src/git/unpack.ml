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

open Lwt.Infix

module Window =
struct
  type t =
    { raw : Cstruct.t
    ; off : int64
    ; len : int } (* [len] must be positive. *)

  let inside offset t =
    offset >= t.off && offset < Int64.add t.off (Int64.of_int t.len)

  let pp ppf window =
    Fmt.pf ppf "{ @[<hov>raw = #raw;@ \
                         off = %Ld;@ \
                         len = %d;@] }"
      window.off window.len
end

module type H = sig

  module Hash: S.HASH

  type error =
    | Reserved_opcode of int
    | Wrong_copy_hunk of int * int * int

  val pp_error : error Fmt.t

  type t =
    { i_off          : int
    ; i_pos          : int
    ; i_len          : int
    ; read           : int
    ; _length        : int
    ; _reference     : reference
    ; _source_length : int
    ; _target_length : int
    ; _hunk          : hunk option
    ; _tmp           : Cstruct.t
    ; state          : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header    of k
    | Stop
    | List      of k
    | Is_insert of (Cstruct.t * int * int)
    | Is_copy   of k
    | End
    | Exception of error
  and res =
    | Wait   of t
    | Error  of t * error
    | Cont   of t
    | Ok     of t * hunks
  and hunk =
    | Insert of Cstruct.t
    | Copy of int * int
  and reference =
    | Offset of int64
    | Hash of Hash.t
  and hunks =
    { reference     : reference
    ; length        : int
    ; source_length : int
    ; target_length : int }

  val partial_hunks: t -> hunks
  val pp_reference: reference Fmt.t
  val pp_hunks: hunks Fmt.t
  val pp: t Fmt.t
  val eval: Cstruct.t -> t -> [ `Hunk of t * hunk | `Await of t | `Error of t * error | `Ok of t * hunks ]
  val default: int -> reference -> t
  val refill: int -> int -> t -> t
  val continue: t -> t
  val current: t -> hunk
  val used_in: t -> int
  val available_in: t -> int
  val read: t -> int
end

(* Implementation of deserialization of a list of hunks (from a PACK file) *)
module Hunk (H : S.HASH) : H with module Hash = H  =
struct
  module Hash = H

  type error =
    | Reserved_opcode of int
    | Wrong_copy_hunk of int * int * int

  let pp_error ppf = function
    | Reserved_opcode byte ->
      Fmt.pf ppf "Got reserved op-code %02d" byte
    | Wrong_copy_hunk (off, len, source) ->
      Fmt.pf ppf "The copy hunk (off: %d, len: %d) is invalid, \
                  it's not inner the source (source: %d)."
        off len source

  type t =
    { i_off          : int
    ; i_pos          : int
    ; i_len          : int
    ; read           : int
    (* XXX(dinosaure): consider than it's not possible to have a hunk serialized
       in bigger than [max_native_int] bytes. In practice, it's not common to
       have a Hunks object serialized in a chunk upper than [max_int32]. But
       this case can happen and, because the architecture of this
       deserialization is non-blocking, we can fix this detail but I'm lazy like
       haskell. TODO! *)
    ; _length        : int
    ; _reference     : reference
    ; _source_length : int
    ; _target_length : int
    ; _hunk          : hunk option
    (* XXX(dinosaure): about memory optimization, this deserializer returns each
       hunk and the user can accumulate or process in the same time the git
       object. so, we allocate a [Cstruct.t] for each [Insert] and keep one
       [Cstruct.t] and used for each [Insert] returned. but internally, we
       accumulate (and copy/allocate) each hunk. in others words, we can avoid
       this and use only the non-blocking memory efficient way but I'm lazy to
       re-implement some top functions like [Decoder.get] to construct in the
       same time the git object requested. TODO! *)
    ; _tmp           : Cstruct.t
    ; state          : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header    of k
    | Stop
    | List      of k
    | Is_insert of (Cstruct.t * int * int)
    | Is_copy   of k
    | End
    | Exception of error
  and res =
    | Wait   of t
    | Error  of t * error
    | Cont   of t
    | Ok     of t * hunks
  and hunk =
    | Insert of Cstruct.t
    | Copy   of int * int
    (* XXX(dinosaure): git apply a delta-ification in only [max_int32] bytes of
       the source. That means the offset and the length can't be upper than
       [max_int32]. In 32-bits architecture, we can have a problem because in
       OCaml the [native int] is stored in 31-bits but in 64-bits architecture,
       it's safe to use in any case the [native int]. TODO! *)
  and reference =
    | Offset of int64
    | Hash   of Hash.t
  and hunks =
    { reference     : reference
    ; length        : int
    ; source_length : int
    ; target_length : int }

  let partial_hunks t =
    { reference     = t._reference
    ; length        = t._length
    ; source_length = t._source_length
    ; target_length = t._target_length }

  let pp_reference ppf = function
    | Hash hash -> Fmt.pf ppf "(Reference %a)" Hash.pp hash
    | Offset off -> Fmt.pf ppf "(Offset %Ld)" off

  let pp_state ppf = function
    | Header _ ->
      Fmt.string ppf "(Header #k)"
    | Stop ->
      Fmt.string ppf "Stop"
    | List _ ->
      Fmt.string ppf "(List #k)"
    | Is_insert (_, off, len) ->
      Fmt.pf ppf "(Is_insert (#raw, %d, %d))" off len
    | Is_copy _ ->
      Fmt.string ppf "(Is_copy #k)"
    | End ->
      Fmt.string ppf "End"
    | Exception exn ->
      Fmt.pf ppf "(Exception %a)" (Fmt.hvbox pp_error) exn

  let pp_hunks ppf hunks =
    Fmt.pf ppf "{ @[<hov>reference = %a;@ \
                         length = %d;@ \
                         source_length = %d;@ \
                         target_length = %d@] }"
      (Fmt.hvbox pp_reference) hunks.reference
      hunks.length hunks.source_length hunks.target_length

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>i_off = %d;@ \
                         i_pos = %d;@ \
                         i_len = %d;@ \
                         read = %d;@ \
                         length = %d;@ \
                         reference = %a;@ \
                         source_length = %d;@ \
                         target_length = %d;@ \
                         state = %a;@] }"
      t.i_off t.i_pos t.i_len t.read
      t._length
      (Fmt.hvbox pp_reference) t._reference
      t._source_length t._target_length
      (Fmt.hvbox pp_state) t.state

  let await t: res =
    Wait t

  let error t exn: res =
    Error ({ t with state = Exception exn }, exn)

  let ok t: res =
    Ok ({ t with state = End },
        { reference     = t._reference
        ; length        = t._length
        ; source_length = t._source_length
        ; target_length = t._target_length })

  let rec get_byte ~ctor k src t =
      if (t.i_len - t.i_pos) > 0
      then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
          k byte src
            { t with i_pos = t.i_pos + 1
                    ; read = t.read + 1 }
      else await { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) }

  module KHeader =
  struct
    let get_byte = get_byte ~ctor:(fun k -> Header k)

    let rec length msb (len, bit) k src t = match msb with
      | true ->
        get_byte
          (fun byte src t ->
            let msb = byte land 0x80 <> 0 in
            (length[@tailcall]) msb (((byte land 0x7F) lsl bit) lor len, bit + 7)
            k src t)
          src t
      | false -> k len src t

    let length k src t =
      get_byte
        (fun byte src t ->
          let msb = byte land 0x80 <> 0 in
          length msb ((byte land 0x7F), 7) k src t)
        src t
  end

  module KList =
  struct
    let get_byte = get_byte ~ctor:(fun k -> List k)
  end

  let copy opcode src t =
    let rec get_byte flag k src t =
      if not flag
      then k 0 src t
      else if (t.i_len - t.i_pos) > 0
      then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
        k byte src
          { t with i_pos = t.i_pos + 1
                 ; read = t.read + 1 }
      else await { t with state = Is_copy (fun src t -> (get_byte[@tailcall]) flag k src t) }
    in

    (get_byte (opcode land 0x01 <> 0)
     @@ fun o0 -> get_byte (opcode land 0x02 <> 0)
     @@ fun o1 -> get_byte (opcode land 0x04 <> 0)
     @@ fun o2 -> get_byte (opcode land 0x08 <> 0)
     @@ fun o3 -> get_byte (opcode land 0x10 <> 0)
     @@ fun l0 -> get_byte (opcode land 0x20 <> 0)
     @@ fun l1 -> get_byte (opcode land 0x40 <> 0)
     @@ fun l2 _ t ->

       let dst = ref 0 in
       let len = ref 0 in

       if opcode land 0x01 <> 0 then dst := o0;
       if opcode land 0x02 <> 0 then dst := !dst lor (o1 lsl 8);
       if opcode land 0x04 <> 0 then dst := !dst lor (o2 lsl 16);
       if opcode land 0x08 <> 0 then dst := !dst lor (o3 lsl 24);

       if opcode land 0x10 <> 0 then len := l0;
       if opcode land 0x20 <> 0 then len := !len lor (l1 lsl 8);
       if opcode land 0x40 <> 0 then len := !len lor (l2 lsl 16);

       let dst = !dst in
       let len = !len in

       let len = if len = 0 then 0x10000 else len in

       if dst + len > t._source_length
       then begin
         error t (Wrong_copy_hunk (dst, len, t._source_length))
         (* Cont { t with state = Stop } (* avoid error *) *)
       end else
         Cont { t with state = Stop
                     ; _hunk = Some (Copy (dst, len)) })
    src t

  let stop _ t = Cont t

  let list src t =
    if t.read < t._length
    then KList.get_byte
        (fun opcode  _ t ->
            if opcode = 0 then error t (Reserved_opcode opcode)
            else match opcode land 0x80 with
              | 0 ->
                Cont { t with state = Is_insert (Cstruct.sub t._tmp 0 opcode, 0, opcode) }
              | _ ->
                Cont { t with state = Is_copy (copy opcode) })
          src t
    else begin
      ok { t with _hunk = None }
    end

  let insert src t buffer (off, rest) =
    let n = min (t.i_len - t.i_pos) rest in

    Cstruct.blit src (t.i_off + t.i_pos) buffer off n;

    if rest - n = 0
    then begin
      Cont ({ t with _hunk = Some (Insert buffer)
                    ; i_pos = t.i_pos + n
                    ; read = t.read + n
                    ; state = Stop })
    end else await { t with i_pos = t.i_pos + n
                      ; read = t.read + n
                      ; state = Is_insert (buffer, off + n, rest - n) }

  let header src t =
    (KHeader.length
    @@ fun _source_length -> KHeader.length
    @@ fun _target_length _ t ->
        Cont ({ t with state = List list
                    ; _source_length
                    ; _target_length }))
    src t

  let eval src t =
    let eval0 t =
      match t.state with
      | Header k -> k src t
      | Stop -> stop src t
      | List k -> k src t
      | Is_insert (buffer, off, rest) -> insert src t buffer (off, rest)
      | Is_copy k -> k src t
      | End -> ok t
      | Exception exn -> error t exn
    in

    let rec loop t =
      match eval0 t with
      | Cont ({ state = Stop
              ; _hunk = Some hunk
              ; _ } as t) -> `Hunk (t, hunk)
      | Cont ({ state = Stop
              ; _ }) -> assert false
      | Cont ({ state = (Header _ | List _ | Is_insert _ | Is_copy _ | End | Exception _)
              ; _ } as t) -> loop t
      | Wait t -> `Await t
      | Error (t, exn) -> `Error (t, exn)
      | Ok (t, objs) -> `Ok (t, objs)
    in

    loop t

  let default _length _reference =
    { i_off = 0
    ; i_pos = 0
    ; i_len = 0
    ; read  = 0
    ; _length
    ; _reference
    ; _source_length = 0
    ; _target_length = 0
    ; _hunk = None
    ; _tmp = Cstruct.create 0x7F
    ; state = Header header }

  let refill off len t =
    if t.i_pos = t.i_len
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
    else raise (Invalid_argument (Fmt.strf "Hunk.refill: you lost something"))

  let continue t =
    match t.state with
    | Stop -> { t with state = List list }
    | Header _ | List _ | Is_insert _ | Is_copy _ | End | Exception _ ->
      raise (Invalid_argument "Hunk.continue: bad state")

  let current t = match t with
    | { state = Stop
      ; _hunk = Some hunk
      ; _ } -> hunk
    | { state = Stop
      ; _hunk = None
      ; _ }
    | { state = (Header _ | List _ | Is_insert _ | Is_copy _ | End | Exception _)
      ; _hunk = (Some _ | None)
      ; _ } -> raise (Invalid_argument "Hunk.current: bad state")

  let available_in t = t.i_len
  let used_in t      = t.i_pos
  let read t         = t.read
end

module Int64 =
struct
  include Int64

  let ( / ) = Int64.div
  let ( * ) = Int64.mul
  let ( + ) = Int64.add
  let ( - ) = Int64.sub
end

module Int32 =
struct
  include Int32

  let ( << ) = Int32.shift_left (* >> (tuareg) *)
  let ( || ) = Int32.logor
end

module type P = sig

  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Hunk: H with module Hash := Hash

  type error =
    | Invalid_byte of int
    | Reserved_kind of int
    | Invalid_kind of int
    | Inflate_error of Inflate.error
    | Hunk_error of Hunk.error
    | Hunk_input of int * int
    | Invalid_length of int * int

  val pp_error: error Fmt.t

  type t

  val pp: t Fmt.t

  type kind =
    | Commit
    | Tree
    | Blob
    | Tag
    | Hunk of Hunk.hunks

  val default: Cstruct.t -> Inflate.window -> t
  val many: t -> int32
  val from_window: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  val process_length: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  val process_metadata: Window.t -> int -> Cstruct.t -> Inflate.window -> t
  val refill: int -> int -> t -> t
  val flush: int -> int -> t -> t
  val next_object: t -> t
  val continue: t -> t
  val kind: t -> kind
  val length: t -> int
  val offset: t -> int64
  val consumed: t -> int
  val crc: t -> Crc32.t
  val output: t -> Cstruct.t * int
  val eval: Cstruct.t -> t -> [ `Object of t | `Hunk of t * Hunk.hunk | `Await of t | `Flush of t | `End of t * Hash.t | `Error of t * error ]
  val eval_length: Cstruct.t -> t -> [ `Length of t | `Await of t | `Flush of t | `End of (t * Hash.t) | `Error of (t * error) ]
  val eval_metadata: Cstruct.t -> t -> [ `Metadata of t | `Await of t | `Flush of t | `End of (t * Hash.t) | `Error of (t * error) ]
end

(* Implementatioon of deserialization of a PACK file *)
module Pack
    (Hash: S.HASH)
    (Inflate: S.INFLATE)
    (Hunk: H with module Hash := Hash)
  : P with module Hash = Hash
       and module Inflate = Inflate
       and module Hunk := Hunk
= struct
  module Hash = Hash
  module Inflate = Inflate
  module Hunk = Hunk

  type error =
    | Invalid_byte of int
    | Reserved_kind of int
    | Invalid_kind of int
    | Inflate_error of Inflate.error
    | Hunk_error of Hunk.error
    | Hunk_input of int * int
    | Invalid_length of int * int

  let pp_error ppf = function
    | Invalid_byte byte ->
      Fmt.pf ppf "Got an invalid byte: %02x" byte
    | Reserved_kind byte ->
      Fmt.pf ppf "Got an reserved byte: %02x" byte
    | Invalid_kind byte ->
      Fmt.pf ppf "Got an invalid kind of object: %02x" byte
    | Inflate_error err ->
      Fmt.pf ppf "Got an inflate error: %a" Inflate.pp_error err
    | Invalid_length (expected, has) ->
      Fmt.pf ppf "Unserialize a corrupted object (length mismatch, expect %d, have %d)"
        expected has
    | Hunk_error err ->
      Fmt.pf ppf "Got a Hunk decoder error: %a"
        Hunk.pp_error err
    | Hunk_input _ -> assert false

  type process =
    [ `All
    | `One
    | `Length
    | `Metadata ]

  type t =
    { i_off   : int
    ; i_pos   : int
    ; i_len   : int
    ; process : process
    ; read    : int64
    ; o_z     : Cstruct.t
    ; o_w     : Inflate.window
    ; version : int32
    ; objects : int32
    ; counter : int32
    ; state   : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header    of k
    | Object    of k
    | VariableLength of k
    | Unzip     of { offset   : int64
                   ; consumed : int
                   ; length   : int
                   ; kind     : kind
                   ; crc      : Crc32.t
                   ; z        : Inflate.t }
    | Hunks     of hunks_state
    | StopHunks of hunks_state
    | Next      of { offset   : int64
                   ; consumed : int
                   ; length   : int
                   ; length'  : int
                   (* XXX(dinosaure): [length] is the value decoded.
                      [length'] is the value returned when we inflate
                      the raw. It must to be the same. However, we can
                      inflate a huge object (like a [Blob] which is
                      not delta-ified).

                      The length of the object can be upper than
                      [max_native_int] (but can't be upper than
                      [max_int64]). So we need to switch these values
                      to [int64]. However, the [Inflate] algorithm
                      provide only a [native_int]. We can bypass this
                      limit and count the length of the object by
                      ourselves with an [int64]. TODO! *)
                   ; crc      : Crc32.t
                   ; kind     : kind }
    | Checksum  of k
    | End       of Hash.t
    | Exception of error
  and hunks_state =
    { offset   : int64
    ; length   : int
    ; consumed : int
    ; crc      : Crc32.t
    ; z        : Inflate.t
    ; h        : Hunk.t }
  and res =
    | Wait  of t
    | Flush of t
    | Error of t * error
    | Cont  of t
    | Ok    of t * Hash.t
  and kind =
    | Commit
    | Tree
    | Blob
    | Tag
    | Hunk of Hunk.hunks

  let pp_kind ppf = function
    | Commit     -> Fmt.pf ppf "Commit"
    | Tree       -> Fmt.pf ppf "Tree"
    | Blob       -> Fmt.pf ppf "Blob"
    | Tag        -> Fmt.pf ppf "Tag"
    | Hunk hunks -> Fmt.pf ppf "(Hunks %a)" (Fmt.hvbox Hunk.pp_hunks) hunks

  let pp_hunks_state ppf { offset; length; consumed; z; h; _ } =
    Fmt.pf ppf "{ @[<hov>offset = %Ld;@ \
                         consumed = %d;@ \
                         length = %d;@ \
                         z = %a;@ \
                         h = %a;@] })"
      offset consumed length
      (Fmt.hvbox Inflate.pp) z (Fmt.hvbox Hunk.pp) h

  let pp_state ppf = function
    | Header _ ->
      Fmt.string ppf "(Header #k)"
    | Object _ ->
      Fmt.string ppf "(Object #k)"
    | VariableLength _ ->
      Fmt.string ppf "(VariableLength #k)"
    | Unzip { offset
            ; consumed
            ; length
            ; kind
            ; z
            ; _ } ->
      Fmt.pf ppf "(Unzip { @[<hov>offset = %Ld;@ \
                                  consumed = %d;@ \
                                  length = %d;@ \
                                  kind = %a;@ \
                                  z = %a;@] })"
        offset consumed length pp_kind kind
        (Fmt.hvbox Inflate.pp) z
    | Hunks hs ->
      Fmt.pf ppf "(Hunks %a)"
        (Fmt.hvbox pp_hunks_state) hs
    | StopHunks hs ->
      Fmt.pf ppf "(StopHunks %a)"
        (Fmt.hvbox pp_hunks_state) hs
    | Next { offset
           ; consumed
           ; length
           ; length'
           ; _ } ->
      Fmt.pf ppf "(Next { @[<hov>offset = %Ld;@ \
                                 consumed = %d;@ \
                                 length = %d;@ \
                                 length' = %d;@] })"
        offset consumed length length'
    | Checksum _ ->
      Fmt.string ppf "(Checksum #k)"
    | End hash ->
      Fmt.pf ppf "(End %a)" Hash.pp hash
    | Exception err ->
      Fmt.pf ppf "(Exception %a)" (Fmt.hvbox pp_error) err

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>i_off = %d;@ \
                         i_pos = %d;@ \
                         i_len = %d;@ \
                         version = %ld;@ \
                         objects = %ld;@ \
                         counter = %ld;@ \
                         state = %a;@] }"
      t.i_off t.i_pos t.i_len
      t.version
      t.objects
      t.counter
      (Fmt.hvbox pp_state) t.state

  (* TODO: need to compute the hash of the input. *)
  let await t: res = Wait t
  let flush t: res = Flush t
  let error t exn: res = Error ({ t with state = Exception exn }, exn)
  let ok t hash: res = Ok ({ t with state = End hash }, hash)

  let rec get_byte ~ctor k src t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
        k byte src { t with i_pos = t.i_pos + 1
                          ; read = Int64.add t.read 1L }
    else await { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) }

  let get_hash ~ctor k src t =
    let buf = Buffer.create Hash.Digest.length in

    let rec loop i src t =
      if i = Hash.Digest.length
      then k (Hash.of_string (Buffer.contents buf)) src t
      else
        get_byte ~ctor
          (fun byte src t ->
             Buffer.add_char buf (Char.chr byte);
             (loop[@tailcall]) (i + 1) src t)
          src t
    in

    loop 0 src t

  module KHeader =
  struct
    let rec check_byte chr k src t =
      if (t.i_len - t.i_pos) > 0 && Cstruct.get_char src (t.i_off + t.i_pos) = chr
      then k src { t with i_pos = t.i_pos + 1
                        ; read = Int64.add t.read 1L }
      else if (t.i_len - t.i_pos) = 0
      then await { t with state = Header (fun src t -> (check_byte[@tailcall]) chr k src t) }
      else error { t with i_pos = t.i_pos + 1
                        ; read = Int64.add t.read 1L }
                (Invalid_byte (Cstruct.get_uint8 src (t.i_off + t.i_pos)))

    let get_byte = get_byte ~ctor:(fun k -> Header k)

    let to_int32 b0 b1 b2 b3 =
      let open Int32 in
      (of_int b0 << 24)    (* >> (tuareg) *)
      || (of_int b1 << 16) (* >> (tuareg) *)
      || (of_int b2 << 8)  (* >> (tuareg) *)
      || (of_int b3)

    let rec get_u32 k src t =
      if (t.i_len - t.i_pos) > 3
      then let num = Cstruct.BE.get_uint32 src (t.i_off + t.i_pos) in
          k num src
            { t with i_pos = t.i_pos + 4
                    ; read = Int64.add t.read 4L }
      else if (t.i_len - t.i_pos) > 0
      then (get_byte
            @@ fun byte0 -> get_byte
            @@ fun byte1 -> get_byte
            @@ fun byte2 -> get_byte
            @@ fun byte3 src t ->
              k (to_int32 byte0 byte1 byte2 byte3) src t)
            src t
      else await { t with state = Header (fun src t -> (get_u32[@tailcall]) k src t) }
  end

  module KVariableLength =
  struct
    let get_byte = get_byte ~ctor:(fun k -> VariableLength k)
  end

  module KObject =
  struct
    let get_byte = get_byte ~ctor:(fun k -> Object k)
    let get_hash = get_hash ~ctor:(fun k -> Object k)
  end

  module KChecksum =
  struct
    let get_hash = get_hash ~ctor:(fun k -> Checksum k)
  end

  let rec length msb (len, bit) crc k src t = match msb with
    | true ->
      KVariableLength.get_byte
        (fun byte src t ->
          let msb = byte land 0x80 <> 0 in
          let crc = Crc32.digestc crc byte in

          (length[@tailcall]) msb (len lor ((byte land 0x7F) lsl bit), bit + 7) crc
          k src t)
        src t
    | false -> k len crc src t

  let rec offset msb off crc k src t = match msb with
    | true ->
      KVariableLength.get_byte
        (fun byte src t ->
          let msb = byte land 0x80 <> 0 in
          let crc = Crc32.digestc crc byte in

          (offset[@tailcall]) msb (Int64.logor (Int64.shift_left (Int64.add off 1L) 7) (Int64.of_int (byte land 0x7F))) crc
          k src t)
        src t
    | false -> k off crc src t

  let stop_hunks _ t hs =
    Cont { t with state = StopHunks hs }

  (* XXX(dinosaure): Need an explanation. We must compute firstly the evaluation
     of the [H] before [Inflate] to be sure we wait something (and ask
     only when it's needed [Inflate]).

     Then, we need to be ensure than [H] consumed all bytes provided
     by the input. That means, when [H] returns [`Await], we ensure
     than [t.o_z] is totally free.

     Then, we can safely [refill] [H] and, at the same time, [flush]
     [t.o_z] because we ensure than to the next call, [Inflate.eval] appear (and
     write something inside [t.o_z]) only when [H] consumed all bytes
     available.

     So, when you change this code or [H], we need to keep these
     assertions. Otherwise, it's end of the world.

     - When [H] returns `Await, we ensure than it consumed all bytes
     available (so, you are free to do what you want with [t.o_z])
     - We [Inflate] only when it's needed (so, when [H] returns
     [`Await]) *)
  let hunks src t offset length consumed crc z h =
    match Hunk.eval t.o_z h with
    | `Await h ->
      (match Inflate.eval ~src ~dst:t.o_z z with
       | `Await z ->
         let consumed = consumed + Inflate.used_in z in
         let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in

         await { t with state = Hunks { offset; length; consumed; crc; z = Inflate.refill 0 0 z; h; }
                      ; i_pos = t.i_pos + Inflate.used_in z
                      ; read  = Int64.add t.read (Int64.of_int (Inflate.used_in z)) }
       | `Flush z ->
         let h = Hunk.refill 0 (Inflate.used_out z) h in
         let z = Inflate.flush 0 (Cstruct.len t.o_z) z in

         Cont { t with state = Hunks { offset; length; consumed; crc; z; h; } }
       | `End z ->
         (* XXX(dinosaure): In [zlib] and [decompress], it could be happen to
            return [`End] and consume a part of the input [t.o_z].

            So, we compute the CRC-32 checksum and update [consumed]. *)
         let consumed = consumed + Inflate.used_in z in
         let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in

         let h = Hunk.refill 0 (Inflate.used_out z) h in

         Cont { t with state = Hunks { offset; length; consumed; crc; z = Inflate.refill 0 0 z; h; }
                     ; i_pos = t.i_pos + Inflate.used_in z
                     ; read  = Int64.(add t.read (of_int (Inflate.used_in z))) }
       | `Error (z, exn) -> error { t with i_pos = t.i_pos + Inflate.used_in z
                                         ; read  = Int64.(add t.read (of_int (Inflate.used_in z))) } (Inflate_error exn))
    | `Hunk (h, _) ->
      Cont { t with state = StopHunks { offset; length; consumed; crc; z; h; } }
    | `Error (_, exn) -> error t (Hunk_error exn)
    | `Ok (_, hunks) ->
      Cont { t with state = Next { length
                                 ; length' = Inflate.write z
                                 ; offset
                                 ; consumed
                                 ; crc
                                 ; kind = Hunk hunks } }

  let size_of_variable_length vl =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (acc + 1) (n lsr 7)
    in
    loop 1 (vl lsr 4) (* avoid type and msb *)

  let size_of_offset off =
    let rec loop acc = function
      | 0L -> acc
      | n -> loop (acc + 1) (Int64.shift_right n 7)
    in
    loop 1 (Int64.shift_right off 7)

  let switch typ off len crc src t =
    match typ with
    | 0b000 | 0b101 -> error t (Reserved_kind typ)
    | 0b001 ->
      Cont { t with state = Unzip { offset   = off
                                  ; length   = len
                                  ; consumed = size_of_variable_length len
                                  ; crc
                                  ; kind = Commit
                                  ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                        @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                        @@ Inflate.default (Inflate.window_reset t.o_w) } }
    | 0b010 ->
      Cont { t with state = Unzip { offset   = off
                                  ; length   = len
                                  ; consumed = size_of_variable_length len
                                  ; crc
                                  ; kind     = Tree
                                  ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                        @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                        @@ Inflate.default (Inflate.window_reset t.o_w) } }
    | 0b011 ->
      Cont { t with state = Unzip { offset   = off
                                  ; length   = len
                                  ; consumed = size_of_variable_length len
                                  ; crc
                                  ; kind     = Blob
                                  ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                        @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                        @@ Inflate.default (Inflate.window_reset t.o_w) } }
    | 0b100 ->
      Cont { t with state = Unzip { offset   = off
                                  ; length   = len
                                  ; consumed = size_of_variable_length len
                                  ; crc
                                  ; kind     = Tag
                                  ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                        @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                        @@ Inflate.default (Inflate.window_reset t.o_w) } }
    | 0b110 ->
      KObject.get_byte
        (fun byte src t ->
          let msb = byte land 0x80 <> 0 in
          let crc = Crc32.digestc crc byte in

          offset msb (Int64.of_int (byte land 0x7F)) crc
            (fun offset crc _ t ->
               Cont { t with state = Hunks { offset   = off
                                           ; length   = len
                                           ; consumed = size_of_variable_length len
                                                        + size_of_offset offset
                                           ; crc
                                           ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                               @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                               @@ Inflate.default (Inflate.window_reset t.o_w)
                                           ; h = Hunk.default len (Hunk.Offset offset) } })
            src t)
        src t
    | 0b111 ->
      KObject.get_hash
        (fun hash _ t ->
          let crc = Crc32.digests crc (Hash.to_string hash |> Bytes.unsafe_of_string) in

          Cont { t with state = Hunks { offset   = off
                                      ; length   = len
                                      ; consumed = Hash.Digest.length + size_of_variable_length len
                                      ; crc
                                      ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                            @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                            @@ Inflate.default (Inflate.window_reset t.o_w)
                                      ; h = Hunk.default len (Hunk.Hash hash) } })
        src t
    | _  -> error t (Invalid_kind typ)

  (* TODO: check the hash procuded and the hash noticed. *)
  let checksum src t =
    KChecksum.get_hash
      (fun hash _ t -> ok t hash)
      src t

  let kind src t =
    let offset = t.read in

    KObject.get_byte
      (fun byte src t ->
        let msb = byte land 0x80 <> 0 in
        let typ = (byte land 0x70) lsr 4 in
        let crc = Crc32.digestc Crc32.default byte in

        length msb (byte land 0x0F, 4) crc (fun len crc _ t -> Cont { t with state = Object (fun src t -> switch typ offset len crc src t) }) src t)
      src t

  let unzip src t offset length consumed crc kind z =
    match Inflate.eval ~src ~dst:t.o_z z with
    | `Await z ->
      let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in
      let consumed = consumed + Inflate.used_in z in

      await { t with state = Unzip { offset
                                   ; length
                                   ; consumed
                                   ; crc
                                   ; kind
                                   ; z = Inflate.refill 0 0 z }
                   ; i_pos = t.i_pos + Inflate.used_in z
                   ; read = Int64.add t.read (Int64.of_int (Inflate.used_in z)) }
    | `Flush z ->
      flush { t with state = Unzip { offset; length; consumed; crc
                                   ; kind
                                   ; z } }
    | `End z ->
      let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in
      let consumed = consumed + Inflate.used_in z in

      if Inflate.used_out z <> 0
      then flush { t with state = Unzip { offset; length; consumed; crc
                                        ; kind
                                        ; z = Inflate.refill 0 0 z }
                        ; i_pos = t.i_pos + Inflate.used_in z
                        ; read = Int64.(add t.read (of_int (Inflate.used_in z))) }
      else
        Cont { t with state = Next { length
                                   ; length' = Inflate.write z
                                   ; consumed = consumed + Inflate.used_in z
                                   ; offset
                                   ; crc
                                   ; kind }
                    ; i_pos = t.i_pos + Inflate.used_in z
                    ; read = Int64.add t.read (Int64.of_int (Inflate.used_in z)) }
    | `Error (z, exn) ->
      error { t with i_pos = t.i_pos + Inflate.used_in z
                   ; read  = Int64.(add t.read (of_int (Inflate.used_in z ))) } (Inflate_error exn)

  let next _ t length length' _ =
    if length = length'
    then Cont t
    else error t (Invalid_length (length, length'))

  let number src t =
    KHeader.get_u32
      (fun objects _ t ->
        Cont { t with objects = objects
                    ; counter = objects
                    ; state = if objects = 0l then Checksum checksum else Object kind })
      src t

  let version src t =
    KHeader.get_u32
      (fun version src t ->
        number src { t with version = version })
      src t

  let header src t =
    (KHeader.check_byte 'P'
    @@ KHeader.check_byte 'A'
    @@ KHeader.check_byte 'C'
    @@ KHeader.check_byte 'K'
    @@ version)
    src t

  let default z_tmp z_win =
    { i_off   = 0
    ; i_pos   = 0
    ; i_len   = 0
    ; process = `All
    ; o_z     = z_tmp
    ; o_w     = z_win
    ; read    = 0L
    ; version = 0l
    ; objects = 0l
    ; counter = 0l
    ; state   = Header header }

  let many { objects; _ } = objects

  let from_window window win_offset z_tmp z_win =
    { i_off   = 0
    ; i_pos   = 0
    ; i_len   = 0
    ; process = `One
    ; o_z     = z_tmp
    ; o_w     = z_win
    ; read    = Int64.add window.Window.off (Int64.of_int win_offset)
    ; version = 0l
    ; objects = 1l
    ; counter = 1l
    ; state   = Object kind }

  let process_length window win_offset z_tmp z_win =
    { i_off   = 0
    ; i_pos   = 0
    ; i_len   = 0
    ; process = `Length
    ; o_z     = z_tmp
    ; o_w     = z_win
    ; read    = Int64.add window.Window.off (Int64.of_int win_offset)
    ; version = 0l
    ; objects = 1l
    ; counter = 1l
    ; state   = Object kind }

  let process_metadata window win_offset z_tmp z_win =
    { i_off   = 0
    ; i_pos   = 0
    ; i_len   = 0
    ; process = `Metadata
    ; o_z     = z_tmp
    ; o_w     = z_win
    ; read    = Int64.add window.Window.off (Int64.of_int win_offset)
    ; version = 0l
    ; objects = 1l
    ; counter = 1l
    ; state   = Object kind }

  let is_end { state; _ } = match state with
    | End _ -> true
    | Header _ | Object _ | VariableLength _ | Unzip _
    | Hunks _ | StopHunks _ | Next _ | Checksum _ | Exception _ -> false

  let refill off len t =
    if (t.i_len - t.i_pos) = 0 && not (is_end t)
    then match t.state with
        | Unzip { offset; length; consumed; crc; kind; z; } ->
          { t with i_off = off
                  ; i_len = len
                  ; i_pos = 0
                  ; state = Unzip { offset
                                  ; length
                                  ; consumed
                                  ; crc
                                  ; kind
                                  ; z = Inflate.refill off len z } }
        | Hunks { offset; length; consumed; crc; z; h; } ->
          { t with i_off = off
                  ; i_len = len
                  ; i_pos = 0
                  ; state = Hunks { offset
                                  ; length
                                  ; consumed
                                  ; crc
                                  ; z = Inflate.refill off len z; h; } }
        | Header _ | Object _ | VariableLength _ | End _
        | StopHunks _ | Next _ | Checksum _ | Exception _ ->
          { t with i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
    else if is_end t
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
      (* XXX(dinosaure): at the end, we don't care if we lost something. *)
    else raise (Invalid_argument (Fmt.strf "Pack.refill: you lost something \
                                            (pos: %d, len: %d)" t.i_pos t.i_len))

  let flush off len t = match t.state with
    | Unzip { offset; length; consumed; crc; kind; z; } ->
      { t with state = Unzip { offset
                             ; length
                             ; consumed
                             ; crc
                             ; kind
                             ; z = Inflate.flush off len z } }
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | StopHunks _ | Next _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.flush: bad state")

  let output t = match t.state with
    | Unzip { z; _ } ->
      t.o_z, Inflate.used_out z
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | StopHunks _ | Next _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.output: bad state")

  let next_object t =
    match t.state with
    | Next _ when t.process = `All ->
      if Int32.pred t.counter = 0l
      then { t with state = Checksum checksum
                  ; counter = Int32.pred t.counter }
      else { t with state = Object kind
                  ; counter = Int32.pred t.counter }
    | Next _ -> { t with state = End (Hash.of_string (String.make Hash.Digest.length '\000')) }
      (* XXX(dinosaure): in the local case, the user don't care about the hash of the PACK file. *)
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | StopHunks _ | Unzip _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.next_object: bad state")

  let kind t = match t.state with
    | Unzip { kind; _ } -> kind
    | StopHunks { h; _ }
    | Hunks { h; _ } -> Hunk (Hunk.partial_hunks h)
    | Next { kind; _ } -> kind
    | End _ | Header _ | Object _ | VariableLength _
    | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.kind: bad state")

  let length t = match t.state with
    | Unzip { length; _ } -> length
    | StopHunks { length; _ } -> length
    | Hunks { length; _ } -> length
    | Next { length; _ } -> length
    | End _ | Header _ | Object _ | VariableLength _
    | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.length: bad state")

  (* XXX(dinosaure): The consumed value calculated in this deserialization is
     different from what git says (a diff of 1 or 2 bytes) - may be it come from
     a wrong compute of the length of the offset value (see {!size_of_offset}).
     It's not very important but FIXME! *)
  let consumed t = match t.state with
    | Next { consumed; _ } -> consumed
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | StopHunks _ | Unzip _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.consumed: bad state")

  let offset t = match t.state with
    | Unzip { offset; _ } -> offset
    | StopHunks { offset; _ } -> offset
    | Hunks { offset; _ } -> offset
    | Next { offset; _ } -> offset
    | End _ | Header _ | Object _ | VariableLength _
    | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.offset: bad state")

  let crc t = match t.state with
    | Next { crc; _ } -> crc
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | StopHunks _ | Unzip _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.crc: bad state")

  let continue t =
    match t.state with
    | StopHunks hs ->
      { t with state = Hunks { hs with h = Hunk.continue hs.h } }
    | End _ | Header _ | Object _ | VariableLength _
    | Hunks _ | Next _ | Unzip _ | Checksum _ | Exception _ ->
      raise (Invalid_argument "Pack.continue: bad state")

  let eval0 src t =
    match t.state with
    | Header k -> k src t
    | Object k -> k src t
    | VariableLength k -> k src t
    | Unzip { offset; length; consumed; crc; kind; z; } ->
      unzip src t offset length consumed crc kind z
    | Hunks { offset; length; consumed; crc; z; h; _ } ->
      hunks src t offset length consumed crc z h
    | StopHunks hs -> stop_hunks src t hs
    | Next { length; length'; kind; _ } -> next src t length length' kind
    | Checksum k -> k src t
    | End hash -> ok t hash
    | Exception exn -> error t exn

  let eval src t =
    let rec loop t =
      match eval0 src t with
      | Cont ({ state = Next _; _ } as t) ->
        `Object t
      | Cont ({ state = StopHunks hs; _ } as t) ->
        `Hunk (t, Hunk.current hs.h)
      | Cont ({ state = (Header _ | Object _ | VariableLength _
                        | Unzip _ | Hunks _ | Checksum _ | End _ | Exception _); _ } as t) -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  let eval_length src t =
    let rec loop t =
      (* XXX(dinosaure): pattern is fragile. *)

      match eval0 src t with
      | Cont (({ state = Next _; _ }
              | { state = Unzip _; _ }
              | { state = Hunks { h = { Hunk.state = Hunk.List _; _ }; _ }; _ }) as t) ->
        `Length t
      | Cont ({ state = StopHunks _; _ } as t) -> loop (continue t)
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    assert (t.process = `Length);
    loop t

  let eval_metadata src t =
    let rec loop t =
      (* XXX(dinosaure): pattern is fragile. *)
      match eval0 src t with
      | Cont (({ state = Next _; _ }
              | { state = Unzip _; _ }
              | { state = Hunks _; _ }) as t) -> `Metadata t
      | Cont ({ state = StopHunks _; _ } as t) -> loop (continue t)
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    assert (t.process = `Metadata);
    loop t
end

module type D = sig

  module Hash: S.HASH
  module Mapper: S.MAPPER
  module Inflate: S.INFLATE

  module Hunk: H with module Hash := Hash
  module Pack: P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := Hunk

  type error =
    | Invalid_hash of Hash.t
    | Invalid_offset of int64
    | Invalid_target of (int * int)
    | Unpack_error of Pack.t * Window.t * Pack.error
    | Mapper_error of Mapper.error

  val pp_error: error Fmt.t

  type kind = [ `Commit | `Blob | `Tree | `Tag ]

  type pack

  val idx: pack -> (Hash.t -> (Crc32.t * int64) option)
  val extern: pack -> (Hash.t -> (kind * Cstruct.t) option Lwt.t)

  val update_idx: (Hash.t -> (Crc32.t * int64) option) -> pack -> pack
  val update_extern: (Hash.t -> (kind * Cstruct.t) option Lwt.t) -> pack -> pack

  val make: ?bucket:int -> Mapper.fd ->
    (Hash.t -> (Crc32.t * int64) option) ->
    (Hash.t -> (kind * Cstruct.t) option Lwt.t) ->
    (pack, Mapper.error) result Lwt.t

  module Diff: sig
    type insert = S of string | C of Cstruct.t
    type t = Insert of insert | Copy of (int * int)

    val len: insert -> int
  end

  module Patch: sig
    type t =
      { length : int
      ; consumed : int
      ; offset : int64
      ; crc : Crc32.t
      ; hunks : Diff.t list
      ; descr : Hunk.hunks }

    val get_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      ?htmp:Cstruct.t ->
      pack -> int64 -> (t, error) result Lwt.t

    type s =
      { kind : kind
      ; length : int
      ; consumed : int
      ; inserts : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; raw : Cstruct.t
      ; descr : Hunk.hunks }

    val apply_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      (kind * Cstruct.t) -> Cstruct.t ->
      pack -> int64 -> (s, error) result Lwt.t
  end

  module Base: sig
    type t =
      { kind : kind
      ; length : int
      ; consumed : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; raw : Cstruct.t }

    val get_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      Cstruct.t ->
      pack -> int64 -> (t, error) result Lwt.t
  end

  module Object: sig
    type t =
      | Patch of Patch.t
      | Root of Base.t

    val get_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      ?htmp:Cstruct.t -> Cstruct.t ->
      pack -> int64 -> (t, error) result Lwt.t
  end

  module Cache: sig
    type ('key, 'value) t =
      { promote : 'key -> 'value -> unit
      ; find : 'key -> 'value option }

    val lru: int ->
      (module Hashtbl.HashedType with type t = 'key) ->
      (module Lru.Weighted with type t = 'value) ->
      ('key, 'value) t
  end

  module Ascendant: sig
    type t =
      | External of { hash : Hash.t; kind : kind; raw : Cstruct.t }
      | Root of Base.t
      | Node of { patch : Patch.t; source : t }

    type metadata =
      { length : int
      ; crc : Crc32.t
      ; offset : int64
      ; consumed : int }

    type s =
      [ `Patch of metadata
      | `Base of metadata
      | `Extern ]

    val needed_cache: int -> (int64, int) Cache.t
    val get_cache: int -> (int64, t) Cache.t
    val apply_cache: int -> (int64, kind * Cstruct.t * int * s) Cache.t

    val get_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, t) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> Cstruct.t ->
      pack -> int64 -> (t, error) result Lwt.t

    val get_from_hash:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, t) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> Cstruct.t ->
      pack -> Hash.t -> (t, error) result Lwt.t

    val get:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, t) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> Cstruct.t ->
      pack -> [ `Hash of Hash.t | `Offset of int64 ] -> (t, error) result Lwt.t

    val reconstruct: (Cstruct.t * Cstruct.t) -> t -> (kind * Cstruct.t * int * s)

    val apply_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, kind * Cstruct.t * int * s) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> (Cstruct.t * Cstruct.t) ->
      pack -> int64 -> (kind * Cstruct.t * int * s, error) result Lwt.t

    val apply_from_hash:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, kind * Cstruct.t * int * [ `Patch of metadata | `Base of metadata | `Extern ]) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> (Cstruct.t * Cstruct.t) ->
      pack -> Hash.t -> (kind * Cstruct.t * int * s, error) result Lwt.t

    val apply:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, kind * Cstruct.t * int * s) Cache.t ->
      ?chunk:int ->
      ?htmp:Cstruct.t array -> (Cstruct.t * Cstruct.t) ->
      pack -> [ `Hash of Hash.t | `Offset of int64 ] -> (kind * Cstruct.t * int * s, error) result Lwt.t

    val needed_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, int) Cache.t ->
      ?chunk:int ->
      pack -> int64 -> (int, error) result Lwt.t

    val needed_from_hash:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, int) Cache.t ->
      ?chunk:int ->
      pack -> Hash.t -> (int, error) result Lwt.t

    val needed:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      cache:(int64, int) Cache.t ->
      ?chunk:int ->
      pack -> [ `Hash of Hash.t | `Offset of int64 ] -> (int, error) result Lwt.t

    val length_from_absolute_offset:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      pack -> int64 -> (int, error) result Lwt.t

    val length_from_hash:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      pack -> Hash.t -> (int, error) result Lwt.t

    val length:
      ztmp:Cstruct.t -> zwin:Inflate.window ->
      ?chunk:int ->
      pack -> [ `Hash of Hash.t | `Offset of int64 ] -> (int, error) result Lwt.t
  end

  module Descendant: sig
    type base =
      { kind : kind
      ; length : int
      ; consumed : int
      ; offset : int64
      ; hash : Hash.t
      ; crc : Crc32.t }

    type patch =
      { length : int
      ; consumed : int
      ; inserts : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; descr : Hunk.hunks }

    type t =
      | Root of { base : base; children : node list }
    and node =
      | Node of { patch : patch
                ; children : node list }
      | Leaf of patch

    val get_from_absolute_offset:
      ztmp:Cstruct.t ->
      zwin:Inflate.window ->
      cache:((int64, int) Cache.t * (int64, kind * Cstruct.t * int * Ascendant.s) Cache.t) ->
      ?chunk:int ->
      children:(int64 * Hash.t -> int64 list) ->
      Cstruct.t ->
      pack -> int64 -> (t, error) result Lwt.t
  end
end

module Decoder
    (Hash: S.HASH)
    (Mapper: S.MAPPER)
    (Inflate: S.INFLATE)
    (Hunk: H with module Hash := Hash)
    (Pack: P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := Hunk)
  : D with module Hash = Hash
       and module Mapper = Mapper
       and module Inflate = Inflate
       and module Hunk := Hunk
       and module Pack := Pack =
struct
  module Log =
  struct
    let src = Logs.Src.create "git.unpack" ~doc:"logs git's unpack event"
    include (val Logs.src_log src : Logs.LOG)
  end

  module Hash = Hash
  module Mapper = Mapper
  module Inflate = Inflate
  module Pack = Pack
  module Hunk = Hunk

  type error =
    | Invalid_hash of Hash.t
    | Invalid_offset of int64
    | Invalid_target of (int * int)
    | Unpack_error of Pack.t * Window.t * Pack.error
    | Mapper_error of Mapper.error

  let pp_error ppf = function
    | Invalid_hash hash ->
      Fmt.pf ppf "Invalid hash: %a" Hash.pp hash
    | Invalid_offset off ->
      Fmt.pf ppf "Invalid offset: %Ld" off
    | Invalid_target (has, expected) ->
      Fmt.pf ppf "Bad object re-constructed (length mismatch, expect: %d, have: %d)"
        expected has
    | Unpack_error (_, _, exn) ->
      Fmt.pf ppf "Got an error while decoding PACK: %a"
        Pack.pp_error exn
    | Mapper_error err ->
      Fmt.pf ppf "Got an error while mmaping: %a"
        Mapper.pp_error err

  type kind = [ `Commit | `Blob | `Tree | `Tag ]

  type pack =
    { file  : Mapper.fd
    ; max   : int64
    ; win   : Window.t Bucket.t
    ; idx   : Hash.t -> (Crc32.t * int64) option
    ; get   : Hash.t -> (kind * Cstruct.t) option Lwt.t }

  let make ?(bucket = 10) file idx get =
    Mapper.length file >|= function
    | Error _ as err -> err
    | Ok max ->
      Ok { file; max; win = Bucket.make bucket; idx; get; }

  module Diff =
  struct
    type insert = S of string | C of Cstruct.t
    type t = Insert of insert | Copy of (int * int)

    let blit_to_cstruct src off0 dst off1 len = match src with
      | S s -> Cstruct.blit_from_string s off0 dst off1 len
      | C c -> Cstruct.blit c off0 dst off1 len

    let len = function S s -> String.length s | C c -> Cstruct.len c
  end

  let map_window t offset_requested =
    let pos = Int64.((offset_requested / 1024L) * 1024L) in (* padding *)
    Mapper.map t.file ~pos (1024 * 1024) >>= function
    | Ok map ->
      Lwt.return (Ok { Window.raw = map
                     ; off   = pos
                     ; len   = Cstruct.len map })
    | Error err -> Lwt.return (Error err)

  let find_window t offset_requested =
    let predicate window = Window.inside offset_requested window in

    match Bucket.find t.win predicate with
    | Some window ->
      Log.debug (fun l ->
          l "Reusing a window already loaded: [%Ld:%d]"
            window.Window.off window.Window.len);
      let relative_offset =
        Int64.to_int Int64.(offset_requested - window.Window.off)
      in
      Lwt.return (Ok (window, relative_offset))
    | None ->
      Log.debug (fun l ->
          l "Loading a new window for the %Ld offset requested." offset_requested);
      map_window t offset_requested >|= function
      | Error err -> Error err
      | Ok window ->
        let relative_offset =
          Int64.to_int Int64.(offset_requested - window.Window.off)
        in
        let () = Bucket.add t.win window in
        Ok (window, relative_offset)

  module Patch =
  struct
    type t =
      { length : int
      ; consumed : int
      ; offset : int64
      ; crc : Crc32.t
      ; hunks : Diff.t list
      ; descr : Hunk.hunks }

    let get_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) ?htmp t absolute_offset =
      find_window t absolute_offset >>= function
      | Error err -> Lwt.return_error (Mapper_error err)
      | Ok (window, relative_offset) ->
        let state = Pack.from_window window relative_offset ztmp zwin in

        let rec go window consumed_in_window writed_in_htmp hunks result state =
          match Pack.eval window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then go window
                (consumed_in_window + rest_in_window)
                writed_in_htmp hunks result
                (Pack.refill consumed_in_window rest_in_window state)
            else begin
              find_window t Int64.(window.Window.off + (of_int consumed_in_window)) >>= function
              | Error err -> Lwt.return_error (Mapper_error err)
              | Ok (window, relative_offset) ->
                go window relative_offset writed_in_htmp hunks result (Pack.refill 0 0 state)
            end
          | `Hunk (state, Hunk.Insert raw) ->
            let len = Cstruct.len raw in

            let insert = match htmp with
             | Some htmp ->
               Cstruct.blit raw 0 htmp writed_in_htmp len;
               Diff.C (Cstruct.sub htmp writed_in_htmp len)
             | None ->
               Diff.S (Cstruct.to_string raw) in

            go window consumed_in_window
              (writed_in_htmp + len)
              (Diff.Insert insert :: hunks)
              result (Pack.continue state)
          | `Hunk (state, Hunk.Copy (off, len)) ->
            go window consumed_in_window
              writed_in_htmp
              (Diff.Copy (off, len) :: hunks)
              result (Pack.continue state)
          | `Flush _ -> invalid_arg "Object is not a patch"
          | `Object state ->
            let descr = match Pack.kind state with
              | Pack.Hunk descr -> descr
              | _ -> invalid_arg "Object is not a patch" in

            let patch =
              { length = Pack.length state
              ; consumed = Pack.consumed state
              ; offset = Pack.offset state
              ; crc = Pack.crc state
              ; hunks = List.rev hunks
              ; descr } in

            go window consumed_in_window writed_in_htmp hunks
              (Some patch) (Pack.next_object state)
          | `Error (state, exn) ->
            Lwt.return_error (Unpack_error (state, window, exn))
          | `End _ -> match result with
            | Some result -> Lwt.return_ok result
            | None -> assert false in

        go window relative_offset 0 [] None state

    type s =
      { kind : kind
      ; length : int
      ; consumed : int
      ; inserts : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; raw : Cstruct.t
      ; descr : Hunk.hunks }

    let apply_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) (kind, source) rtmp t absolute_offset =
      find_window t absolute_offset >>= function
      | Error err -> Lwt.return_error (Mapper_error err)
      | Ok (window, relative_offset) ->
        let state = Pack.from_window window relative_offset ztmp zwin in

        let rec go window consumed_in_window writed_in_rtmp writed_in_htmp ctx result state =
          match Pack.eval window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then go window
                (consumed_in_window + rest_in_window)
                writed_in_rtmp writed_in_htmp ctx result
                (Pack.refill consumed_in_window rest_in_window state)
            else begin
              find_window t Int64.(window.Window.off + (of_int consumed_in_window)) >>= function
              | Error err -> Lwt.return_error (Mapper_error err)
              | Ok (window, relative_offset) ->
                go window relative_offset writed_in_rtmp writed_in_htmp ctx result (Pack.refill 0 0 state)
            end
          | `Hunk (state, Hunk.Insert raw) ->
            let len = Cstruct.len raw in
            let ctx = match ctx with
              | Some ctx ->
                let ctx = Hash.Digest.feed ctx raw in
                Some ctx
              | None ->
                let len = match Pack.kind state with
                  | Pack.Hunk { Hunk.target_length; _ } -> target_length
                  | _ -> invalid_arg "Object is not a patch" in
                let ctx = Hash.Digest.init () in
                let hdr = Fmt.strf "%s %d\000"
                    (match kind with
                     | `Commit -> "commit"
                     | `Tree -> "tree"
                     | `Tag -> "tag"
                     | `Blob -> "blob")
                    len in
                let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                let ctx = Hash.Digest.feed ctx raw in
                Some ctx in

            Cstruct.blit raw 0 rtmp writed_in_rtmp len;
            go window consumed_in_window
              (writed_in_rtmp + len) (writed_in_htmp + len) ctx result
              (Pack.continue state)
          | `Hunk (state, Hunk.Copy (off, len)) ->
            let ctx = match ctx with
              | Some ctx ->
                let ctx = Hash.Digest.feed ctx (Cstruct.sub source off len) in
                Some ctx
              | None ->
                let len' = match Pack.kind state with
                  | Pack.Hunk { Hunk.target_length; _ } -> target_length
                  | _ -> invalid_arg "Object is not a patch" in
                let ctx = Hash.Digest.init () in
                let hdr = Fmt.strf "%s %d\000"
                    (match kind with
                     | `Commit -> "commit"
                     | `Tree -> "tree"
                     | `Tag -> "tag"
                     | `Blob -> "blob")
                    len' in
                let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                let ctx = Hash.Digest.feed ctx (Cstruct.sub source off len) in
                Some ctx in

            Cstruct.blit source off rtmp writed_in_rtmp len;
            go window consumed_in_window
              (writed_in_rtmp + len) writed_in_htmp ctx result
              (Pack.continue state)
          | `Flush _ -> invalid_arg "Object is not a patch"
          | `Object state ->
            let descr = match Pack.kind state with
              | Pack.Hunk descr -> descr
              | _ -> invalid_arg "Object is not a patch" in
            let ctx = match ctx with
              | Some ctx -> ctx
              | None ->
                let len = match Pack.kind state with
                  | Pack.Hunk { Hunk.target_length; _ } -> target_length
                  | _ -> invalid_arg "Object is not a patch" in
                let ctx = Hash.Digest.init () in
                let hdr = Fmt.strf "%s %d\000"
                    (match kind with
                     | `Commit -> "commit"
                     | `Tree -> "tree"
                     | `Tag -> "tag"
                     | `Blob -> "blob")
                    len in
                let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                ctx in

            let patch =
              { kind
              ; length = Pack.length state
              ; consumed = Pack.consumed state
              ; inserts = writed_in_htmp
              ; offset = Pack.offset state
              ; crc = Pack.crc state
              ; hash = Hash.Digest.get ctx
              ; raw = Cstruct.sub rtmp 0 writed_in_rtmp
              ; descr } in

            go window consumed_in_window writed_in_rtmp writed_in_htmp None (Some patch) (Pack.next_object state)
          | `Error (state, exn) ->
            Lwt.return_error (Unpack_error (state, window, exn))
          | `End _ -> match result with
            | Some patch -> Lwt.return_ok patch
            | None -> assert false in

        go window relative_offset 0 0 None None state
  end

  module Base =
  struct
    type t =
      { kind : kind
      ; length : int
      ; consumed : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; raw : Cstruct.t }

    let get_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) rtmp t absolute_offset =
      find_window t absolute_offset >>= function
      | Error err -> Lwt.return_error (Mapper_error err)
      | Ok (window, relative_offset) ->
        let state = Pack.from_window window relative_offset ztmp zwin in

        let rec go window consumed_in_window writed_in_rtmp ctx result state =
          match Pack.eval window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then go window
                (consumed_in_window + rest_in_window)
                writed_in_rtmp ctx result
                (Pack.refill consumed_in_window rest_in_window state)
            else begin
              find_window t Int64.(window.Window.off + (of_int consumed_in_window)) >>= function
              | Error err -> Lwt.return_error (Mapper_error err)
              | Ok (window, relative_offset) ->
                go window relative_offset writed_in_rtmp ctx result (Pack.refill 0 0 state)
            end
          | `Hunk _ -> invalid_arg "Object is not a base"
          | `Flush state ->
            let buf, len = Pack.output state in

            let ctx = match ctx with
              | Some ctx ->
                let ctx = Hash.Digest.feed ctx (Cstruct.sub buf 0 len) in
                Some ctx
              | None ->
                let ctx = Hash.Digest.init () in
                let hdr = Fmt.strf "%s %d\000"
                    (match Pack.kind state with
                     | Pack.Commit -> "commit"
                     | Pack.Tag -> "tag"
                     | Pack.Tree -> "tree"
                     | Pack.Blob -> "blob"
                     | _ -> invalid_arg "Object is not a base")
                    (Pack.length state) in
                let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                let ctx = Hash.Digest.feed ctx (Cstruct.sub buf 0 len) in
                Some ctx in

            Cstruct.blit buf 0 rtmp writed_in_rtmp len;
            go window consumed_in_window (writed_in_rtmp + len) ctx result (Pack.flush 0 (Cstruct.len buf) state)
          | `Object state ->
            let base =
              { kind = (match Pack.kind state with
                    | Pack.Commit -> `Commit
                    | Pack.Tree -> `Tree
                    | Pack.Tag -> `Tag
                    | Pack.Blob -> `Blob
                    | _ -> invalid_arg "Object is not a base")
              ; length = Pack.length state
              ; consumed = Pack.consumed state
              ; offset = Pack.offset state
              ; crc = Pack.crc state
              ; hash = (match ctx with
                    | Some ctx -> Hash.Digest.get ctx
                    | None ->
                      let ctx = Hash.Digest.init () in
                      let hdr = Fmt.strf "%s %d\000"
                          (match Pack.kind state with
                           | Pack.Commit -> "commit"
                           | Pack.Tag -> "tag"
                           | Pack.Tree -> "tree"
                           | Pack.Blob -> "blob"
                           | _ -> invalid_arg "Object is not a base")
                          (Pack.length state) in
                      let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                      Hash.Digest.get ctx)
              ; raw = Cstruct.sub rtmp 0 (Pack.length state) } in
            go window consumed_in_window writed_in_rtmp None (Some base) (Pack.next_object state)
          | `Error (state, exn) ->
            Lwt.return_error (Unpack_error (state, window, exn))
          | `End _ -> match result with
            | Some base -> Lwt.return_ok base
            | None -> assert false in

        go window relative_offset 0 None None state
  end

  module Object =
  struct
    type t =
      | Patch of Patch.t
      | Root of Base.t

    let k2k = function
      | Pack.Commit -> `Commit
      | Pack.Tree -> `Tree
      | Pack.Tag -> `Tag
      | Pack.Blob -> `Blob
      | _ -> invalid_arg "k2k"

    let get_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) ?htmp rtmp t absolute_offset =
      find_window t absolute_offset >>= function
      | Error err -> Lwt.return_error (Mapper_error err)
      | Ok (window, relative_offset) ->
        let state = Pack.from_window window relative_offset ztmp zwin in

        let rec go window consumed_in_window writed_in_rtmp writed_in_htmp ctx hunks result state =
          match Pack.eval window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then go window
                (consumed_in_window + rest_in_window)
                writed_in_rtmp writed_in_htmp ctx hunks result
                (Pack.refill consumed_in_window rest_in_window state)
            else begin
              find_window t Int64.(window.Window.off + (of_int consumed_in_window)) >>= function
              | Error err -> Lwt.return_error (Mapper_error err)
              | Ok (window, relative_offset) ->
                go window relative_offset writed_in_rtmp writed_in_htmp ctx hunks result (Pack.refill 0 0 state)
            end
          | `Hunk (state, Hunk.Insert raw) ->
            let len = Cstruct.len raw in

            let insert = match htmp with
              | Some htmp ->
                Cstruct.blit raw 0 htmp writed_in_htmp len;
                Diff.C (Cstruct.sub htmp writed_in_htmp len)
              | None ->
                Diff.S (Cstruct.to_string raw) in

            go window consumed_in_window
              writed_in_rtmp
              (writed_in_htmp + len)
              ctx (Diff.Insert insert :: hunks) result
              (Pack.continue state)
          | `Hunk (state, Hunk.Copy (off, len)) ->
            go window consumed_in_window
              writed_in_rtmp writed_in_htmp
              ctx (Diff.Copy (off, len) :: hunks) result
              (Pack.continue state)
          | `Flush state ->
            let buf, len = Pack.output state in

            let ctx = match ctx with
              | Some ctx ->
                let ctx = Hash.Digest.feed ctx (Cstruct.sub buf 0 len) in
                Some ctx
              | None ->
                let ctx = Hash.Digest.init () in
                let hdr = Fmt.strf "%s %d\000"
                    (match Pack.kind state with
                     | Pack.Commit -> "commit"
                     | Pack.Tag -> "tag"
                     | Pack.Tree -> "tree"
                     | Pack.Blob -> "blob"
                     | _ -> assert false)
                    (Pack.length state) in
                let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                let ctx = Hash.Digest.feed ctx (Cstruct.sub buf 0 len) in
                Some ctx in

            Cstruct.blit buf 0 rtmp writed_in_rtmp len;
            go window consumed_in_window
              (writed_in_rtmp + len) writed_in_htmp
              ctx hunks result
              (Pack.flush 0 (Cstruct.len buf) state)
          | `Object state ->
            let obj = match Pack.kind state with
              | (Pack.Commit | Pack.Tag | Pack.Tree | Pack.Blob) as kind ->
                Root { Base.kind = k2k kind
                     ; length = Pack.length state
                     ; consumed = Pack.consumed state
                     ; offset = Pack.offset state
                     ; crc = Pack.crc state
                     ; hash = (match ctx with
                           | Some ctx -> Hash.Digest.get ctx
                           | None ->
                             let ctx = Hash.Digest.init () in
                             let hdr = Fmt.strf "%s %d\000"
                                 (match Pack.kind state with
                                  | Pack.Commit -> "commit"
                                  | Pack.Tag -> "tag"
                                  | Pack.Tree -> "tree"
                                  | Pack.Blob -> "blob"
                                  | _ -> assert false)
                                 (Pack.length state) in
                             let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
                             Hash.Digest.get ctx)
                     ; raw = Cstruct.sub rtmp 0 (Pack.length state) }
              | Pack.Hunk descr ->
                Patch { Patch.length = Pack.length state
                      ; consumed = Pack.consumed state
                      ; offset = Pack.offset state
                      ; crc = Pack.crc state
                      ; hunks = List.rev hunks
                      ; descr } in

            go window consumed_in_window
              writed_in_rtmp  writed_in_htmp
              None [] (Some obj)
              (Pack.next_object state)
          | `Error (state, exn) ->
            Lwt.return_error (Unpack_error (state, window, exn))
          | `End _ -> match result with
            | Some obj -> Lwt.return_ok obj
            | None -> assert false in

        go window relative_offset 0 0 None [] None state
  end

  module Cache =
  struct
    type ('key, 'value) t =
      { promote : 'key -> 'value -> unit
      ; find : 'key -> 'value option }

    let lru
      : type key value.
        int ->
        (module Hashtbl.HashedType with type t = key) ->
        (module Lru.Weighted with type t = value) ->
        (key, value) t
      = fun weight (module H) (module W) ->
      let module Lru = Lru.M.Make(H)(W) in
      let cache = Lru.create weight in

      { promote = (fun key value -> Lru.add  key value cache)
      ; find = (fun key -> Lru.find key cache) }
  end

  let memoize promote find process =
    let memoized x = match find x with
      | Some v -> Lwt.return v
      | None -> process x >>= fun y -> promote x y; Lwt.return y in
    memoized

  let memoize_rec ~promote ~find process_no_rec =
    let process = ref (fun _ -> assert false) in
    let process_rec = memoize promote find (fun x -> process_no_rec !process x) in
    process := process_rec; process_rec

  module Ascendant =
  struct
    type t =
      | External of { hash : Hash.t; kind : kind; raw : Cstruct.t; }
      | Root of Base.t
      | Node of { patch : Patch.t; source : t }

    type metadata =
      { length : int
      ; crc : Crc32.t
      ; offset : int64
      ; consumed : int }

    type s =
      [ `Patch of metadata
      | `Base of metadata
      | `Extern ]

    let metadata_of_base base =
      { length = base.Base.length
      ; crc = base.Base.crc
      ; offset = base.Base.offset
      ; consumed = base.Base.consumed }

    let metadata_of_patch : Patch.t -> metadata = fun patch ->
      { length = patch.Patch.length
      ; crc = patch.Patch.crc
      ; offset = patch.Patch.offset
      ; consumed = patch.Patch.consumed }

    let needed_cache weight =
      let module Int64 = struct include Int64 let hash n = Hashtbl.hash n end in
      let module Int = struct type t = int let weight _ = 1 end in
      Cache.lru weight (module Int64) (module Int)

    let get_cache weight =
      let module Int64 = struct include Int64 let hash n = Hashtbl.hash n end in
      let module Path = struct type nonrec t = t let weight _ = 1 end in
      Cache.lru weight (module Int64) (module Path)

    let apply_cache weight =
      let module Int64 = struct include Int64 let hash n = Hashtbl.hash n end in
      let module Object = struct type t = kind * Cstruct.t * int * s let weight (_, raw, _, _) = Cstruct.len raw end in
      Cache.lru weight (module Int64) (module Object)

    let get_from_absolute_offset ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t absolute_offset =
      let get_free_htmp htmp depth = match htmp with
        | Some htmp ->
          if depth < Array.length htmp
          then Some (Array.get htmp depth)
          else None
        | None -> None in

      let go mu (absolute_offset, depth) =
        Object.get_from_absolute_offset ~ztmp ~zwin ~chunk ?htmp:(get_free_htmp htmp depth) rtmp t absolute_offset >>= function
        | Error err -> Lwt.return_error err
        | Ok (Object.Root base) -> Lwt.return_ok (Root base)
        | Ok (Object.Patch ({ Patch.descr = { Hunk.reference = Hunk.Offset rel_off; _ }; _ } as patch)) ->
          (mu (Int64.(sub absolute_offset rel_off), succ depth) >>= function
            | Ok source -> Lwt.return_ok (Node { patch; source; })
            | Error err -> Lwt.return_error err)
        | Ok (Object.Patch ({ Patch.descr = { Hunk.reference = Hunk.Hash hash; _ }; _ } as patch)) ->
          match t.idx hash with
          | Some (_, abs_off) ->
            (mu (abs_off, succ depth) >>= function
              | Ok source -> Lwt.return_ok (Node { patch; source; })
              | Error err -> Lwt.return_error err)
          | None -> t.get hash >>= function
            | Some (kind, raw) -> Lwt.return_ok (Node { patch; source = External { hash; kind; raw; }; })
            | None -> Lwt.return_error (Invalid_hash hash) in

      let promote (abs_off, _) = function
        | Ok node -> cache.Cache.promote abs_off node
        | Error _ -> () in

      let find (abs_off, _) = match cache.Cache.find abs_off with
        | Some node -> Some (Ok node)
        | None -> None in

      let memoized_go = memoize_rec ~promote ~find go in
      memoized_go (absolute_offset, 0)

    let get_from_hash ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t hash =
      match t.idx hash with
      | Some (_, abs_off) -> get_from_absolute_offset ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t abs_off
      | None -> Lwt.return_error (Invalid_hash hash)

    let get ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t = function
      | `Hash hash -> get_from_hash ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t hash
      | `Offset absolute_offset -> get_from_absolute_offset ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t absolute_offset

    let reconstruct rtmp =
      let get_free_rtmp (rtmp0, rtmp1) = function true -> rtmp0 | false -> rtmp1 in

      let apply source patch target =
        let target_length =
          List.fold_left (fun target_off -> function
              | Diff.Copy (off, len) ->
                Cstruct.blit source off target target_off len; target_off + len
              | Diff.Insert insert ->
                Diff.blit_to_cstruct insert 0 target target_off (Diff.len insert);
                target_off + (Diff.len insert))
            0 patch.Patch.hunks in

        if target_length <> patch.Patch.descr.Hunk.target_length
        then invalid_arg "apply";

        Cstruct.sub target 0 patch.Patch.descr.Hunk.target_length in

      let rec go swap depth = function
        | External { kind; raw; _ } -> (kind, raw, depth, `Extern)
        | Root ({ Base.kind; Base.raw; _ } as base) -> (kind, raw, depth, `Base (metadata_of_base base))
        | Node { patch; source; } ->
          let (kind, raw, deepen, _) = go (not swap) (succ depth) source in
          (kind, apply raw patch (get_free_rtmp rtmp swap), deepen, `Patch (metadata_of_patch patch)) in

      go true 0

    let apply_from_absolute_offset ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t absolute_offset =
      let get_free_rtmp (rtmp0, rtmp1) = function true -> rtmp0 | false -> rtmp1 in
      let get_free_htmp htmp depth = match htmp with
        | Some htmp ->
          if depth < Array.length htmp
          then Some (Array.get htmp depth)
          else None
        | None -> None in

      let apply source patch target =
        let target_length =
          List.fold_left (fun target_off -> function
              | Diff.Copy (off, len) ->
                Cstruct.blit source off target target_off len;
                target_off + len
              | Diff.Insert insert ->
                Diff.blit_to_cstruct insert 0 target target_off (Diff.len insert);
                target_off + (Diff.len insert))
            0 patch.Patch.hunks in

        if target_length <> patch.Patch.descr.Hunk.target_length
        then invalid_arg "apply";

        Cstruct.sub target 0 patch.Patch.descr.Hunk.target_length in

      let go mu (absolute_offset, switch, depth) =
        Object.get_from_absolute_offset ~ztmp ~zwin ~chunk ?htmp:(get_free_htmp htmp depth) (get_free_rtmp rtmp switch) t absolute_offset >>= function
        | Error err -> Lwt.return_error err
        | Ok (Object.Root base) ->
          Lwt.return_ok (base.Base.kind, base.Base.raw,
                         depth, `Base (metadata_of_base base))
        | Ok (Object.Patch ({ Patch.descr = { Hunk.reference = Hunk.Offset rel_off; _ }; _ } as patch)) ->
          (mu (Int64.(sub absolute_offset rel_off), (not switch), (succ depth)) >>= function
            | Ok (kind, source, deepen, _) ->
              Lwt.return_ok (kind, apply source patch (get_free_rtmp rtmp switch),
                             deepen, `Patch (metadata_of_patch patch))
            | Error err -> Lwt.return_error err)
        | Ok (Object.Patch ({ Patch.descr = { Hunk.reference = Hunk.Hash hash; _ }; _ } as patch)) ->
          match t.idx hash with
          | Some (_, abs_off) ->
            (mu (abs_off, (not switch), (succ depth)) >>= function
              | Ok (kind, source, deepen, _) ->
                Lwt.return_ok (kind, apply source patch (get_free_rtmp rtmp switch),
                               deepen, `Patch (metadata_of_patch patch))
              | Error err -> Lwt.return_error err)
          | None -> t.get hash >>= function
            | Some (kind, raw) -> Lwt.return_ok (kind, apply raw patch (get_free_rtmp rtmp switch), depth, `Patch (metadata_of_patch patch))
            | None -> Lwt.return_error (Invalid_hash hash) in

      let find (abs_off, _, _) = match cache.Cache.find abs_off with
        | Some (kind, raw, depth, metadata) -> Some (Ok (kind, raw, depth, metadata))
        | None -> None in

      let promote (abs_off, _, _) = function
        | Ok (kind, raw, depth, metadata) -> cache.Cache.promote abs_off (kind, raw, depth, metadata)
        | Error _ -> () in

      let memoized_go = memoize_rec ~promote ~find go in
      memoized_go (absolute_offset, true, 0)

    let apply_from_hash ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t hash =
      match t.idx hash with
      | Some (_, abs_off) -> apply_from_absolute_offset ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t abs_off
      | None -> Lwt.return_error (Invalid_hash hash)

    let apply ~ztmp ~zwin ~cache ?(chunk = 0x8000) ?htmp rtmp t = function
      | `Hash hash -> apply_from_hash ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t hash
      | `Offset absolute_offset -> apply_from_absolute_offset ~ztmp ~zwin ~cache ~chunk ?htmp rtmp t absolute_offset

    let length_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) t absolute_offset =
      find_window t absolute_offset >>= function
      | Error err -> Lwt.return_error (Mapper_error err)
      | Ok (window, relative_offset) ->
        let state = Pack.process_length window relative_offset ztmp zwin in

        let rec go window consumed_in_window state =
          match Pack.eval_length window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then go window (consumed_in_window + rest_in_window) (Pack.refill consumed_in_window rest_in_window state)
            else begin
              find_window t Int64.(window.Window.off + (of_int consumed_in_window)) >>= function
              | Error err -> Lwt.return_error (Mapper_error err)
              | Ok (window, relative_offset) ->
                go window relative_offset (Pack.refill 0 0 state)
            end
          | `Flush state -> Lwt.return_ok (`Base (Pack.length state))
          | `Error (state, exn) -> Lwt.return_error (Unpack_error (state, window, exn))
          | `End _ -> assert false
          | `Length state -> match Pack.kind state with
            | Pack.Hunk ({ Hunk.target_length; source_length; _ } as descr) -> Lwt.return_ok (`Patch (descr, target_length, source_length))
            | _ -> Lwt.return_ok (`Base (Pack.length state)) in

        go window relative_offset state

    let needed_from_absolute_offset ~ztmp ~zwin ~cache ?(chunk = 0x8000) t absolute_offset =
      let go mu (needed, absolute_offset) =
        length_from_absolute_offset ~ztmp ~zwin ~chunk t absolute_offset >>= function
        | Error err -> Lwt.return_error err
        | Ok (`Patch ({ Hunk.reference = Offset rel_off; _ }, target_length, source_length)) ->
          mu (max (max target_length source_length) needed, Int64.(sub absolute_offset rel_off))
        | Ok (`Base length) -> Lwt.return_ok (max length needed)
        | Ok (`Patch ({ Hunk.reference = Hash hash; _ }, target_length, source_length)) ->
          match t.idx hash with
          | Some (_, abs_off) -> mu (max (max target_length source_length) needed, abs_off)
          | None ->
            t.get hash >>= function
            | Some (_, raw) -> Lwt.return_ok (max (Cstruct.len raw) (max (max target_length source_length) needed))
            | None -> Lwt.return_error (Invalid_hash hash) in

      let find (_, abs_off) = match cache.Cache.find abs_off with
        | Some needed -> (Some (Ok needed))
        | None -> None in

      let promote (_, abs_off) = function
        | Ok needed -> cache.Cache.promote abs_off needed
        | Error _ -> () in

      let memoized_go = memoize_rec ~find ~promote go in
      memoized_go (0, absolute_offset)

    let needed_from_hash ~ztmp ~zwin ~cache ?(chunk = 0x8000) t hash =
      match t.idx hash with
      | Some (_, abs_off) -> needed_from_absolute_offset ~ztmp ~zwin ~cache ~chunk t abs_off
      | None -> Lwt.return_error (Invalid_hash hash)

    let needed ~ztmp ~zwin ~cache ?(chunk = 0x8000) t = function
      | `Hash hash -> needed_from_hash ~ztmp ~zwin ~cache ~chunk t hash
      | `Offset abs_off -> needed_from_absolute_offset ~ztmp ~zwin ~cache ~chunk t abs_off

    let length_from_absolute_offset ~ztmp ~zwin ?(chunk = 0x8000) t absolute_offset =
      length_from_absolute_offset ~ztmp ~zwin ~chunk t absolute_offset >>= function
      | Ok (`Patch (_, target_length, _)) -> Lwt.return_ok target_length
      | Ok (`Base length) -> Lwt.return_ok length
      | Error err -> Lwt.return_error err

    let length_from_hash ~ztmp ~zwin ?(chunk = 0x8000) t hash =
      match t.idx hash with
      | Some (_, abs_off) -> length_from_absolute_offset ~ztmp ~zwin ~chunk t abs_off
      | None -> Lwt.return_error (Invalid_hash hash)

    let length ~ztmp ~zwin ?(chunk = 0x8000) t = function
      | `Hash hash -> length_from_hash ~ztmp ~zwin ~chunk t hash
      | `Offset abs_off -> length_from_absolute_offset ~ztmp ~zwin ~chunk t abs_off
  end

  module Descendant =
  struct
    type base =
      { kind : kind
      ; length : int
      ; consumed : int
      ; offset : int64
      ; hash : Hash.t
      ; crc : Crc32.t }

    let of_base base =
      { kind = base.Base.kind
      ; length = base.Base.length
      ; consumed = base.Base.consumed
      ; offset = base.Base.offset
      ; hash = base.Base.hash
      ; crc = base.Base.crc }

    type patch =
      { length : int
      ; consumed : int
      ; inserts : int
      ; offset : int64
      ; crc : Crc32.t
      ; hash : Hash.t
      ; descr : Hunk.hunks }

    let of_patch patch =
      { length = patch.Patch.length
      ; consumed = patch.Patch.consumed
      ; inserts = patch.Patch.inserts
      ; offset = patch.Patch.offset
      ; crc = patch.Patch.crc
      ; hash = patch.Patch.hash
      ; descr = patch.Patch.descr }

    type t =
      | Root of { base : base
                ; children : node list }
    and node =
      | Node of { patch : patch
                ; children : node list }
      | Leaf of patch

    let get_from_absolute_offset ~ztmp ~zwin ~cache:(cache_needed, cache_object) ?(chunk = 0x8000) ~children rtmp t absolute_offset =
      Base.get_from_absolute_offset ~ztmp ~zwin ~chunk rtmp t absolute_offset >>= function
      | Error err -> Lwt.return_error err
      | Ok base ->
        let load_base absolute_offset =
          match cache_object.Cache.find absolute_offset with
          | Some (kind, raw, _, _) -> Lwt.return_ok (kind, raw)
          | None ->
            Ascendant.needed_from_absolute_offset ~ztmp ~zwin ~cache:cache_needed ~chunk t absolute_offset >>= function
            | Error err -> Lwt.return_error err
            | Ok needed ->
              let rtmp = Cstruct.create (needed * 2) in
              let rtmp = Cstruct.sub rtmp 0 needed, Cstruct.sub rtmp needed needed in

              Ascendant.apply_from_absolute_offset ~ztmp ~zwin ~cache:cache_object ~chunk rtmp t absolute_offset >>= function
              | Ok (kind, raw, _, _) -> Lwt.return_ok (kind, raw)
              | Error err -> Lwt.return_error err in

        let load (parent_absolute_offset, absolute_offset) =
          load_base parent_absolute_offset >>= function
          | Error err -> Lwt.return_error err
          | Ok (parent_kind, parent_raw) ->
            Ascendant.length_from_absolute_offset ~ztmp ~zwin ~chunk t absolute_offset >>= function
            | Error err -> Lwt.return_error err
            | Ok length ->
              let target = Cstruct.create length in
              Patch.apply_from_absolute_offset ~ztmp ~zwin ~chunk (parent_kind, parent_raw) target t absolute_offset >>= function
              | Error err -> Lwt.return_error err
              | Ok s ->
                let metadata =
                  { Ascendant.length = s.Patch.length
                  ; crc = s.Patch.crc
                  ; consumed = s.Patch.consumed
                  ; offset = s.Patch.offset } in
                cache_object.Cache.promote absolute_offset (s.Patch.kind, s.Patch.raw, 0, `Patch metadata);
                Lwt.return_ok s in

        let rec go acc = function
          | [] -> Lwt.return_ok acc
          | x :: r ->
            load x >>= function
            | Error err -> Lwt.return_error err
            | Ok patch ->
              let children' = children (patch.Patch.offset, patch.Patch.hash)
                              |> List.map (fun abs_off -> patch.Patch.offset, abs_off) in

              go [] children' >>= function
              | Error err -> Lwt.return_error err
              | Ok [] -> go (Leaf (of_patch patch) :: acc) r
              | Ok children' ->
                go (Node { patch = of_patch patch
                         ; children = children' } :: acc) r in

        go [] (children (base.Base.offset, base.Base.hash)
               |> List.map (fun abs_off -> base.Base.offset, abs_off))
        >>= function
        | Ok children ->
          Lwt.return_ok (Root { base = of_base base
                              ; children })
        | Error err -> Lwt.return_error err
  end

  let idx { idx; _ } = idx
  let extern { get; _ } = get

  let update_idx idx t = { t with idx; }
  let update_extern get t = { t with get; }
end

module Stream
    (Hash: S.HASH)
    (Inflate: S.INFLATE)
= struct
  module Hunk = Hunk(Hash)
  module Pack = Pack(Hash)(Inflate)(Hunk)

  include Pack
end

module Random_access
    (Hash: S.HASH)
    (Mapper: S.MAPPER)
    (Inflate: S.INFLATE)
= struct
  module Hunk = Hunk(Hash)
  module Pack = Pack(Hash)(Inflate)(Hunk)
  module Decoder = Decoder(Hash)(Mapper)(Inflate)(Hunk)(Pack)

  include Decoder
end
