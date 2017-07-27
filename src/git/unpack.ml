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

module type HASH =
sig
  type t = Bytes.t

  val pp        : t Fmt.t
  val length    : int
  val of_string : string -> t
end

module type H =
sig
  module Hash : HASH

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

  val partial_hunks : t -> hunks

  val pp_reference : reference Fmt.t
  val pp_hunks : hunks Fmt.t
  val pp : t Fmt.t

  val eval : Cstruct.t -> t -> [ `Hunk of t * hunk | `Await of t | `Error of t * error | `Ok of t * hunks ]

  val default : int -> reference -> t
  val refill : int -> int -> t -> t
  val continue : t -> t

  val current : t -> hunk

  val used_in : t -> int
  val available_in : t -> int
  val read : t -> int
end

(* Implementation of deserialization of a list of hunks (from a PACK file) *)
module MakeHunkDecoder (Hash : HASH) : H with module Hash = Hash  =
struct
  module Hash = Hash

  type error =
    | Reserved_opcode of int
    | Wrong_copy_hunk of int * int * int

  let pp_error ppf = function
    | Reserved_opcode byte ->
      Fmt.pf ppf "(Reserved_opcode %02x)" byte
    | Wrong_copy_hunk (off, len, source) ->
      Fmt.pf ppf "(Wrong_copy_hunk (i@[<hov>off: %d,@ len: %d,@ source: %d@]))"
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
    | Header k ->
      Fmt.string ppf "(Header #k)"
    | Stop ->
      Fmt.string ppf "Stop"
    | List k ->
      Fmt.string ppf "(List #k)"
    | Is_insert (raw, off, len) ->
      Fmt.pf ppf "(Is_insert (#raw, %d, %d))" off len
    | Is_copy k ->
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

  let await t     = Wait t
  let error t exn = Error ({ t with state = Exception exn }, exn)
  let ok t        = Ok ({ t with state = End },
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
     @@ fun l2 src t ->

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

  let stop src t = Cont t

  let list src t =
    if t.read < t._length
    then KList.get_byte
        (fun opcode  src t ->
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
    @@ fun _target_length src t ->
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
              ; _hunk = Some hunk } as t) -> `Hunk (t, hunk)
      | Cont ({ state = Stop }) -> assert false
      | Cont t -> loop t
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
    else raise (Invalid_argument (Fmt.strf "HunkDecoder.refill: you lost something"))

  let continue t =
    match t.state with
    | Stop -> { t with state = List list }
    | _ -> raise (Invalid_argument "HunkDecoder.continue: bad state")

  let current t = match t with
    | { state = Stop
      ; _hunk = Some hunk } -> hunk
    | _ -> raise (Invalid_argument "HunkDecoder.current: bad state")

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

module type P =
sig
  module Hash    : HASH
  module Inflate : Common.INFLATE
  module H       : H with module Hash = Hash

  type error =
    | Invalid_byte of int
    | Reserved_kind of int
    | Invalid_kind of int
    | Inflate_error of Inflate.error
    | Hunk_error of H.error
    | Hunk_input of int * int
    | Invalid_length of int * int

  val pp_error : error Fmt.t

  type t

  val pp : t Fmt.t

  type kind =
    | Commit
    | Tree
    | Blob
    | Tag
    | Hunk of H.hunks

  val default : Cstruct.t -> Inflate.window -> t
  val from_window : Window.t -> int -> Cstruct.t -> Inflate.window -> t
  val process_length : Window.t -> int -> Cstruct.t -> Inflate.window -> t
  val process_metadata : Window.t -> int -> Cstruct.t -> Inflate.window -> t

  val refill : int -> int -> t -> t
  val flush : int -> int -> t -> t
  val next_object : t -> t
  val continue : t -> t

  val kind : t -> kind
  val length : t -> int
  val offset : t -> int64
  val consumed : t -> int
  val crc : t -> Crc32.t
  val output : t -> Cstruct.t * int

  val eval : Cstruct.t -> t -> [ `Object of t | `Hunk of t * H.hunk | `Await of t | `Flush of t | `End of t * Hash.t | `Error of t * error ]
  val eval_length : Cstruct.t -> t -> [ `Length of t | `Await of t | `Flush of t | `End of (t * Hash.t) | `Error of (t * error) ]
  val eval_metadata : Cstruct.t -> t -> [ `Metadata of t | `Await of t | `Flush of t | `End of (t * Hash.t) | `Error of (t * error) ]
end

(* Implementatioon of deserialization of a PACK file *)
module MakePACKDecoder (H : HASH) (Inflate : Common.INFLATE)
  : P with module Hash    = H
       and module Inflate = Inflate
       and module H       = MakeHunkDecoder(H)
= struct
  module Hash    = H
  module Inflate = Inflate
  module H       = MakeHunkDecoder(H)

  type error =
    | Invalid_byte of int
    | Reserved_kind of int
    | Invalid_kind of int
    | Inflate_error of Inflate.error
    | Hunk_error of H.error
    | Hunk_input of int * int
    | Invalid_length of int * int

  let pp_error ppf = function
    | Invalid_byte byte              -> Fmt.pf ppf "(Invalid_byte %02x)" byte
    | Reserved_kind byte             -> Fmt.pf ppf "(Reserved_byte %02x)" byte
    | Invalid_kind byte              -> Fmt.pf ppf "(Invalid_kind %02x)" byte
    | Inflate_error err              -> Fmt.pf ppf "(Inflate_error %a)" (Fmt.hvbox Inflate.pp_error) err
    | Invalid_length (expected, has) -> Fmt.pf ppf "(Invalid_length (%d <> %d))" expected has
    | Hunk_error err                 -> Fmt.pf ppf "(Hunk_error %a)" H.pp_error err
    | Hunk_input (expected, has)     -> Fmt.pf ppf "(Hunk_input (%d <> %d))" expected has

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
                   (* XXX(dinosaure): [length] is the value decoded. [length']
                      is the value returned when we inflate the raw. It must to
                      be the same. However, we can inflate a huge object (like a
                      [Blob] which is not delta-ified).

                                      The length of the object can be upper than
                      [max_native_int] (but can't be upper than [max_int64]). So
                      we need to switch these values to [int64]. However, the
                      [Inflate] algorithm provide only a [native_int]. We can
                      bypass this limit and count the length of the object by
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
    ; h        : H.t }
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
    | Hunk of H.hunks

  let pp_kind ppf = function
    | Commit     -> Fmt.pf ppf "Commit"
    | Tree       -> Fmt.pf ppf "Tree"
    | Blob       -> Fmt.pf ppf "Blob"
    | Tag        -> Fmt.pf ppf "Tag"
    | Hunk hunks -> Fmt.pf ppf "(Hunks %a)" (Fmt.hvbox H.pp_hunks) hunks

  let pp_hunks_state ppf { offset; length; consumed; z; h; } =
    Fmt.pf ppf "{ @[<hov>offset = %Ld;@ \
                         consumed = %d;@ \
                         length = %d;@ \
                         z = %a;@ \
                         h = %a;@] })"
      offset consumed length
      (Fmt.hvbox Inflate.pp) z (Fmt.hvbox H.pp) h

  let pp_state ppf = function
    | Header _ ->
      Fmt.string ppf "(Header #k)"
    | Object k ->
      Fmt.string ppf "(Object #k)"
    | VariableLength k ->
      Fmt.string ppf "(VariableLength #k)"
    | Unzip { offset
            ; consumed
            ; length
            ; kind
            ; z } ->
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
          ; length' } ->
      Fmt.pf ppf "(Next { @[<hov>offset = %Ld;@ \
                                 consumed = %d;@ \
                                 length = %d;@ \
                                 length' = %d;@] })"
        offset consumed length length'
    | Checksum k ->
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
  let await t      = Wait t
  let flush t      = Flush t
  let error t exn  = Error ({ t with state = Exception exn }, exn)
  let ok t hash    = Ok ({ t with state = End hash }, hash)

  let rec get_byte ~ctor k src t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
        k byte src { t with i_pos = t.i_pos + 1
                          ; read = Int64.add t.read 1L }
    else await { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) }

  let get_hash ~ctor k src t =
    let buf = Buffer.create Hash.length in

    let rec loop i src t =
      if i = Hash.length
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

  let stop_hunks src t hs =
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
    match H.eval t.o_z h with
    | `Await h ->
      (match Inflate.eval src t.o_z z with
       | `Await z ->
         let consumed = consumed + Inflate.used_in z in
         let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in

         await { t with state = Hunks { offset; length; consumed; crc; z = Inflate.refill 0 0 z; h; }
                      ; i_pos = t.i_pos + Inflate.used_in z
                      ; read  = Int64.add t.read (Int64.of_int (Inflate.used_in z)) }
       | `Flush z ->
         let h = H.refill 0 (Inflate.used_out z) h in
         let z = Inflate.flush 0 (Cstruct.len t.o_z) z in

         Cont { t with state = Hunks { offset; length; consumed; crc; z; h; } }
       | `End z ->
         (* XXX(dinosaure): In [zlib] and [decompress], it could be happen to
            return [`End] and consume a part of the input [t.o_z].

            So, we compute the CRC-32 checksum and update [consumed]. *)
         let consumed = consumed + Inflate.used_in z in
         let crc = Crc32.digest ~off:(t.i_off + t.i_pos) ~len:(Inflate.used_in z) crc src in
         let h = H.refill 0 (Inflate.used_out z) h in

         Cont { t with state = Hunks { offset; length; consumed; crc; z = Inflate.refill 0 0 z; h; }
                     ; i_pos = t.i_pos + Inflate.used_in z
                     ; read  = Int64.(add t.read (of_int (Inflate.used_in z))) }
       | `Error (z, exn) -> error { t with i_pos = t.i_pos + Inflate.used_in z
                                         ; read  = Int64.(add t.read (of_int (Inflate.used_in z))) } (Inflate_error exn))
    | `Hunk (h, hunk) ->
      Cont { t with state = StopHunks { offset; length; consumed; crc; z; h; } }
    | `Error (h, exn) -> error t (Hunk_error exn)
    | `Ok (h, hunks) ->
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
            (fun offset crc src t ->
               Cont { t with state = Hunks { offset   = off
                                           ; length   = len
                                           ; consumed = size_of_variable_length len
                                                        + size_of_offset offset
                                           ; crc
                                           ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                               @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                               @@ Inflate.default (Inflate.window_reset t.o_w)
                                           ; h = H.default len (H.Offset offset) } })
            src t)
        src t
    | 0b111 ->
      KObject.get_hash
        (fun hash src t ->
          let crc = Crc32.digests crc hash in

          Cont { t with state = Hunks { offset   = off
                                      ; length   = len
                                      ; consumed = Hash.length + size_of_variable_length len
                                      ; crc
                                      ; z = Inflate.flush 0 (Cstruct.len t.o_z)
                                            @@ Inflate.refill (t.i_off + t.i_pos) (t.i_len - t.i_pos)
                                            @@ Inflate.default (Inflate.window_reset t.o_w)
                                      ; h = H.default len (H.Hash hash) } })
        src t
    | _  -> error t (Invalid_kind typ)

  (* TODO: check the hash procuded and the hash noticed. *)
  let checksum src t =
    KChecksum.get_hash
      (fun hash src t -> ok t hash)
      src t

  let kind src t =
    let offset = t.read in

    KObject.get_byte
      (fun byte src t ->
        let msb = byte land 0x80 <> 0 in
        let typ = (byte land 0x70) lsr 4 in
        let crc = Crc32.digestc Crc32.default byte in
        length msb (byte land 0x0F, 4) crc (switch typ offset) src t)
      src t

  let unzip src t offset length consumed crc kind z =
    match Inflate.eval src t.o_z z with
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

  let next src t length length' kind =
    if length = length'
    then Cont t
    else error t (Invalid_length (length, length'))

  let number src t =
    KHeader.get_u32
      (fun objects src t ->
        Cont { t with objects = objects
                    ; counter = objects
                    ; state = Object kind })
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

  let is_end { state; _ } = match state with End _ -> true | _ -> false

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
        | _ -> { t with i_off = off
                      ; i_len = len
                      ; i_pos = 0 }
    else if is_end t
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
      (* XXX(dinosaure): at the end, we don't care if we lost something. *)
    else raise (Invalid_argument (Fmt.strf "PACKDecoder.refill: you lost something \
                                            (pos: %d, len: %d)" t.i_pos t.i_len))

  let flush off len t = match t.state with
    | Unzip { offset; length; consumed; crc; kind; z } ->
      { t with state = Unzip { offset
                             ; length
                             ; consumed
                             ; crc
                             ; kind
                             ; z = Inflate.flush off len z } }
    | _ -> raise (Invalid_argument "PACKDecoder.flush: bad state")

  let output t = match t.state with
    | Unzip { z; _ } ->
      t.o_z, Inflate.used_out z
    | _ -> raise (Invalid_argument "PACKDecoder.output: bad state")

  let next_object t =
    match t.state with
    | Next _ when t.process = `All ->
      if Int32.pred t.counter = 0l
      then { t with state = Checksum checksum
                  ; counter = Int32.pred t.counter }
      else { t with state = Object kind
                  ; counter = Int32.pred t.counter }
    | Next _ -> { t with state = End (Hash.of_string (String.make Hash.length '\000')) }
      (* XXX(dinosaure): in the local case, the user don't care about the hash of the PACK file. *)
    | _ -> raise (Invalid_argument "PACKDecoder.next_object: bad state")

  let kind t = match t.state with
    | Unzip { kind; _ } -> kind
    | StopHunks { h; _ }
    | Hunks { h; _ } -> Hunk (H.partial_hunks h)
    | Next { kind; _ } -> kind
    | _ -> raise (Invalid_argument "PACKDecoder.kind: bad state")

  let length t = match t.state with
    | Unzip { length; _ } -> length
    | StopHunks { length; _ } -> length
    | Hunks { length; _ } -> length
    | Next { length; _ } -> length
    | _ -> raise (Invalid_argument "PACKDecoder.length: bad state")

  (* XXX(dinosaure): The consumed value calculated in this deserialization is
     different from what git says (a diff of 1 or 2 bytes) - may be it come from
     a wrong compute of the length of the offset value (see {!size_of_offset}).
     It's not very important but FIXME! *)
  let consumed t = match t.state with
    | Next { consumed; _ } -> consumed
    | _ -> raise (Invalid_argument "PACKDecoder.consumed: bad state")

  let offset t = match t.state with
    | Unzip { offset; _ } -> offset
    | StopHunks { offset; _ } -> offset
    | Hunks { offset; _ } -> offset
    | Next { offset; _ } -> offset
    | _ -> raise (Invalid_argument "PACKDecoder.offset: bad state")

  let crc t = match t.state with
    | Next { crc; _ } -> crc
    | _ -> raise (Invalid_argument "PACKDecoder.crc: bad state")

  let continue t =
    match t.state with
    | StopHunks hs ->
      { t with state = Hunks { hs with h = H.continue hs.h } }
    | _ -> raise (Invalid_argument "PACKDecoder.continue: bad state")

  let eval0 src t =
    match t.state with
    | Header k -> k src t
    | Object k -> k src t
    | VariableLength k -> k src t
    | Unzip { offset; length; consumed; crc; kind; z; } ->
      unzip src t offset length consumed crc kind z
    | Hunks { offset; length; consumed; crc; z; h; } ->
      hunks src t offset length consumed crc z h
    | StopHunks hs -> stop_hunks src t hs
    | Next { length; length'; kind; } -> next src t length length' kind
    | Checksum k -> k src t
    | End hash -> ok t hash
    | Exception exn -> error t exn

  let eval src t =
    let rec loop t =
      match eval0 src t with
      | Cont ({ state = Next _ } as t) ->
        `Object t
      | Cont ({ state = StopHunks hs } as t) ->
        `Hunk (t, H.current hs.h)
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  let eval_length src t =
    let rec loop t =
      match eval0 src t with
      | Cont (({ state = Next _ } | { state = Unzip _ } | { state = Hunks { h = { H.state = H.List _ } } }) as t) -> `Length t
      | Cont ({ state = StopHunks hs } as t) -> loop (continue t)
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
      match eval0 src t with
      | Cont (({ state = Next _ } | { state = Unzip _ } | { state = Hunks _ }) as t) -> `Metadata t
      | Cont ({ state = StopHunks hs } as t) -> loop (continue t)
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok (t, hash) -> `End (t, hash)
      | Error (t, exn) -> `Error (t, exn)
    in

    assert (t.process = `Metadata);
    loop t
end

module type DECODER =
sig
  module Hash    : HASH
  module Mapper  : Fs.MAPPER
  module Inflate : Common.INFLATE

  module H : H with module Hash = Hash
  module P : P with module Hash = Hash
                and module Inflate = Inflate
                and module H = H
  type error =
    | Invalid_hash of Hash.t
    | Invalid_offset of int64
    | Invalid_target of (int * int)
    | Unpack_error of P.t * Window.t * P.error
    | Mapper_error of Mapper.error

  val pp_error : error Fmt.t

  type t

  type kind = [ `Commit | `Blob | `Tree | `Tag ]

  module Object :
  sig
    type from =
      | Offset of { length   : int
                  ; consumed : int
                  ; offset   : int64
                  ; crc      : Crc32.t
                  ; base     : from
                  ; }
      | External of Hash.t
      | Direct of { consumed : int
                  ; offset   : int64
                  ; crc      : Crc32.t
                  ; }
    and t =
      { kind   : kind
      ; raw    : Cstruct.t
      ; length : int64
      ; from   : from
      ; }

    val pp : t Fmt.t

    val first_crc_exn : t -> Crc32.t
  end

  val find : t -> int64 -> ((Window.t * int), Mapper.error) result Lwt.t

  val make : ?bucket:int -> Mapper.fd
    -> (Hash.t -> Object.t option)
    -> (Hash.t -> (Crc32.t * int64) option)
    -> (int64 -> Hash.t option)
    -> (Hash.t -> (kind * Cstruct.t) option Lwt.t)
    -> (t, Mapper.error) result Lwt.t

  val idx : t -> (Hash.t -> (Crc32.t * int64) option)
  val cache : t -> (Hash.t -> Object.t option)
  val revidx : t -> (int64 -> Hash.t option)
  val extern : t -> (Hash.t -> (kind * Cstruct.t) option Lwt.t)

  val update_idx    : (Hash.t -> (Crc32.t * int64) option) -> t -> t
  val update_cache  : (Hash.t -> Object.t option) -> t -> t
  val update_revidx : (int64 -> Hash.t option) -> t -> t
  val update_extern : (Hash.t -> (kind * Cstruct.t) option Lwt.t) -> t -> t

  val length : ?chunk:int -> t -> Hash.t -> Cstruct.t -> Inflate.window -> (int, error) result Lwt.t
  val needed : ?chunk:int -> ?cache:(Hash.t -> int option) -> t -> Hash.t -> Cstruct.t -> Inflate.window -> (int, error) result Lwt.t
  val optimized_get' : ?chunk:int -> ?limit:bool -> ?h_tmp:Cstruct.t array -> t -> int64 -> (Cstruct.t * Cstruct.t * int) -> Cstruct.t -> Inflate.window -> (Object.t, error) result Lwt.t
  val optimized_get : ?chunk:int -> ?limit:bool -> ?h_tmp:Cstruct.t array -> t -> Hash.t -> (Cstruct.t * Cstruct.t * int) -> Cstruct.t -> Inflate.window -> (Object.t, error) result Lwt.t
  val get' : ?chunk:int -> t -> int64 -> Cstruct.t -> Inflate.window -> (Cstruct.t * Cstruct.t) -> (Object.t, error) result Lwt.t
  val get : ?chunk:int -> t -> Hash.t -> Cstruct.t -> Inflate.window -> (Cstruct.t * Cstruct.t) -> (Object.t, error) result Lwt.t
  val get_with_allocation : ?chunk:int -> ?h_tmp:Cstruct.t array -> t -> Hash.t -> Cstruct.t -> Inflate.window -> (Object.t, error) result Lwt.t
  val get_with_allocation' : ?chunk:int -> ?h_tmp:Cstruct.t array -> t -> int64 -> Cstruct.t -> Inflate.window -> (Object.t, error) result Lwt.t
end

module MakeDecoder (Hash : HASH) (Mapper : Fs.MAPPER with type raw = Cstruct.t) (Inflate : Common.INFLATE)
  : DECODER with type Hash.t = Hash.t
             and module Hash = Hash
             and module Mapper = Mapper
             and module Inflate = Inflate =
struct
  module Hash    = Hash
  module Mapper  = Mapper
  module Inflate = Inflate

  module P       = MakePACKDecoder(Hash)(Inflate)
  module H       = MakeHunkDecoder(Hash)

  type error =
    | Invalid_hash of Hash.t
    | Invalid_offset of int64
    | Invalid_target of (int * int)
    | Unpack_error of P.t * Window.t * P.error
    | Mapper_error of Mapper.error

  let pp_error ppf = function
    | Invalid_hash hash ->
      Fmt.pf ppf "(Invalid_hash %a)" Hash.pp hash
    | Invalid_offset off ->
      Fmt.pf ppf "(Invalid_offset %Ld)" off
    | Invalid_target (has, expected) ->
      Fmt.pf ppf "(Invalid_target (%d, %d))"
        has expected
    | Unpack_error (state, window, exn) ->
      Fmt.pf ppf "(Unpack_error { @[<hov>state = %a;@ \
                                         window = %a;@ \
                                         exn = %a;@] })"
        (Fmt.hvbox P.pp) state
        (Fmt.hvbox Window.pp) window
        (Fmt.hvbox P.pp_error) exn
    | Mapper_error err ->
      Fmt.pf ppf "(Mapper_error %a)" (Fmt.hvbox Mapper.pp_error) err

  type kind = [ `Commit | `Blob | `Tree | `Tag ]

  let pp_kind ppf = function
    | `Commit -> Fmt.pf ppf "Commit"
    | `Blob -> Fmt.pf ppf "Blob"
    | `Tree -> Fmt.pf ppf "Tree"
    | `Tag -> Fmt.pf ppf "Tag"

  type partial =
    { _length : int
    ; _consumed : int
    ; _offset : int64
    ; _crc : Crc32.t
    ; _hunks : hunk list }
  and hunk =
    | Copy of (int * int)
    | Insert of Cstruct.t

  module Object =
  struct
    type from =
      | Offset of { length   : int
                  ; consumed : int
                  ; offset   : int64 (* absolute offset *)
                  ; crc      : Crc32.t
                  ; base     : from }
      | External of Hash.t
      | Direct of { consumed : int
                  ; offset   : int64
                  ; crc      : Crc32.t }

    and t =
      { kind     : kind
      ; raw      : Cstruct.t
      ; length   : int64
      ; from     : from }

    let to_partial = function
      | { length; from = Offset { consumed; crc; offset; _ } }
      | { length; from = Direct { consumed; crc; offset; } } ->
        { _length = Int64.to_int length
        ; _consumed = consumed
        ; _offset = offset
        ; _crc = crc
        ; _hunks = [] }
      | _ -> raise (Invalid_argument "Object.to_partial: this object is external of the current PACK file")


    let rec pp_from ppf = function
      | Offset { length; consumed; offset; crc; base; } ->
        Fmt.pf ppf "(Hunk { @[<hov>length = %d;@ \
                                   consumed = %d;@ \
                                   offset = %Lx;@ \
                                   crc = %a;@ \
                                   base = %a;@] })"
          length consumed offset
          Crc32.pp crc
          (Fmt.hvbox pp_from) base
      | External hash ->
        Fmt.pf ppf "(External %a)" Hash.pp hash
      | Direct { consumed; offset; crc; } ->
        Fmt.pf ppf "(Direct { @[<hov>consumed = %d;@ \
                                     offset = %Lx;@ \
                                     crc = %a;@] })"
          consumed offset Crc32.pp crc

    let pp ppf t =
      Fmt.pf ppf "{ @[<hov>kind = %a;@ \
                           raw = #raw;@ \
                           length = %Ld;@ \
                           from = %a;@] }"
        pp_kind t.kind t.length (Fmt.hvbox pp_from) t.from

    let first_crc_exn t =
      match t.from with
      | Direct { crc; _ } -> crc
      | Offset { crc; _ } -> crc
      | External _ -> raise (Invalid_argument "Object.first_crc")
  end

  type pack_object =
    | Hunks  of partial * H.hunks
    | Object of kind * partial * Cstruct.t
    | External of Hash.t * kind * Cstruct.t

  type t =
    { file  : Mapper.fd
    ; max   : int64
    ; win   : Window.t Bucket.t
    ; cache : Hash.t -> Object.t option
    ; idx   : Hash.t -> (Crc32.t * int64) option
    ; rev   : int64 -> Hash.t option
    ; get   : Hash.t -> (kind * Cstruct.t) option Lwt.t
    ; hash  : Hash.t }

  let to_kind = function
    | P.Commit -> `Commit
    | P.Blob -> `Blob
    | P.Tree -> `Tree
    | P.Tag -> `Tag
    | _ -> assert false

  let map_window t offset_requested =
    let open Lwt.Infix in
    let pos = Int64.((offset_requested / 1024L) * 1024L) in (* padding *)
    Mapper.map t.file ~pos ~share:false (1024 * 1024)
    >>= function
    | Ok map ->
      Lwt.return (Ok { Window.raw = map
                 ; off   = pos
                 ; len   = Cstruct.len map })
    | Error err -> Lwt.return (Error err)

  let find t offset_requested =
    let predicate window = Window.inside offset_requested window in

    match Bucket.find t.win predicate with
    | Some window ->
      let relative_offset = Int64.to_int Int64.(offset_requested - window.Window.off) in
      Lwt.return (Ok (window, relative_offset))
    | None ->
      let open Lwt.Infix in
      map_window t offset_requested
      >>= function
      | Ok window ->
        let relative_offset = Int64.to_int Int64.(offset_requested - window.Window.off) in
        let () = Bucket.add t.win window in
        Lwt.return (Ok (window, relative_offset))
      | Error err -> Lwt.return (Error err)

  let apply partial_hunks hunks_header hunks base raw =
    if Cstruct.len raw < hunks_header.H.target_length
    then raise (Invalid_argument "Decoder.apply");

    let target_length = List.fold_left
      (fun acc -> function
        | Insert insert ->
          Cstruct.blit insert 0 raw acc (Cstruct.len insert); acc + Cstruct.len insert
        | Copy (off, len) ->
          Cstruct.blit base.Object.raw off raw acc len; acc + len)
      0 hunks
      in

      if (target_length = hunks_header.H.target_length)
      then Ok Object.{ kind     = base.Object.kind
                   ; raw      = Cstruct.sub raw 0 target_length
                   ; length   = Int64.of_int hunks_header.H.target_length
                   ; from     = Offset { length   = partial_hunks._length
                                       ; consumed = partial_hunks._consumed
                                       ; offset   = partial_hunks._offset
                                       ; crc      = partial_hunks._crc
                                       ; base     = base.from } }
      else Error (Invalid_target (target_length, hunks_header.H.target_length))

  let result_bind ~err f = function Ok a -> f a | Error exn -> err

  let get_pack_object ?(chunk = 0x8000) ?(limit = false) ?h_tmp t reference source_length source_offset z_tmp z_win r_tmp =
    if Cstruct.len r_tmp < source_length && not limit
    then raise (Invalid_argument (Fmt.strf "Decoder.delta: expect %d and have %d" source_length (Cstruct.len r_tmp)));

    let open Lwt.Infix in

    let aux = function
      | Error exn -> Lwt.return (Error exn)
      | Ok absolute_offset ->
        find t absolute_offset
        >>= function
        | Error err -> Lwt.return (Error (Mapper_error err))
        | Ok (window, relative_offset) ->
        let state    = P.from_window window relative_offset z_tmp z_win in

        let rec loop window consumed_in_window writed_in_raw writed_in_hnk hunks git_object state =
          match P.eval window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then
              loop window
                (consumed_in_window + rest_in_window) writed_in_raw writed_in_hnk
                hunks git_object
                (P.refill consumed_in_window rest_in_window state)
            else
              (find t Int64.(window.Window.off + (of_int consumed_in_window))
               >>= function
               | Error err -> Lwt.return (Error (Mapper_error err))
               | Ok (window, relative_offset) ->
                 (* XXX(dinosaure): we try to find a new window to compute the rest of the current object. *)
                 loop window
                   relative_offset writed_in_raw writed_in_hnk
                   hunks git_object
                   (P.refill 0 0 state))
          | `Hunk (state, hunk) ->
            (match h_tmp, hunk with
             | Some hnk, P.H.Insert raw ->
               let len = Cstruct.len raw in
               Cstruct.blit raw 0 hnk writed_in_hnk len;
               loop window
                 consumed_in_window writed_in_raw (writed_in_hnk + len)
                 (Insert (Cstruct.sub hnk writed_in_hnk len) :: hunks) git_object
                 (P.continue state)
             | None, P.H.Insert raw ->
               let len = Cstruct.len raw in
               let res = Cstruct.create len in
               Cstruct.blit raw 0 res 0 len;
               loop window
                 consumed_in_window writed_in_raw writed_in_hnk
                 (Insert res :: hunks) git_object
                 (P.continue state)
             | _, P.H.Copy (off, len) ->
               loop window
                 consumed_in_window writed_in_raw writed_in_hnk
                 (Copy (off, len) :: hunks) git_object
                 (P.continue state))
          | `Flush state ->
            let o, n = P.output state in
            let n' = min n (Cstruct.len r_tmp - writed_in_raw) in
            Cstruct.blit o 0 r_tmp writed_in_raw n';

            if n' > 0
            then
              loop window
                consumed_in_window (writed_in_raw + n) writed_in_hnk
                hunks git_object
                (P.flush 0 n state)
            else Lwt.return (Ok (P.kind state,
                                 { _length   = P.length state
                                 ; _consumed = 0
                                 ; _offset   = P.offset state
                                 ; _crc      = Crc32.default
                                 ; _hunks    = [] }))
          (* XXX(dinosaure): to be clear, this situation appear only when we
             have a [limit = true] and the git object is bigger than 0x10000FFFE
             bytes - so, not a common case. In this case, we are interested only
             by the raw data (and we don't care about the meta-data) to compute
             the undelta-ification.

             When [r_tmp] is full, we returns partial information because, we
             only need the raw-data to construct in a top of this call the git
             object requested (necessarily different that this current git
             object). *)

          | `Object state ->
            loop window
              consumed_in_window writed_in_raw writed_in_hnk
              hunks
              (Some (P.kind state,
                    { _length   = P.length state
                    ; _consumed = P.consumed state
                    ; _offset   = P.offset state
                    ; _crc      = P.crc state
                    ; _hunks    = List.rev hunks }))
              (P.next_object state)
          | `Error (state, exn) ->
            Lwt.return (Error (Unpack_error (state, window, exn)))
          | `End (state, _) ->
            match git_object with
            | Some (kind, partial) ->
              Lwt.return (Ok (kind, partial))
            | None -> assert false
            (* XXX: This is not possible, the [`End] state comes only after the
               [`Object] state and this state changes [kind] to [Some x]. *)
        in

        loop window relative_offset 0 0 [] None state >>= function
        | Ok (P.Hunk hunks, partial) ->
          Lwt.return (Ok (Hunks (partial, hunks)))
        | Ok (kind, partial) ->
          let r_tmp =
            if (not limit) || (partial._length < 0x10000FFFE && limit)
            then Cstruct.sub r_tmp 0 partial._length
            else r_tmp
          in

          Lwt.return (Ok (Object (to_kind kind, partial, r_tmp)))
        | Error exn -> Lwt.return (Error exn)
    in

    match reference with
    | H.Offset off ->
      let absolute_offset =
        if off < t.max && off >= 0L
        then Ok (Int64.sub source_offset off)
        (* XXX(dinosaure): git has an invariant, [source_offset > off]. That
           means, the source referenced is only in the past from the current
           object. git did a topological sort to produce a PACK file to ensure
           all sources are before all targets. *)
        else Error (Invalid_offset off)
      in (match result_bind ~err:None t.rev absolute_offset with
          | None -> aux absolute_offset
          | Some hash -> match t.cache hash with
            | Some base ->
              Lwt.return (Ok (Object (base.Object.kind, Object.to_partial base, base.Object.raw)))
            | None -> aux absolute_offset)
    | H.Hash hash ->
      match t.cache hash with
      | Some base ->
        Lwt.return (Ok (Object (base.Object.kind, Object.to_partial base, base.Object.raw)))
      | None -> match t.idx hash with
        | Some (crc, absolute_offset) ->
          let absolute_offset =
            if absolute_offset < t.max && absolute_offset >= 0L
            then Ok absolute_offset
            else Error (Invalid_hash hash)
          in
          aux absolute_offset
        | None ->
          let open Lwt.Infix in

          (* XXX(dinosaure): in this case, we can have an allocation. A good
             idea is to send [r_tmp] to [t.get] and keep the control about the
             memory consumption. TODO!

             In real case, we can not determine what is needed to get this
             external object. But, if [limit = true], that means we don't care
             about the object and want only the raw data. In this case, and only
             in this case, it's probably better to use [r_tmp]. *)
          t.get hash >>= function
          | Some (kind, raw) -> Lwt.return (Ok (External (hash, kind, raw)))
          | None -> Lwt.return (Error (Invalid_hash hash))

  let make ?(bucket = 10) file cache idx rev get' =
    let open Lwt.Infix in

    Mapper.length file
    >>= function
    | Ok max ->
      Lwt.return (Ok { file
                     ; max
                     ; win  = Bucket.make bucket
                     ; cache
                     ; idx
                     ; rev
                     ; get = get' (* XXX(dinosaure): clash of name with [Lwt.geŧ]. *)
                     ; hash = (Hash.of_string (String.make Hash.length '\000')) (* TODO *) })
    | Error err -> Lwt.return (Error err)

  let idx { idx; _ } = idx
  let cache { cache; _ } = cache
  let revidx { rev; _ } = rev
  let extern { get; _ } = get

  let update_idx idx t = { t with idx; }
  let update_cache cache t = { t with cache; }
  let update_revidx rev t = { t with rev; }
  let update_extern get t = { t with get; }

  let length ?(chunk = 0x8000) t hash z_tmp z_win =
    let open Lwt.Infix in

    let get absolute_offset =
      find t absolute_offset
      >>= function
      | Error err -> Lwt.return (Error (Mapper_error err))
      | Ok (window, relative_offset) ->
        let state = P.process_length window relative_offset z_tmp z_win in

        let rec loop window consumed_in_window state =
          match P.eval_length window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then
              loop window
                (consumed_in_window + rest_in_window)
                (P.refill consumed_in_window rest_in_window state)
            else
              find t Int64.(window.Window.off + (of_int consumed_in_window))
              >>= (function
                  | Error err -> Lwt.return (Error (Mapper_error err))
                  | Ok (window, relative_offset) ->
                    loop window
                      relative_offset
                      (P.refill 0 0 state))
          | `Error (state, exn) -> Lwt.return (Error (Unpack_error (state, window, exn)))
          | `End (state, _) -> assert false
          | `Flush state
          | `Length state ->
            match P.kind state with
            | P.Hunk hunks ->
              Lwt.return (Ok hunks.H.target_length)
            | _ -> Lwt.return (Ok (P.length state))
        in

        loop window relative_offset state
    in

    match t.idx hash with
    | Some (_, off) -> get off
    | None -> Lwt.return (Error (Invalid_hash hash))

  (* XXX(dinosaure): this function returns the max length needed to undelta-ify
     a PACK object. *)
  let needed' ?(chunk = 0x8000) ?(cache = (fun _ -> None)) t value z_tmp z_win =
    let open Lwt.Infix in

    let get absolute_offset =
      find t absolute_offset
      >>= function
      | Error err -> Lwt.return (`Error (Mapper_error err))
      | Ok (window, relative_offset) ->
        let state = P.process_length window relative_offset z_tmp z_win in

        let rec loop window consumed_in_window state =
          match P.eval_length window.Window.raw state with
          | `Await state ->
            let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

            if rest_in_window > 0
            then
              loop window
                (consumed_in_window + rest_in_window)
                (P.refill consumed_in_window rest_in_window state)
            else
              find t Int64.(window.Window.off + (of_int consumed_in_window))
              >>= (function
                  | Error err -> Lwt.return (`Error (Mapper_error err))
                  | Ok (window, relative_offset) ->
                    loop window
                      relative_offset
                      (P.refill 0 0 state))
          | `Flush state ->
            Lwt.return (`Direct (P.length state))
          | `Error (state, exn) -> Lwt.return (`Error (Unpack_error (state, window, exn)))
          | `End (state, _) -> assert false
          | `Length state ->
            match P.kind state with
            | P.Hunk ({ H.reference = H.Offset off; _ } as hunks) ->
              Lwt.return (`IndirectOff (Int64.sub (P.offset state) off, max (P.length state) @@ max hunks.H.target_length hunks.H.source_length))
            | P.Hunk ({ H.reference = H.Hash hash; _ } as hunks) ->
              Lwt.return (`IndirectHash (hash, max (P.length state) @@ max hunks.H.target_length hunks.H.source_length))
            | _ -> Lwt.return (`Direct (P.length state))
      in

      loop window relative_offset state
    in

    let rec loop length = function
      | `IndirectHash (hash, length') ->
        (match cache hash with
         | Some length'' -> Lwt.return (Ok (max length (max length' length'')))
         | None -> (match t.idx hash with
             | Some (_, off) -> (get off >>= loop (max length length'))
             | None -> Lwt.return (Error (Invalid_hash hash))))
      | `IndirectOff (absolute_offset, length') ->
        (get absolute_offset
         >>= loop (max length length'))
      | `Direct length' ->
        Lwt.return (Ok (max length length'))
      | `Error exn -> Lwt.return (Error exn)
    in

    loop 0 value

  let needed ?(chunk = 0x8000) ?(cache = (fun _ -> None)) t hash z_tmp z_win =
    needed' ~chunk ~cache t (`IndirectHash (hash, 0)) z_tmp z_win

  (* XXX(dinosaure): Need an explanation. This function does not allocate any
     [Cstruct.t]. The purpose of this function is to get a git object from a
     PACK file (represented by [t]). The user requests the git object by the
     [hash].

     Then, to get the git object, we need 4 buffers.

     - One to store the inflated PACK object
     - The Window used to inflate the PACK object
     - Two buffer to undelta-ified the PACK object

     We can have 2 two cases in this function:

     - We get directly the git object (so, we just need to inflate the PACK
     object)

     - We get a {!H.hunks} object. In this case, we need to undelta-ify the
     object

     So, we use 2 [Cstruct.t] and swap themselves for each undelta-ification.
     Then, we return a {!Object.t} git object and return the true [Cstruct.t].

     However, to be clear, this function allocates some buffers but in same way
     as [git]. To read a PACK file, we need to allocate a buffer which contains
     the data of the PACK file. It's the purpose of the {!MAPPER} module.

     So, we [mmap] a part of the PACK file (a part of [1024 * 1024] bytes) which
     contains the offset of the PACK object requested - it's a {!Window.t}.
     Then, we compute the deserialization of the PACK object (note: sometimes,
     the {!Window.t} is not sufficient to deserialize the PACK object requested,
     so we allocate a new {!Window.t} which contains the rest of the PACK
     object).

     Finally, we limit the number of {!Window.t} available by 10 (value by
     default) and limit the allocation. Hopefully, we amortized the allocation
     because, for one {!Window.t}, we can compute some PACK objects.

     Another point is about the [limit] argument. Sometimes we want an object
     only to construction by the undelta-ification an other git object. In this
     case, we need only 0x10000FFFE bytes of this object (according to the limit
     of the offset described by the PACK format). So, we notice to this function
     than we want to store the git object in a limited (fixed) size buffer and
     if the object if upper than 0x10000FFFE bytes, we discard the rest.

     For this call, we don't case about the meta-data of the object requested
     (where it come from, the CRC-32 checksum, etc.) and just want the raw data.
  *)
  let optimized_get' ?(chunk = 0x8000) ?(limit = false) ?h_tmp t absolute_offset (raw0, raw1, length) z_tmp z_win =
    let get_free_raw = function
      | true -> raw0
      | false -> raw1
    in

    let open Lwt.Infix in

    find t absolute_offset >>= function
    | Error err -> Lwt.return (Error (Mapper_error err))
    | Ok (window, relative_offset) ->
      let state  = P.from_window window relative_offset z_tmp z_win in

      let rec loop window consumed_in_window writed_in_raw writed_in_hnk hunks swap git_object state =
        match P.eval window.Window.raw state with
        | `Await state ->
          let rest_in_window = min (window.Window.len - consumed_in_window) chunk in

          if rest_in_window > 0
          then
            loop window
              (consumed_in_window + rest_in_window) writed_in_raw writed_in_hnk
              hunks swap git_object
              (P.refill consumed_in_window rest_in_window state)
          else
            find t Int64.(window.Window.off + (of_int consumed_in_window))
            >>= (function
                | Error err -> Lwt.return (Error (Mapper_error err))
                | Ok (window, relative_offset) ->
                  loop window
                    relative_offset writed_in_raw writed_in_hnk
                    hunks swap git_object
                    (P.refill 0 0 state))
      | `Flush state ->
        let o, n = P.output state in
        let n' = min (Cstruct.len (get_free_raw swap) - writed_in_raw) n in

        Cstruct.blit o 0 (get_free_raw swap) writed_in_raw n';

        if n' > 0
        then
          loop window
            consumed_in_window (writed_in_raw + n) writed_in_hnk
            hunks swap git_object
            (P.flush 0 (Cstruct.len o) state)
        else Lwt.return (Ok (Object.{ kind = to_kind (P.kind state)
                                    ; raw  = get_free_raw swap
                                    ; length = Int64.of_int (P.length state)
                                    ; from   = Direct { consumed = 0
                                                      ; offset   = P.offset state
                                                      ; crc      = Crc32.default }}))
      | `Hunk (state, hunk) ->
        (match h_tmp, hunk with
         | Some hnks, P.H.Insert raw ->
           let len = Cstruct.len raw in
           Cstruct.blit raw 0 hnks.(0)  writed_in_hnk len;
           loop window
             consumed_in_window writed_in_raw (writed_in_hnk + len)
             (Insert (Cstruct.sub hnks.(0) writed_in_hnk len) :: hunks)
             swap git_object (P.continue state)
         | None, P.H.Insert raw ->
           let len = Cstruct.len raw in
           let res = Cstruct.create len in
           Cstruct.blit raw 0 res 0 len;
           loop window
             consumed_in_window writed_in_raw writed_in_hnk
             (Insert res :: hunks) swap git_object
             (P.continue state)
         | _, P.H.Copy (off, len) ->
           loop window consumed_in_window writed_in_raw writed_in_hnk
             (Copy (off, len) :: hunks) swap git_object
             (P.continue state))
      | `Object state ->
        (match P.kind state with
         | P.Hunk hunks_header ->
           let partial_hunks =
             { _length   = P.length state
             ; _consumed = P.consumed state
             ; _offset   = P.offset state
             ; _crc      = P.crc state
             ; _hunks    = List.rev hunks }
           in

           let rec undelta depth partial hunks swap =
             get_pack_object
               ~chunk
               ?h_tmp:(match h_tmp with Some hnks -> Some hnks.(depth) | None -> None)
               t
               hunks.H.reference hunks.H.source_length partial._offset z_tmp z_win (get_free_raw swap)
             >>= function
             | Error exn -> Lwt.return (Error exn)
             | Ok (Hunks (partial_hunks, hunks)) ->
               (undelta (depth + 1) partial_hunks hunks (not swap) >|= function
                | Ok base ->
                  apply partial_hunks hunks partial_hunks._hunks base (get_free_raw swap)
                | Error exn -> Error exn)
             | Ok (Object (kind, partial, raw)) ->
               Lwt.return (Ok Object.{ kind
                                     ; raw
                                     ; length = Int64.of_int partial._length
                                     ; from   = Direct { consumed = partial._consumed
                                                       ; offset   = partial._offset
                                                       ; crc      = partial._crc } })
             | Ok (External (hash, kind, raw)) ->
               Lwt.return (Ok Object.{ kind
                                     ; raw
                                     ; length = Int64.of_int (Cstruct.len raw)
                                     ; from   = External hash })
           in

           (undelta 1 partial_hunks hunks_header swap >>= function
            | Ok base ->
              (match apply partial_hunks hunks_header partial_hunks._hunks base (get_free_raw (not swap)) with
               | Ok obj ->
                 loop window
                   consumed_in_window writed_in_raw writed_in_hnk
                   hunks swap (Some obj)
                   (P.next_object state)
               | Error exn -> Lwt.return (Error exn))
            | Error exn -> Lwt.return (Error exn))
         | kind ->
           let obj =
             Object.{ kind   = to_kind kind
                    ; raw    =
                      if (not limit) || ((P.length state) < 0x10000FFFE && limit)
                      then Cstruct.sub (get_free_raw swap) 0 (P.length state)
                      else (get_free_raw swap)
                    ; length = Int64.of_int (P.length state)
                    ; from   = Direct { consumed = P.consumed state
                                      ; offset   = P.offset state
                                      ; crc      = P.crc state } }
           in

           loop window
             consumed_in_window writed_in_raw writed_in_hnk
             hunks (not swap) (Some obj)
             (P.next_object state))
      | `Error (state, exn) ->
        Lwt.return (Error (Unpack_error (state, window, exn)))
      | `End (t, _) -> match git_object with
        | Some obj ->
          Lwt.return (Ok obj)
        | None -> assert false
    in

    loop window relative_offset 0 0 [] true None state

  let optimized_get ?(chunk = 0x8000) ?(limit = false) ?h_tmp t hash v_tmp z_tmp z_win =
    match t.idx hash with
    | Some (_, absolute_offset) -> optimized_get' ~chunk ~limit ?h_tmp t absolute_offset v_tmp z_tmp z_win
    | None -> Lwt.return (Error (Invalid_hash hash))

  let get' ?chunk t absolute_offset z_tmp z_win (raw0, raw1) =
    let open Lwt.Infix in
    needed' ?chunk t (`IndirectOff (absolute_offset, 0)) z_tmp z_win >>= function
    | Error exn -> Lwt.return (Error exn)
    | Ok length ->
      if Cstruct.len raw0 <> Cstruct.len raw1
      || Cstruct.len raw0 < length
      || Cstruct.len raw1 < length
      then raise (Invalid_argument "Decoder.get': invalid raws");

      optimized_get' ?chunk t absolute_offset (raw0, raw1, length) z_tmp z_win

  let get ?chunk t hash z_tmp z_win (raw0, raw1) =
    let open Lwt.Infix in
    needed t hash z_tmp z_win >>= function
    | Error exn -> Lwt.return (Error exn)
    | Ok length ->
      if Cstruct.len raw0 <> Cstruct.len raw1
      || Cstruct.len raw0 < length
      || Cstruct.len raw1 < length
      then raise (Invalid_argument "Decoder.get': invalid raws");

      optimized_get ?chunk t hash (raw0, raw1, length) z_tmp z_win

  let get_with_allocation ?chunk ?h_tmp t hash z_tmp z_win =
    let open Lwt.Infix in
    needed t hash z_tmp z_win >>= function
    | Error exn -> Lwt.return (Error exn)
    | Ok length ->
      let tmp = Cstruct.create length, Cstruct.create length, length in

      optimized_get ?chunk ?h_tmp t hash tmp z_tmp z_win

  let get_with_allocation' ?chunk ?h_tmp t absolute_offset z_tmp z_win =
    let open Lwt.Infix in
    needed' t (`IndirectOff (absolute_offset, 0)) z_tmp z_win >>= function
    | Error exn -> Lwt.return (Error exn)
    | Ok length ->
      let tmp = Cstruct.create length, Cstruct.create length, length in

      optimized_get' ?chunk ?h_tmp t absolute_offset tmp z_tmp z_win
end
