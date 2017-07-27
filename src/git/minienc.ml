module Queue =
struct
  type 'a digit =
    | Zero
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a

  type 'a t =
    | Shallow of 'a digit
    | Deep of { s : int
              ; f : 'a digit
              ; m : ('a * 'a) t Lazy.t
              ; r : 'a digit }

  let empty = Shallow Zero

  exception Empty

  let _one x = Shallow (One x)
  let _two x y = Shallow (Two (x, y))
  let _deep s f m r =
    assert (f <> Zero && r <> Zero);
    Deep { s; f; m; r; }

  let is_empty = function
    | Shallow Zero -> true
    | _ -> false

  let _empty = Lazy.from_val empty

  let rec push : 'a. 'a t -> 'a -> 'a t
    = fun q x -> match q with
      | Shallow Zero -> _one x
      | Shallow (One y) -> Shallow (Two (y, x))
      | Shallow (Two (y, z)) -> Shallow (Three (y, z, x))
      | Shallow (Three (y, z, z')) ->
        _deep 4 (Two (y, z)) _empty (Two (z', x))
      | Deep { r = Zero; _ } -> assert false
      | Deep { s; f; m; r = One y; } ->
        _deep (s + 1) f m (Two (y, x))
      | Deep { s; f; m; r = Two (y, z) } ->
        _deep (s + 1) f m (Three (y, z, x))
      | Deep { s; f; m = lazy q'; r = Three (y, z, z') } ->
        _deep (s + 1) f (lazy (push q' (y, z))) (Two (z', x))

  let rec shift : 'a. 'a t -> ('a * 'a t)
    = fun q -> match q with
      | Shallow Zero -> raise Empty
      | Shallow (One x) -> x, empty
      | Shallow (Two (x, y)) -> x, Shallow (One y)
      | Shallow (Three (x, y, z)) -> x, Shallow (Two (y, z))
      | Deep { f = Zero; _ } -> assert false
      | Deep { s; f = One x; m = lazy q'; r; } ->
        if is_empty q'
        then x, Shallow r
        else
          let (y, z), q' = shift q' in
          x, _deep (s - 1) (Two (y, z)) (Lazy.from_val q') r
      | Deep { s; f = Two (x, y); m; r; } ->
        x, _deep (s - 1) (One y) m r
      | Deep { s; f = Three (x, y, z); m; r; } ->
        x, _deep (s - 1) (Two (y, z)) m r

  let rec cons : 'a. 'a t -> 'a -> 'a t
    = fun q x -> match q with
      | Shallow Zero -> Shallow (One x)
      | Shallow (One y) -> Shallow (Two (x, y))
      | Shallow (Two (y, z)) -> Shallow (Three (x, y, z))
      | Shallow (Three (y, z, z')) ->
        _deep 4 (Two (x, y)) _empty (Two (z, z'))
      | Deep { f = Zero; _ } -> assert false
      | Deep { s; f = One y; m; r; } ->
        _deep (s + 1) (Two (x, y)) m r
      | Deep { s; f = Two (y, z); m; r; } ->
        _deep (s + 1) (Three (x, y, z)) m r
      | Deep { s; f = Three (y, z, z'); m = lazy q'; r; } ->
        _deep (s + 1) (Two (x, y)) (lazy (cons q' (z, z'))) r

  let _digit_to_seq d k = match d with
    | Zero -> ()
    | One x -> k x
    | Two (x, y) -> k x; k y
    | Three (x, y, z) -> k x; k y; k z

  type 'a sequence = ('a -> unit) -> unit

  let rec to_seq : 'a. 'a t -> 'a sequence
    = fun q k -> match q with
      | Shallow d -> _digit_to_seq d k
      | Deep { f; m = lazy q'; r; _ } ->
        _digit_to_seq f k;
        to_seq q' (fun (x, y) -> k x; k y);
        _digit_to_seq r k

  let iter f q = to_seq q f

  let _fold_digit f acc d = match d with
    | Zero -> acc
    | One x -> f acc x
    | Two (x, y) -> f (f acc x) y
    | Three (x, y, z) -> f (f (f acc x) y) z

  let rec fold : 'a 'b. ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    = fun func acc q -> match q with
      | Shallow d -> _fold_digit func acc d
      | Deep { f; m = lazy q'; r } ->
        let acc = _fold_digit func acc f in
        let acc = fold (fun acc (x, y) -> func (func acc x) y) acc q' in
        _fold_digit func acc r

  let to_list q =
    let l = ref [] in
    to_seq q (fun x -> l := x :: !l);
    List.rev !l

  let pp ppv ppf q =
    Fmt.pf ppf "[ %a ]"
    (Fmt.hvbox (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf ";@ ") ppv)) (to_list q)
end

module type VALUE =
sig
  type t

  val weight : t -> int
  val pp     : t Fmt.t
end

module RBQ (V : VALUE) =
struct
  type t =
    { c : int
    ; w : int
    ; q :  V.t Queue.t }

  let make capacity =
    { c = capacity
    ; w = 0
    ; q = Queue.empty }

  let pp ppf { c; w; q; } =
    Fmt.pf ppf "{ @[<hov>c = %d;@ \
                         w = %d;@ \
                         q = %a;@] }"
      c w (Fmt.hvbox (Queue.pp V.pp)) q

  let available t =
    t.c - t.w

  let push t v =
    let w = t.w + V.weight v in

    if w > t.c
    then Error t
    else Ok { t with w; q = Queue.push t.q v }

  let shift t = match Queue.shift t.q with
    | (v, q) -> Some v, { t with w = t.w - V.weight v
                               ; q }
    | exception Queue.Empty -> None, t

  let cons t v =
    let w = t.w + V.weight v in

    if w > t.c
    then Error t
    else Ok { t with w; q = Queue.cons t.q v }

  let cons_exn t v =
    { t with w = t.w + V.weight v; q = Queue.cons t.q v }

  let weight t =
    Queue.fold (fun acc x -> acc + V.weight x) 0 t.q

  let to_list t =
    Queue.to_list t.q
end

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let pp_chr =
  Fmt.using
    (function '\032' .. '\126' as x -> x
            | _ -> '.')
    Fmt.char

let pp_scalar : type buffer. get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t
  = fun ~get ~length ppf b ->
  let l = length b in

  for i = 0 to l / 16
  do Fmt.pf ppf "%08x: " (i * 16);
    let j = ref 0 in

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  ";

      if !j mod 2 <> 0 then Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "  ";
    j := 0;

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "@\n"
  done

module RBA =
struct
  type t =
    { r : int
    ; w : int
    ; c : int
    ; b : bigstring }

  let create capacity =
    { r = 0
    ; w = 0
    ; c = capacity
    ; b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout capacity }

  let pp ppf { r; w; c; b; } =
    Fmt.pf ppf
      "{ @[<hov>r = %d;@ \
                w = %d;@ \
                c = %d;@ \
                b = @[<hov>%a@];@] }"
      r w c (pp_scalar ~get:Bigarray.Array1.get ~length:Bigarray.Array1.dim) b

  let mask t v = v land (t.c - 1)
  let empty t = t.r = t.w
  let size t = t.w - t.r
  let available t = t.c - size t
  let full t = size t = t.c

  let push t v =
    assert (not (full t));
    Bigarray.Array1.set t.b (mask t t.w) v;
    { t with w = t.w + 1 }

  let shift t v =
    assert (not (empty t));
    let r = Bigarray.Array1.get t.b (mask t t.r) in
    r, { t with r = t.r + 1 }

  module N =
  struct
    let push t ~blit ~length ?(off = 0) ?len v =
      let len = match len with
        | None -> length v
        | Some len -> len
      in

      assert (available t >= len);

      let pre = t.c - mask t t.w in
      let extra = len - pre in

      let areas =
        if extra > 0
        then begin
          blit v off t.b (mask t t.w) pre;
          blit v (off + pre) t.b 0 extra;
          [ Bigarray.Array1.sub t.b (mask t t.w) pre
          ; Bigarray.Array1.sub t.b 0 extra ]
        end else begin
          blit v off t.b (mask t t.w) len;
          [ Bigarray.Array1.sub t.b (mask t t.w) len ]
        end
      in

      areas, { t with w = t.w + len }

    let keep t ~blit ~length ?(off = 0) ?len v =
      let len = match len with
        | None -> length v
        | Some len -> len
      in

      assert (size t >= len);

      let pre = t.c - mask t t.r in
      let extra = len - pre in

      if extra > 0
      then begin
        blit t.b (mask t t.r) v off pre;
        blit t.b 0 v (off + pre) extra;
      end else
        blit t.b (mask t t.r) v off len

    let shift t len =
      assert (size t >= len);
      { t with r = t.r + len }
  end
end

module Buffer =
struct
  type t =
    [ `Bigstring of bigstring
    | `String of string
    | `Bytes of Bytes.t ]

  let weight = function
    | `Bigstring raw -> Bigarray.Array1.dim raw
    | `String raw -> String.length raw
    | `Bytes raw -> Bytes.length raw

  let ppw_bigstring ppf b =
    let len = Bigarray.Array1.dim b in
    for i = 0 to len - 1
    do Fmt.char ppf (Bigarray.Array1.unsafe_get b i) done

  let ppw ppf = function
    | `Bigstring b -> ppw_bigstring ppf b
    | `String b -> Fmt.string ppf b
    | `Bytes b -> Fmt.string ppf (Bytes.unsafe_to_string b)

  let pp ppf = function
    | `Bigstring b ->
      Fmt.pf ppf "(`Bigstring %a)"
        (Fmt.hvbox @@ pp_scalar ~get:Bigarray.Array1.get ~length:Bigarray.Array1.dim) b
    | `Bytes b ->
      Fmt.pf ppf "(`Bytes %a)"
        (Fmt.hvbox @@ pp_scalar ~get:Bytes.get ~length:Bytes.length) b
    | `String b ->
      Fmt.pf ppf "(`String %a)"
        (Fmt.hvbox @@ pp_scalar ~get:String.get ~length:String.length) b

  let sub buffer off len = match buffer with
    | `Bigstring b -> `Bigstring (Bigarray.Array1.sub b off len)
    | `String b -> `String (String.sub b off len)
    | `Bytes b -> `Bytes (Bytes.sub b off len)
end

module IOVec =
struct
  type t =
    { buffer : Buffer.t
    ; off    : int
    ; len    : int }

  let weight { len; _ } = len

  let make buffer off len =
    { buffer
    ; off
    ; len }

  let length { len; _ } = len

  let lengthv = List.fold_left (fun acc x -> length x + acc) 0

  let shift { buffer; off; len; } n =
    assert (n <= len);

    { buffer
    ; off = off + n
    ; len = len - n }

  let split { buffer; off; len; } n =
    assert (n <= len);

    { buffer = Buffer.sub buffer off n
    ; off = 0
    ; len = n},
    { buffer = Buffer.sub buffer (off + n) (len - n)
    ; off = 0
    ; len = len - n}

  let ppw ppf = function
    | { buffer = `Bigstring b
      ; off
      ; len } -> Buffer.ppw_bigstring ppf (Bigarray.Array1.sub b off len)
    | { buffer = `String b
      ; off
      ; len } -> Fmt.string ppf (String.sub b off len)
    | { buffer = `Bytes b
      ; off
      ; len } -> Fmt.string ppf (Bytes.sub_string b off len)

  let pp ppf { buffer; off; len; } =
    Fmt.pf ppf "{ @[<hov>buffer = %a;@ \
                         off = %d;@ \
                         len = %d:@] }"
      (Fmt.hvbox Buffer.pp) buffer off len
end

module RBS = RBQ(IOVec)

type encoder =
  { sched : RBS.t
  ; write : RBA.t
  ; flush : (int * (unit -> unit)) Queue.t }

let pp ppf { sched; write; _ } =
  Fmt.pf ppf "{ @[<hov>sched = %a;@ \
                       write = %a;@] }"
    (Fmt.hvbox RBS.pp) sched (Fmt.hvbox RBA.pp) write

type 'v state =
  | Flush    of { continue : int -> 'v state
                ; iovecs   : IOVec.t list }
  | Continue of { continue : encoder -> 'v state
                ; encoder  : encoder }
  | End      of 'v
and 'v k = encoder -> 'v

let create len =
  { sched = RBS.make (len * 2)
  ; write = RBA.create len
  ; flush = Queue.empty }

(* XXX(dinosaure): this code checks if a [`Bigstring] [iovec] is a sub of
   [t.write.RBA.b]. *)
let check iovec t = match iovec with
  | { IOVec.buffer = `Bigstring bigstring
    ; off
    ; len } ->
    let stop = Bigarray.Array1.sub t.write.RBA.b (Bigarray.Array1.dim t.write.RBA.b) 0 in

    let sub_ptr : int = Obj.obj @@ Obj.field (Obj.repr bigstring) 1 in
    let raw_ptr : int = Obj.obj @@ Obj.field (Obj.repr t.write.RBA.b) 1 in
    let end_ptr : int = Obj.obj @@ Obj.field (Obj.repr stop) 1 in

    (sub_ptr >= raw_ptr && sub_ptr <= end_ptr)
  | _ -> false

let shift n t =
  let rec aux rest acc t = match RBS.shift t.sched with
    | (Some iovec, shifted) ->
      let len = IOVec.length iovec in
      if rest > len
      then aux (rest - len) (iovec :: acc)
          { t with sched = shifted
                 ; write = if check iovec t
                     then RBA.N.shift t.write len
                     else t.write }
      else if rest > 0
      then let last, rest = IOVec.split iovec rest in
        List.rev (last :: acc),
        { t with sched = RBS.cons_exn shifted rest 
               ; write = if check iovec t
                   then RBA.N.shift t.write (IOVec.length last)
                   else t.write }
      else List.rev acc, t
    | (None, sched) ->
      List.rev acc, { t with sched }
  in

  aux n [] t

let has t =
  RBS.weight t.sched

let drain drain t =
  let rec go rest t = match RBS.shift t.sched with
    | (Some iovec, shifted) ->
      let len = IOVec.length iovec in
      if rest > len
      then go (rest - len) { t with sched = shifted
                                  ; write = if check iovec t
                                      then RBA.N.shift t.write len
                                      else t.write }
      else { t with sched = RBS.cons_exn shifted (IOVec.shift iovec rest)
                  ; write = if check iovec t
                      then RBA.N.shift t.write rest
                      else t.write }
    | (None, sched) ->
      assert (rest = 0);
      { t with sched }
  in

  go drain t

let flush k t =
  let continue n =
    let t = drain n t in
    k t
  in

  Flush { continue
        ; iovecs = RBS.to_list t.sched }

let continue continue encoder =
  Continue { continue
           ; encoder }

let rec schedule k ~length ~buffer ?(off = 0) ?len v t =
  let len = match len with
    | Some len -> len
    | None -> length v - off
  in

  match RBS.push t.sched (IOVec.make (buffer v) off len) with
  | Ok sched ->
    continue k { t with sched }
  | Error _ ->
    let max = RBS.available t.sched in

    let k t = (schedule[@tailcall]) k ~length ~buffer ~off:(off + max) ~len:(len - max) v t in

    schedule (flush k)
      ~length ~buffer ~off ~len:max v t

let schedule_string =
  let length = String.length in
  let buffer x = `String x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bytes =
  let length = Bytes.length in
  let buffer x = `Bytes x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

let schedule_bigstring =
  let length = Bigarray.Array1.dim in
  let buffer x = `Bigstring x in
  fun k t ?(off = 0) ?len v -> schedule k ~length ~buffer ~off ?len v t

external identity : 'a -> 'a = "%identity"

let schedulev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (length, off, len, buffer) :: r ->
      schedule (fun t -> (aux[@tailcall]) t r) ~length ?off ?len ~buffer:identity buffer t
  in aux t l

let rec write k ~blit ~length ?(off = 0) ?len buffer t =
  let len = match len with
    | Some len -> len
    | None -> length buffer - off
  in

  (* XXX(dinosaure): we can factorize the first and the second branch. *)
  if RBA.available t.write >= len
  then begin
    let areas, write = RBA.N.push t.write ~blit ~length ~off ~len buffer in
    schedulev k
      (List.map
         (fun x -> (function `Bigstring x -> Bigarray.Array1.dim x
                           | _ -> assert false),
                   Some 0,
                   None,
                   `Bigstring x) areas)
      { t with write }
  end else if RBA.available t.write > 0
  then begin
    let max = RBA.available t.write in
    let k t = (write[@tailcall]) k ~blit ~length ~off:(off + max) ~len:(len - max) buffer t in
    let areas, write = RBA.N.push t.write ~blit ~length ~off ~len:max buffer in

    schedulev (flush k)
      (List.map
         (fun x -> (function `Bigstring x -> Bigarray.Array1.dim x
                           | _ -> assert false),
                   Some 0,
                   None,
                   `Bigstring x) areas)
      { t with write }
  end else
    let k t = (write[@tailcall]) k ~blit ~length ~off ~len buffer t in
    flush k t

let writev k l t =
  let rec aux t = function
    | [] -> continue k t
    | (blit, length, off, len, buffer) :: r ->
      write (fun t -> (aux[@tailcall]) t r) ~blit ~length ?off ~len buffer t
  in aux t l

let bigarray_blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bigarray.Array1.unsafe_set
      dst (dst_off + i)
      (String.unsafe_get src (src_off + i)) done

let bigarray_blit_from_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bigarray.Array1.unsafe_set
      dst (dst_off + i)
      (Bytes.unsafe_get src (src_off + i)) done

let bigarray_blit src src_off dst dst_off len =
  Bigarray.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let write_string =
  let length = String.length in
  let blit = bigarray_blit_from_string in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_bytes =
  let length = Bytes.length in
  let blit = bigarray_blit_from_bytes in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_bigstring =
  let length = Bigarray.Array1.dim in
  let blit = bigarray_blit in
  fun ?(off = 0) ?len a k t ->
    write k ~blit ~length ~off ?len a t

let write_char =
  let length a = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0);
    assert (len = 1);
    EndianBigstring.BigEndian_unsafe.set_char dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

let write_uint8 =
  let length a = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0);
    assert (len = 1);
    EndianBigstring.BigEndian_unsafe.set_int8 dst dst_off src
  in
  fun a k t -> write k ~length ~blit ~off:0 ~len:1 a t

module type EndianBigstringSig = EndianBigstring.EndianBigstringSig
module type EndianBytesSig = EndianBytes.EndianBytesSig

module MakeE (EBigstring : EndianBigstringSig) =
struct
  let _length a = assert false

  let write_uint16 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 2);
      EBigstring.set_int16 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:2 a t

  let write_uint32 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 4);
      EBigstring.set_int32 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:4 a t

  let write_uint64 =
    let length = _length in
    let blit src src_off dst dst_off len =
      assert (src_off = 0);
      assert (len = 8);
      EBigstring.set_int64 dst dst_off src
    in
    fun a k t -> write k ~length ~blit ~off:0 ~len:8 a t
end

module LE = MakeE(EndianBigstring.LittleEndian_unsafe)
module BE = MakeE(EndianBigstring.BigEndian_unsafe)
