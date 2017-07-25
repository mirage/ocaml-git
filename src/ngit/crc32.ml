type t = private int32

external crc32_bytes : t -> Bytes.t -> int -> int -> t = "caml_st_crc32"
external crc32_bigarray : t -> Cstruct.buffer -> int -> int -> t = "caml_ba_crc32"

let digest v ?(off = 0) ?len c =
  let len = match len with
    | Some len -> len
    | None -> Cstruct.len c
  in
  let b = Cstruct.to_bigarray c in
  crc32_bigarray v b off len

let digestv v cs = List.fold_left (fun v c -> digest v c) v cs

let digestc v byte = crc32_bytes v (Bytes.make 1 (Char.unsafe_chr byte)) 0 1

let digests v ?(off = 0) ?len s =
  let len = match len with
    | Some len -> len
    | None -> Bytes.length s
  in
  crc32_bytes v s off len

external of_int32 : int32 -> t = "%identity"
external to_int32 : t -> int32 = "%identity"

let pp fmt x =
  Format.fprintf fmt "%08lx" (to_int32 x)

let default = of_int32 0l

let eq : t -> t -> bool = fun a b -> Int32.equal (to_int32 a) (to_int32 b)
let neq : t -> t -> bool = fun a b -> not (eq a b)
