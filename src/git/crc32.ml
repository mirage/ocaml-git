let digestc crc byte =
  Checkseum.Crc32.digest_string
    (String.make 1 (Char.chr (byte land 0xff)))
    0 1 crc

let digests ?off ?len crc str =
  let off, len =
    match (off, len) with
    | Some off, Some len -> (off, len)
    | Some off, None -> (off, String.length str - off)
    | None, Some len -> (0, len)
    | None, None -> (0, String.length str) in
  Checkseum.Crc32.digest_string str off len crc

let default = Checkseum.Crc32.default

let digestb ~off ~len crc buf =
  Checkseum.Crc32.digest_bigstring (Cstruct.to_bigarray buf) off len crc

let pp = Optint.pp

let to_int32 = Optint.to_int32

let of_int32 = Optint.of_int32
