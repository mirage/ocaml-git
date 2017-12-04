open Decompress

type t = (B.bs, B.bs) Deflate.t
and error = Deflate.error

let pp_error: error Fmt.t = Deflate.pp_error

let default level: t = Deflate.default ~proof:B.proof_bigstring level

let finish: t -> t = Deflate.finish
let used_in: t -> int = Deflate.used_in
let used_out: t -> int = Deflate.used_out
let no_flush: int -> int -> t -> t = Deflate.no_flush
let sync_flush: int -> int -> t -> t = Deflate.sync_flush
let flush: int -> int -> t -> t = Deflate.flush

let eval ~src:src' ~dst:dst' t:
  [ `Await of t
  | `Flush of t
  | `Error of t * error
  | `End of t ]
  =
  let src = B.from_bigstring @@ Cstruct.to_bigarray src' in
  let dst = B.from_bigstring @@ Cstruct.to_bigarray dst' in

  Deflate.eval src dst t
