open Decompress

type t = (B.bs, B.bs) Deflate.t
and error = Deflate.error

let pp_error = Deflate.pp_error

let default level = Deflate.default ~proof:B.proof_bigstring 4

let finish     = Deflate.finish
let used_in    = Deflate.used_in
let used_out   = Deflate.used_out
let no_flush   = Deflate.no_flush
let sync_flush = Deflate.sync_flush
let flush      = Deflate.flush

let eval src' dst' t =
  let src = B.from_bigstring @@ Cstruct.to_bigarray src' in
  let dst = B.from_bigstring @@ Cstruct.to_bigarray dst' in

  Deflate.eval src dst t
