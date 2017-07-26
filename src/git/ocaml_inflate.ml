open Decompress

type t = (B.bs, B.bs) Inflate.t
type error = [ `Inflate of Inflate.error ]
type window = B.bs Window.t

let pp_error fmt = function `Inflate err -> Format.fprintf fmt "(`Inflate @[<hov>%a@])" Inflate.pp_error err
let pp = Inflate.pp

let window_reset = Window.reset
let window () = Window.create ~proof:B.proof_bigstring

let default = Inflate.default

let eval src dst t =
  let src' = B.from_bigstring (Cstruct.to_bigarray src) in
  let dst' = B.from_bigstring (Cstruct.to_bigarray dst) in
  Inflate.eval src' dst' t
  |> function `Error (t, err) -> `Error (t, `Inflate err)
            | (`Await _ | `Flush _ | `End _) as x -> x

let used_in = Inflate.used_in
let used_out = Inflate.used_out
let write = Inflate.write

let flush = Inflate.flush
let refill = Inflate.refill
