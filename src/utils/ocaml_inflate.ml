open Decompress

type t = (B.bs, B.bs) Inflate.t
type error = [ `Inflate of Inflate.error ]
type window = B.bs Window.t

module Log =
struct
  let src = Logs.Src.create "decompress.inflate" ~doc:"logs inflate event"
  include (val Logs.src_log src : Logs.LOG)
end

let pp_error : error Fmt.t = fun ppf -> function `Inflate err ->
  Fmt.pf ppf "(`Inflate %a)" (Fmt.hvbox Inflate.pp_error) err

let pp : t Fmt.t = Inflate.pp

let window_reset : window -> window = Window.reset
let window () = Window.create ~proof:B.proof_bigstring

let default : window -> t = Inflate.default

let eval ~src ~dst t : [ `Await of t | `End of t | `Error of t * error | `Flush of t ] =
  Log.debug (fun l -> l "evaluation of %a." (Fmt.hvbox Inflate.pp) t);

  let src' = B.from_bigstring (Cstruct.to_bigarray src) in
  let dst' = B.from_bigstring (Cstruct.to_bigarray dst) in
  Inflate.eval src' dst' t
  |> function `Error (t, err) -> `Error (t, `Inflate err)
            | (`Await _ | `Flush _ | `End _) as x -> x

let used_in  : t -> int = Inflate.used_in
let used_out : t -> int = Inflate.used_out
let write    : t -> int = Inflate.write

let flush    : int -> int -> t -> t = Inflate.flush
let refill   : int -> int -> t -> t = Inflate.refill
