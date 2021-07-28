open Lwt.Infix

type flow = {
  mutable i : Cstruct.t;
  mutable o : Cstruct.t;
  mutable c : bool;
  push : Cstruct.t -> unit;
}

type endpoint = string list * (Cstruct.t -> unit)
type error = |
type write_error = [ `Closed ]

let pp_error : error Fmt.t = fun _ppf -> function _ -> .
let closed_by_peer = "Closed by peer"
let pp_write_error ppf = function `Closed -> Fmt.string ppf closed_by_peer

let connect (i, push) =
  let i = String.concat "" i in
  let i = Cstruct.of_string i in
  Lwt.return_ok { i; o = Cstruct.create 0; c = false; push }

let read flow =
  if Cstruct.length flow.i = 0 then (
    flow.c <- true;
    Lwt.return_ok `Eof)
  else
    let res = Cstruct.create 0x1000 in
    let len = min (Cstruct.length res) (Cstruct.length flow.i) in
    Cstruct.blit flow.i 0 res 0 len;
    flow.i <- Cstruct.shift flow.i len;
    Lwt.return_ok (`Data (Cstruct.sub res 0 len))

let ( <.> ) f g x = f (g x)

let write flow str =
  if flow.c then Lwt.return_error `Closed
  else (
    flow.o <- Cstruct.append flow.o str;
    Lwt.return_ok ())

let writev flow sstr =
  let rec go = function
    | [] -> Lwt.return_ok ()
    | hd :: tl -> (
        write flow hd >>= function
        | Ok () -> go tl
        | Error _ as err -> Lwt.return err)
  in
  go sstr

let close flow =
  flow.c <- true;
  flow.push flow.o;
  Lwt.return ()
