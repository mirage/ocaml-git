type input = Cstruct.t
type output = Cstruct.t
type +'a io = 'a Lwt.t
type flow = { mutable i : Cstruct.t; mutable o : Cstruct.t; mutable c : bool }
type endpoint = string list
type error = [ `Closed ]

let closed_by_peer = "Closed by peer"
let pp_error ppf = function `Closed -> Fmt.string ppf closed_by_peer

let connect i =
  let i = String.concat "" i in
  let i = Cstruct.of_string i in
  Lwt.return_ok { i; o = Cstruct.create 0x1000; c = false }

let recv flow buf =
  if Cstruct.len flow.i = 0 then (
    flow.c <- true;
    Lwt.return_ok `End_of_flow )
  else
    let len = min (Cstruct.len buf) (Cstruct.len flow.i) in
    Cstruct.blit flow.i 0 buf 0 len;
    flow.i <- Cstruct.shift flow.i len;
    Lwt.return_ok (`Input len)

let ( <.> ) f g x = f (g x)

let send flow str =
  if flow.c then Lwt.return_error `Closed
  else (
    flow.o <- Cstruct.append flow.o str;
    Lwt.return_ok (Cstruct.len str) )

let close flow =
  flow.c <- true;
  Lwt.return_ok ()
