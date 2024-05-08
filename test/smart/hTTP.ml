open Lwt.Infix

type state = Handshake | Get | Post | Error
type flow = { advertised_refs : string; clone : string; mutable state : state }
type error = [ `Msg of string ]
type write_error = [ `Closed | `Msg of string ]

let pp_error ppf (`Msg err) = Fmt.string ppf err

let pp_write_error ppf = function
  | `Closed -> Fmt.string ppf "Connection closed by peer"
  | `Msg err -> Fmt.string ppf err

let write t _cs =
  match t.state with
  | Handshake | Get -> Lwt.return_error (`Msg "Handshake has not been done")
  | Error -> Lwt.return_error (`Msg "Handshake got an error")
  | Post -> Lwt.return_ok ()

let writev t css =
  let rec go = function
    | [] -> Lwt.return_ok ()
    | x :: r -> (
        write t x >>= function
        | Ok () -> go r
        | Error _ as err -> Lwt.return err)
  in
  go css

let read t =
  match t.state with
  | Handshake -> Lwt.return_error (`Msg "Handshake has not been done")
  | Error -> Lwt.return_error (`Msg "Handshake got an error")
  | Get ->
      t.state <- Post;
      Lwt.return_ok (`Data (Cstruct.of_string t.advertised_refs))
  | Post -> Lwt.return_ok (`Data (Cstruct.of_string t.clone))

let close _ = Lwt.return_unit

let shutdown _ _ = Lwt.return_unit

type endpoint = string * string

let connect (advertised_refs, clone) =
  Lwt.return_ok { advertised_refs; clone; state = Handshake }
