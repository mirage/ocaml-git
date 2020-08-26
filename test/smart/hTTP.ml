module None = struct
  type input = Cstruct.t

  and output = Cstruct.t

  type +'a io = 'a Lwt.t

  type endpoint = string Queue.t

  type flow = string Queue.t

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect x = Lwt.return_ok x

  let recv _ _ = assert false

  let send _ _ = assert false

  let close _ = assert false
end

(* XXX(dinosaure): just pass the given value and
 * inherits the totality axiom of [conduit]. *)

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

let protocol = Conduit_lwt.register ~protocol:(module None)

module Protocol = struct
  include (val Conduit_lwt.repr protocol)
end

type error = Conduit_lwt.error

let pp_error = Conduit_lwt.pp_error

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let get ~resolvers ?headers:_ _uri =
  Conduit_lwt.resolve resolvers ~protocol localhost >>? function
  | Protocol.T (Conduit.Value queue) ->
      let v = Queue.pop queue in
      Lwt.return_ok ((), v)
  | _ -> assert false

let post ~resolvers ?headers:_ _uri _contents =
  Conduit_lwt.resolve resolvers ~protocol localhost >>? function
  | Protocol.T (Conduit.Value queue) ->
      let v = Queue.pop queue in
      Lwt.return_ok ((), v)
  | _ -> assert false
